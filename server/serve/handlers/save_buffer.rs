use crate::diff_view::change_leaves::BufferReadinessSummary;
use crate::dbs::in_rust_graph::{
  in_rust_graph_coherent_with_nodeInstructions_in, new_handle };
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::from_text
  ::buffer_to_validated_saveplan_with_fork_skgrepos_in_graph;
use crate::from_text::strandeds::{
  StrandedsReview, StrandedRelationship,
  raised_relationship_warning, reset_to_default_relationships,
  stranded_relationships};
use crate::git_ops::diff::compute_diff_for_skgrepo;
use crate::save::{
  apply_delete_propagation_cleanup,
  DeleteFrom,
  PreparedSave, prepare_save_under_mutation_gate,
  restricted_records_naming_deleted_graphnodes,
};
use crate::serve::ViewsState;
use crate::skgrepo_sets::{SkgrepoRestriction, require_unrestricted};
use crate::serve::protocol::TcpToClient;
use crate::serve::handlers::telescope_hoist::{
  HoistCandidate,
  approved_pids_from_request as hoist_approved_pids_from_request,
  candidates_from_disk as hoist_candidates_from_disk,
  confirmation_response as hoist_confirmation_response,
  needs_confirmation as hoist_needs_confirmation,
  repair_saves_for_unwritten_candidates,
};
use crate::diff_view::readiness_requests::{
  consume_readiness_requests, viewforest_has_readiness_requests};
use crate::diff_view::restore_requests::{
  add_restore_instructions, viewforest_has_restore_requests};
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::serve::handlers::text_release::{
  TextReleaseDecision, save_rerender_challenge_response};
use crate::serve::handlers::save_dependencies::{
  SaveAffectedIds, dirty_view_conflicts, format_conflict_error,
};
use crate::serve::handlers::save_staleness::{
  WhatTheBaselineShows, left_as_the_graph_has_it_warning, stale_write_errors,
  what_the_baseline_shows};
use crate::serve::util::{
  viewforest_id_from_request,
  buffer_response_entries,
  format_fork_confirmation_response_sexp,
  format_lock_views_sexp,
  read_length_prefixed_content,
  send_response_with_length_prefix,
  sexp_to_client_string,
  tag_sexp_response,
  value_from_request_sexp };
use crate::from_text::fork::build_fork_confirmation_buffer;
use crate::types::env::SkgEnv;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::git::SkgrepoDiff;
use crate::types::misc::{ID, SkgrepoName, SkgConfig};
use crate::types::save::{
  NodeInstruction, PostSkgsaveCommitNoticeCandidate, SavePlan, format_save_error_as_org };
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{DiffViewBufferKind, ViewforestId, ViewnodeIdRecord};
use crate::rerender_pass::update_views_after_save;

use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;
use std::path::Path;

/// The ordinary terminal message in the save protocol. Hoist, fork, and
/// text-release confirmations are alternative terminal messages.
/// Contains the re-rendered saved buffer and any warnings/errors when sent.
/// See <docs/api-and-formats.org § Save buffer> for the full sequence:
///   save-lock → save-relax-lock → collateral-view* → save-result.
pub struct SaveResponse {
  pub saved_view          : String,
  pub errors              : Vec<String>,
  pub warnings            : Vec<String>,
  pub save_point_position : Option<SavePointPosition>,
  /// Some when this save found fork candidates and was not pre-approved:
  /// nothing was skgsave-committed, 'saved_view' instead holds the write-protected
  /// fork-confirmation buffer, and this holds the one-line minibuffer
  /// prompt. The handler then sends a 'fork-confirmation' message rather
  /// than 'save-result'. None for an ordinary save.
  pub fork_confirmation   : Option<String>,
  /// Some when this save found an overPrivateText current disk telescope without an
  /// exact publication approval. This is checked before fork confirmation;
  /// nothing was skgsave-committed and the response contains no title/body text.
  pub hoist_confirmation  : Option<Vec<HoistCandidate>>,
  /// Fully tagged overPrivateText-telescope-confirmation response produced after the
  /// save's rerenders have been staged but before any text is released.
  /// It names the views whose rerender was withheld, which a "rerender
  /// views" request releases ('rerender_views.rs').
  pub text_release_confirmation : Option<String>,
  /// Some when a home skgrepo move would strand relationships and the request
  /// carried no review of them: nothing was skgsave-committed, and the
  /// handler sends a 'strandeds-confirmation' listing these. Checked
  /// before every other confirmation.
  pub strandeds_confirmation : Option<Vec<StrandedRelationship>>,
  /// Some when the save's deletes would strip records the skgrepo
  /// restriction keeps out and the request carried no
  /// '(delete-from ...)': nothing was skgsave-committed, and the
  /// handler sends a 'delete-from-confirmation' naming this count of
  /// such records. Checked before every other confirmation.
  pub delete_from_confirmation : Option<usize>,
  /// Some when the saved buffer is one of the diff view's: the
  /// readiness summary the client shows after the save.
  pub readiness_summary   : Option<BufferReadinessSummary>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClientViewSnapshot {
  pub viewforest_id : ViewforestId,
  pub dirty    : bool,
  pub baseline : Option<String>,
  pub current  : Option<String>,
}

pub struct SaveRequestEnvelope {
  pub saved_buffer : String,
  /// The saved buffer's baseline: its text as the server last drew it,
  /// before any edit. A new empty view's is the text the client
  /// created it with.
  pub baseline     : String,
  pub other_views  : Vec<ClientViewSnapshot>,
}

#[derive(Clone)]
pub struct SavePointPosition {
  pub point_lines_below_focused_headline    : usize,
  pub point_column                          : usize,
  pub point_screen_lines_below_window_start : usize,
}

impl SaveResponse {
  /// The response of a save that skgsave-committed and rerendered.
  pub fn saved (
    saved_view         : String,
    errors             : Vec<String>,
    warnings           : Vec<String>,
    readiness_summary  : Option<BufferReadinessSummary>,
  ) -> SaveResponse {
    SaveResponse { saved_view, errors, warnings, readiness_summary,
                   .. SaveResponse::empty () } }

  /// A save that skgsave-committed nothing, asking to review the
  /// relationships its home skgrepo moves would strand.
  pub fn strandeds_confirmation (
    warnings  : Vec<String>,
    strandeds : Vec<StrandedRelationship>,
  ) -> SaveResponse {
    SaveResponse { warnings,
                   strandeds_confirmation : Some (strandeds),
                   .. SaveResponse::empty () } }

  /// A save that skgsave-committed nothing, asking where its deletes
  /// may strip the deleted IDs: 'count' records the skgrepo restriction
  /// keeps out name a deleted graphnode.
  pub fn delete_from_confirmation (
    warnings : Vec<String>,
    count    : usize,
  ) -> SaveResponse {
    SaveResponse { warnings,
                   delete_from_confirmation : Some (count),
                   .. SaveResponse::empty () } }

  /// A save that skgsave-committed nothing, asking to approve
  /// publishing overPrivateText telescopes.
  pub fn hoist_confirmation (
    warnings   : Vec<String>,
    candidates : Vec<HoistCandidate>,
  ) -> SaveResponse {
    SaveResponse { warnings,
                   hoist_confirmation : Some (candidates),
                   .. SaveResponse::empty () } }

  /// A save that skgsave-committed nothing, holding the fork-confirmation
  /// buffer and its prompt.
  pub fn fork_confirmation (
    warnings : Vec<String>,
    buffer   : String,
    prompt   : String,
  ) -> SaveResponse {
    SaveResponse { saved_view : buffer, warnings,
                   fork_confirmation : Some (prompt),
                   .. SaveResponse::empty () } }

  /// A save whose rerender holds text the skgrepo restriction may not
  /// release without approval: the challenge, naming the views whose
  /// rerender is withheld, and no text.
  pub fn text_release_challenge (
    decision                : &TextReleaseDecision,
    withheld_viewforest_ids : &[ViewforestId],
  ) -> SaveResponse {
    SaveResponse { text_release_confirmation :
                     save_rerender_challenge_response (
                       decision, withheld_viewforest_ids ),
                   .. SaveResponse::empty () } }

  fn empty (
  ) -> SaveResponse {
    SaveResponse {
      saved_view                : String::new (),
      errors                    : Vec::new (),
      warnings                  : Vec::new (),
      save_point_position       : None,
      fork_confirmation         : None,
      hoist_confirmation        : None,
      text_release_confirmation : None,
      strandeds_confirmation    : None,
      delete_from_confirmation  : None,
      readiness_summary         : None, } }

  /// Format: ((content "...") (errors (...)) (warnings (...))
  /// POINT... (readiness-summary ...)), where the point entries and
  /// the readiness summary are there when the save has them.
  fn to_sexp_string (&self) -> String {
    let mut items : Vec<Sexp> = buffer_response_entries (
      & self . saved_view, & self . errors, & self . warnings );
    if let Some (point_position) = &self . save_point_position {
      push_save_point_position_to_sexp_items (&mut items, point_position); }
    if let Some (readiness_summary) = &self . readiness_summary {
      items . push (readiness_summary . to_sexp ()); }
    sexp_to_client_string (&Sexp::List (items)) }}

/// Handles save buffer requests from Emacs.
/// - Reads the buffer content (with length prefix).
/// - Sends the early broad lock (viewforest_ids_of_views_to_lock) before the slow pipeline.
/// - Runs 'update_from_and_rerender_buffer' (parse + validate -> SavePlan, update
///   the graph, then rerender + stream the saved and collateral views).
/// - Responds to Emacs (with length prefix).
pub fn handle_save_buffer_request (
  reader     : &mut BufReader <TcpStream>,
  stream     : &mut TcpStream, // PITFALL: writes to the same TCP stream as 'reader'
  request    : &str,
  env        : &mut SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let viewforest_id_from_request_result : Result<ViewforestId, String> =
    viewforest_id_from_request (request);
  let save_point_position : Option<SavePointPosition> =
    save_point_position_from_request (request);
  let approved_forks : bool =
    // Forking is gated on confirmation: the first save returns a
    // fork-confirmation buffer, and the client re-issues the save with
    // this field once the user approves (the same retry shape used by
    // other request-scoped confirmations).
    fork_approved_from_request (request);
  let fork_skgrepos : HashMap<ID, SkgrepoName> =
    // The per-fork clone skgrepos the user chose in the confirmation
    // buffer, riding back on the approve re-save. Empty otherwise.
    fork_skgrepos_from_request (request);
  let approved_hoist_pids : HashSet<ID> =
    hoist_approved_pids_from_request (request);
  let strandeds_review : StrandedsReview =
    strandeds_review_from_request (request);
  let delete_from : DeleteFrom =
    delete_from_from_request (request);
  let named_diffView_buffer_kind_result : Result<Option<DiffViewBufferKind>, String> =
    named_diffView_buffer_kind_from_request (request);
  { // Send the early broad lock before reading the buffer, so the client's
    // one-shot save-lock handler always fires exactly once and balances its
    // pending-count -- even when the read below fails (otherwise only
    // save-result would arrive, leaving the count unbalanced and wedging the
    // next save's wait). save-result unlocks regardless. Conservative/broad
    // here: the SavePlan is not yet computed.
    let viewforest_ids_to_lock : Vec<ViewforestId> =
      viewforest_ids_of_views_to_lock (
        &viewforest_id_from_request_result, views_state );
    let lock_sexp : String =
      format_lock_views_sexp ( &viewforest_ids_to_lock );
    send_response_with_length_prefix (
      stream,
      & tag_sexp_response ( TcpToClient::SaveLock, &lock_sexp )); }
  match ( read_length_prefixed_content (reader)
            .and_then (|content| parse_save_request_envelope (
              &content, viewforest_id_from_request_result . as_ref () . ok ())),
          named_diffView_buffer_kind_result )
  {
    (Ok (_envelope), Err (error_msg)) => {
      // The envelope was read, so the stream stays in step; the save is refused.
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          TcpToClient::SaveResult,
          & sexp_to_client_string ( &empty_response_sexp (
            &error_msg, &[], &save_point_position )) )); }
    (Ok (envelope), Ok (named_diffView_buffer_kind)) => {
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "update_from_and_rerender_buffer" ). entered();
        match block_on(
          update_from_and_rerender_buffer_with_approvals (
            stream,
            & envelope . saved_buffer,
            & envelope . baseline,
            env,
            &viewforest_id_from_request_result,
            views_state,
            skgrepo_restriction,
            approved_forks,
            &fork_skgrepos,
            &approved_hoist_pids,
            &strandeds_review,
            delete_from,
            &envelope . other_views,
            named_diffView_buffer_kind ))
        { Ok (mut save_response) => {
            save_response . save_point_position =
              save_point_position . clone ();
            if let Some (count) = save_response . delete_from_confirmation {
              send_response_with_length_prefix (
                stream,
                & tag_sexp_response (
                  TcpToClient::DeleteFromConfirmation,
                  & delete_from_confirmation_response (count) ));
              return; }
            if let Some (strandeds) = &save_response . strandeds_confirmation {
              send_response_with_length_prefix (
                stream,
                & tag_sexp_response (
                  TcpToClient::StrandedsConfirmation,
                  & strandeds_confirmation_response (strandeds) ));
              return; }
            match (&save_response . hoist_confirmation,
                   &save_response . fork_confirmation,
                   &save_response . text_release_confirmation) {
              (Some (candidates), _, _) =>
                send_response_with_length_prefix (
                  stream,
                  & tag_sexp_response (
                    TcpToClient::TelescopeHoistConfirmation,
                    &hoist_confirmation_response (candidates) )),
              (None, Some (to_minibuffer), _) =>
                // A save that found forks and was not approved: nothing
                // skgsave-committed; send the confirmation buffer instead of a
                // save-result.
                send_response_with_length_prefix (
                  stream,
                  & tag_sexp_response (
                    TcpToClient::ForkConfirmation,
                    & format_fork_confirmation_response_sexp (
                      & save_response . saved_view, to_minibuffer ))),
              (None, None, Some (confirmation)) =>
                send_response_with_length_prefix (stream, confirmation),
              (None, None, None) =>
                send_response_with_length_prefix (
                  stream,
                  & tag_sexp_response (
                    TcpToClient::SaveResult,
                    & save_response . to_sexp_string () )), }}
          Err (err) => { // Check if this is a SaveError that should be formatted for the client
            if let Some (save_error) = err . downcast_ref::<SaveError>() {
              // Warnings always accompany errors (decided 2026-06-12):
              // a failed validation carries the parse-time warnings it
              // collected before aborting.
              let warnings : &[String] = match save_error {
                SaveError::BufferValidationErrors { warnings, .. } =>
                  warnings,
                _ => &[], };
              let response_sexp : String =
                sexp_to_client_string ( &empty_response_sexp (
                  & format_save_error_as_org (save_error),
                  warnings,
                  & save_point_position ));
              send_response_with_length_prefix (
                stream,
                & tag_sexp_response (
                  TcpToClient::SaveResult, & response_sexp ));
            } else {
              let error_msg : String =
                format!("Error processing buffer content: {}", err);
              tracing::error!("{}", error_msg);
              let response_sexp : String =
                sexp_to_client_string ( &empty_response_sexp (
                  &error_msg,
                  &[],
                  &save_point_position ));
              send_response_with_length_prefix (
                stream,
                & tag_sexp_response (
                  TcpToClient::SaveResult, &response_sexp )); }} }}; }
    (Err (err), _) => {
      let error_msg : String =
        format! ("NOTHING WAS SAVED\n\nError reading save payload: {}", err );
      tracing::error! ( "{}", error_msg );
      let response_sexp : String =
        sexp_to_client_string ( &empty_response_sexp (
          &error_msg,
          &[],
          &save_point_position ));
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          TcpToClient::SaveResult, &response_sexp )); }} }

pub fn parse_save_request_envelope (
  content   : &str,
  saved_viewforest_id : Option<&ViewforestId>,
) -> Result<SaveRequestEnvelope, Box<dyn Error>> {
  let parsed : Sexp = sexp::parse (content)
    .map_err (|error| format! ("Malformed save envelope: {}", error)) ?;
  let fields : &[Sexp] = sexp_list (&parsed, "save envelope") ?;
  let saved_buffer_value : &Sexp = unique_field (
    fields, "saved-buffer", "save envelope") ?;
  let saved_buffer : String = sexp_atom_string (
    saved_buffer_value, "saved-buffer") ?;
  let baseline : String = sexp_atom_string (
    unique_field (fields, "baseline", "save envelope") ?,
    "baseline") ?;
  let other_views_value : &Sexp = unique_field (
    fields, "other-views", "save envelope") ?;
  let other_view_entries : &[Sexp] = sexp_list (
    other_views_value, "other-views") ?;
  let mut other_views : Vec<ClientViewSnapshot> = Vec::new ();
  let mut seen_viewforest_ids : HashSet<ViewforestId> = HashSet::new ();
  for entry in other_view_entries {
    let entry_fields : &[Sexp] = sexp_list (entry, "other view") ?;
    let viewforest_id : ViewforestId = ViewforestId::from_client_string (sexp_atom_string (
      unique_field (entry_fields, "view-id", "other view") ?,
      "other view-id") ?);
    if saved_viewforest_id == Some (&viewforest_id) {
      return Err (format! (
        "Malformed save envelope: saved view ID {} appears in other-views",
        viewforest_id . repr_in_client ()) . into ()); }
    if ! seen_viewforest_ids . insert (viewforest_id . clone ()) {
      return Err (format! (
        "Malformed save envelope: duplicate other view ID {}",
        viewforest_id . repr_in_client ()) . into ()); }
    let dirty_text : String = sexp_atom_string (
      unique_field (entry_fields, "dirty", "other view") ?, "dirty") ?;
    let dirty : bool = match dirty_text . as_str () {
      "true"  => true,
      "false" => false,
      _ => return Err (format! (
        "Malformed save envelope: dirty must be true or false for {}",
        viewforest_id . repr_in_client ()) . into ()), };
    let baseline_field : Option<&Sexp> = optional_unique_field (
      entry_fields, "baseline", "other view") ?;
    let current_field : Option<&Sexp> = optional_unique_field (
      entry_fields, "current", "other view") ?;
    let (baseline, current) : (Option<String>, Option<String>) = if dirty {
      let baseline : Option<String> = parse_baseline (
        baseline_field . ok_or_else (|| format! (
          "Malformed save envelope: dirty view {} has no baseline field",
          viewforest_id . repr_in_client ())) ?) ?;
      let current : String = sexp_atom_string (
        current_field . ok_or_else (|| format! (
          "Malformed save envelope: dirty view {} has no current text",
          viewforest_id . repr_in_client ())) ?, "current") ?;
      (baseline, Some (current))
    } else {
      if baseline_field . is_some () || current_field . is_some () {
        return Err (format! (
          "Malformed save envelope: clean view {} carries dirty snapshots",
          viewforest_id . repr_in_client ()) . into ()); }
      (None, None)
    };
    for field in entry_fields {
      let (key, _) : (&str, &Sexp) = field_pair (field, "other view") ?;
      if ! ["view-id", "dirty", "baseline", "current"] . contains (&key) {
        return Err (format! (
          "Malformed save envelope: unknown other-view field {}", key)
          . into ()); }}
    other_views . push (ClientViewSnapshot {
      viewforest_id, dirty, baseline, current, }); }
  for field in fields {
    let (key, _) : (&str, &Sexp) = field_pair (field, "save envelope") ?;
    if ! ["saved-buffer", "baseline", "other-views"] . contains (&key) {
      return Err (format! (
        "Malformed save envelope: unknown field {}", key) . into ()); }}
  Ok (SaveRequestEnvelope { saved_buffer, baseline, other_views })
}

fn parse_baseline (
  value : &Sexp,
) -> Result<Option<String>, Box<dyn Error>> {
  match value {
    Sexp::Atom (Atom::S (status)) if status == "unavailable" => Ok (None),
    Sexp::List (items) if items . len () == 2 => {
      let status : String = sexp_atom_string (&items [0], "baseline status") ?;
      if status != "present" {
        return Err ("Malformed save envelope: baseline wrapper must begin with present" . into ()); }
      Ok (Some (sexp_atom_string (&items [1], "baseline text") ?)) }
    _ => Err ("Malformed save envelope: baseline must be unavailable or (present TEXT)" . into ()), }
}

fn unique_field<'a> (
  fields  : &'a [Sexp],
  key     : &str,
  context : &str,
) -> Result<&'a Sexp, Box<dyn Error>> {
  optional_unique_field (fields, key, context) ? . ok_or_else (||
    format! ("Malformed {}: missing {}", context, key) . into ())
}

fn optional_unique_field<'a> (
  fields  : &'a [Sexp],
  key     : &str,
  context : &str,
) -> Result<Option<&'a Sexp>, Box<dyn Error>> {
  let mut matches : Vec<&Sexp> = Vec::new ();
  for field in fields {
    let (candidate, value) : (&str, &Sexp) = field_pair (field, context) ?;
    if candidate == key { matches . push (value); }}
  match matches . as_slice () {
    []      => Ok (None),
    [value] => Ok (Some (*value)),
    _       => Err (format! (
      "Malformed {}: duplicate {}", context, key) . into ()), }
}

fn field_pair<'a> (
  value   : &'a Sexp,
  context : &str,
) -> Result<(&'a str, &'a Sexp), Box<dyn Error>> {
  let items : &[Sexp] = sexp_list (value, context) ?;
  if items . len () != 2 {
    return Err (format! (
      "Malformed {}: each field must contain a key and value", context)
      . into ()); }
  let key : &str = match &items [0] {
    Sexp::Atom (Atom::S (key)) => key,
    _ => return Err (format! (
      "Malformed {}: field key must be an atom", context) . into ()), };
  Ok ((key, &items [1]))
}

fn sexp_list<'a> (
  value   : &'a Sexp,
  context : &str,
) -> Result<&'a [Sexp], Box<dyn Error>> {
  match value {
    Sexp::List (items) => Ok (items),
    _ => Err (format! ("Malformed {}: expected a list", context) . into ()), }
}

fn sexp_atom_string (
  value   : &Sexp,
  context : &str,
) -> Result<String, Box<dyn Error>> {
  match value {
    Sexp::Atom (Atom::S (text)) => Ok (text . clone ()),
    _ => Err (format! ("Malformed {}: expected text", context) . into ()), }
}

/// Create an s-expression with nil content and an error message.
fn empty_response_sexp (
  error_buffer_content : &str,
  warnings             : &[String],
  save_point_position   : &Option<SavePointPosition>,
) -> Sexp {
  let mut items : Vec<Sexp> = vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( "nil" . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "errors" . to_string () )),
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S (
          error_buffer_content . to_string () )) ] ) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "warnings" . to_string () )),
      Sexp::List (
        warnings . iter ()
          . map ( |warning| Sexp::Atom (
            Atom::S ( warning . clone () )) )
          . collect () ) ] ) ];
  if let Some (point_position) = save_point_position {
    push_save_point_position_to_sexp_items (
      &mut items, point_position ); }
  Sexp::List (items) }

fn push_save_point_position_to_sexp_items (
  items          : &mut Vec<Sexp>,
  point_position : &SavePointPosition,
) {
  items . push (
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S (
        "point-lines-below-focused-headline" . to_string () )),
      Sexp::Atom ( Atom::I (
        point_position . point_lines_below_focused_headline
        as i64 )), ] ));
  items . push (
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S (
        "point-column" . to_string () )),
      Sexp::Atom ( Atom::I (
        point_position . point_column
        as i64 )), ] ));
  items . push (
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S (
        "point-screen-lines-below-window-start" . to_string () )),
      Sexp::Atom ( Atom::I (
        point_position . point_screen_lines_below_window_start
        as i64 )), ] )); }

/// The request's '(diff-view-buffer . "NAME")', naming the buffer of
/// the diff view the save is of ('DiffViewBufferKind::wire_name'); None
/// without one. A name no buffer has refuses the save.
fn named_diffView_buffer_kind_from_request (
  request : &str,
) -> Result<Option<DiffViewBufferKind>, String> {
  match value_from_request_sexp ("diff-view-buffer", request) {
    Err (_) => Ok (None),
    Ok (name) => DiffViewBufferKind::from_wire_name (&name)
      . map (Some)
      . ok_or_else ( || format! (
        "The save request names the diff view buffer {:?}, which the diff view does not have. Nothing was saved.",
        name )), }}

fn save_point_position_from_request (
  request : &str,
) -> Option<SavePointPosition> {
  let point_lines_below_focused_headline : usize =
    nat_from_request (
      request,
      "point-lines-below-focused-headline" ) ?;
  let point_column : usize =
    nat_from_request (
      request,
      "point-column" ) ?;
  let point_screen_lines_below_window_start : usize =
    nat_from_request (
      request,
      "point-screen-lines-below-window-start" ) ?;
  Some ( SavePointPosition {
    point_lines_below_focused_headline,
    point_column,
    point_screen_lines_below_window_start, } ) }

fn nat_from_request (
  request : &str,
  key     : &str,
) -> Option<usize> {
  value_from_request_sexp (key, request) . ok () ? . parse () . ok () }

/// The request's review of the relationships a home skgrepo move strands:
/// '(strandeds-reviewed . "true")' marks a re-save after a
/// strandeds-confirmation, and '(reset-strandeds (RELATION RECORDER
/// MEMBER) ...)' names the ones the user chose to reset to default. Without
/// the first, not reviewed.
fn strandeds_review_from_request (
  request : &str,
) -> StrandedsReview {
  let reviewed : bool =
    value_from_request_sexp ("strandeds-reviewed", request)
    . map ( |v| v == "true" )
    . unwrap_or (false);
  if ! reviewed { return StrandedsReview::NotReviewed; }
  let mut chosen : HashSet<RelationshipKey> = HashSet::new ();
  let Ok (Sexp::List (items)) = sexp::parse (request)
    else { return StrandedsReview::Reviewed (chosen); };
  for item in &items {
    let Sexp::List (field) = item else { continue; };
    let Some ( Sexp::Atom ( Atom::S (key) )) = field . first ()
      else { continue; };
    if key != "reset-strandeds" { continue; }
    for entry in &field [1..] {
      if let Sexp::List (triple) = entry {
        if let [ Sexp::Atom ( Atom::S (relation) ),
                 Sexp::Atom ( Atom::S (recorder) ),
                 Sexp::Atom ( Atom::S (member) ) ] = & triple [..]
        { if let Some (relation) = NodeRelation::from_relation_name (relation) {
            chosen . insert ( RelationshipKey {
              relation,
              recorder : ID::from (recorder . as_str ()),
              member   : ID::from (member . as_str ()), } ); }} }} }
  StrandedsReview::Reviewed (chosen) }

/// The strandeds-confirmation payload: each stranded relationship (its
/// relation, recorder and member IDs, where the save would leave it
/// and its new default), and a prompt. IDs only, no titles: the
/// response precedes any text-release decision.
fn strandeds_confirmation_response (
  strandeds : &[StrandedRelationship],
) -> String {
  let atom = |text : &str| -> Sexp { Sexp::Atom ( Atom::S ( text . to_string () )) };
  let pair = |key : &str, value : &str| -> Sexp {
    Sexp::List ( vec! [ atom (key), atom (value) ] ) };
  let entries : Vec<Sexp> = strandeds . iter ()
    . map ( |stranded| Sexp::List ( vec! [
      pair ("relation", stranded . relationship . relation . relation_name ()),
      pair ("recorder", stranded . relationship . recorder . as_str ()),
      pair ("member",   stranded . relationship . member . as_str ()),
      pair ("relRepo",  stranded . relRepo . as_str ()),
      pair ("default",  stranded . new_default . as_str ()), ] ))
    . collect ();
  let count : usize = strandeds . len ();
  let prompt : String = format! (
    "This move would leave {} relationship{} in {} old, more private skgrepo{}. Nothing was saved. Reset them to their new defaults?",
    count, if count == 1 { "" } else { "s" },
    if count == 1 { "its" } else { "their" },
    if count == 1 { "" } else { "s" } );
  Sexp::List ( vec! [
    Sexp::List ( vec! [ atom ("strandeds"), Sexp::List (entries) ] ),
    pair ("prompt", &prompt), ] ) . to_string () }

/// The request's choice of where its deletes may strip the deleted
/// IDs: '(delete-from . "visible")' or '(delete-from . "all")', present
/// only on a retry after a 'delete-from-confirmation'. Absent or any
/// other value, not chosen.
fn delete_from_from_request (
  request : &str,
) -> DeleteFrom {
  match value_from_request_sexp ("delete-from", request) . as_deref () {
    Ok ("visible") => DeleteFrom::VisibleRecords,
    Ok ("all")     => DeleteFrom::AllOwnedRecords,
    _              => DeleteFrom::NotChosen, }}

/// The delete-from-confirmation payload: how many records the
/// skgrepo restriction keeps out name a graphnode the save deletes,
/// and a prompt, after which the clients ask "Delete from: (c)ancel,
/// (v)isible records only, (a)ll owned records". No IDs: the user
/// could not see these records.
fn delete_from_confirmation_response (
  count : usize,
) -> String {
  let atom = |text : &str| -> Sexp { Sexp::Atom ( Atom::S ( text . to_string () )) };
  let prompt : String = format! (
    "{} record{} in skgrepos the skgrepo restriction keeps out name{} a graphnode this save deletes. Nothing was saved.",
    count, if count == 1 { "" } else { "s" }, if count == 1 { "s" } else { "" } );
  Sexp::List ( vec! [
    Sexp::List ( vec! [ atom ("count"), Sexp::Atom ( Atom::I (count as i64) ) ] ),
    Sexp::List ( vec! [ atom ("prompt"), atom (&prompt) ] ), ] ) . to_string () }

/// Whether this save request carries '(approved-forks . "true")', set by
/// the client when the user approves a fork-confirmation. Absent or any
/// other value means not approved.
fn fork_approved_from_request (
  request : &str,
) -> bool {
  value_from_request_sexp ("approved-forks", request)
    . map ( |v| v == "true" )
    . unwrap_or (false) }

/// Parse the optional '(fork-repos ((N . REPO) ...))' field of a
/// save request into a map from each forked node's id N to the owned
/// skgrepo the user chose for its clone (rotated, or left at the default,
/// in the fork-confirmation buffer). Absent on an ordinary save and on
/// the first (unapproved) save -- then every clone skgrepo resolves by
/// inference-else-default. The chosen skgrepo is validated owned + unrestricted
/// downstream in 'validate_fork_specs'.
fn fork_skgrepos_from_request (
  request : &str,
) -> HashMap<ID, SkgrepoName> {
  let mut map : HashMap<ID, SkgrepoName> = HashMap::new ();
  let Ok (sexp) = sexp::parse (request) else { return map; };
  let Sexp::List (items) = sexp else { return map; };
  for item in & items {
    let Sexp::List (pair) = item else { continue; };
    let Some ( Sexp::Atom ( Atom::S (k) )) = pair . first ()
      else { continue; };
    if k != "fork-repos" { continue; }
    let Some ( Sexp::List (entries) ) = pair . get (1)
      else { continue; };
    for entry in entries {
      // Each entry is a dotted pair (N . REPO), which the sexp crate
      // parses as the 3-element list [N, ".", REPO].
      if let Sexp::List (kv) = entry {
        if let [ Sexp::Atom ( Atom::S (gnid) ),
                 Sexp::Atom ( Atom::S (dot) ),
                 Sexp::Atom ( Atom::S (skgrepo) ) ] = & kv [..]
        { if dot == "." {
            map . insert ( ID ( gnid . clone () ),
                           SkgrepoName::from ( skgrepo . as_str () )); }} }} }
  map }

/// If 'err' is a buffer-validation SaveError that carries no warnings
/// of its own, attach the save's parse-time warnings so a failed save
/// still reports them. A no-op for other errors or when there are no
/// parse warnings.
fn backfill_parse_warnings (
  err            : Box<dyn Error>,
  parse_warnings : &[String],
) -> Box<dyn Error> {
  if parse_warnings . is_empty () { return err; }
  match err . downcast::<SaveError> () {
    Ok (boxed) => match *boxed {
      SaveError::BufferValidationErrors { errors, warnings } => {
        let warnings : Vec<String> =
          if warnings . is_empty () { parse_warnings . to_vec () }
          else { warnings };
        Box::new (SaveError::BufferValidationErrors { errors, warnings }) }
      other => Box::new (other), },
    Err (err) => err, } }

/// PURPOSE: Process the buffer that a user wants to save.
/// - "save": Update dbs and filesystem.
/// - "rerender": Create a new buffer for the user.
/// ERRORS: If the buffer is invalid.
/// COMPLEX:
/// - Validation must happen at many stages.
/// - NodeMerges must follow the execution of other nodeInstructions, because the user may have updated one of the nodes to be merged.
/// - The rerender is a refresh of each view ('refresh_viewforest', 'server/refresh/'): one level-order walk that runs the refresh's rungs at each parent, then a tail that sets the derived facts and prunes the childless tombstones.
pub async fn update_from_and_rerender_buffer (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  env                         : &mut SkgEnv,
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                  : &mut ViewsState,
  skgrepo_restriction         : &SkgrepoRestriction,
  approved_forks              : bool, // true once the user has approved the forks (a re-issued save); false on the first save, which returns a fork-confirmation instead of committing.
  fork_skgrepos               : &HashMap<ID, SkgrepoName>, // per-fork clone skgrepos the user chose in the confirmation buffer (keyed by N's pid); empty otherwise.
) -> Result<SaveResponse, Box<dyn Error>> {
  let no_hoist_approvals : HashSet<ID> = HashSet::new ();
  update_from_and_rerender_buffer_with_hoist_approval (
    stream, org_buffer_text, env,
    viewforest_id_from_request_result, views_state, skgrepo_restriction,
    approved_forks, fork_skgrepos, &no_hoist_approvals ) . await
}

pub async fn update_from_and_rerender_buffer_with_hoist_approval (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  env                         : &mut SkgEnv,
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                  : &mut ViewsState,
  skgrepo_restriction         : &SkgrepoRestriction,
  approved_forks              : bool,
  fork_skgrepos               : &HashMap<ID, SkgrepoName>,
  approved_hoist_pids         : &HashSet<ID>,
) -> Result<SaveResponse, Box<dyn Error>> {
  update_from_and_rerender_buffer_with_approvals (
    stream, org_buffer_text,
    "", // the empty baseline: this entry point's buffers were drawn by no server, and an empty baseline writes nothing
    env,
    viewforest_id_from_request_result, views_state, skgrepo_restriction,
    approved_forks, fork_skgrepos, approved_hoist_pids,
    &StrandedsReview::NotReviewed, DeleteFrom::NotChosen,
    &[], None ) . await
}

pub async fn update_from_and_rerender_buffer_with_approvals (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  baseline                    : &str, // the buffer's text as the server last drew it
  env                         : &mut SkgEnv,
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                  : &mut ViewsState,
  skgrepo_restriction         : &SkgrepoRestriction,
  approved_forks              : bool,
  fork_skgrepos               : &HashMap<ID, SkgrepoName>,
  approved_hoist_pids         : &HashSet<ID>,
  strandeds_review          : &StrandedsReview,
  delete_from               : DeleteFrom,
  other_views               : &[ClientViewSnapshot],
  named_diffView_buffer_kind : Option<DiffViewBufferKind>, // the request's '(diff-view-buffer . "...")': the buffer of the diff view it saves, for a view the connection does not hold
) -> Result<SaveResponse, Box<dyn Error>> {
  error_unless_view_was_rendered_under_current_restriction (
    viewforest_id_from_request_result, views_state, skgrepo_restriction,
    & env . runtime_snapshot () . config ) ?;
  // Save-plan construction reads the current files and graph.  Hold the sole
  // writer gate from before those reads through the graph swap-in, or two requests
  // can serialize their writes yet still act on the same stale disk state.
  let mutation_gate = env . mutation_gate ();
  let mutation_guard = mutation_gate . lock () . await;
  let runtime = env . runtime_snapshot ();
  let working_graph = new_handle ((*runtime . graph) . clone ());
  let mut working_tantivy = runtime . tantivy_index . clone ();
  let baseline : WhatTheBaselineShows =
    // Its writes are its copies of the graphnodes changed since the
    // buffer was rendered. The plan builder leaves out the ones the
    // buffer did not edit; the stale check refuses the rest.
    what_the_baseline_shows (
      &runtime . graph, &runtime . config, skgrepo_restriction, baseline )
    . map_err ( |error| Box::new ( SaveError::BufferValidationErrors {
        errors : vec! [error], warnings : Vec::new (), } ) as Box<dyn Error> ) ?;
  let ( viewforest, save_plan, parse_warnings )
    : ( ViewForest, SavePlan, Vec<String> ) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
            "buffer_to_validated_saveplan"
          ) . entered();
        buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
          org_buffer_text, &runtime . graph, &runtime . config,
          skgrepo_restriction, fork_skgrepos, &baseline )
      } . map_err (
        |e| Box::new (e) as Box<dyn Error> ) ?;
  if viewforest . is_empty ()
    { return Err ( "Nothing to save found in org_buffer_text"
                   . into() ); }
  { // A buffer the connection does not hold keeps the viewnode IDs it
    // carries, and fresh ones are counted from past them.
    let held : bool = viewforest_id_from_request_result . as_ref () . ok ()
      . is_some_and ( |viewforest_id| views_state . open_views . views . contains_key (viewforest_id) );
    if ! held {
      ViewnodeIdRecord::error_unless_carried_viewnode_ids_leave_room (&viewforest)
        . map_err ( |e| Box::new ( SaveError::BufferValidationErrors {
            errors : vec! [ BufferValidationError::Other (e) ],
            warnings : parse_warnings . clone (), } ) as Box<dyn Error> ) ?; }}
  let SavePlan {
    node_instructions      : mut nonmerge_nodeInstructions,
    nodeMerge_pairs,
    skgrepo_moves,
    fork_specs,
    post_skgsave_commit_notice_candidates,
    untouched_outdated_graphnodes }
    = save_plan;
  { // A node the buffer would write that changed after the buffer was
    // rendered refuses the whole save, before any confirmation is asked.
    let stale_errors : Vec<BufferValidationError> =
      stale_write_errors (
        &runtime . graph, &baseline . writes, &viewforest,
        &nonmerge_nodeInstructions, &fork_specs );
    if ! stale_errors . is_empty () {
      return Err (Box::new (SaveError::BufferValidationErrors {
        errors : stale_errors, warnings : parse_warnings, })); }}
  { // Restore and readiness requests write or approve what a skgrepo
    // restriction may hide: refused unless the connection is
    // unrestricted.
    let unrestricted = |what : &str| -> Result<(), Box<dyn Error>> {
      require_unrestricted (what, skgrepo_restriction)
        . map_err ( |e| Box::new ( SaveError::BufferValidationErrors {
            errors : vec! [ crate::types::errors::BufferValidationError::Other (e) ],
            warnings : parse_warnings . clone (), } ) as Box<dyn Error> ) };
    if viewforest_has_restore_requests (&viewforest) {
      unrestricted ("A restore request") ?; }
    if viewforest_has_readiness_requests (&viewforest) {
      unrestricted ("A readiness request") ?; }}
  let restore_warnings : Vec<String> =
    // The restore requests become more graph edits, written from the
    // before-graph of the graph straddle; built only when needed.
    if viewforest_has_restore_requests (&viewforest) {
      let straddle : std::sync::Arc<GraphStraddle> =
        current_graph_straddle_in_slot (
          &mut views_state . graph_straddle, &runtime . config,
          runtime . graph . clone () )
        . map_err ( |e| Box::new ( SaveError::BufferValidationErrors {
            errors : vec! [ crate::types::errors::BufferValidationError::Other (
              format! ( "The restore requests could not be applied: {}", e )) ],
            warnings : parse_warnings . clone (), } ) as Box<dyn Error> ) ?;
      add_restore_instructions (
        &viewforest, &mut nonmerge_nodeInstructions,
        &runtime . graph, &straddle, &runtime . config )
    } else { Vec::new () };
  let unrestricted : SkgrepoRestriction =
    SkgrepoRestriction::unrestricted (&runtime . config);
  let restricted_records_left : &SkgrepoRestriction =
  { // Delete propagation adds collateral writes. Derive them before the
    // disk Hoist classification so "touched pids" means every telescope
    // this save will actually rewrite, not only what appeared in the buffer.
    // Where records the skgrepo restriction keeps out name a deleted
    // graphnode, the user chooses first whether they go too.
    let graph_snap = working_graph . load_full ();
    let restricted_count : usize =
      restricted_records_naming_deleted_graphnodes (
        &nonmerge_nodeInstructions, &graph_snap, &runtime . config,
        skgrepo_restriction );
    let restricted_records_left : &SkgrepoRestriction =
      match (restricted_count, delete_from) {
        (0, _) => &unrestricted,
        (_, DeleteFrom::NotChosen) =>
          return Ok ( SaveResponse::delete_from_confirmation (
            parse_warnings, restricted_count )),
        (_, DeleteFrom::VisibleRecords) => skgrepo_restriction,
        (_, DeleteFrom::AllOwnedRecords) => &unrestricted, };
    apply_delete_propagation_cleanup (
      &mut nonmerge_nodeInstructions, &graph_snap, &runtime . config,
      restricted_records_left );
    restricted_records_left };
  let strandeds_warnings : Vec<String> =
  { // What the home skgrepo moves strand (server/from_text/strandeds.rs). This
    // precedes the Hoist classification, which must see the recorders
    // that resetting strandeds to their defaults adds.
    let graph_snap = working_graph . load_full ();
    let strandeds : Vec<StrandedRelationship> =
      stranded_relationships (
        &skgrepo_moves, &nonmerge_nodeInstructions,
        &graph_snap, &runtime . config );
    let (raised, asked) : (Vec<&StrandedRelationship>,
                           Vec<&StrandedRelationship>) =
      strandeds . iter () . partition ( |stranded| stranded . raised_automatically );
    match strandeds_review {
      StrandedsReview::NotReviewed if ! asked . is_empty () =>
        return Ok ( SaveResponse::strandeds_confirmation (
          parse_warnings, asked . into_iter () . cloned () . collect () )),
      StrandedsReview::NotReviewed => {},
      StrandedsReview::Reviewed (chosen) => {
        let chosen : Vec<&StrandedRelationship> = asked . into_iter ()
          . filter ( |stranded| chosen . contains (&stranded . relationship) )
          . collect ();
        reset_to_default_relationships (
          &mut nonmerge_nodeInstructions, &chosen, &graph_snap ); }, }
    reset_to_default_relationships (
      &mut nonmerge_nodeInstructions, &raised, &graph_snap );
    raised . into_iter () . map (raised_relationship_warning) . collect () };
  let hoist_candidates : Vec<HoistCandidate> =
    hoist_candidates_from_disk (
      &nonmerge_nodeInstructions, &nodeMerge_pairs, &runtime . config ) ?;
  if hoist_needs_confirmation (
      &hoist_candidates, approved_hoist_pids ) {
    return Ok ( SaveResponse::hoist_confirmation (
      parse_warnings, hoist_candidates )); }
  // In particular, make a dirty nodeMerge acquiree clean before the merge
  // copies its text into a fresh preservation node and deletes it.
  nonmerge_nodeInstructions . extend (
    repair_saves_for_unwritten_candidates (
      &hoist_candidates, &nonmerge_nodeInstructions, &runtime . config ) ? );
  if ! fork_specs . is_empty () && ! approved_forks {
    // A save that found forks but was not pre-approved skgsave-commits nothing.
    // Return a write-protected fork-confirmation buffer; the client shows it,
    // and on approval re-issues the save with (approved-forks . "true").
    // (Monogamy and skgrepo validation already ran in
    // buffer_to_validated_saveplan, so every fork here is admissible.)
    return Ok ( SaveResponse::fork_confirmation (
      parse_warnings,
      build_fork_confirmation_buffer (&fork_specs),
      format! ( "{} node(s) will be forked. Save again to approve, \
                 or kill this buffer to decline.",
                fork_specs . len () ))); }
  // Forks detected this save (approved, or none): editing a foreign node
  // N is a request to clone it. The clone C is skgsave-committed with the rest of the
  // save -- its 'overrides = [N]' relationship rides in the same
  // NodeInstructions, so the touched-override-invariant check (which reads the
  // simulated post-save graph) sees C before validating.
  let nonmerge_nodeInstructions : Vec<NodeInstruction> = {
    let mut nodes : Vec<NodeInstruction> = nonmerge_nodeInstructions;
    for spec in &fork_specs {
      nodes . push ( NodeInstruction::Save ( spec . clone . clone () )); }
    nodes };

  let prepared_save : PreparedSave =
  { // Prepare every graph/filesystem phase while the writer gate is held.
    let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "prepare_save" ). entered();
    prepare_save_under_mutation_gate (
      nonmerge_nodeInstructions . clone(),
      &nodeMerge_pairs,
      &skgrepo_moves,
      &runtime . config,
      &working_graph,
      approved_hoist_pids,
      restricted_records_left )
    // Warnings always accompany errors: a post-parse validation
    // failure (e.g. the override-invariant check) back-fills the
    // save's parse-time warnings, which the error itself did not see.
    . map_err ( |e| backfill_parse_warnings (e, &parse_warnings) ) ? };
  let node_instructions : Vec<NodeInstruction> = // includes the nodeMerges
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "node_instructions_build" ). entered();
      nonmerge_nodeInstructions . iter () . cloned ()
      . chain ( prepared_save . nodeMerges () . iter ()
                . flat_map ( |nodeMerge| nodeMerge . to_vec () ))
    . collect () };
  let affected_gnids : SaveAffectedIds =
    SaveAffectedIds::from_prepared (&prepared_save);
  let dirty_conflicts = dirty_view_conflicts (
    other_views, views_state, &affected_gnids,
    prepared_save . graph_before_save (),
    prepared_save . final_candidate ())
    .map_err (|message| -> Box<dyn Error> {
      Box::new (SaveError::BufferValidationErrors {
        errors : vec![crate::types::errors::BufferValidationError::Other (
          message)],
        warnings : parse_warnings . clone (), }) }) ?;
  if ! dirty_conflicts . is_empty () {
    return Err (Box::new (SaveError::BufferValidationErrors {
      errors : vec![crate::types::errors::BufferValidationError::Other (
        format_conflict_error (&dirty_conflicts))],
      warnings : parse_warnings, })); }
  // Known limit: only the saving connection's views are re-rendered.
  // Other connections to this server are not refreshed, because the
  // repo watcher drops the server's own writes (server/repo_watch/own_writes.rs).
  // With one person per server this does not bite; if two people ever
  // share a server, the save path, not the watcher, should re-render
  // the other connections' views.
  let collateral_viewforest_ids : Vec<ViewforestId> = viewforest_id_from_request_result . as_ref ()
    .map (|saved_viewforest_id| affected_gnids . collateral_viewforest_ids (
      saved_viewforest_id, other_views, views_state,
      prepared_save . graph_before_save (),
      prepared_save . final_candidate ()))
    .unwrap_or_default ();
  let mut narrowed_lock_viewforest_ids : Vec<ViewforestId> = collateral_viewforest_ids . clone ();
  narrowed_lock_viewforest_ids . extend (
    other_views . iter () . filter (|view| view . dirty)
      . map (|view| view . viewforest_id . clone ()));
  narrowed_lock_viewforest_ids . sort_by_key (ViewforestId::repr_in_client);
  narrowed_lock_viewforest_ids . dedup ();
  // The client keeps the saved view ID implicitly. Every dirty view stays locked
  // because its exact buffer snapshot was an input to the conflict decision,
  // though no dirty view is a collateral target, client-only or not.
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::SaveRelaxLock,
      & format_lock_views_sexp (&narrowed_lock_viewforest_ids) ));
  if collateral_viewforest_ids . is_empty () {
    tracing::debug!("save preparation selected no collateral views");
  } else {
    tracing::info!(
      "save preparation selected {} collateral view(s): {:?}",
      collateral_viewforest_ids . len (),
      collateral_viewforest_ids . iter ()
        . map (ViewforestId::repr_in_client)
        . collect::<Vec<_>> ()); }
  { // Apply the already-checked preparation. Prominence sources are
    // computed from the post-save graph in the ordinary apply path.
    let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "apply_prepared_save" ). entered();
    prepared_save . apply (
      (*runtime . config) . clone (), &mut working_tantivy, &working_graph )
    .map_err ( |e| backfill_parse_warnings (e, &parse_warnings) ) ? }
  let swapped_in = env . runtime . swap_in (
    runtime . config . clone (), working_graph . load_full (), working_tantivy);
  // Rerender reads the newly swapped-in graph but performs no authoritative
  // mutation, so it must not lengthen the writer critical section.
  drop (mutation_guard);

  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "coherence_debug_assert" ). entered();
    debug_assert! (
      // TODO | PITFALL: This is quite a weak assertion.
      // PURPOSE: The in-Rust graph must already reflect every Save and Delete in 'node_instructions' by the time this function runs. Violating this invariant (e.g. by reordering the save pipeline so that 'update_views_after_save' runs before the prepared graph swap-in) would let the rerender read stale Graphnodes from the in-Rust graph.
      in_rust_graph_coherent_with_nodeInstructions_in (
          &swapped_in . graph, &node_instructions
        ) . is_ok (),
      "update_views_after_save: in-Rust graph not coherent with node_instructions" ); }

  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "update_views_after_save" ). entered();
    let readiness_warnings : Vec<String> =
      // The graph edits succeeded, so the readiness requests are consumed.
      consume_readiness_requests (
        &viewforest, env, &swapped_in, &mut views_state . graph_straddle );
    let mut response : SaveResponse =
      update_views_after_save (
        stream,
        viewforest,
        collateral_viewforest_ids,
        env,
        swapped_in . clone (),
        viewforest_id_from_request_result,
        views_state,
        skgrepo_restriction,
        &fork_specs,
        named_diffView_buffer_kind ) ?;
    { // Nonfatal parse warnings (e.g. discarded folder headline text)
      // precede the rerender's warnings.
      let mut warnings : Vec<String> = parse_warnings;
      warnings . extend ( response . warnings );
      warnings . extend ( strandeds_warnings );
      warnings . extend ( untouched_outdated_graphnodes . iter ()
                          . map (left_as_the_graph_has_it_warning) );
      warnings . extend ( restore_warnings );
      warnings . extend ( readiness_warnings );
      warnings . extend (
        post_skgsave_commit_hiddenoutside_warnings (
          &swapped_in . graph, &post_skgsave_commit_notice_candidates ) );
      response . warnings = warnings; }
    Ok (response) } }

/// Refuse a save of a view the connection holds that was last
/// rendered under a skgrepo restriction showing other skgrepos than
/// 'skgrepo_restriction', the connection's current one. The buffer
/// shows what that other restriction let it show, so its silence
/// about a graphnode means something else under this one. The
/// clients refuse to switch the restriction while a view has unsaved
/// edits; this makes the server independent of that, and covers any
/// change of the connection's restriction that rerenders no view. A
/// view the connection does not hold has no record to compare.
fn error_unless_view_was_rendered_under_current_restriction (
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                       : &ViewsState,
  skgrepo_restriction               : &SkgrepoRestriction,
  config                            : &SkgConfig, // orders the skgrepos the refusal names
) -> Result<(), Box<dyn Error>> {
  let Some (rendered_under) : Option<&SkgrepoRestriction> =
    viewforest_id_from_request_result . as_ref () . ok ()
    . and_then ( |viewforest_id| views_state . open_views . views . get (viewforest_id) )
    . map ( |state| &state . skgrepo_restriction_at_last_render )
    else { return Ok (( )); };
  if rendered_under . skgrepos == skgrepo_restriction . skgrepos {
    return Ok (( )); }
  Err ( Box::new ( SaveError::BufferValidationErrors {
    errors : vec! [ BufferValidationError::RenderedUnderAnotherSkgrepoRestriction {
      skgrepos_then : skgrepos_in_config_order (config, &rendered_under . skgrepos),
      skgrepos_now  : skgrepos_in_config_order (config, &skgrepo_restriction . skgrepos), } ],
    warnings : Vec::new (), } )) }

/// 'skgrepos' in the config's order ('ordered_skgrepos': the owned ones
/// most public first), then any the config does not name, by name.
fn skgrepos_in_config_order (
  config   : &SkgConfig,
  skgrepos : &BTreeSet<SkgrepoName>,
) -> Vec<SkgrepoName> {
  let named : Vec<SkgrepoName> = config . ordered_skgrepos ();
  named . iter ()
    . filter ( |skgrepo| skgrepos . contains (skgrepo) )
    . chain ( skgrepos . iter () . filter ( |skgrepo| ! named . contains (skgrepo) ))
    . cloned ()
    . collect () }

/// Turn a successfully skgsave-committed HiddenOutside addition into a message only
/// when the final graph classifies it as hidden *inside* a subscribee.  This is
/// intentionally downstream of graph/filesystem mutation: failure and every
/// confirmation return leave the candidate silent.
fn post_skgsave_commit_hiddenoutside_warnings (
  graph      : &crate::dbs::in_rust_graph::InRustGraph,
  candidates : &[PostSkgsaveCommitNoticeCandidate],
) -> Vec<String> {
  let key = |gnid : &ID| -> ID {
    graph . pid_else_raw_id (gnid) };
  let label = |gnid : &ID| -> String {
    match graphnode_from_graph (graph, gnid) {
      Some (node) => format! ("{} ({})", node . title, node . pid),
      None => gnid . to_string (), } };
  let mut warnings : Vec<String> = Vec::new ();
  for candidate in candidates {
    let PostSkgsaveCommitNoticeCandidate::HiddenOutsideAdded {
      subscriber, member } = candidate;
    let Some (subscriber_node) =
      graphnode_from_graph (graph, subscriber)
    else { continue; };
    let mut containing_subscribees : Vec<String> = Vec::new ();
    for subscribee in subscriber_node . subscribesTo . or_default () {
      let Some (subscribee_node) =
        graphnode_from_graph (graph, &subscribee . member)
      else { continue; };
      if subscribee_node . contains . iter ()
        . any (|content| key (&content . member) == key (member))
      { containing_subscribees . push (label (&subscribee_node . pid)); }}
    if ! containing_subscribees . is_empty () {
      warnings . push (format! (
        "Saved hide for {}; its display moved to HiddenInSubscribeeFolder under {}.",
        label (member), containing_subscribees . join (", ") )); }}
  warnings
}

pub fn compute_diff_for_every_skgrepo (
  config : &SkgConfig
) -> HashMap<SkgrepoName, SkgrepoDiff> {
  let mut skgrepo_diffs : HashMap<SkgrepoName, SkgrepoDiff> =
    HashMap::new();
  for (skgrepo_name, skgrepo_config) in &config . skgrepos {
    let skgrepo_path : &Path =
      Path::new ( &skgrepo_config . path );
    match compute_diff_for_skgrepo (skgrepo_path) {
      Ok (diff) => {
        skgrepo_diffs . insert ( skgrepo_name . clone(), diff ); },
      Err (e) => { // Log error but continue with other skgrepos
        tracing::warn! (
          "Failed to compute diff for repo '{}': {}",
          skgrepo_name, e ); }} }
  skgrepo_diffs }

/// Every other open view sharing at least one PID with the saved view.
/// Over-approximates true collateral (which requires parsing the buffer
/// to know which PIDs actually changed). This is intentional: locking
/// too many buffers briefly is harmless; missing one could lose edits.
/// Sorted, as the relax-lock list is.
fn viewforest_ids_of_views_to_lock (
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                  : &ViewsState,
) -> Vec<ViewforestId> {
  let saved_viewforest_id : &ViewforestId = match viewforest_id_from_request_result {
    Ok (viewforest_id) => viewforest_id,
    Err (_)  => return Vec::new () };
  let pids : Vec<ID> =
    views_state . open_views . viewforest_id_to_pids (saved_viewforest_id);
  let mut collateral_views : HashSet<ViewforestId> = HashSet::new ();
  for pid in &pids {
    for viewforest_id in views_state . open_views . views_containing (pid) {
      if &viewforest_id != saved_viewforest_id
      { collateral_views . insert (viewforest_id); } } }
  let mut lock_viewforest_ids : Vec<ViewforestId> =
    collateral_views . into_iter () . collect ();
  lock_viewforest_ids . sort_by_key (ViewforestId::repr_in_client);
  lock_viewforest_ids }
