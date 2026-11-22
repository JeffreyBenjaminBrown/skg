//! The "save buffer" request, skg's central write: a saved buffer
//! becomes a plan ('from_text.rs'), the plan is carried out
//! ('save.rs'), and the saved view and every other open view it
//! changed are rerendered and streamed back ('rerender_pass.rs').
//! 'handle_save_buffer_request' reads the request and sends the early
//! lock; 'update_from_and_rerender_buffer' and its '_with_*' forms run
//! the pipeline. Instead of saving, the answer may be a preview or a
//! confirmation to ask for (forks, strandeds, deletes under a skgrepo
//! restriction, telescope hoists, overPrivateText text). The request's
//! fields and answers are 'docs/api-and-formats.org', "Save buffer".
//! The request's fields are read by 'save_buffer/request.rs'. Stale
//! graphnodes are 'save_staleness.rs'; conflicting dirty views,
//! 'save_dependencies/'.

pub mod request;

use request::{
  delete_from_from_request,
  fork_approved_from_request,
  fork_skgrepos_from_request,
  named_diffView_buffer_kind_from_request,
  parse_save_request_envelope,
  perspective_from_save_request,
  preview_from_request,
  save_point_position_from_request,
  strandeds_review_from_request,
};
use crate::diff_view::change_leaves::BufferProcessedSummary;
use crate::dbs::in_rust_graph::{
  InRustGraph, InRustGraphHandle,
  in_rust_graph_coherent_with_nodeInstructions_in, new_handle };
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::from_text
  ::buffer_to_validated_saveplan_with_fork_skgrepos_in_graph;
use crate::from_text::strandeds::{
  StrandedsReview, StrandedRelationship,
  raised_relationship_warning, reset_to_default_relationships,
  stranded_relationships};
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
use crate::diff_view::processed_requests::{
  consume_processed_requests, viewforest_has_processed_requests};
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
  format_string_list_sexp,
  viewforest_id_from_request,
  buffer_response_entries,
  format_fork_confirmation_response_sexp,
  format_lock_views_sexp,
  read_length_prefixed_content,
  send_response_with_length_prefix,
  sexp_to_client_string,
  string_atom_sexp,
  string_entry_sexp,
  tag_sexp_response };
use crate::from_text::fork::build_fork_confirmation_buffer;
use crate::types::env::SkgEnv;
use crate::types::errors::{BufferValidationError, SaveError, format_save_error_as_org};
use crate::types::config::SkgConfig;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::save::{
  ForkSpec, NodeInstruction, PostSkgsaveCommitNoticeCandidate, SavePlan,
  SkgrepoMove, UneditedStaleGraphnode };
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::ViewnodeId;
use crate::types::views_state::{DiffViewBufferKind, Perspective, ViewforestId, ViewnodeIdRecord};
use crate::rerender_pass::update_views_after_save;
use crate::view_text_delta::{changed_headlines, changed_headlines_entry};

use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;
use std::ops::ControlFlow;
use std::sync::Arc;

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
  /// processed summary the client shows after the save.
  pub processed_summary   : Option<BufferProcessedSummary>,
  /// The redraw delta from the saved buffer's text to 'saved_view'
  /// ('changed_headlines'): Some on a save-result whose texts differ
  /// only in headline lines, None otherwise.
  pub changed_headlines   : Option<Vec<ViewnodeId>>,
  /// What the save hid and stopped hiding, and each removal from a
  /// forker that hid nothing ('hide_report_lines' in
  /// 'server/from_text/hide_report.rs'). Sent as '(hides ...)' when
  /// not empty.
  pub hides               : Vec<String>,
  /// True for the answer to a save sent with '(preview . "true")',
  /// which wrote nothing: the handler sends it as 'save-preview'.
  pub preview             : bool,
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
  /// The response of a save that skgsave-committed and rerendered
  /// 'saved_buffer_text' as 'saved_view'.
  pub fn saved (
    saved_buffer_text  : &str,
    saved_view         : String,
    errors             : Vec<String>,
    warnings           : Vec<String>,
    processed_summary  : Option<BufferProcessedSummary>,
  ) -> SaveResponse {
    let changed_headlines : Option<Vec<ViewnodeId>> =
      changed_headlines (saved_buffer_text, &saved_view);
    SaveResponse { saved_view, errors, warnings, processed_summary,
                   changed_headlines,
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

  /// The answer to a save sent as a preview, which wrote nothing: what
  /// the save would hide and stop hiding.
  pub fn preview (
    warnings : Vec<String>,
    hides    : Vec<String>,
  ) -> SaveResponse {
    SaveResponse { warnings, hides, preview : true,
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
      processed_summary         : None,
      changed_headlines         : None,
      hides                     : Vec::new (),
      preview                   : false, } }

  /// Format: ((content "...") (errors (...)) (warnings (...))
  /// POINT... (processed-summary ...) (hides ("..." ...))
  /// (changed-headlines (V...))), where the point entries, the
  /// processed summary, the hides and the redraw delta are there when
  /// the save has them.
  fn to_sexp_string (&self) -> String {
    let mut items : Vec<Sexp> = buffer_response_entries (
      & self . saved_view, & self . errors, & self . warnings );
    if let Some (point_position) = &self . save_point_position {
      push_save_point_position_to_sexp_items (&mut items, point_position); }
    if let Some (processed_summary) = &self . processed_summary {
      items . push (processed_summary . to_sexp ()); }
    if ! self . hides . is_empty () {
      items . push ( format_string_list_sexp ("hides", & self . hides) ); }
    if let Some (changed_headlines) = &self . changed_headlines {
      items . push (changed_headlines_entry (changed_headlines)); }
    sexp_to_client_string (&Sexp::List (items)) }

  /// Format: ((hides ("..." ...)) (errors (...)) (warnings (...))):
  /// the answer to a preview, whose hides are there even when empty.
  fn to_preview_sexp_string (&self) -> String {
    sexp_to_client_string ( &Sexp::List ( vec! [
      format_string_list_sexp ("hides", & self . hides),
      format_string_list_sexp ("errors", & self . errors),
      format_string_list_sexp ("warnings", & self . warnings) ] )) }}

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
  let preview : bool = // only say what the save would hide; write nothing
    preview_from_request (request);
  let named_diffView_buffer_kind_result : Result<Option<DiffViewBufferKind>, String> =
    named_diffView_buffer_kind_from_request (request);
  let perspective_result : Result<Perspective, String> =
    perspective_from_save_request (
      request, & env . runtime_snapshot () . config );
  send_early_broad_save_lock (
    stream, &viewforest_id_from_request_result, views_state );
  match ( read_length_prefixed_content (reader)
            .and_then (|content| parse_save_request_envelope (
              &content, viewforest_id_from_request_result . as_ref () . ok ())),
          named_diffView_buffer_kind_result
            . and_then ( |kind| perspective_result
                         . map ( |followed| (kind, followed) )))
  {
    (Ok (_envelope), Err (error_msg)) if preview =>
      send_preview_refusal (stream, vec! [ error_msg ], Vec::new ()),
    (Ok (_envelope), Err (error_msg)) =>
      // The envelope was read, so the stream stays in step; the save is refused.
      send_save_refusal (stream, &error_msg, &[], &save_point_position),
    (Ok (envelope), Ok ((named_diffView_buffer_kind, perspective))) => {
      if let (Ok (viewforest_id), false) =
        (&viewforest_id_from_request_result, preview) {
        crate::view_snapshots::record_received (
          viewforest_id, &envelope . saved_buffer ); }
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "update_from_and_rerender_buffer" ). entered();
        match block_on(
          update_from_and_rerender_buffer_with_approvals_or_preview (
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
            named_diffView_buffer_kind,
            perspective,
            preview ))
        { Ok (mut save_response) => {
            save_response . save_point_position =
              save_point_position . clone ();
            send_save_response (stream, &save_response); }
          Err (err) if preview =>
            send_preview_refusal_of_pipeline_error (stream, &err),
          Err (err) =>
            send_save_refusal_of_pipeline_error (
              stream, &err, &save_point_position ), }}; }
    (Err (err), _) if preview =>
      send_preview_refusal (
        stream,
        vec! [ format! ( "NOTHING WAS SAVED\n\nError reading save payload: {}", err ) ],
        Vec::new () ),
    (Err (err), _) => {
      let error_msg : String =
        format! ("NOTHING WAS SAVED\n\nError reading save payload: {}", err );
      tracing::error! ( "{}", error_msg );
      send_save_refusal (
        stream, &error_msg, &[], &save_point_position ); }} }

/// Send the early broad lock before reading the buffer, so the client's
/// one-shot save-lock handler always fires exactly once and balances its
/// pending-count -- even when the read fails (otherwise only
/// save-result would arrive, leaving the count unbalanced and wedging the
/// next save's wait). save-result unlocks regardless. Conservative/broad
/// here: the SavePlan is not yet computed.
fn send_early_broad_save_lock (
  stream                            : &mut TcpStream,
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                       : &ViewsState,
) {
  let viewforest_ids_to_lock : Vec<ViewforestId> =
    viewforest_ids_of_views_to_lock (
      viewforest_id_from_request_result, views_state );
  let lock_sexp : String =
    format_lock_views_sexp ( &viewforest_ids_to_lock );
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response ( TcpToClient::SaveLock, &lock_sexp )); }

/// A preview the save would refuse: its errors, as a save's.
fn send_preview_refusal_of_pipeline_error (
  stream : &mut TcpStream,
  err    : &Box<dyn Error>,
) {
  let (errors, warnings) : (Vec<String>, Vec<String>) =
    match err . downcast_ref::<SaveError>() {
      Some (save_error) => (
        vec! [ format_save_error_as_org (save_error) ],
        match save_error {
          SaveError::BufferValidationErrors { warnings, .. } =>
            warnings . clone (),
          _ => Vec::new (), } ),
      None => ( vec! [ format! (
        "Error processing buffer content: {}", err ) ],
                Vec::new () ), };
  send_preview_refusal (stream, errors, warnings); }

/// Refuse a save the pipeline failed: a 'SaveError' formatted for the
/// client, with its warnings; any other error logged and sent as it reads.
fn send_save_refusal_of_pipeline_error (
  stream              : &mut TcpStream,
  err                 : &Box<dyn Error>,
  save_point_position : &Option<SavePointPosition>,
) {
  if let Some (save_error) = err . downcast_ref::<SaveError>() {
    // Warnings always accompany errors (decided 2026-06-12):
    // a failed validation carries the parse-time warnings it
    // collected before aborting.
    let warnings : &[String] = match save_error {
      SaveError::BufferValidationErrors { warnings, .. } =>
        warnings,
      _ => &[], };
    send_save_refusal (
      stream, & format_save_error_as_org (save_error),
      warnings, save_point_position );
  } else {
    let error_msg : String =
      format!("Error processing buffer content: {}", err);
    tracing::error!("{}", error_msg);
    send_save_refusal (
      stream, &error_msg, &[], save_point_position ); }}

/// Send the answer to a save the pipeline finished: a preview's,
/// a confirmation the save asks for (each checked in this order),
/// or else its save-result.
fn send_save_response (
  stream        : &mut TcpStream,
  save_response : &SaveResponse,
) {
  if save_response . preview {
    send_response_with_length_prefix (
      stream,
      & tag_sexp_response (
        TcpToClient::SavePreview,
        & save_response . to_preview_sexp_string () ));
    return; }
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
          & save_response . to_sexp_string () )), } }

/// Answer a refused save: 'save-result' with no content, the refusal
/// as its one error, and the request's point position.
fn send_save_refusal (
  stream              : &mut TcpStream,
  error               : &str,
  warnings            : &[String],
  save_point_position : &Option<SavePointPosition>,
) {
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::SaveResult,
      & sexp_to_client_string ( &empty_response_sexp (
        error, warnings, save_point_position )) )); }

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

/// The strandeds-confirmation payload: each stranded relationship (its
/// relation, recorder and member IDs, where the save would leave it
/// and its new default), and a prompt. IDs only, no titles: the
/// response precedes any text-release decision.
fn strandeds_confirmation_response (
  strandeds : &[StrandedRelationship],
) -> String {
  let entries : Vec<Sexp> = strandeds . iter ()
    . map ( |stranded| Sexp::List ( vec! [
      string_entry_sexp ("relation", stranded . relationship . relation . relation_name ()),
      string_entry_sexp ("recorder", stranded . relationship . recorder . as_str ()),
      string_entry_sexp ("member",   stranded . relationship . member . as_str ()),
      string_entry_sexp ("relRepo",  stranded . relRepo . as_str ()),
      string_entry_sexp ("default",  stranded . new_default . as_str ()), ] ))
    . collect ();
  let count : usize = strandeds . len ();
  let prompt : String = format! (
    "This move would leave {} relationship{} in {} old, more private skgrepo{}. Nothing was saved. Reset them to their new defaults?",
    count, if count == 1 { "" } else { "s" },
    if count == 1 { "its" } else { "their" },
    if count == 1 { "" } else { "s" } );
  Sexp::List ( vec! [
    Sexp::List ( vec! [ string_atom_sexp ("strandeds"), Sexp::List (entries) ] ),
    string_entry_sexp ("prompt", &prompt), ] ) . to_string () }

/// The delete-from-confirmation payload: how many records the
/// skgrepo restriction keeps out name a graphnode the save deletes,
/// and a prompt, after which the clients ask "Delete from: (c)ancel,
/// (v)isible records only, (a)ll owned records". No IDs: the user
/// could not see these records.
fn delete_from_confirmation_response (
  count : usize,
) -> String {
  let prompt : String = format! (
    "{} record{} in skgrepos the skgrepo restriction keeps out name{} a graphnode this save deletes. Nothing was saved.",
    count, if count == 1 { "" } else { "s" }, if count == 1 { "s" } else { "" } );
  Sexp::List ( vec! [
    Sexp::List ( vec! [ string_atom_sexp ("count"), Sexp::Atom ( Atom::I (count as i64) ) ] ),
    Sexp::List ( vec! [ string_atom_sexp ("prompt"), string_atom_sexp (&prompt) ] ), ] ) . to_string () }

/// Answer a preview whose save would be refused: 'save-preview' with
/// no hides and the refusal's errors and warnings.
fn send_preview_refusal (
  stream   : &mut TcpStream,
  errors   : Vec<String>,
  warnings : Vec<String>,
) {
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::SavePreview,
      & SaveResponse { errors, warnings,
                       .. SaveResponse::preview (Vec::new (), Vec::new ()) }
        . to_preview_sexp_string () )); }

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
    &[], None, Perspective::RootHomes ) . await
}

pub async fn update_from_and_rerender_buffer_with_approvals (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  baseline                    : &str,
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
  named_diffView_buffer_kind : Option<DiffViewBufferKind>,
  perspective                : Perspective,
) -> Result<SaveResponse, Box<dyn Error>> {
  update_from_and_rerender_buffer_with_approvals_or_preview (
    stream, org_buffer_text, baseline, env,
    viewforest_id_from_request_result, views_state, skgrepo_restriction,
    approved_forks, fork_skgrepos, approved_hoist_pids, strandeds_review,
    delete_from, other_views, named_diffView_buffer_kind,
    perspective, false ) . await }

/// As 'update_from_and_rerender_buffer_with_approvals', but when
/// 'preview', stop once the plan is built and checked against the
/// stale buffer, writing nothing and asking nothing, and answer what
/// the save would hide and stop hiding ('SaveResponse::preview').
pub async fn update_from_and_rerender_buffer_with_approvals_or_preview (
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
  perspective                : Perspective, // the request's '(perspective . "...")': the perspective the buffer was drawn from
  preview                    : bool, // the request's '(preview . "true")'
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
  let is_diff_view : bool = // the saved buffer is one of the diff view's, held or named
    named_diffView_buffer_kind . is_some ()
    || viewforest_id_from_request_result . as_ref () . ok ()
       . and_then ( |viewforest_id| views_state . open_views
                    . diffView_buffer_kind_of_view (viewforest_id) )
       . is_some ();
  let baseline : WhatTheBaselineShows =
    // Its writes are its copies of the graphnodes changed since the
    // buffer was rendered. The plan builder leaves out the ones the
    // buffer did not edit; the stale check refuses the rest.
    what_the_baseline_shows (
      &runtime . graph, &runtime . config, skgrepo_restriction, baseline,
      is_diff_view )
    . map_err ( |error| Box::new ( SaveError::BufferValidationErrors {
        errors : vec! [error], warnings : Vec::new (), } ) as Box<dyn Error> ) ?;
  let ( viewforest, save_plan, parse_warnings )
    : ( ViewForest, SavePlan, Vec<String> ) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
            "buffer_to_validated_saveplan"
          ) . entered();
        buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
          org_buffer_text, &runtime . graph, &runtime . config,
          skgrepo_restriction, fork_skgrepos, &baseline, perspective . clone (),
          is_diff_view )
      } . map_err (
        |e| Box::new (e) as Box<dyn Error> ) ?;
  if viewforest . is_empty ()
    { return Err ( "Nothing to save found in org_buffer_text"
                   . into() ); }
  error_unless_unheld_buffer_leaves_room_past_its_viewnode_ids (
    &viewforest, viewforest_id_from_request_result, views_state,
    &parse_warnings ) ?;
  let SavePlan {
    node_instructions      : mut nonmerge_nodeInstructions,
    nodeMerge_pairs,
    skgrepo_moves,
    fork_specs,
    post_skgsave_commit_notice_candidates,
    unedited_stale_graphnodes,
    hide_report }
    = save_plan;
  error_if_buffer_writes_stale_graphnodes (
    &runtime . graph, &baseline . writes, &viewforest,
    &nonmerge_nodeInstructions, &fork_specs, &parse_warnings ) ?;
  error_unless_unrestricted_for_restore_and_processed_requests (
    &viewforest, skgrepo_restriction, &parse_warnings ) ?;
  if preview { // nothing below has run, so nothing is written or held
    return Ok ( SaveResponse::preview (parse_warnings, hide_report) ); }
  let restore_warnings : Vec<String> =
    add_restore_instructions_if_requested (
      &viewforest, &mut nonmerge_nodeInstructions,
      &mut views_state . graph_straddle, &runtime . graph, &runtime . config,
      &parse_warnings ) ?;
  let unrestricted : SkgrepoRestriction =
    SkgrepoRestriction::unrestricted (&runtime . config);
  let restricted_records_left : &SkgrepoRestriction =
    match propagate_deletes_unless_asking_where (
      &mut nonmerge_nodeInstructions, &working_graph, &runtime . config,
      skgrepo_restriction, &unrestricted, delete_from )
    { ControlFlow::Continue (restricted_records_left) => restricted_records_left,
      ControlFlow::Break (restricted_count) =>
        return Ok ( SaveResponse::delete_from_confirmation (
          parse_warnings, restricted_count )), };
  let strandeds_warnings : Vec<String> =
    match reset_strandeds_unless_asking (
      &mut nonmerge_nodeInstructions, &skgrepo_moves, &working_graph,
      &runtime . config, strandeds_review )
    { ControlFlow::Continue (strandeds_warnings) => strandeds_warnings,
      ControlFlow::Break (asked) =>
        return Ok ( SaveResponse::strandeds_confirmation (
          parse_warnings, asked )), };
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
    // (The already-forked and skgrepo checks ran in
    // buffer_to_validated_saveplan, so every fork here is admissible.)
    return Ok ( fork_confirmation_response (parse_warnings, &fork_specs) ); }
  // Forks detected this save (approved, or none): editing a foreign node
  // N is a request to clone it. The clone C is skgsave-committed with the rest of the
  // save -- its 'forks = [N]' relationship rides in the same
  // NodeInstructions, so the fork rules' check of the touched notes (which reads the
  // simulated post-save graph) sees C before validating.
  let nonmerge_nodeInstructions : Vec<NodeInstruction> =
    with_fork_clones_saved (nonmerge_nodeInstructions, &fork_specs);

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
    // failure (e.g. the fork rules' check) back-fills the
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
  error_if_dirty_views_conflict (
    other_views, views_state, &affected_gnids, &prepared_save,
    &parse_warnings ) ?;
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
  send_relaxed_lock_and_log_collateral_views (
    stream, &collateral_viewforest_ids, other_views );
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
    let processed_warnings : Vec<String> =
      // The graph edits succeeded, so the processed requests are consumed.
      consume_processed_requests (
        &viewforest, env, &swapped_in, &mut views_state . graph_straddle );
    let mut response : SaveResponse =
      update_views_after_save (
        stream,
        viewforest,
        org_buffer_text,
        collateral_viewforest_ids,
        env,
        swapped_in . clone (),
        viewforest_id_from_request_result,
        views_state,
        skgrepo_restriction,
        &fork_specs,
        named_diffView_buffer_kind,
        perspective ) ?;
    response . warnings =
      save_warnings_in_the_order_shown (
        parse_warnings, response . warnings, strandeds_warnings,
        &unedited_stale_graphnodes, restore_warnings, processed_warnings,
        post_skgsave_commit_hiddenoutside_warnings (
          &swapped_in . graph, &post_skgsave_commit_notice_candidates ));
    response . hides = hide_report;
    Ok (response) } }

/// A buffer the connection does not hold keeps the viewnode IDs it
/// carries, and fresh ones are counted from past them: refused unless
/// they leave room for that.
fn error_unless_unheld_buffer_leaves_room_past_its_viewnode_ids (
  viewforest                        : &ViewForest,
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                       : &ViewsState,
  parse_warnings                    : &[String],
) -> Result<(), Box<dyn Error>> {
  let held : bool = viewforest_id_from_request_result . as_ref () . ok ()
    . is_some_and ( |viewforest_id| views_state . open_views . views . contains_key (viewforest_id) );
  if ! held {
    ViewnodeIdRecord::error_unless_carried_viewnode_ids_leave_room (viewforest)
      . map_err ( |e| Box::new ( SaveError::BufferValidationErrors {
          errors : vec! [ BufferValidationError::Other (e) ],
          warnings : parse_warnings . to_vec (), } ) as Box<dyn Error> ) ?; }
  Ok (( )) }

/// A node the buffer would write that changed after the buffer was
/// rendered refuses the whole save, before any confirmation is asked.
fn error_if_buffer_writes_stale_graphnodes (
  graph                     : &InRustGraph,
  baseline_writes           : &BTreeMap<ID, Graphnode>,
  viewforest                : &ViewForest,
  nonmerge_nodeInstructions : &[NodeInstruction],
  fork_specs                : &[ForkSpec],
  parse_warnings            : &[String],
) -> Result<(), Box<dyn Error>> {
  let stale_errors : Vec<BufferValidationError> =
    stale_write_errors (
      graph, baseline_writes, viewforest,
      nonmerge_nodeInstructions, fork_specs );
  if ! stale_errors . is_empty () {
    return Err (Box::new (SaveError::BufferValidationErrors {
      errors : stale_errors, warnings : parse_warnings . to_vec (), })); }
  Ok (( )) }

/// A save that found forks and was not approved: the write-protected
/// fork-confirmation buffer, and the prompt that goes with it.
fn fork_confirmation_response (
  parse_warnings : Vec<String>,
  fork_specs     : &[ForkSpec],
) -> SaveResponse {
  SaveResponse::fork_confirmation (
    parse_warnings,
    build_fork_confirmation_buffer (fork_specs),
    format! ( "{} node(s) will be forked. Save again to approve, \
               or kill this buffer to decline.",
              fork_specs . len () )) }

/// The save's nodeInstructions, then a Save of each fork's clone.
fn with_fork_clones_saved (
  nonmerge_nodeInstructions : Vec<NodeInstruction>,
  fork_specs                : &[ForkSpec],
) -> Vec<NodeInstruction> {
  let mut nodes : Vec<NodeInstruction> = nonmerge_nodeInstructions;
  for spec in fork_specs {
    nodes . push ( NodeInstruction::Save ( spec . clone . clone () )); }
  nodes }

/// Every warning of a saved save, in the order the user reads them.
/// Nonfatal parse warnings (e.g. discarded folder headline text)
/// precede the rerender's warnings.
fn save_warnings_in_the_order_shown (
  parse_warnings            : Vec<String>,
  rerender_warnings         : Vec<String>,
  strandeds_warnings        : Vec<String>,
  unedited_stale_graphnodes : &[UneditedStaleGraphnode],
  restore_warnings          : Vec<String>,
  processed_warnings        : Vec<String>,
  hiddenoutside_warnings    : Vec<String>,
) -> Vec<String> {
  let mut warnings : Vec<String> = parse_warnings;
  warnings . extend ( rerender_warnings );
  warnings . extend ( strandeds_warnings );
  warnings . extend ( unedited_stale_graphnodes . iter ()
                      . map (left_as_the_graph_has_it_warning) );
  warnings . extend ( restore_warnings );
  warnings . extend ( processed_warnings );
  warnings . extend ( hiddenoutside_warnings );
  warnings }

/// Restore and processed requests write or approve what a skgrepo
/// restriction may hide: refused unless the connection is
/// unrestricted.
fn error_unless_unrestricted_for_restore_and_processed_requests (
  viewforest          : &ViewForest,
  skgrepo_restriction : &SkgrepoRestriction,
  parse_warnings      : &[String],
) -> Result<(), Box<dyn Error>> {
  let unrestricted = |what : &str| -> Result<(), Box<dyn Error>> {
    require_unrestricted (what, skgrepo_restriction)
      . map_err ( |e| Box::new ( SaveError::BufferValidationErrors {
          errors : vec! [ crate::types::errors::BufferValidationError::Other (e) ],
          warnings : parse_warnings . to_vec (), } ) as Box<dyn Error> ) };
  if viewforest_has_restore_requests (viewforest) {
    unrestricted ("A restore request") ?; }
  if viewforest_has_processed_requests (viewforest) {
    unrestricted ("A processed request") ?; }
  Ok (( )) }

/// The restore requests become more graph edits, written from the
/// before-graph of the graph straddle; built only when needed.
/// Returns the warnings of restoring.
fn add_restore_instructions_if_requested (
  viewforest                : &ViewForest,
  nonmerge_nodeInstructions : &mut Vec<NodeInstruction>,
  graph_straddle_slot       : &mut Option<Arc<GraphStraddle>>,
  live_graph                : &Arc<InRustGraph>,
  config                    : &SkgConfig,
  parse_warnings            : &[String],
) -> Result<Vec<String>, Box<dyn Error>> {
  if ! viewforest_has_restore_requests (viewforest) {
    return Ok (Vec::new ()); }
  let straddle : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (
      graph_straddle_slot, config, live_graph . clone () )
    . map_err ( |e| Box::new ( SaveError::BufferValidationErrors {
        errors : vec! [ crate::types::errors::BufferValidationError::Other (
          format! ( "The restore requests could not be applied: {}", e )) ],
        warnings : parse_warnings . to_vec (), } ) as Box<dyn Error> ) ?;
  Ok ( add_restore_instructions (
    viewforest, nonmerge_nodeInstructions, live_graph, &straddle, config )) }

/// Delete propagation adds collateral writes. Derive them before the
/// disk Hoist classification so "touched pids" means every telescope
/// this save will actually rewrite, not only what appeared in the buffer.
/// Where records the skgrepo restriction keeps out name a deleted
/// graphnode, the user chooses first whether they go too: unless
/// 'delete_from' says, this breaks with how many such records there
/// are, before propagating anything. Otherwise it continues with the
/// restriction whose hidden records the deletes leave in place.
fn propagate_deletes_unless_asking_where<'a> (
  nonmerge_nodeInstructions : &mut Vec<NodeInstruction>,
  working_graph             : &InRustGraphHandle,
  config                    : &SkgConfig,
  skgrepo_restriction       : &'a SkgrepoRestriction,
  unrestricted              : &'a SkgrepoRestriction,
  delete_from               : DeleteFrom,
) -> ControlFlow<usize, &'a SkgrepoRestriction> {
  let graph_snap : Arc<InRustGraph> = working_graph . load_full ();
  let restricted_count : usize =
    restricted_records_naming_deleted_graphnodes (
      nonmerge_nodeInstructions, &graph_snap, config,
      skgrepo_restriction );
  let restricted_records_left : &SkgrepoRestriction =
    match (restricted_count, delete_from) {
      (0, _) => unrestricted,
      (_, DeleteFrom::NotChosen) =>
        return ControlFlow::Break (restricted_count),
      (_, DeleteFrom::VisibleRecords) => skgrepo_restriction,
      (_, DeleteFrom::AllOwnedRecords) => unrestricted, };
  apply_delete_propagation_cleanup (
    nonmerge_nodeInstructions, &graph_snap, config,
    restricted_records_left );
  ControlFlow::Continue (restricted_records_left) }

/// What the home skgrepo moves strand (server/from_text/strandeds.rs). This
/// precedes the Hoist classification, which must see the recorders
/// that resetting strandeds to their defaults adds.
/// Unreviewed strandeds the user must be asked about break, before
/// anything is reset. Otherwise the reviewed ones the user chose and
/// the ones raised automatically are reset to their defaults, and this
/// continues with a warning for each one raised.
fn reset_strandeds_unless_asking (
  nonmerge_nodeInstructions : &mut Vec<NodeInstruction>,
  skgrepo_moves             : &[SkgrepoMove],
  working_graph             : &InRustGraphHandle,
  config                    : &SkgConfig,
  strandeds_review          : &StrandedsReview,
) -> ControlFlow<Vec<StrandedRelationship>, Vec<String>> {
  let graph_snap : Arc<InRustGraph> = working_graph . load_full ();
  let strandeds : Vec<StrandedRelationship> =
    stranded_relationships (
      skgrepo_moves, nonmerge_nodeInstructions,
      &graph_snap, config );
  let (raised, asked) : (Vec<&StrandedRelationship>,
                         Vec<&StrandedRelationship>) =
    strandeds . iter () . partition ( |stranded| stranded . raised_automatically );
  match strandeds_review {
    StrandedsReview::NotReviewed if ! asked . is_empty () =>
      return ControlFlow::Break (
        asked . into_iter () . cloned () . collect () ),
    StrandedsReview::NotReviewed => {},
    StrandedsReview::Reviewed (chosen) => {
      let chosen : Vec<&StrandedRelationship> = asked . into_iter ()
        . filter ( |stranded| chosen . contains (&stranded . relationship) )
        . collect ();
      reset_to_default_relationships (
        nonmerge_nodeInstructions, &chosen, &graph_snap ); }, }
  reset_to_default_relationships (
    nonmerge_nodeInstructions, &raised, &graph_snap );
  ControlFlow::Continue (
    raised . into_iter () . map (raised_relationship_warning) . collect () ) }

/// Refuse the save if a dirty view the client holds shows something
/// the prepared save changes ('save_dependencies/').
fn error_if_dirty_views_conflict (
  other_views    : &[ClientViewSnapshot],
  views_state    : &ViewsState,
  affected_gnids : &SaveAffectedIds,
  prepared_save  : &PreparedSave,
  parse_warnings : &[String],
) -> Result<(), Box<dyn Error>> {
  let dirty_conflicts = dirty_view_conflicts (
    other_views, views_state, affected_gnids,
    prepared_save . graph_before_save (),
    prepared_save . final_candidate ())
    .map_err (|message| -> Box<dyn Error> {
      Box::new (SaveError::BufferValidationErrors {
        errors : vec![crate::types::errors::BufferValidationError::Other (
          message)],
        warnings : parse_warnings . to_vec (), }) }) ?;
  if ! dirty_conflicts . is_empty () {
    return Err (Box::new (SaveError::BufferValidationErrors {
      errors : vec![crate::types::errors::BufferValidationError::Other (
        format_conflict_error (&dirty_conflicts))],
      warnings : parse_warnings . to_vec (), })); }
  Ok (( )) }

/// Narrow the save's lock to the collateral views and the dirty ones,
/// and log which collateral views the save will rerender.
fn send_relaxed_lock_and_log_collateral_views (
  stream                    : &mut TcpStream,
  collateral_viewforest_ids : &[ViewforestId],
  other_views               : &[ClientViewSnapshot],
) {
  let mut narrowed_lock_viewforest_ids : Vec<ViewforestId> = collateral_viewforest_ids . to_vec ();
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
        . collect::<Vec<_>> ()); }}

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
/// when the final graph classifies it as hidden *inside* a forkee.  This is
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
      forker, member } = candidate;
    let Some (forker_node) =
      graphnode_from_graph (graph, forker)
    else { continue; };
    let mut containing_forkees : Vec<String> = Vec::new ();
    for forkee in forker_node . forks . or_default () {
      let Some (forkee_node) =
        graphnode_from_graph (graph, &forkee . member)
      else { continue; };
      if forkee_node . contains . iter ()
        . any (|content| key (&content . member) == key (member))
      { containing_forkees . push (label (&forkee_node . pid)); }}
    if ! containing_forkees . is_empty () {
      warnings . push (format! (
        "Saved hide for {}; its display moved to HiddenInForkPartnerFolder under {}.",
        label (member), containing_forkees . join (", ") )); }}
  warnings
}

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
