use crate::dbs::in_rust_graph::in_rust_graph_coherent_with_save_instructions;
use crate::from_text::buffer_to_validated_saveplan;
use crate::git_ops::diff::compute_diff_for_source;
use crate::git_ops::read_repo::{open_repo, head_is_merge_commit};
use crate::save::update_graph_including_nodeMerges;
use crate::serve::ViewsState;
use crate::source_sets::ActiveSourceSet;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  view_uri_from_request,
  format_buffer_response_sexp,
  format_fork_confirmation_response_sexp,
  format_lock_views_sexp,
  read_length_prefixed_content,
  send_response_with_length_prefix,
  tag_sexp_response,
  value_from_request_sexp };
use crate::from_text::fork::build_fork_confirmation_buffer;
use crate::types::env::SkgEnv;
use crate::types::errors::SaveError;
use crate::types::git::{SourceDiff, GitDiffStatus};
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::{DefineNode, SavePlan, format_save_error_as_org};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::ViewUri;
use crate::update_buffer::update_views_after_save;

use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;
use std::path::Path;

/// The terminal message in the save protocol.
/// Sent after collateral-view updates (if there are any).
/// Contains the re-rendered saved buffer and any warnings/errors.
/// See <api-and-formats.md § Save buffer> for the full sequence:
///   save-lock → collateral-view* → save-result.
pub struct SaveResponse {
  pub saved_view          : String,
  pub errors              : Vec<String>,
  pub warnings            : Vec<String>,
  pub save_point_position : Option<SavePointPosition>,
  /// Some when this save found fork candidates and was NOT pre-approved:
  /// nothing was committed, 'saved_view' instead holds the read-only
  /// fork-confirmation buffer, and this holds the one-line minibuffer
  /// prompt. The handler then sends a 'fork-confirmation' message rather
  /// than 'save-result'. None for an ordinary save.
  pub fork_confirmation   : Option<String>,
}

#[derive(Clone)]
pub struct SavePointPosition {
  pub point_lines_below_focused_headline    : usize,
  pub point_screen_lines_below_window_start : usize,
}

impl SaveResponse {
  /// Format: ((content "...") (errors (...)) (warnings (...)))
  fn to_sexp_string (&self) -> String {
    let mut response : Sexp =
      sexp::parse (
        &format_buffer_response_sexp (
          & self . saved_view,
          & self . errors,
          & self . warnings ))
      . expect (
        "format_buffer_response_sexp should produce valid sexp" );
    if let ( Sexp::List (items),
             Some (point_position) ) =
      ( &mut response, &self . save_point_position )
    { push_save_point_position_to_sexp_items (
        items, point_position ); }
    response . to_string () }}

/// Handles save buffer requests from Emacs.
/// - Reads the buffer content (with length prefix).
/// - Sends the early broad lock (uris_of_views_to_lock) before the slow pipeline.
/// - Runs 'update_from_and_rerender_buffer' (parse + validate -> SavePlan, update
///   the graph, then rerender + stream the saved and collateral views).
/// - Responds to Emacs (with length prefix).
pub fn handle_save_buffer_request (
  reader     : &mut BufReader <TcpStream>,
  stream     : &mut TcpStream, // PITFALL: writes to the same TCP stream as 'reader'
  request    : &str,
  env        : &mut SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  let viewuri_from_request_result : Result<ViewUri, String> =
    view_uri_from_request (request);
  let save_point_position : Option<SavePointPosition> =
    save_point_position_from_request (request);
  let fork_approved : bool =
    // Forking is gated on confirmation: the first save returns a
    // fork-confirmation buffer, and the client re-issues the save with
    // this field once the user approves (mirroring the override-menu's
    // (override-choice . "bypass") field on a re-issued view request).
    fork_approved_from_request (request);
  { // Send the early broad lock BEFORE reading the buffer, so the client's
    // one-shot save-lock handler always fires exactly once and balances its
    // pending-count -- even when the read below fails (otherwise only
    // save-result would arrive, leaving the count unbalanced and wedging the
    // next save's wait). save-result unlocks regardless. Conservative/broad
    // here: the SavePlan is not yet computed.
    let uris_to_lock : Vec<ViewUri> =
      uris_of_views_to_lock (
        &viewuri_from_request_result, views_state );
    let lock_sexp : String =
      format_lock_views_sexp ( &uris_to_lock );
    send_response_with_length_prefix (
      stream,
      & tag_sexp_response ( TcpToClient::SaveLock, &lock_sexp )); }
  match read_length_prefixed_content (reader) {
    Ok (initial_buffer_content) => {
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "update_from_and_rerender_buffer" ). entered();
        match block_on(
          update_from_and_rerender_buffer (
            stream,
            & initial_buffer_content,
            env,
            views_state . diff_mode_enabled,
            &viewuri_from_request_result,
            views_state,
            Some (active_source_set),
            fork_approved ))
        { Ok (mut save_response) => {
            save_response . save_point_position =
              save_point_position . clone ();
            match & save_response . fork_confirmation {
              Some (to_minibuffer) =>
                // A save that found forks and was not approved: nothing
                // committed; send the confirmation buffer instead of a
                // save-result.
                send_response_with_length_prefix (
                  stream,
                  & tag_sexp_response (
                    TcpToClient::ForkConfirmation,
                    & format_fork_confirmation_response_sexp (
                      & save_response . saved_view, to_minibuffer ))),
              None =>
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
                empty_response_sexp (
                  & format_save_error_as_org (save_error),
                  warnings,
                  & save_point_position )
                . to_string ();
              send_response_with_length_prefix (
                stream,
                & tag_sexp_response (
                  TcpToClient::SaveResult, & response_sexp ));
            } else {
              let error_msg : String =
                format!("Error processing buffer content: {}", err);
              tracing::error!("{}", error_msg);
              let response_sexp : String =
                empty_response_sexp (
                  &error_msg,
                  &[],
                  &save_point_position )
                . to_string ();
              send_response_with_length_prefix (
                stream,
                & tag_sexp_response (
                  TcpToClient::SaveResult, &response_sexp )); }} }}; }
    Err (err) => {
      let error_msg : String =
        format! ("Error reading buffer content: {}", err );
      tracing::error! ( "{}", error_msg );
      let response_sexp : String =
        empty_response_sexp (
          &error_msg,
          &[],
          &save_point_position )
        . to_string ();
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          TcpToClient::SaveResult, &response_sexp )); }} }

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
        "point-screen-lines-below-window-start" . to_string () )),
      Sexp::Atom ( Atom::I (
        point_position . point_screen_lines_below_window_start
        as i64 )), ] )); }

fn save_point_position_from_request (
  request : &str,
) -> Option<SavePointPosition> {
  let point_lines_below_focused_headline : usize =
    nat_from_request (
      request,
      "point-lines-below-focused-headline" ) ?;
  let point_screen_lines_below_window_start : usize =
    nat_from_request (
      request,
      "point-screen-lines-below-window-start" ) ?;
  Some ( SavePointPosition {
    point_lines_below_focused_headline,
    point_screen_lines_below_window_start, } ) }

fn nat_from_request (
  request : &str,
  key     : &str,
) -> Option<usize> {
  value_from_request_sexp (key, request) . ok () ? . parse () . ok () }

/// Whether this save request carries '(fork-approved . "true")', set by
/// the client when the user approves a fork-confirmation. Absent or any
/// other value means not approved.
fn fork_approved_from_request (
  request : &str,
) -> bool {
  value_from_request_sexp ("fork-approved", request)
    . map ( |v| v == "true" )
    . unwrap_or (false) }

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
/// - NodeMerges must follow the execution of other save instructions, because the user may have updated one of the nodes to be merged.
/// - complete_viewforest is complex: it is one level-order BFS in which each node is completed at its own visit (content, cols, view requests, inline diff), then a postorder prune sweep removes the empty self-deletable nodes.
pub async fn update_from_and_rerender_buffer (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  env                         : &mut SkgEnv,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &mut ViewsState,
  active_source_set            : Option<&ActiveSourceSet>,
  fork_approved                : bool, // true once the user has approved the forks (a re-issued save); false on the first save, which returns a fork-confirmation instead of committing.
) -> Result<SaveResponse, Box<dyn Error>> {
  if diff_mode_enabled { // diff mode is undefined for merge commits
    let sources : Vec<SourceName> =
      env . config . sources . keys() . cloned() . collect();
    validate_no_merge_commits ( &sources, &env . config )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?; }

  let ( viewforest, save_plan, parse_warnings )
    : ( ViewForest, SavePlan, Vec<String> ) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
            "buffer_to_validated_saveplan"
          ) . entered();
        buffer_to_validated_saveplan (
          org_buffer_text, &env . config, &env . driver,
          active_source_set ) . await
      } . map_err (
        |e| Box::new (e) as Box<dyn Error> ) ?;
  if viewforest . is_empty ()
    { return Err ( "Nothing to save found in org_buffer_text"
                   . into() ); }
  let SavePlan {
    define_nodes       : nonmerge_defineNodes,
    nodeMerge_instructions : nodeMerges,
    source_moves,
    fork_specs }
    = save_plan;
  if ! fork_specs . is_empty () && ! fork_approved {
    // A save that found forks but was not pre-approved commits NOTHING.
    // Return a read-only fork-confirmation buffer; the client shows it,
    // and on approval re-issues the save with (fork-approved . "true").
    // (Monogamy and source validation already ran in
    // buffer_to_validated_saveplan, so every fork here is admissible.)
    return Ok ( SaveResponse {
      saved_view          : build_fork_confirmation_buffer (&fork_specs),
      errors              : Vec::new (),
      warnings            : parse_warnings,
      save_point_position : None,
      fork_confirmation   : Some (
        format! ( "{} node(s) will be forked. Save again to approve, \
                   or kill this buffer to decline.",
                  fork_specs . len () )), } ); }
  // Forks detected this save (approved, or none): editing a foreign node
  // N is a request to clone it. The clone C commits with the rest of the
  // save -- its 'overrides_view_of = [N]' edge rides in the same
  // DefineNodes, so the touched-override-invariant check (which reads the
  // simulated post-save graph) sees C before validating.
  let nonmerge_defineNodes : Vec<DefineNode> = {
    let mut nodes : Vec<DefineNode> = nonmerge_defineNodes;
    for spec in &fork_specs {
      nodes . push ( DefineNode::Save ( spec . clone . clone () )); }
    nodes };

  { // update the graph. Context origin types (for search ranking) are
    // computed from the post-save in-Rust graph and written inside the
    // single Tantivy index pass, so there is no separate context pass.
    let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "update_graph_including_nodeMerges" ). entered();
    update_graph_including_nodeMerges (
      nonmerge_defineNodes . clone(),
      &nodeMerges,
      &source_moves,
      env . config . clone(),
      &mut env . tantivy_index,
      &env . driver,
      &env . in_rust_graph ) . await
    // Warnings always accompany errors: a post-parse validation
    // failure (e.g. the override-invariant check) back-fills the
    // save's parse-time warnings, which the error itself did not see.
    . map_err ( |e| backfill_parse_warnings (e, &parse_warnings) ) ?; }

  let define_nodes : Vec<DefineNode> = // includes the nodeMerges
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "define_nodes_build" ). entered();
      nonmerge_defineNodes . iter () . cloned ()
      . chain ( nodeMerges . iter ()
                . flat_map ( |nodeMerge| nodeMerge . to_vec () ))
      . collect () };

  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "coherence_debug_assert" ). entered();
    debug_assert! (
      // TODO | PITFALL: This is quite a weak assertion.
      // PURPOSE: The in-Rust graph must already reflect every Save and Delete in 'define_nodes' by the time this function runs. Violating this invariant (e.g. by reordering the save pipeline so that 'update_views_after_save' runs before 'apply_definenodes') would let the rerender read stale NodeCompletes from the in-Rust graph.
      in_rust_graph_coherent_with_save_instructions (
          &define_nodes
        ) . is_ok (),
      "update_views_after_save: in-Rust graph not coherent with define_nodes" ); }

  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "update_views_after_save" ). entered();
    let mut response : SaveResponse =
      update_views_after_save (
        stream,
        viewforest,
        define_nodes,
        diff_mode_enabled,
        env,
        viewuri_from_request_result,
        views_state,
        active_source_set ) . await ?;
    { // Nonfatal parse warnings (e.g. discarded col headline text)
      // precede the completion-repair warnings.
      let mut warnings : Vec<String> = parse_warnings;
      warnings . extend ( response . warnings );
      response . warnings = warnings; }
    Ok (response) } }

/// Check if any source's HEAD is a merge commit.
/// Returns an error message if so,
/// as diff computation is ambiguous for merge commits.
pub fn validate_no_merge_commits (
  sources : &[SourceName],
  config  : &SkgConfig,
) -> Result<(), String> {
  for source in sources { // Get the source path from config
    if let Some (source_config) = config . sources . get (source) {
      let source_path : &Path =
        Path::new ( &source_config . path );
      if let Some (repo) = open_repo (source_path) {
        match head_is_merge_commit (&repo) {
          Ok (true) => {
            return Err ( format! (
              "Cannot compute diff: HEAD is a merge commit in source '{}'.",
              source )); },
          Ok (false) => {},
          Err (e) => { // Git error - log but continue
            tracing::warn! ( "Could not check merge commit status for '{}': {}",
                        source, e ); }} }} }
  Ok (( )) }

pub fn compute_diff_for_every_source (
  config : &SkgConfig
) -> HashMap<SourceName, SourceDiff> {
  let mut source_diffs : HashMap<SourceName, SourceDiff> =
    HashMap::new();
  for (source_name, source_config) in &config . sources {
    let source_path : &Path =
      Path::new ( &source_config . path );
    match compute_diff_for_source (source_path) {
      Ok (diff) => {
        source_diffs . insert ( source_name . clone(), diff ); },
      Err (e) => { // Log error but continue with other sources
        tracing::warn! (
          "Failed to compute diff for source '{}': {}",
          source_name, e ); }} }
  source_diffs }

/// Build a map from ID to source for all deleted files.
/// This is used to determine the source of nodes
/// that exist in git HEAD but not in the worktree.
pub fn deleted_ids_to_source (
  source_diffs : &HashMap<SourceName, SourceDiff>
) -> HashMap<ID, SourceName> {
  let mut result : HashMap<ID, SourceName> =
    HashMap::new();
  for (source_name, source_diff) in source_diffs {
    for diffs in [ &source_diff . staged,
                   &source_diff . unstaged ] {
      for (path, nodecomplete_diff) in diffs {
        if nodecomplete_diff . status == GitDiffStatus::Deleted {
          if let Some (stem) = path . file_stem() {
            let id : ID = ID ( stem . to_string_lossy()
                               . into_owned() );
            result . insert ( id,
                              source_name . clone() ); }} }} }
  result }

/// Every other open view sharing at least one PID with the saved view.
/// Over-approximates true collateral (which requires parsing the buffer
/// to know which PIDs actually changed). This is intentional: locking
/// too many buffers briefly is harmless; missing one could lose edits.
fn uris_of_views_to_lock (
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &ViewsState,
) -> Vec<ViewUri> {
  let saved_uri : &ViewUri = match viewuri_from_request_result {
    Ok (uri) => uri,
    Err (_)  => return Vec::new () };
  let pids : Vec<ID> =
    views_state . open_views . viewuri_to_pids (saved_uri);
  let mut collateral_views : HashSet<ViewUri> = HashSet::new ();
  for pid in &pids {
    for uri in views_state . open_views . views_containing (pid) {
      if &uri != saved_uri
      { collateral_views . insert (uri); } } }
  collateral_views . into_iter () . collect () }
