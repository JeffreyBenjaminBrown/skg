use crate::context::update_context_types_for_saved_nodes;
use crate::types::env::SkgEnv;
use crate::from_text::{
  SavePlan,
  buffer_to_viewforest_and_save_instructions};
use crate::git_ops::diff::compute_diff_for_source;
use crate::git_ops::read_repo::{open_repo, head_is_merge_commit};
use crate::save::update_graph_including_merges;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  view_uri_from_request,
  format_buffer_response_sexp,
  format_lock_views_sexp,
  read_length_prefixed_content,
  send_response_with_length_prefix,
  tag_sexp_response,
  tag_text_response};
use crate::types::errors::SaveError;
use crate::types::git::{SourceDiff, GitDiffStatus};
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::format_save_error_as_org;
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
  pub saved_view : String,
  pub errors     : Vec<String>,
}

impl SaveResponse {
  /// Format: ((content "...") (errors (...)))
  fn to_sexp_string (&self) -> String {
    format_buffer_response_sexp (
      & self . saved_view,
      & self . errors ) }}

/// Handles save buffer requests from Emacs.
/// - Reads the buffer content (with length prefix).
/// - Builds an initial NodeCompleteMap from the ViewsState.s open views. It can change during the save process.
/// - 'update_from_and_rerender_buffer'
/// - Responds to Emacs (with length prefix).
pub fn handle_save_buffer_request (
  reader     : &mut BufReader <TcpStream>,
  stream     : &mut TcpStream, // PITFALL: writes to the same TCP stream as 'reader'
  request    : &str,
  env        : &mut SkgEnv,
  views_state : &mut ViewsState,
) {
  let viewuri_from_request_result : Result<ViewUri, String> =
    view_uri_from_request (request);
  match read_length_prefixed_content (reader) {
    Ok (initial_buffer_content) => {
      { // Send early lock message before the expensive pipeline.
        let uris_to_lock : Vec<ViewUri> =
          uris_of_views_to_lock (
            &viewuri_from_request_result, views_state );
        let lock_sexp : String =
          format_lock_views_sexp ( &uris_to_lock );
        send_response_with_length_prefix (
          stream,
          & tag_sexp_response ( TcpToClient::SaveLock, &lock_sexp )); }
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "update_from_and_rerender_buffer" ). entered();
        match block_on(
          update_from_and_rerender_buffer (
            stream,
            & initial_buffer_content,
            env,
            views_state . diff_mode_enabled,
            &viewuri_from_request_result,
            views_state ))
        { Ok (save_response) => {
            send_response_with_length_prefix (
              stream,
              & tag_sexp_response (
                TcpToClient::SaveResult,
                & save_response . to_sexp_string () )); }
          Err (err) => { // Check if this is a SaveError that should be formatted for the client
            if let Some (save_error) = err . downcast_ref::<SaveError>() {
              let response_sexp : String =
                empty_response_sexp (
                  & format_save_error_as_org (save_error) )
                . to_string ();
              send_response_with_length_prefix (
                stream,
                & tag_sexp_response (
                  TcpToClient::SaveResult, & response_sexp ));
            } else {
              let error_msg : String =
                format!("Error processing buffer content: {}", err);
              tracing::error!("{}", error_msg);
              send_response_with_length_prefix (
                stream,
                & tag_text_response (
                  TcpToClient::SaveResult, &error_msg )); }} }}; }
    Err (err) => {
      let error_msg : String =
        format! ("Error reading buffer content: {}", err );
      tracing::error! ( "{}", error_msg );
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::SaveResult, &error_msg )); }} }

/// Create an s-expression with nil content and an error message.
fn empty_response_sexp (
  error_buffer_content : &str
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( "nil" . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "errors" . to_string () )),
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S (
          error_buffer_content . to_string () )) ] ) ] ) ] ) }

/// PURPOSE: Process the buffer that a user wants to save.
/// - "save": Update dbs and filesystem.
/// - "rerender": Create a new buffer for the user.
/// ERRORS: If the buffer is invalid.
/// COMPLEX:
/// - Validation must happen at many stages.
/// - Merges must follow the execution of other save instructions, because the user may have updated one of the nodes to be merged.
/// - complete_viewforest is complex: it runs a preorder pass (completing and reconciling each node) followed by a postorder pass (populating scaffolds like IDCol, AliasCol, etc.).
pub async fn update_from_and_rerender_buffer (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  env                         : &mut SkgEnv,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &mut ViewsState,
) -> Result<SaveResponse, Box<dyn Error>> {
  if diff_mode_enabled {
    let sources : Vec<SourceName> =
      env . config . sources . keys() . cloned() . collect();
    validate_no_merge_commits ( &sources, &env . config )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?; }

  let save_plan : SavePlan =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
            "buffer_to_viewforest_and_save_instructions"
          ) . entered();
        buffer_to_viewforest_and_save_instructions (
          org_buffer_text, &env . config, &env . driver ) . await
      } . map_err (
        |e| Box::new (e) as Box<dyn Error> ) ?;
  if save_plan . viewforest . root() . children() . next() . is_none()
    { return Err ( "Nothing to save found in org_buffer_text"
                   . into() ); }
  let SavePlan {
    viewforest,
    define_nodes,
    merge_instructions,
    source_moves,
  } = save_plan;

  { // update the graph
    update_graph_including_merges (
      define_nodes . clone(),
      &merge_instructions,
      &source_moves,
      env . config . clone(),
      &mut env . tantivy_index,
      &env . driver,
      &env . in_rust_graph ) . await ?;
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "update_context_types_for_saved_nodes" ). entered();
      update_context_types_for_saved_nodes (
        &env . tantivy_index, &env . config . db_name,
        &env . driver, &define_nodes ) . await
      . unwrap_or_else ( |e| tracing::warn! (
        "context type recomputation failed: {}", e )); } }

  update_views_after_save (
    stream,
    viewforest,
    define_nodes,
    &merge_instructions,
    diff_mode_enabled,
    env,
    viewuri_from_request_result,
    views_state ) . await }

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
