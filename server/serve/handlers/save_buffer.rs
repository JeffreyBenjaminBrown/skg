use crate::from_text::buffer_to_viewnode_forest_and_save_instructions;
use crate::git_ops::diff::compute_diff_for_source;
use crate::git_ops::read_repo::{open_repo, head_is_merge_commit};
use crate::merge::merge_nodes;
use crate::save::update_graph_minus_merges;
use crate::serve::ConnectionState;
use crate::serve::protocol::ResponseType;
use crate::serve::util::{
  view_uri_from_request,
  format_buffer_response_sexp_with_updates,
  format_collateral_uris_sexp,
  read_length_prefixed_content,
  send_response_with_length_prefix,
  tag_sexp_response,
  tag_text_response};
use crate::types::errors::SaveError;
use crate::types::git::{SourceDiff, GitDiffStatus};
use crate::types::misc::{ID, SourceName, SkgConfig, TantivyIndex};
use crate::types::save::{DefineNode, Merge, format_save_error_as_org};
use crate::types::memory::SkgNodeMap;
use crate::types::memory::ViewUri;
use crate::types::viewnode::ViewNode;
use crate::update_buffer::update_views_after_save;

use ego_tree::Tree;
use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;
use std::path::Path;
use typedb_driver::TypeDBDriver;

/// Rust's response to Emacs for a save operation.
/// Contains the regenerated buffer content and any warnings/errors.
/// 'Collateral views' are other views that need updating
/// as a result of the save.
pub struct SaveResponse {
  pub saved_view       : String,
  pub errors           : Vec < String >,
  pub collateral_views : Vec<(ViewUri, String)>,
}

impl SaveResponse {
  /// Format: ((content "...") (errors (...)) (other-views-to-update (("URI1" "c1") ...)))
  fn to_sexp_string (&self) -> String {
    format_buffer_response_sexp_with_updates (
      & self . saved_view,
      & self . errors,
      & self . collateral_views ) }}

/// Handles save buffer requests from Emacs.
/// - Reads the buffer content (with length prefix).
/// - Builds an initial SkgnodeMap from the ConnectionState's memory. It can change during the save process.
/// - 'update_from_and_rerender_buffer'
/// - Responds to Emacs (with length prefix).
pub fn handle_save_buffer_request (
  reader        : &mut BufReader <TcpStream>,
  stream        : &mut TcpStream, // PITFALL: writes to the same TCP stream as 'reader'
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  tantivy_index : &mut TantivyIndex,
  conn_state    : &mut ConnectionState,
) {
  let viewuri_from_request_result : Result<ViewUri, String> =
    view_uri_from_request (request);
  let saveview_skgnodes_pre_save : SkgNodeMap = {
    // the skgnodes already in memory for this view
    let mut it : SkgNodeMap = SkgNodeMap::new ();
    if let Ok (ref uri) = viewuri_from_request_result {
      for pid in conn_state . memory . viewuri_to_pids (uri) {
        if let Some (skgnode)
          = conn_state . memory . pool . get (&pid)
          { it . insert ( pid, skgnode . clone () ); }} }
    tracing::debug!("save-diagnostic: view-uri={}, pool-size={}, pool-total={}",
              match &viewuri_from_request_result {
                Ok (uri) => uri . repr_in_client (),
                Err (_) => "<nil>" . to_string () },
              it . len(),
              conn_state . memory . pool . len());
    it };
  match read_length_prefixed_content (reader) {
    Ok (initial_buffer_content) => {
      { // Send early lock message before the expensive pipeline.
        let uris_to_lock : Vec<ViewUri> =
          uris_of_views_to_lock (
            &viewuri_from_request_result, conn_state );
        let lock_sexp : String =
          format_collateral_uris_sexp ( &uris_to_lock );
        send_response_with_length_prefix (
          stream,
          & tag_sexp_response ( ResponseType::SaveLock, &lock_sexp )); }
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "update_from_and_rerender_buffer" ). entered();
        match block_on(
          update_from_and_rerender_buffer (
            & initial_buffer_content,
            typedb_driver, config, tantivy_index,
            conn_state . diff_mode_enabled,
            saveview_skgnodes_pre_save,
            &viewuri_from_request_result,
            conn_state ))
        { Ok (save_response) => {
            send_response_with_length_prefix (
              stream,
              & tag_sexp_response (
                ResponseType::SaveResult,
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
                  ResponseType::SaveResult, & response_sexp ));
            } else {
              let error_msg : String =
                format!("Error processing buffer content: {}", err);
              tracing::error!("{}", error_msg);
              send_response_with_length_prefix (
                stream,
                & tag_text_response (
                  ResponseType::SaveResult, &error_msg )); }} }}; }
    Err (err) => {
      let error_msg : String =
        format! ("Error reading buffer content: {}", err );
      tracing::error! ( "{}", error_msg );
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          ResponseType::SaveResult, &error_msg )); }} }

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
/// - complete_viewtree is complex: it runs a preorder pass (completing and reconciling each node) followed by a postorder pass (populating scaffolds like IDCol, AliasCol, etc.).
pub async fn update_from_and_rerender_buffer (
  org_buffer_text             : &str,
  typedb_driver               : &TypeDBDriver,
  config                      : &SkgConfig,
  tantivy_index               : &mut TantivyIndex,
  diff_mode_enabled           : bool,
  saveview_skgnodes_pre_save  : SkgNodeMap,
  viewuri_from_request_result : &Result<ViewUri, String>,
  conn_state                  : &mut ConnectionState,
) -> Result<SaveResponse, Box<dyn Error>> {
  if diff_mode_enabled {
    let sources : Vec<SourceName> =
      config . sources . keys() . cloned() . collect();
    validate_no_merge_commits ( &sources, config )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?; }

  let (forest, save_instructions, merge_instructions)
    : ( Tree<ViewNode>, Vec<DefineNode>, Vec<Merge> )
    = { let _span : tracing::span::EnteredSpan = tracing::info_span!(
            "buffer_to_viewnode_forest_and_save_instructions"
          ) . entered();
        buffer_to_viewnode_forest_and_save_instructions (
          org_buffer_text, config, typedb_driver,
          &saveview_skgnodes_pre_save ) . await
      } . map_err (
        |e| Box::new (e) as Box<dyn Error> ) ?;
  if forest . root() . children() . next() . is_none()
    { return Err ( "Nothing to save found in org_buffer_text"
                   . into() ); }

  { // update the graph
    let save_replacement : Option<TantivyIndex> =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "update_graph_minus_merges" ). entered();
        update_graph_minus_merges (
          save_instructions . clone(),
          config . clone(),
          tantivy_index,
          typedb_driver ) . await } ?;
    if let Some (new_index) = save_replacement {
      *tantivy_index = new_index; }
    let merge_replacement : Option<TantivyIndex> =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "merge_nodes" ). entered();
        merge_nodes (
          merge_instructions,
          config . clone(),
          tantivy_index,
          typedb_driver ) . await } ?;
    if let Some (new_index) = merge_replacement {
      *tantivy_index = new_index; } }

  update_views_after_save (
    forest,
    save_instructions,
    saveview_skgnodes_pre_save,
    diff_mode_enabled,
    config,
    typedb_driver,
    viewuri_from_request_result,
    conn_state ) . await }

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
    for (path, skgnode_diff) in &source_diff . skgnode_diffs {
      if skgnode_diff . status == GitDiffStatus::Deleted {
        if let Some (stem) = path . file_stem() {
          let id : ID = ID ( stem . to_string_lossy()
                             . into_owned() );
          result . insert ( id,
                            source_name . clone() ); }} }}
  result }

/// Every other open view sharing at least one PID with the saved view.
/// Over-approximates true collateral (which requires parsing the buffer
/// to know which PIDs actually changed). This is intentional: locking
/// too many buffers briefly is harmless; missing one could lose edits.
fn uris_of_views_to_lock (
  viewuri_from_request_result : &Result<ViewUri, String>,
  conn_state                  : &ConnectionState,
) -> Vec<ViewUri> {
  let saved_uri : &ViewUri = match viewuri_from_request_result {
    Ok (uri) => uri,
    Err (_)  => return Vec::new () };
  let pids : Vec<ID> =
    conn_state . memory . viewuri_to_pids (saved_uri);
  let mut collateral_views : HashSet<ViewUri> = HashSet::new ();
  for pid in &pids {
    for uri in conn_state . memory . views_containing (pid) {
      if &uri != saved_uri
        && ! uri . is_search ()
      { collateral_views . insert (uri); } } }
  collateral_views . into_iter () . collect () }
