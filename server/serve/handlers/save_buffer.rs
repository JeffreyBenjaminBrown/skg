use crate::from_text::buffer_to_viewnode_forest_and_save_instructions;
use crate::git_ops::diff::compute_diff_for_source;
use crate::git_ops::read_repo::{open_repo, head_is_merge_commit};
use crate::serve::ConnectionState;
use crate::types::git::{SourceDiff, NodeDiffStatus, GitDiffStatus};
use crate::types::misc::{ID, SourceName};
use crate::types::viewnode::ViewUri;
use crate::merge::merge_nodes;
use crate::org_to_text::viewnode_forest_to_string;
use crate::save::update_graph_minus_merges;
use crate::serve::timing_log::{timed, timed_async};
use crate::serve::util::{
  view_uri_from_request,
  format_buffer_response_sexp_with_updates,
  read_length_prefixed_content,
  send_response};
use crate::update_buffer::complete::{complete_viewtree, complete_viewtree_with_deleted_pids};
use crate::to_org::util::DefinitiveMap;
use crate::types::errors::SaveError;
use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold};
use crate::types::save::{DefineNode, SaveNode, Merge, format_save_error_as_org};
use crate::types::skgnodemap::{SkgNodeMap, skgnode_map_from_save_instructions};
use crate::types::tree::generic::{
  do_everywhere_in_tree_dfs,
  do_everywhere_in_tree_dfs_prunable };
use crate::update_buffer::graphnodestats::set_graphnodestats_in_forest;
use crate::update_buffer::viewnodestats::set_viewnodestats_in_forest;

use ego_tree::{Tree, NodeId, NodeMut};
use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io::{BufReader, Write};
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

/// The full result of update_from_and_rerender_buffer,
/// including data needed for pool updates and re-rendering.
/// This is only constructed once at the very end of
/// update_from_and_rerender_buffer,
/// so every field is accurate the moment it springs into existence.
pub struct SaveResult {
  // The re-rendered buffer content and any errors/warnings. Sent back to Emacs as the response to the save request.
  pub response                     : SaveResponse,
  // The SkgNodeMap after complete_viewtree has finished. Contains every node the pipeline touched (from save instructions + pool seed + nodes fetched during completion). Merged back into SkgnodesInMemory.pool so subsequent requests see fresh data.
  pub skgnodemap_after_completion  : SkgNodeMap,
  // The parsed save/delete instructions from the buffer. Used after the pipeline to determine which PIDs changed, so we can find other views that contain those PIDs and re-render them.
  pub save_instructions            : Vec<DefineNode>,
  // Every TrueNode PID in the completed forest (all descendants). Passed to memory.update_view so the view's PID set stays current (used for pool GC and for finding affected views on future saves).
  pub forest_pids                  : Vec<ID>,
  // The completed viewnode forest. Stored in ViewState so collateral views can be completed (rather than re-rendered from scratch) on future saves.
  pub completed_forest             : Tree<ViewNode>,
}

impl SaveResponse {
  /// Format: ((content "...") (errors (...)) (other-views-to-update (("URI1" "c1") ...)))
  fn to_sexp_string ( &self ) -> String {
    format_buffer_response_sexp_with_updates (
      & self . saved_view,
      & self . errors,
      & self . collateral_views ) }}

/// Handles save buffer requests from Emacs.
/// - Reads the buffer content (with length prefix).
/// - Builds an initial SkgnodeMap from the ConnectionState's memory. It can change during the save process.
/// - 'update_from_and_rerender_buffer'
/// - 'update_memory_for_saved_view'
/// - 'rerender_collateral_views'
/// - Responds to Emacs (with length prefix).
pub fn handle_save_buffer_request (
  reader        : &mut BufReader <TcpStream>,
  stream        : &mut TcpStream, // PITFALL: writes to the same TCP stream as 'reader'
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  tantivy_index : &TantivyIndex,
  conn_state    : &mut ConnectionState,
) {
  let viewuri_from_request_result : Result<ViewUri, String> =
    view_uri_from_request ( request );
  let saveview_skgnodes_pre_save : SkgNodeMap = {
    // the skgnodes already in memory for this view
    let mut it : SkgNodeMap = SkgNodeMap::new ();
    if let Ok (ref uri) = viewuri_from_request_result {
      for pid in conn_state . memory . pids_in_view (uri) {
        if let Some (skgnode)
          = conn_state . memory . pool . get ( &pid )
          { it . insert ( pid, skgnode . clone () ); }} }
    it };
  match read_length_prefixed_content (reader) {
    Ok (initial_buffer_content) => {
      timed ( config, "update_from_and_rerender_buffer", || {
        match block_on(
          update_from_and_rerender_buffer (
            & initial_buffer_content,
            typedb_driver, config, tantivy_index,
            conn_state . diff_mode_enabled,
            saveview_skgnodes_pre_save ))
        { Ok (pipeline_result) => {
            let mut pipeline_result : SaveResult =
              pipeline_result;
            if let Some ( view_uri ) =
              update_memory_for_saved_view (
                &viewuri_from_request_result,
                &pipeline_result,
                conn_state )
            { pipeline_result . response . collateral_views =
                rerender_collateral_views (
                  view_uri,
                  &pipeline_result,
                  conn_state,
                  typedb_driver,
                  config ); }
            stream . write_all (
                { let response_sexp : String =
                    pipeline_result . response . to_sexp_string ();
                  let header : String =
                    format! ( "Content-Length: {}\r\n\r\n",
                                 response_sexp . len () );
                  format! ( "{}{}", header, response_sexp ) }
                . as_bytes() ). unwrap ();
            stream . flush() . unwrap (); }
          Err (err) => { // Check if this is a SaveError that should be formatted for the client
            if let Some(save_error) = err.downcast_ref::<SaveError>() {
              stream.write_all(
                { let response_sexp : String =
                    { let response : Sexp =
                        empty_response_sexp (
                          & { let error_buffer_content : String =
                                format_save_error_as_org(save_error);
                              error_buffer_content } );
                      response }
                    . to_string ();
                  let full_response : String =
                    format! (
                      "{}{}",
                      { let header : String =
                          format! ( "Content-Length: {}\r\n\r\n",
                                    response_sexp . len ( ));
                        header },
                      response_sexp );
                  full_response
                } .as_bytes( )) . unwrap();
              stream.flush().unwrap();
            } else {
              let error_msg : String =
                format!("Error processing buffer content: {}", err);
              println!("{}", error_msg);
              send_response(stream, &error_msg); }} }} ); }
    Err(err) => {
      let error_msg : String =
        format! ("Error reading buffer content: {}", err );
      println! ( "{}", error_msg );
      send_response ( stream, &error_msg ); }} }

/// Update the SkgnodesInMemory's pool of skgnodes,
/// and its view of the saved buffer (but not the collateral ones).
/// Returns the ViewUri on success,
/// or None if viewuri_from_request_result was Err.
/// TODO : On error, should instead
/// unwind the entire save and report the error.
fn update_memory_for_saved_view<'a> (
  viewuri_from_request_result : &'a Result<ViewUri, String>,
  pipeline_result : &SaveResult,
  conn_state      : &mut ConnectionState,
) -> Option<&'a ViewUri> {
  let view_uri : &ViewUri = match viewuri_from_request_result {
    Ok ( uri ) => uri,
    Err ( _ ) =>
      return None };
  for (pid, skgnode) // update the skgnode pool
    in &pipeline_result . skgnodemap_after_completion
    { conn_state . memory . pool . insert ( pid . clone (),
                                            skgnode . clone () ); }
  conn_state . memory . update_view (
    view_uri,
    pipeline_result . completed_forest . clone (),
    &pipeline_result . forest_pids );
  Some ( view_uri ) }

/// When the user saves a buffer, the nodes they edited
/// might also appear in other open Emacs buffers. Those are
/// "collateral views", which this updates.
fn rerender_collateral_views (
  view_uri        : &ViewUri,
  pipeline_result : &SaveResult,
  conn_state      : &mut ConnectionState,
  typedb_driver   : &TypeDBDriver,
  config          : &SkgConfig,
) -> Vec<(ViewUri, String)> {
  let changed_pids : HashSet<ID> =
    pipeline_result . save_instructions . iter ()
    . filter_map ( |instr| match instr {
      DefineNode::Save ( SaveNode (n)) =>
        n . ids . first () . cloned (),
      DefineNode::Delete ( dn ) =>
        Some ( dn . id . clone () ) } )
    . collect ();
  let deleted_pids : HashSet<ID> =
    pipeline_result . save_instructions . iter ()
    . filter_map ( |instr| match instr {
      DefineNode::Delete ( dn ) =>
        Some ( dn . id . clone () ),
      _ => None } )
    . collect ();
  let collateral_views : HashSet<ViewUri> =
    changed_pids . iter ()
    . flat_map ( |pid| conn_state . memory . views_containing (pid) )
    . filter ( |uri| // this, the saved view, is already up to date
               uri != view_uri )
    . collect ();
  let mut updates : Vec<(ViewUri, String)> =
    Vec::new ();
  for uri in collateral_views {
    if let Some ( (uri, text) ) =
      complete_collateral_view (
        &uri, conn_state, typedb_driver,
        config, &deleted_pids )
    { updates . push ( (uri, text) ); } }
  updates }

/// Complete a single collateral view by re-running the
/// completion pipeline on the stored forest. Nodes whose
/// PIDs are in deleted_pids are degraded to DeletedNode.
/// The server completes collateral views from its stored
/// forest rather than requesting the buffer from Emacs.
/// This works correctly only if the user adheres to the
/// recommendation that at most one buffer has unsaved
/// changes at a time. In the future, hopefully, this
/// recommendation will be retracted, and we will instead
/// update by parsing the raw buffer text of the collateral
/// view.
fn complete_collateral_view (
  uri           : &ViewUri,
  conn_state    : &mut ConnectionState,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  deleted_pids  : &HashSet<ID>,
) -> Option<(ViewUri, String)> {
  let mut forest : Tree<ViewNode> =
    conn_state . memory . forest_in_view ( uri ) ? . clone ();
  let mut map : SkgNodeMap = {
    let mut it : SkgNodeMap = SkgNodeMap::new ();
    for pid in conn_state . memory . pids_in_view ( uri ) {
      if let Some ( skgnode )
        = conn_state . memory . pool . get ( &pid )
        { it . insert ( pid, skgnode . clone () ); } }
    it };
  match block_on ( async {
    let mut errors : Vec<String> = Vec::new ();
    let mut defmap : DefinitiveMap = DefinitiveMap::new ();
    complete_viewtree_with_deleted_pids (
      &mut forest, &mut map, &mut defmap,
      &None, config, typedb_driver,
      &mut errors, &HashMap::new (),
      deleted_pids ) . await ?;
    let ( container_to_contents, content_to_containers ) =
      set_graphnodestats_in_forest (
        &mut forest, &mut map,
        config, typedb_driver ) . await ?;
    set_viewnodestats_in_forest (
      &mut forest,
      &container_to_contents,
      &content_to_containers );
    let text : String =
      viewnode_forest_to_string ( &forest ) ?;
    Result::<String, Box<dyn Error>>::Ok ( text ) } )
  { Ok ( text ) => {
      for (pid, skgnode) in map {
        conn_state . memory . pool . insert (
          pid, skgnode ); }
      let pids : Vec<ID> = allpids_from_forest ( &forest );
      conn_state . memory . update_view (
        uri, forest, &pids );
      Some ( (uri . clone (), text) ) },
    Err ( e ) => {
      eprintln! (
        "Warning: Failed to complete collateral view: {}",
        e );
      None }} }

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
  org_buffer_text            : &str,
  typedb_driver              : &TypeDBDriver,
  config                     : &SkgConfig,
  tantivy_index              : &TantivyIndex,
  diff_mode_enabled          : bool,
  saveview_skgnodes_pre_save : SkgNodeMap,
) -> Result<SaveResult, Box<dyn Error>> {
  if diff_mode_enabled {
    let sources : Vec<SourceName> =
      config . sources . keys() . cloned() . collect();
    validate_no_merge_commits ( &sources, config )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?; }

  let (mut forest, save_instructions, merge_instructions)
    : ( Tree<ViewNode>, Vec<DefineNode>, Vec<Merge> )
    = timed_async (
        config,
        "buffer_to_viewnode_forest_and_save_instructions",
        buffer_to_viewnode_forest_and_save_instructions (
          org_buffer_text, config, typedb_driver,
          &saveview_skgnodes_pre_save )
      ). await . map_err (
        |e| Box::new(e) as Box<dyn Error> ) ?;
  if forest . root() . children() . next() . is_none()
    { return Err ( "Nothing to save found in org_buffer_text"
                   . into() ); }

  { // Remove diff data from tree.
    remove_all_branches_marked_removed ( &mut forest ) ?;
    clear_diff_metadata ( &mut forest ) ?; }

  { // update the graph
    timed_async ( config, "update_graph_minus_merges",
                  async {
                    update_graph_minus_merges (
                      save_instructions.clone(),
                      config.clone(),
                      tantivy_index,
                      typedb_driver ). await ?;
                    Result::<(), Box<dyn Error>>::Ok (( )) }
                ). await ?;
    timed_async ( config, "merge_nodes",
                  async {
                    merge_nodes (
                      merge_instructions,
                      config.clone(),
                      tantivy_index,
                      typedb_driver ). await ?;
                    Result::<(), Box<dyn Error>>::Ok (( )) }
                ). await ?; }

  { // update the view and return it to the client
    let mut errors : Vec < String > = Vec::new ();
    let mut skgnode_map : SkgNodeMap = {
      let mut it : SkgNodeMap =
        skgnode_map_from_save_instructions ( & save_instructions );
      for (pid, skgnode) in saveview_skgnodes_pre_save { // Use these to fill in missing values, giving preference to the user's edits.
        it . entry ( pid )
          . or_insert_with ( || skgnode ); }
      it };
    let mut forest_mut : Tree<ViewNode> = forest;
    let source_diffs : Option<HashMap<SourceName, SourceDiff>> =
      // Used for view request execution.
      if diff_mode_enabled
      { Some ( compute_diff_for_every_source ( config )) }
      else { None };
    let deleted_id_src_map : HashMap<ID, SourceName> =
      source_diffs . as_ref()
      . map ( |d| deleted_ids_to_source ( d ))
      . unwrap_or_default();
    { // mutate it before re-rendering it
      let mut visited : DefinitiveMap = DefinitiveMap::new();
      timed_async ( config, "complete_viewtree",
                    complete_viewtree (
                      &mut forest_mut,
                      &mut skgnode_map,
                      &mut visited,
                      &source_diffs,
                      config,
                      typedb_driver,
                      &mut errors,
                      &deleted_id_src_map )) . await ?;
      let ( container_to_contents, content_to_containers ) =
        timed_async ( config, "set_graphnodestats_in_forest",
                      set_graphnodestats_in_forest (
                        &mut forest_mut,
                        &mut skgnode_map,
                        config,
                        typedb_driver )) . await ?;
      timed ( config, "set_viewnodestats_in_forest",
              || set_viewnodestats_in_forest (
                &mut forest_mut,
                &container_to_contents,
                &content_to_containers )); }
    let forest_pids : Vec<ID> =
      allpids_from_forest (&forest_mut);
    let buffer_content : String =
      timed ( config, "save_render",
              || viewnode_forest_to_string ( & forest_mut )) ?;
    Ok ( SaveResult {
      response : SaveResponse {
        saved_view : buffer_content, errors,
        collateral_views : Vec::new () },
      skgnodemap_after_completion : skgnode_map,
      save_instructions,
      forest_pids,
      completed_forest : forest_mut } ) }}

/// Every True/Deleted PID at any depth in the forest.
fn allpids_from_forest (
  forest : &Tree<ViewNode>
) -> Vec<ID> {
  let mut ids : Vec<ID> = Vec::new ();
  for node_ref in forest . root () . descendants () {
    match &node_ref . value () . kind {
      ViewNodeKind::True ( t )    =>
        ids . push ( t . id . clone () ),
      ViewNodeKind::Deleted ( d ) =>
        ids . push ( d . id . clone () ),
      _ => {} } }
  ids }

/// Strip from the forest every branch whose root
///   is marked as removed or removed-here.
/// Exception: Does not strip:
///   - top-level branches (children of BufferRoot)
///   - parent_ignores nodes
/// Uses single-pass DFS: removes branch roots as encountered,
/// skipping recursion into removed branches.
pub fn remove_all_branches_marked_removed (
  forest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let forest_root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs_prunable (
    forest,
    forest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<bool, String> {
      let is_forest_root_child : bool = {
        match node . parent() {
          Some ( mut p ) =>
            matches! ( &p . value() . kind,
                       ViewNodeKind::Scaff ( Scaffold::BufferRoot )),
          None => false } };
      let should_remove : bool =
        match &node . value() . kind {
          ViewNodeKind::True ( t ) => {
            matches! ( t . diff,
                       Some ( NodeDiffStatus::Removed ) |
                       Some ( NodeDiffStatus::RemovedHere ))
            && ! is_forest_root_child
            && ! t . parent_ignores },
          ViewNodeKind::Scaff ( _ ) |
          ViewNodeKind::Deleted ( _ ) |
          ViewNodeKind::DeletedScaff => false };
      if should_remove {
        node . detach();
        Ok ( false ) // Prune: branch removed, so don't recurse
      } else { Ok ( true ) }} )? ; // recurse into children
  Ok (( )) }

/// Clear diff metadata from all TrueNodes in the forest.
/// Scaffolds with diff fields (Alias, ID) are already removed
/// by remove_diff_only_scaffolds before this runs.
pub fn clear_diff_metadata (
  forest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let forest_root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs (
    forest,
    forest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<(), String> {
      // IGNORES scaffolds, even though some scaffolds *can* have diff data. Since all such kinds are regenerated from scratch, they don't need processing here. See remove_regenerable_scaffolds.
      if let ViewNodeKind::True ( t ) = &mut node . value() . kind {
        t . diff = None; }
      Ok (()) }
  ) ?;
  Ok (()) }

/// Check if any source's HEAD is a merge commit.
/// Returns an error message if so,
/// as diff computation is ambiguous for merge commits.
pub fn validate_no_merge_commits (
  sources : &[SourceName],
  config  : &SkgConfig,
) -> Result<(), String> {
  for source in sources { // Get the source path from config
    if let Some ( source_config ) = config . sources . get ( source ) {
      let source_path : &Path =
        Path::new ( &source_config . path );
      if let Some ( repo ) = open_repo ( source_path ) {
        match head_is_merge_commit ( &repo ) {
          Ok ( true ) => {
            return Err ( format! (
              "Cannot compute diff: HEAD is a merge commit in source '{}'.",
              source )); },
          Ok ( false ) => {},
          Err ( e ) => { // Git error - log but continue
            eprintln! ( "Warning: Could not check merge commit status for '{}': {}",
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
    match compute_diff_for_source ( source_path ) {
      Ok ( diff ) => {
        source_diffs . insert ( source_name . clone(), diff ); },
      Err ( e ) => { // Log error but continue with other sources
        eprintln! (
          "Warning: Failed to compute diff for source '{}': {}",
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
        if let Some ( stem ) = path . file_stem() {
          let id : ID = ID ( stem . to_string_lossy()
                             . into_owned() );
          result . insert ( id,
                            source_name . clone() ); }} }}
  result }
