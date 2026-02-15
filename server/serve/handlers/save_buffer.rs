use crate::from_text::buffer_to_viewnode_forest_and_save_instructions;
use crate::git_ops::diff::compute_diff_for_source;
use crate::git_ops::read_repo::{open_repo, head_is_merge_commit};
use crate::types::git::{SourceDiff, NodeDiffStatus, GitDiffStatus};
use crate::types::misc::{ID, SourceName};
use crate::merge::merge_nodes;
use crate::org_to_text::viewnode_forest_to_string;
use crate::save::update_graph_minus_merges;
use crate::serve::util::{ format_buffer_response_sexp, read_length_prefixed_content, send_response};
use crate::update_buffer::complete::complete_viewtree;
use crate::to_org::util::DefinitiveMap;
use crate::types::errors::SaveError;
use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold};
use crate::types::save::{DefineNode, Merge, format_save_error_as_org};
use crate::types::skgnodemap::{SkgNodeMap, skgnode_map_from_save_instructions};
use crate::types::tree::generic::{
  do_everywhere_in_tree_dfs,
  do_everywhere_in_tree_dfs_prunable };
use crate::update_buffer::graphnodestats::set_graphnodestats_in_forest;
use crate::update_buffer::viewnodestats::set_viewnodestats_in_forest;

use ego_tree::{Tree, NodeId, NodeMut, NodeRef};
use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::HashMap;
use std::error::Error;
use std::io::{BufReader, Write};
use std::net::TcpStream;
use std::path::Path;
use typedb_driver::TypeDBDriver;

/// Rust's response to Emacs for a save operation.
/// Contains the regenerated buffer content and any warnings/errors.
pub struct SaveResponse {
  pub buffer_content : String,
  pub errors         : Vec < String >,
}

impl SaveResponse {
  /// Format the response as an s-expression.
  /// Format: ((content "...") (errors ("error1" "error2" ...)))
  fn to_sexp_string ( &self ) -> String {
    format_buffer_response_sexp (
      & self . buffer_content,
      & self . errors ) }}

/// Handles save buffer requests from Emacs.
/// - Reads the buffer content with length prefix.
/// - Puts that text through `update_from_and_rerender_buffer`.
/// - Sends that back to Emacs (with a length prefix).
pub fn handle_save_buffer_request (
  reader            : &mut BufReader <TcpStream>,
  stream            : &mut TcpStream,
  typedb_driver     : &TypeDBDriver,
  config            : &SkgConfig,
  tantivy_index     : &TantivyIndex,
  diff_mode_enabled : bool ) {
  match read_length_prefixed_content (reader) {
    Ok (initial_buffer_content) => {
      match block_on(
        update_from_and_rerender_buffer (
          // Most of the work happens here.
          & initial_buffer_content,
          typedb_driver, config, tantivy_index,
          diff_mode_enabled ))
      { Ok (save_response) =>
        { // S-exp response format: ((content "...") (errors (...)))
          stream . write_all (
              { let response_sexp : String =
                  save_response . to_sexp_string ();
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
            send_response(stream, &error_msg);
          }} }}
    Err(err) => {
      let error_msg : String =
        format! ("Error reading buffer content: {}", err );
      println! ( "{}", error_msg );
      send_response ( stream, &error_msg ); }} }

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
  org_buffer_text   : &str,
  typedb_driver     : &TypeDBDriver,
  config            : &SkgConfig,
  tantivy_index     : &TantivyIndex,
  diff_mode_enabled : bool,
) -> Result<SaveResponse, Box<dyn Error>> {
  if diff_mode_enabled {
    let sources : Vec<SourceName> =
      config . sources . keys() . cloned() . collect();
    validate_no_merge_commits ( &sources, config )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?; }
  let (mut forest, save_instructions, merge_instructions)
    : ( Tree<ViewNode>, Vec<DefineNode>, Vec<Merge> )
    = buffer_to_viewnode_forest_and_save_instructions (
        org_buffer_text, config, typedb_driver
      ). await . map_err (
        |e| Box::new(e) as Box<dyn Error> ) ?;
  if forest.root().children().next().is_none() { return Err (
    "Nothing to save found in org_buffer_text" . into( )); }

  { // Remove diff data from tree.
    remove_all_branches_marked_removed ( &mut forest ) ?;
    remove_regenerable_scaffolds ( &mut forest ) ?;
    clear_diff_metadata ( &mut forest ) ?; }

  { // update the graph
    update_graph_minus_merges (
      save_instructions.clone(),
      config.clone(),
      tantivy_index,
      typedb_driver ). await ?;
    merge_nodes (
      merge_instructions,
      config.clone(),
      tantivy_index,
      typedb_driver ). await ?; }

  { // update the view and return it to the client
    let mut errors : Vec < String > = Vec::new ();
    let mut skgnode_map : SkgNodeMap =
      skgnode_map_from_save_instructions ( & save_instructions );
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
      complete_viewtree (
        &mut forest_mut,
        &mut skgnode_map,
        &mut visited,
        &source_diffs,
        config,
        typedb_driver,
        &mut errors,
        &deleted_id_src_map ). await ?;
      let ( container_to_contents, content_to_containers ) =
        set_graphnodestats_in_forest (
          &mut forest_mut,
          &mut skgnode_map,
          config,
          typedb_driver ). await ?;
      set_viewnodestats_in_forest (
        &mut forest_mut,
        &container_to_contents,
        &content_to_containers ); }
    let buffer_content : String =
      viewnode_forest_to_string ( & forest_mut ) ?;
    Ok ( SaveResponse { buffer_content, errors } ) }}

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
          ViewNodeKind::Scaff ( _ ) => false };
      if should_remove {
        node . detach();
        Ok ( false ) // Prune: branch removed, so don't recurse
      } else { Ok ( true ) }} )? ; // recurse into children
  Ok (( )) }

/// It's cheaper to regenerate these than to reconcile user edits.
/// If a deleted branch contains focus, passes it up to the parent.
pub fn remove_regenerable_scaffolds (
  forest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let forest_root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs_prunable (
    forest,
    forest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<bool, String> {
      let is_regenerable_scaffold : bool =
        matches! ( &node . value() . kind,
                   ViewNodeKind::Scaff ( Scaffold::IDCol ) |
                   ViewNodeKind::Scaff ( Scaffold::ID { .. } ) |
                   ViewNodeKind::Scaff ( Scaffold::TextChanged ) |
                   ViewNodeKind::Scaff ( Scaffold::AliasCol ) |
                   ViewNodeKind::Scaff ( Scaffold::Alias { .. } ));
      if is_regenerable_scaffold {
        let node_id : NodeId = node . id();
        if subtree_has_focus ( node . tree(), node_id )
        { if let Some ( mut parent ) = node . parent()
            { parent . value() . focused = true; }}
        node . detach();
        Ok ( false ) // subtree is gone, so don't recurse
      } else { Ok ( true ) }} ) ?;
  Ok (( )) }

fn subtree_has_focus (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
) -> bool {
  let node_ref : NodeRef<ViewNode> =
    match tree . get ( node_id ) {
      Some ( n ) => n,
      None => return false };
  if node_ref . value() . focused {
    return true; }
  for child in node_ref . children() {
    if subtree_has_focus ( tree, child . id() ) {
      return true; }}
  false }

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
