/// Diff application module for git diff view.
/// Applies diff information to a forest of ViewNodes.

use crate::types::git::{SourceDiff, SkgnodeDiff, GitDiffStatus, NodeDiffStatus, FieldDiffStatus, NodeChanges};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::phantom::{source_for_phantom, title_for_phantom, phantom_diff_status};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold, viewnode_from_scaffold, mk_phantom_viewnode, };
use crate::types::tree::generic::do_everywhere_in_tree_dfs;
use crate::types::tree::viewnode_skgnode::pid_and_source_from_treenode;

use ego_tree::{Tree, NodeMut, NodeRef, NodeId};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::PathBuf;

/// Apply diff information to a forest of ViewNodes.
/// This modifies nodes in place to
/// - add diff markers
/// - insert removed ('phantom') nodes
pub fn apply_diff_to_forest (
  forest             : &mut Tree<ViewNode>,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs (
    forest, root_id,
    &mut |node_mut| { process_node_for_diff (
                        node_mut, source_diffs,
                        deleted_id_src_map, config ) } )?;
  Ok (( )) }

/// Process a single node for diff markers.
/// So far, only TrueNodes need processing; scaffolds are regenerated fresh.
/// TODO : Sharing relationships (hides, overrides, subscribes)
/// will need processing here. But none of the regenerable scaffolds
/// (see remove_regenerable_scaffolds) ever will.
fn process_node_for_diff (
  mut node_mut       : NodeMut<ViewNode>,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), String> {
  match &node_mut . value() . kind . clone() {
    ViewNodeKind::True ( _ ) =>
      process_truenode_diff (
        node_mut, source_diffs, deleted_id_src_map, config ),
    _ => Ok (()) }}

/// Process a TrueNode:
/// - set diff marker based on file status
/// - prepend TextChanged/IDCol child if needed
/// - mark NewHere children and insert phantoms for removed children.
fn process_truenode_diff (
  mut node_mut       : NodeMut<ViewNode>,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), String> {
  let tree_node_id : NodeId =
    node_mut . id();
  let (pid, source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      node_mut . tree(), tree_node_id, "process_truenode_diff"
    ) . map_err ( |e| e . to_string() ) ?;
  let source_diff : &SourceDiff =
    match source_diffs . get ( &source ) {
      Some ( d ) => d,
      None => return Ok (( )) };
  if ! source_diff . is_git_repo { // Mark node as not-in-git
    if let ViewNodeKind::True ( ref mut t )
      = node_mut . value() . kind
      { t . diff = Some ( NodeDiffStatus::NotInGit ); }
    return Ok (( )); }
  let skgnode_diff : &SkgnodeDiff = {
    let file_path : PathBuf =
      PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
    match source_diff . skgnode_diffs . get ( &file_path ) {
      Some ( d ) => d,
      None => return Ok (( )) }};
  if maybe_mark_file_level_diff ( &mut node_mut, skgnode_diff ) {
    return Ok (( )); }
  if let Some ( ref node_changes ) = skgnode_diff . node_changes {
    if node_changes . text_changed {
      node_mut . prepend (
        viewnode_from_scaffold ( Scaffold::TextChanged )); }
    if ! node_changes . ids_diff . iter()
           . all ( |d| matches! ( d, Diff_Item::Unchanged(_) )) {
      prepend_idcol_with_children (
        &mut node_mut, node_changes ); }
    process_truenode_contains_diff (
      &mut node_mut, tree_node_id, node_changes,
      source_diff, source_diffs, deleted_id_src_map, config ) ?; }
  Ok (()) }

/// Try to handle file-level diff status (Added/Deleted).
/// Returns - true if handled (caller should return),
///         - false for Modified files.
fn maybe_mark_file_level_diff (
  node_mut  : &mut NodeMut<ViewNode>,
  skgnode_diff : &SkgnodeDiff,
) -> bool {
  let node_diff_status : Option<NodeDiffStatus> =
    match skgnode_diff . status {
      GitDiffStatus::Added   => Some ( NodeDiffStatus::New ),
      GitDiffStatus::Deleted => Some ( NodeDiffStatus::Removed ),
      GitDiffStatus::Modified => None };
  match node_diff_status {
    Some ( diff_status ) => {
      if let ViewNodeKind::True ( ref mut t )
        = node_mut . value() . kind
        { t . diff = Some ( diff_status ); }
      true },
    None => false }}

/// Prepend an IDCol scaffold to the input's children,
/// populated with ID scaffold grandchildren.
/// (The DFS traversal won't visit nodes created mid-traversal,
/// so we populate ahead of time with this.)
fn prepend_idcol_with_children (
  node_mut     : &mut NodeMut<ViewNode>,
  node_changes : &NodeChanges,
) {
  let idcol_node : ViewNode =
    viewnode_from_scaffold ( Scaffold::IDCol );
  let idcol_treeid : NodeId =
    node_mut . prepend ( idcol_node ) . id();
  let mut idcol_mut : NodeMut<ViewNode> =
    node_mut . tree() . get_mut ( idcol_treeid ) . unwrap();
  for entry in &node_changes . ids_diff {
    let (id_str, diff) : (String, Option<FieldDiffStatus>) =
      match entry {
        Diff_Item::Unchanged ( id ) =>
          ( id . 0 . clone(), None ),
        Diff_Item::New ( id ) =>
          ( id . 0 . clone(), Some ( FieldDiffStatus::New )),
        Diff_Item::Removed ( id ) =>
          ( id . 0 . clone(), Some ( FieldDiffStatus::Removed )), };
    let id_scaffold : Scaffold =
      Scaffold::ID { id: id_str.into(), diff };
    let id_viewnode : ViewNode =
      viewnode_from_scaffold ( id_scaffold );
    idcol_mut . append ( id_viewnode ); }}

/// Mark existing children as NewHere if added to contains,
/// and insert phantom nodes for removed children.
fn process_truenode_contains_diff (
  node_mut           : &mut NodeMut<ViewNode>,
  tree_node_id       : NodeId,
  node_changes       : &NodeChanges,
  source_diff        : &SourceDiff,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), String> {
  let added_ids : HashSet<&ID> =
    node_changes . contains_diff . iter()
      . filter_map ( |d| match d {
          Diff_Item::New ( id ) => Some ( id ),
          _ => None })
      . collect();
  mark_newhere_children (
    node_mut, tree_node_id, &added_ids, source_diff );
  insert_phantom_nodes_for_removed_children (
    node_mut, node_changes,
    source_diffs, deleted_id_src_map, config ) }

/// If a child corresponds to a file that already existed in HEAD
/// but in HEAD it was not content here, mark it NewHere.
fn mark_newhere_children (
  node_mut     : &mut NodeMut<ViewNode>,
  tree_node_id : NodeId,
  added_ids    : &HashSet<&ID>,
  source_diff  : &SourceDiff,
) {
  let child_ids : Vec<NodeId> = {
    let node_ref : NodeRef<ViewNode> =
      node_mut . tree() . get ( tree_node_id ) . unwrap();
    node_ref . children() . map ( |c| c . id() ) . collect() };
  for child_id in child_ids {
    let mut child : NodeMut<ViewNode> =
      node_mut . tree() . get_mut ( child_id ) . unwrap();
    if let ViewNodeKind::True ( ref mut t ) = child . value() . kind {
      if added_ids . contains ( &t.id ) {
        let child_file_path : PathBuf =
          PathBuf::from ( format! ( "{}.skg", t.id . 0 ));
        let is_new_file : bool =
          source_diff . skgnode_diffs . get ( &child_file_path )
            . map ( |fd| fd . status == GitDiffStatus::Added )
            . unwrap_or ( false );
        if ! is_new_file {
          t . diff = Some ( NodeDiffStatus::NewHere ); }} } }}

fn insert_phantom_nodes_for_removed_children (
  node_mut           : &mut NodeMut<ViewNode>,
  node_changes       : &NodeChanges,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), String> {
  let empty_children : HashMap<ID, SourceName> = HashMap::new();
  let empty_map : HashMap<ID, SkgNode> = HashMap::new();
  let removed_ids : Vec<&ID> =
    node_changes . contains_diff . iter()
      . filter_map ( |d| match d {
          Diff_Item::Removed ( id ) => Some ( id ),
          _ => None })
      . collect();
  for removed_child_id in removed_ids {
    let child_source : SourceName =
      source_for_phantom(
        removed_child_id, &empty_children,
        deleted_id_src_map, &empty_map, config ) ?;
    let child_diff_status : NodeDiffStatus =
      phantom_diff_status(
        removed_child_id, &child_source,
        Some( source_diffs ) );
    let child_title : String =
      title_for_phantom(
        removed_child_id, &child_source,
        Some( source_diffs ), &empty_map, config );
    node_mut . prepend (
      mk_phantom_viewnode (
        removed_child_id . clone(),
        child_source,
        child_title,
        child_diff_status )); }
  Ok (()) }
