/// Diff application module for git diff view.
/// Applies diff information to a forest of OrgNodes.

use crate::dbs::filesystem::one_node::skgnode_from_pid_and_source;
use crate::types::git::{SourceDiff, FileDiff, GitDiffStatus, NodeDiffStatus, FieldDiffStatus, NodeChanges};
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{
  OrgNode, OrgNodeKind, Scaffold,
  orgnode_from_scaffold, mk_indefinitive_orgnode,
};
use crate::types::tree::generic::do_everywhere_in_tree_dfs;
use crate::types::tree::orgnode_skgnode::pid_and_source_from_treenode;

use ego_tree::{Tree, NodeMut, NodeRef, NodeId};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::PathBuf;

/// Apply diff information to a forest of OrgNodes.
/// This modifies nodes in place to
/// - add diff markers
/// - insert removed ('phantom') nodes
pub fn apply_diff_to_forest (
  forest       : &mut Tree<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
  config       : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs (
    forest, root_id,
    &mut |node_mut| { process_node_for_diff (
                        node_mut, source_diffs, config ) } )?;
  Ok (( )) }

/// Process a single node for diff markers.
/// Dispatches to specific handlers based on node kind.
fn process_node_for_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
  config       : &SkgConfig,
) -> Result<(), String> {
  match &node_mut . value() . kind . clone() {
    OrgNodeKind::True ( _ ) =>
      process_truenode_diff ( node_mut, source_diffs, config ),
    OrgNodeKind::Scaff ( Scaffold::AliasCol ) =>
      process_aliascol_diff ( node_mut, source_diffs ),
    OrgNodeKind::Scaff ( Scaffold::Alias { .. } ) =>
      process_alias_diff ( node_mut, source_diffs ),
    OrgNodeKind::Scaff ( Scaffold::IDCol ) =>
      process_idcol_diff ( node_mut, source_diffs ),
    OrgNodeKind::Scaff ( Scaffold::ID { .. } ) =>
      process_id_diff ( node_mut, source_diffs ),
    _ => Ok (()) }}

/// Process a TrueNode:
/// - set diff marker based on file status
/// - prepend TextChanged/IDCol child if needed
/// - mark NewHere children and insert phantoms for removed children.
fn process_truenode_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
  config       : &SkgConfig,
) -> Result<(), String> {
  let tree_node_id : NodeId =
    node_mut . id();
  let (pid, source) : (ID, String) =
    pid_and_source_from_treenode (
      node_mut . tree(), tree_node_id, "process_truenode_diff"
    ) . map_err ( |e| e . to_string() ) ?;
  let source_diff : &SourceDiff =
    match source_diffs . get ( &source ) {
      Some ( d ) => d,
      None => return Ok (( )) };
  if ! source_diff . is_git_repo { // Mark node as not-in-git
    if let OrgNodeKind::True ( ref mut t )
      = node_mut . value() . kind
      { t . diff = Some ( NodeDiffStatus::NotInGit ); }
    return Ok (( )); }
  let file_diff : &FileDiff = {
    let file_path : PathBuf =
      PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
    match source_diff . file_diffs . get ( &file_path ) {
      Some ( d ) => d,
      None => return Ok (( )) }};
  if maybe_mark_file_level_diff ( &mut node_mut, file_diff ) {
    return Ok (( )); }
  if let Some ( ref node_changes ) = file_diff . node_changes {
    if node_changes . text_changed {
      node_mut . prepend (
        orgnode_from_scaffold ( Scaffold::TextChanged )); }
    if ! ( node_changes . ids_diff . added   . is_empty() &&
           node_changes . ids_diff . removed . is_empty() ) {
      node_mut . prepend (
        orgnode_from_scaffold ( Scaffold::IDCol )); }
    process_truenode_contains_diff (
      &mut node_mut, tree_node_id,
      node_changes, source_diff, &source, config ) ?; }
  Ok (()) }

/// Try to handle file-level diff status (Added/Deleted).
/// Returns - true if handled (caller should return),
///         - false for Modified files.
fn maybe_mark_file_level_diff (
  node_mut  : &mut NodeMut<OrgNode>,
  file_diff : &FileDiff,
) -> bool {
  let node_diff_status : Option<NodeDiffStatus> =
    match file_diff . status {
      GitDiffStatus::Added   => Some ( NodeDiffStatus::New ),
      GitDiffStatus::Deleted => Some ( NodeDiffStatus::Removed ),
      GitDiffStatus::Modified => None };
  match node_diff_status {
    Some ( diff_status ) => {
      if let OrgNodeKind::True ( ref mut t )
        = node_mut . value() . kind
        { t . diff = Some ( diff_status ); }
      true },
    None => false }}

/// Mark existing children as NewHere if added to contains,
/// and insert phantom nodes for removed children.
fn process_truenode_contains_diff (
  node_mut     : &mut NodeMut<OrgNode>,
  tree_node_id : NodeId,
  node_changes : &NodeChanges,
  source_diff  : &SourceDiff,
  source       : &str,
  config       : &SkgConfig,
) -> Result<(), String> {
  let added_ids : HashSet<&ID> =
    node_changes . contains_diff . added . iter() . collect();
  mark_newhere_children (
    node_mut, tree_node_id, &added_ids, source_diff );
  insert_phantom_nodes_for_removed_children (
    node_mut, node_changes, source_diff, source, config ) }

/// If a child corresponds to a file that already existed in HEAD
/// but in HEAD it was not content here, mark it NewHere.
fn mark_newhere_children (
  node_mut     : &mut NodeMut<OrgNode>,
  tree_node_id : NodeId,
  added_ids    : &HashSet<&ID>,
  source_diff  : &SourceDiff,
) {
  let child_ids : Vec<NodeId> = {
    let node_ref : NodeRef<OrgNode> =
      node_mut . tree() . get ( tree_node_id ) . unwrap();
    node_ref . children() . map ( |c| c . id() ) . collect() };
  for child_id in child_ids {
    let mut child : NodeMut<OrgNode> =
      node_mut . tree() . get_mut ( child_id ) . unwrap();
    if let OrgNodeKind::True ( ref mut t ) = child . value() . kind {
      if let Some ( ref id ) = t . id_opt {
        if added_ids . contains ( id ) {
          let child_file_path : PathBuf =
            PathBuf::from ( format! ( "{}.skg", id . 0 ) );
          let is_new_file : bool =
            source_diff . file_diffs . get ( &child_file_path )
              . map ( |fd| fd . status == GitDiffStatus::Added )
              . unwrap_or ( false );
          if ! is_new_file {
            t . diff = Some ( NodeDiffStatus::NewHere ); }} }} }}

/// Insert phantom nodes for children removed from contents.
fn insert_phantom_nodes_for_removed_children (
  node_mut     : &mut NodeMut<OrgNode>,
  node_changes : &NodeChanges,
  source_diff  : &SourceDiff,
  source       : &str,
  config       : &SkgConfig,
) -> Result<(), String> {
  for removed_child_id in &node_changes . contains_diff . removed {
    let child_file_diff : Option<&FileDiff> = {
      let child_file_path : PathBuf =
        PathBuf::from ( format! ( "{}.skg", removed_child_id . 0 ) );
      source_diff . file_diffs . get ( &child_file_path ) };
    let child_is_deleted : bool =
      child_file_diff
        . map ( |fd| fd . status == GitDiffStatus::Deleted )
        . unwrap_or ( false );
    let child_diff_status : NodeDiffStatus =
      if child_is_deleted { NodeDiffStatus::Removed }
      else                { NodeDiffStatus::RemovedHere };
    let child_title : String =
      title_for_phantom ( removed_child_id, child_is_deleted,
                          source_diff, source, config ) ?;
    node_mut . prepend (
      mk_phantom_orgnode (
        removed_child_id . clone(),
        source . to_string(),
        child_title,
        child_diff_status )); }
  Ok (()) }

/// Get the title for a phantom node.
/// For deleted files, uses deleted_nodes from SourceDiff.
/// For moved files (RemovedHere), reads from disk.
fn title_for_phantom (
  removed_child_id : &ID,
  child_is_deleted : bool, // gone from worktree, not just here
  source_diff      : &SourceDiff,
  source           : &str,
  config           : &SkgConfig,
) -> Result<String, String> {
  if child_is_deleted {
    source_diff . deleted_nodes . get ( removed_child_id )
      . map ( |n| n . title . clone() )
      . ok_or_else ( || format! (
        "Cannot determine title for deleted node '{}': \
         not found in deleted_nodes",
        removed_child_id . 0 ))
  } else {
    skgnode_from_pid_and_source (
        config, removed_child_id . clone(), source )
      . map ( |n| n . title )
      . map_err ( |e| format! (
        "Cannot determine title for moved node '{}': {}",
        removed_child_id . 0, e )) }}

/// Create an indefinitive phantom OrgNode with a diff status.
fn mk_phantom_orgnode (
  id     : ID,
  source : String,
  title  : String,
  diff   : NodeDiffStatus,
) -> OrgNode {
  let mut orgnode : OrgNode =
    mk_indefinitive_orgnode ( id, source, title, false );
  if let OrgNodeKind::True ( ref mut t ) = orgnode . kind
    { t . diff = Some ( diff ); }
  orgnode }

/// Process an AliasCol: append removed Alias children.
fn process_aliascol_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  let file_diff : &FileDiff =
    match get_ancestor_file_diff ( &mut node_mut, source_diffs, 1 ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_changes : &NodeChanges =
    match &file_diff . node_changes {
      Some ( c ) => c,
      None => return Ok (()) };
  for removed_alias in &node_changes . aliases_diff . removed {
    let alias_scaffold : Scaffold =
      Scaffold::Alias {
        text: removed_alias . clone(),
        diff: Some ( FieldDiffStatus::Removed ) };
    let alias_orgnode : OrgNode =
      orgnode_from_scaffold ( alias_scaffold );
    node_mut . append ( alias_orgnode ); }
  Ok (()) }

/// Process an Alias scaffold: mark as new if in added list.
fn process_alias_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  let alias_text : String =
    match &node_mut . value() . kind {
      OrgNodeKind::Scaff ( Scaffold::Alias { text, .. } ) => text . clone(),
      _ => return Ok (()) };
  let file_diff : &FileDiff = // Grandparent is the TrueNode
    match get_ancestor_file_diff ( &mut node_mut, source_diffs, 2 ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_changes : &NodeChanges =
    match &file_diff . node_changes {
      Some ( c ) => c,
      None => return Ok (()) };
  if node_changes . aliases_diff . added . contains ( &alias_text ) {
    if let OrgNodeKind::Scaff ( Scaffold::Alias { diff, .. } ) = &mut node_mut . value() . kind {
      *diff = Some ( FieldDiffStatus::New ); }}
  Ok (()) }

/// Process an IDCol: append added/removed ID children.
fn process_idcol_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  let file_diff : &FileDiff =
    match get_ancestor_file_diff ( &mut node_mut, source_diffs, 1 ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_changes : &NodeChanges =
    match &file_diff . node_changes {
      Some ( c ) => c,
      None => return Ok (()) };
  for added_id in &node_changes . ids_diff . added {
    let id_scaffold : Scaffold =
      Scaffold::ID {
        value: added_id . 0 . clone(),
        diff: Some ( FieldDiffStatus::New ) };
    let id_orgnode : OrgNode =
      orgnode_from_scaffold ( id_scaffold );
    node_mut . append ( id_orgnode ); }
  for removed_id in &node_changes . ids_diff . removed {
    let id_scaffold : Scaffold =
      Scaffold::ID {
        value: removed_id . 0 . clone(),
        diff: Some ( FieldDiffStatus::Removed ) };
    let id_orgnode : OrgNode =
      orgnode_from_scaffold ( id_scaffold );
    node_mut . append ( id_orgnode ); }
  Ok (()) }

/// Process an ID scaffold: mark as new if in added list.
fn process_id_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  let id_value : ID =
    match &node_mut . value() . kind {
      OrgNodeKind::Scaff ( Scaffold::ID { value, .. } ) =>
        ID ( value . clone() ),
      _ => return Ok (()) };
  let file_diff : &FileDiff = // Grandparent is the TrueNode
    match get_ancestor_file_diff ( &mut node_mut, source_diffs, 2 ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_changes : &NodeChanges =
    match &file_diff . node_changes {
      Some ( c ) => c,
      None => return Ok (()) };
  if node_changes . ids_diff . added . contains ( &id_value ) {
    if let OrgNodeKind::Scaff ( Scaffold::ID { diff, .. } ) = &mut node_mut . value() . kind {
      *diff = Some ( FieldDiffStatus::New ); }}
  Ok (()) }

/// Get the FileDiff for an ancestor TrueNode.
/// generation: 1 = parent, 2 = grandparent, etc.
/// Gen 0 is valid.
fn get_ancestor_file_diff<'a> (
  node_mut     : &mut NodeMut<OrgNode>,
  source_diffs : &'a HashMap<String, SourceDiff>,
  generation   : usize,
) -> Option<&'a FileDiff> {
  let start_id : NodeId =
    node_mut . id();
  let ancestor_id : NodeId = { // Climb to the ancestor
    let mut node_ref : NodeRef<OrgNode> =
      node_mut . tree() . get ( start_id ) ?;
    for _ in 0..generation {
      node_ref = node_ref . parent() ?; }
    node_ref . id() };
  let (pid, source) : (ID, String) =
    pid_and_source_from_treenode (
      node_mut . tree(), ancestor_id, "get_ancestor_file_diff"
    ) . ok() ?;
  let source_diff : &SourceDiff =
    source_diffs . get ( &source ) ?;
  if ! source_diff . is_git_repo {
    return None; }
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  source_diff . file_diffs . get ( &file_path ) }
