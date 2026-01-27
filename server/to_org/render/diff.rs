/// Diff application module for git diff view.
/// Applies diff information to a forest of OrgNodes.

use crate::types::git::{SourceDiff, FileDiff, DiffStatus, NodeDiff, FieldDiff};
use crate::types::misc::ID;
use crate::types::orgnode::{
  OrgNode, OrgNodeKind, Scaffold,
  orgnode_from_scaffold,
};
use crate::types::tree::generic::do_everywhere_in_tree_dfs;

use ego_tree::{Tree, NodeMut};
use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;

/// Apply diff information to a forest of OrgNodes.
/// This modifies nodes in place to add diff markers and inserts removed nodes.
pub fn apply_diff_to_forest (
  forest       : &mut Tree<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), Box<dyn Error>> {
  let root_id =
    forest . root() . id();
  do_everywhere_in_tree_dfs ( forest, root_id, &mut |node_mut| {
    process_node_for_diff ( node_mut, source_diffs )
  } ) ?;
  Ok (()) }

/// Process a single node for diff markers.
/// Dispatches to specific handlers based on node kind.
fn process_node_for_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  match &node_mut . value() . kind . clone() {
    OrgNodeKind::True ( _ ) =>
      process_truenode_diff ( node_mut, source_diffs ),
    OrgNodeKind::Scaff ( Scaffold::AliasCol ) =>
      process_aliascol_diff ( node_mut, source_diffs ),
    OrgNodeKind::Scaff ( Scaffold::Alias { .. } ) =>
      process_alias_diff ( node_mut, source_diffs ),
    OrgNodeKind::Scaff ( Scaffold::IDCol ) =>
      process_idcol_diff ( node_mut, source_diffs ),
    OrgNodeKind::Scaff ( Scaffold::ID { .. } ) =>
      process_id_diff ( node_mut, source_diffs ),
    _ => Ok (()) }}

/// Process a TrueNode: set diff marker, prepend TextChanged/IDCol if needed.
fn process_truenode_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  let (source_opt, id_opt) : (Option<String>, Option<String>) = {
    match &node_mut . value() . kind {
      OrgNodeKind::True ( t ) =>
        ( t . source_opt . clone(),
          t . id_opt . as_ref() . map ( |id| id . 0 . clone() ) ),
      _ => return Ok (()) }};
  let source : String =
    match source_opt {
      Some ( s ) => s,
      None => return Ok (()) };
  let source_diff : &SourceDiff =
    match source_diffs . get ( &source ) {
      Some ( d ) => d,
      None => return Ok (()) };
  if ! source_diff . is_git_repo { // Mark node as not-in-git
    if let OrgNodeKind::True ( ref mut t ) = node_mut . value() . kind {
      t . diff = Some ( NodeDiff::NotInGit ); }
    return Ok (()); }
  let node_id_val : String =
    match id_opt {
      Some ( s ) => s,
      None => return Ok (()) };
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", node_id_val ) );
  let file_diff : &FileDiff =
    match source_diff . file_diffs . get ( &file_path ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_diff_status : Option<NodeDiff> = // Set node diff status based on file status
    match file_diff . status {
      DiffStatus::Added => Some ( NodeDiff::New ),
      DiffStatus::Deleted => Some ( NodeDiff::Removed ),
      DiffStatus::Modified => None };
  if let Some ( diff_status ) = node_diff_status {
    if let OrgNodeKind::True ( ref mut t ) = node_mut . value() . kind {
      t . diff = Some ( diff_status ); }
    return Ok (()); }
  if let Some ( ref node_changes ) = file_diff . node_changes { // For modified files, check for text/ID changes
    if node_changes . text_changed { // Prepend TextChanged scaffold
      let scaffold : OrgNode =
        orgnode_from_scaffold ( Scaffold::TextChanged );
      node_mut . prepend ( scaffold ); }
    if ! node_changes . ids_diff . added . is_empty()
       || ! node_changes . ids_diff . removed . is_empty() { // Prepend IDCol if there are ID changes (will be populated when we visit it)
      // IDCol is only created during diff processing, so we can just prepend
      let idcol : OrgNode =
        orgnode_from_scaffold ( Scaffold::IDCol );
      node_mut . prepend ( idcol ); }}
  Ok (()) }

/// Process an AliasCol: append removed Alias children.
fn process_aliascol_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  let file_diff : &FileDiff =
    match get_parent_file_diff ( &mut node_mut, source_diffs ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_changes =
    match &file_diff . node_changes {
      Some ( c ) => c,
      None => return Ok (()) };
  for removed_alias in &node_changes . aliases_diff . removed { // Append removed aliases
    let alias_scaffold : Scaffold =
      Scaffold::Alias {
        text: removed_alias . clone(),
        diff: Some ( FieldDiff::Removed ) };
    let alias_orgnode : OrgNode =
      orgnode_from_scaffold ( alias_scaffold );
    node_mut . append ( alias_orgnode ); }
  Ok (()) }

/// Process an Alias scaffold: mark as new if in added list.
fn process_alias_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  let alias_text : String = // Get this alias's text
    match &node_mut . value() . kind {
      OrgNodeKind::Scaff ( Scaffold::Alias { text, .. } ) => text . clone(),
      _ => return Ok (()) };
  let file_diff : &FileDiff = // Grandparent is the TrueNode
    match get_grandparent_file_diff ( &mut node_mut, source_diffs ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_changes =
    match &file_diff . node_changes {
      Some ( c ) => c,
      None => return Ok (()) };
  if node_changes . aliases_diff . added . contains ( &alias_text ) {
    if let OrgNodeKind::Scaff ( Scaffold::Alias { diff, .. } ) = &mut node_mut . value() . kind {
      *diff = Some ( FieldDiff::New ); }}
  Ok (()) }

/// Process an IDCol: append added/removed ID children.
fn process_idcol_diff (
  mut node_mut : NodeMut<OrgNode>,
  source_diffs : &HashMap<String, SourceDiff>,
) -> Result<(), String> {
  let file_diff : &FileDiff =
    match get_parent_file_diff ( &mut node_mut, source_diffs ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_changes =
    match &file_diff . node_changes {
      Some ( c ) => c,
      None => return Ok (()) };
  for added_id in &node_changes . ids_diff . added { // Append added IDs
    let id_scaffold : Scaffold =
      Scaffold::ID {
        value: added_id . 0 . clone(),
        diff: Some ( FieldDiff::New ) };
    let id_orgnode : OrgNode =
      orgnode_from_scaffold ( id_scaffold );
    node_mut . append ( id_orgnode ); }
  for removed_id in &node_changes . ids_diff . removed { // Append removed IDs
    let id_scaffold : Scaffold =
      Scaffold::ID {
        value: removed_id . 0 . clone(),
        diff: Some ( FieldDiff::Removed ) };
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
      OrgNodeKind::Scaff ( Scaffold::ID { value, .. } ) => ID ( value . clone() ),
      _ => return Ok (()) };
  let file_diff : &FileDiff = // Grandparent is the TrueNode
    match get_grandparent_file_diff ( &mut node_mut, source_diffs ) {
      Some ( d ) => d,
      None => return Ok (()) };
  let node_changes =
    match &file_diff . node_changes {
      Some ( c ) => c,
      None => return Ok (()) };
  if node_changes . ids_diff . added . contains ( &id_value ) {
    if let OrgNodeKind::Scaff ( Scaffold::ID { diff, .. } ) = &mut node_mut . value() . kind {
      *diff = Some ( FieldDiff::New ); }}
  Ok (()) }

/// Get the FileDiff for a node's parent TrueNode.
fn get_parent_file_diff<'a> (
  node_mut     : &mut NodeMut<OrgNode>,
  source_diffs : &'a HashMap<String, SourceDiff>,
) -> Option<&'a FileDiff> {
  let mut parent : NodeMut<OrgNode> =
    node_mut . parent() ?;
  file_diff_for_truenode_mut ( &mut parent, source_diffs ) }

/// Get the FileDiff for a node's grandparent TrueNode.
fn get_grandparent_file_diff<'a> (
  node_mut     : &mut NodeMut<OrgNode>,
  source_diffs : &'a HashMap<String, SourceDiff>,
) -> Option<&'a FileDiff> {
  let mut parent : NodeMut<OrgNode> =
    node_mut . parent() ?;
  let mut grandparent : NodeMut<OrgNode> =
    parent . parent() ?;
  file_diff_for_truenode_mut ( &mut grandparent, source_diffs ) }

/// Get the FileDiff for a TrueNode (from a NodeMut).
fn file_diff_for_truenode_mut<'a> (
  node_mut     : &mut NodeMut<OrgNode>,
  source_diffs : &'a HashMap<String, SourceDiff>,
) -> Option<&'a FileDiff> {
  let (source, id) : (String, String) =
    match &node_mut . value() . kind {
      OrgNodeKind::True ( t ) =>
        ( t . source_opt . clone() ?,
          t . id_opt . as_ref() ? . 0 . clone() ),
      _ => return None };
  let source_diff : &SourceDiff =
    source_diffs . get ( &source ) ?;
  if ! source_diff . is_git_repo {
    return None; }
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", id ) );
  source_diff . file_diffs . get ( &file_path ) }
