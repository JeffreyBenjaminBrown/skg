/// Diff application module for git diff view.
/// Applies diff information to a viewforest of ViewNodes.
///
/// Each TrueNode and Scaffold is decorated with per-stage diff axes:
///   X (existence) describes whether the node's '.skg' file changed
///     between HEAD↔index (staged) or index↔worktree (unstaged).
///   M (membership) describes whether the node's appearance at this
///     position in its parent's contains list changed in each stage.
/// Phantoms are inserted wherever some stage's parent.contains had the
/// child but the worktree's parent.contains lacks it.

use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, NodeCompleteDiff, GitDiffStatus, NodeChanges, axes_from_per_stage_diffs};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::phantom::title_for_phantom;
use crate::types::memory::find_source_many_ways;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold, viewnode_from_scaffold, mk_phantom_viewnode };
use crate::types::tree::generic::do_everywhere_in_tree_dfs;
use crate::types::tree::viewnode_nodecomplete::pid_and_source_from_treenode;

use ego_tree::{Tree, NodeMut, NodeRef, NodeId};
use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;

/// Apply diff information to a viewforest of ViewNodes.
/// Modifies nodes in place to add per-stage diff axes
/// and insert phantom nodes for positions missing from worktree.
pub fn apply_diff_to_viewforest (
  viewforest             : &mut Tree<ViewNode>,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let root_id : NodeId =
    viewforest . root() . id();
  do_everywhere_in_tree_dfs (
    viewforest, root_id,
    &mut |node_mut| { process_node_for_diff (
                        node_mut, source_diffs,
                        deleted_since_head_pid_src_map, config ) } )?;
  Ok (( )) }

/// Process a single node for diff markers.
/// This is only used for de-novo views (multi_root_view).
/// TODO : Sharing relationships (hides, overrides, subscribes) will need processing here.
fn process_node_for_diff (
  mut node_mut       : NodeMut<ViewNode>,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), String> {
  match &node_mut . value() . kind . clone() {
    ViewNodeKind::True (_) =>
      process_truenode_diff (
        node_mut, source_diffs, deleted_since_head_pid_src_map,
        config ),
    _ => Ok (()) }}

/// Decorate a TrueNode (and its scaffold/phantom children) with the
/// per-stage axes derived from staged and unstaged NodeCompleteDiffs.
fn process_truenode_diff (
  mut node_mut       : NodeMut<ViewNode>,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), String> {
  let tree_node_id : NodeId =
    node_mut . id();
  let (pid, source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      node_mut . tree(), tree_node_id, "process_truenode_diff"
    ) . map_err ( |e| e . to_string() ) ?;
  let source_diff : &SourceDiff =
    match source_diffs . get (&source) {
      Some (d) => d,
      None => return Ok (( )) };
  if ! source_diff . is_git_repo {
    if let ViewNodeKind::True ( ref mut t )
      = node_mut . value() . kind
      { t . not_in_git = true; }
    return Ok (( )); }
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  let staged   : Option<&NodeCompleteDiff> =
    source_diff . staged   . get (&file_path);
  let unstaged : Option<&NodeCompleteDiff> =
    source_diff . unstaged . get (&file_path);
  if staged . is_none () && unstaged . is_none ()
    { return Ok (( )); }
  // Stamp the node's existence axes from the per-stage file statuses.
  let staged_x   : Option<Sign> =
    staged   . and_then ( |d| d . status . to_existence_sign ());
  let unstaged_x : Option<Sign> =
    unstaged . and_then ( |d| d . status . to_existence_sign ());
  if let ViewNodeKind::True ( ref mut t ) = node_mut . value() . kind {
    t . existence . staged   = staged_x;
    t . existence . unstaged = unstaged_x; }
  // For an Added or Deleted file we don't read node_changes
  // (the comparison is degenerate). NewHere/RemovedHere on children
  // and IDcol/textChanged scaffolds only apply to Modified files.
  let staged_changes   : Option<&NodeChanges> =
    staged   . and_then ( |d| match d . status {
      GitDiffStatus::Modified => d . node_changes . as_ref (),
      _                       => None });
  let unstaged_changes : Option<&NodeChanges> =
    unstaged . and_then ( |d| match d . status {
      GitDiffStatus::Modified => d . node_changes . as_ref (),
      _                       => None });
  let staged_text   : bool = staged_changes
    . map ( |c| c . text_changed ) . unwrap_or (false);
  let unstaged_text : bool = unstaged_changes
    . map ( |c| c . text_changed ) . unwrap_or (false);
  if staged_text || unstaged_text {
    node_mut . prepend (
      viewnode_from_scaffold (
        Scaffold::TextChanged { staged   : staged_text,
                                unstaged : unstaged_text } )); }
  let merged_ids : Vec<(ID, MembershipAxes)> =
    axes_from_per_stage_diffs (
      staged_changes   . map ( |c| c . ids_diff . as_slice () ),
      unstaged_changes . map ( |c| c . ids_diff . as_slice () ) );
  if merged_ids . iter () . any ( |(_, m)| ! m . is_empty () ) {
    prepend_idcol_with_children ( &mut node_mut, &merged_ids ); }
  // Compute per-stage contains diff for the parent so we can decorate
  // worktree children with M axes and insert phantoms.
  let merged_contains : Vec<(ID, MembershipAxes)> =
    axes_from_per_stage_diffs (
      staged_changes   . map ( |c| c . contains_diff . as_slice () ),
      unstaged_changes . map ( |c| c . contains_diff . as_slice () ) );
  mark_membership_on_existing_children (
    &mut node_mut, tree_node_id, &merged_contains );
  insert_phantoms_for_missing_contains (
    &mut node_mut, &merged_contains,
    source_diff, source_diffs,
    deleted_since_head_pid_src_map, config ) ?;
  Ok (( )) }

/// Prepend an IDCol scaffold populated with per-id ID scaffolds.
fn prepend_idcol_with_children (
  node_mut   : &mut NodeMut<ViewNode>,
  merged_ids : &[(ID, MembershipAxes)],
) {
  let idcol_node : ViewNode =
    viewnode_from_scaffold (Scaffold::IDCol);
  let idcol_treeid : NodeId =
    node_mut . prepend (idcol_node) . id();
  let mut idcol_mut : NodeMut<ViewNode> =
    node_mut . tree() . get_mut (idcol_treeid) . unwrap();
  for (id, membership) in merged_ids {
    let id_scaffold : Scaffold =
      Scaffold::ID { id: id . clone (),
                     membership: *membership };
    let id_viewnode : ViewNode =
      viewnode_from_scaffold (id_scaffold);
    idcol_mut . append (id_viewnode); } }

/// For each existing child in the worktree's contains list, copy any
/// per-stage M-axis values from the merged contains diff.
fn mark_membership_on_existing_children (
  node_mut       : &mut NodeMut<ViewNode>,
  tree_node_id   : NodeId,
  merged_contains: &[(ID, MembershipAxes)],
) {
  let by_id : HashMap<&ID, &MembershipAxes> =
    merged_contains . iter () . map ( |(id, m)| (id, m) ) . collect ();
  let child_ids : Vec<NodeId> = {
    let node_ref : NodeRef<ViewNode> =
      node_mut . tree() . get (tree_node_id) . unwrap();
    node_ref . children() . map ( |c| c . id() ) . collect() };
  for child_id in child_ids {
    let mut child : NodeMut<ViewNode> =
      node_mut . tree() . get_mut (child_id) . unwrap();
    if let ViewNodeKind::True ( ref mut t ) = child . value() . kind {
      if let Some (m) = by_id . get ( &t . id ) {
        // Only Plus signs are meaningful here (the child appears in
        // worktree.contains; if a stage shows '-' that would imply the
        // child *was* there in some baseline but isn't in the worktree,
        // which would make it a phantom -- handled separately).
        if m . staged   == Some (Sign::Plus)
          { t . membership . staged   = Some (Sign::Plus); }
        if m . unstaged == Some (Sign::Plus)
          { t . membership . unstaged = Some (Sign::Plus); } } } } }

/// Insert phantoms for IDs that appear in the merged contains diff with
/// any '-' (Removed) sign on any stage. These are positions missing
/// from worktree.contains. Each phantom carries the appropriate M axes
/// (and X axes if its file is also gone in some stage).
fn insert_phantoms_for_missing_contains (
  node_mut           : &mut NodeMut<ViewNode>,
  merged_contains    : &[(ID, MembershipAxes)],
  source_diff        : &SourceDiff,
  source_diffs       : &HashMap<SourceName, SourceDiff>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
) -> Result<(), String> {
  let empty_children : HashMap<ID, SourceName> = HashMap::new();
  for (id, membership) in merged_contains {
    let has_negative : bool =
      membership . staged   == Some (Sign::Minus)
      || membership . unstaged == Some (Sign::Minus);
    if ! has_negative { continue; }
    let child_source : SourceName =
      find_source_many_ways (
        id, &empty_children,
        deleted_since_head_pid_src_map, config ) ?;
    let child_existence : ExistenceAxes =
      existence_axes_for_phantom (id, &child_source, source_diff, source_diffs);
    let child_title : String =
      title_for_phantom (
        id, &child_source,
        Some (source_diffs), config );
    node_mut . prepend (
      mk_phantom_viewnode (
        id . clone (), child_source, child_title,
        child_existence, *membership )); }
  Ok (( )) }

/// Compute existence axes for a phantom: derived from whether the
/// child's '.skg' file shows up as Deleted in either stage.
fn existence_axes_for_phantom (
  id           : &ID,
  source       : &SourceName,
  source_diff  : &SourceDiff,
  source_diffs : &HashMap<SourceName, SourceDiff>,
) -> ExistenceAxes {
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", id . 0 ));
  // Prefer the source_diff for the phantom's own source if available,
  // otherwise fall back to the parent's source_diff.
  let resolved : &SourceDiff =
    source_diffs . get (source) . unwrap_or (source_diff);
  let staged_x : Option<Sign> = resolved . staged   . get (&file_path)
    . and_then ( |d| d . status . to_existence_sign ());
  let unstaged_x : Option<Sign> = resolved . unstaged . get (&file_path)
    . and_then ( |d| d . status . to_existence_sign ());
  ExistenceAxes { staged: staged_x, unstaged: unstaged_x } }
