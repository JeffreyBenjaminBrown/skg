/// Per-node git-diff decoration for the git diff view.
/// process_activeNode_diff decorates one Active vognode and generates its
/// diff-only children. TODO/DONE/local-view-update/plan_v2.org §9 reversal (#3): it is now called INLINE, at each
/// node's own BFS visit (server/update_buffer/complete.rs), for both the
/// post-save and de-novo paths.
///
/// Each ActiveNode and Scaffold is decorated with per-stage diff axes:
///   X (existence) describes whether the node's '.skg' file changed
///     between HEAD↔index (staged) or index↔worktree (unstaged).
///   M (membership) describes whether the node's appearance at this
///     position in its parent's contains list changed in each stage.
/// Phantoms are inserted wherever some stage's parent.contains had the
/// child but the worktree's parent.contains lacks it.

use crate::types::env::find_source_with_optional_tantivy;
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, NodeCompleteDiff, GitDiffStatus, NodeChanges, added_membership_from_per_stage_diffs, existence_axes_in_source_diff, net_diff_from_per_stage, removed_membership_from_per_stage_diffs};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::types::phantom::title_for_phantom;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, mk_phantom_viewnode };
use crate::types::viewnode::{Vognode, Phantom, QualCol, Qual};
use crate::types::tree::viewnode_nodecomplete::pid_and_source_from_treenode;

use ego_tree::{NodeMut, NodeRef, NodeId};
use std::collections::HashMap;
use std::path::PathBuf;

/// Decorate a active vognode and generate any diff-only children
/// implied by staged and unstaged NodeCompleteDiffs. Called inline per Normal
/// node at its own BFS visit (for both de-novo and post-save), TODO/DONE/local-view-update/plan_v2.org §9 reversal / #3:
/// the node flips to a phantom here and its cols then self-deaden via their own
/// generalized-orphan check at their later visits.
pub(crate) fn process_activeNode_diff (
  mut node_mut                   : NodeMut<ViewNode>,
  source_diffs                   : &HashMap<SourceName, SourceDiff>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  tantivy_index                  : Option<&TantivyIndex>,
  config                         : &SkgConfig,
) -> Result<(), String> {
  let tree_node_id : NodeId =
    node_mut . id();
  let (pid, source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      node_mut . tree(), tree_node_id, "process_activeNode_diff"
    ) . map_err ( |e| e . to_string() ) ?;
  let source_diff : &SourceDiff =
    match source_diffs . get (&source) {
      Some (d) => d,
      None => return Ok (( )) };
  if ! source_diff . is_git_repo {
    if let ViewNodeKind::Vognode (Vognode::Active ( ref mut t ))
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
  if let ViewNodeKind::Vognode (Vognode::Active ( ref mut t ))
    = node_mut . value() . kind
    { t . existence . staged   = staged_x;
      t . existence . unstaged = unstaged_x; }
  node_mut . value() . normal_to_phantom ();
  let node_flipped_to_phantom : bool =
    matches! ( node_mut . value() . kind,
      ViewNodeKind::Phantom (Phantom::Diff (_)) );
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
      ViewNode {
        focused     : false,
        folded      : false,
        body_folded : false,
        kind        : ViewNodeKind::Qual (
          Qual::TextChanged {
            staged   : staged_text,
            unstaged : unstaged_text } ) } ); }
  // The IDCol/AliasCol diff scaffolds are cols, so if this node flipped to a
  // phantom they would be generalized orphans (a col requires a Active-vognode
  // ancestor) and get deadened + pruned at their own BFS visit -- i.e. emitted
  // here only to be destroyed before render. Skip creating them on a flipped
  // node: same final tree, without the wasted work. (The node's id/alias
  // sub-diffs are noise on a removed node anyway.)
  if ! node_flipped_to_phantom {
    // Emit an EMPTY IDCol / AliasCol when this node's id-list / alias-list
    // changed. Their per-id / per-alias children (each carrying its membership
    // axes) are filled when the BFS later reaches the col, by
    // reconcile_id_col_children / reconcile_alias_col_children -- the single
    // place that derives them from the per-stage diff. So we only DECIDE here
    // (cheaply: did any entry get added or removed?), and do not duplicate the
    // per-stage merge. A col the node ALREADY carries (the buffer being
    // completed can hold one -- e.g. a prior diff render, saved back) is
    // reused, never doubled: a second col would also reconcile to the full
    // list, duplicating every entry in the view (TODO/more.org, "aliases
    // should be merged, not added").
    if list_diff_has_change (
         staged_changes   . map ( |c| c . ids_diff . as_slice () ),
         unstaged_changes . map ( |c| c . ids_diff . as_slice () ) )
      && ! has_qualcol_child ( &mut node_mut, tree_node_id, QualCol::ID ) {
      prepend_empty_diff_col ( &mut node_mut, QualCol::ID ); }
    if list_diff_has_change (
         staged_changes   . map ( |c| c . aliases_diff . as_slice () ),
         unstaged_changes . map ( |c| c . aliases_diff . as_slice () ) )
      && ! has_qualcol_child ( &mut node_mut, tree_node_id, QualCol::Alias ) {
      prepend_empty_diff_col ( &mut node_mut, QualCol::Alias ); } }
  // Per-stage contains diff for the parent, split into position-specific
  // membership axes. A REORDERED id appears in one stage as both Removed (its
  // old slot) and New (its new slot); a single MembershipAxes keyed by id
  // (axes_from_per_stage_diffs) collapses that pair to whichever it applies
  // last, so the moved member's two slots cannot both be labelled. Keeping the
  // added (Plus on New) and removed (Minus on Removed) maps apart lets the live
  // worktree child render 'newM' at its new slot and the phantom 'removedM' at
  // its old slot -- a git-style move that round-trips (the old slot, carrying a
  // Minus, re-parses as a phantom, not a duplicate live vognode).
  let added_membership_by_id : HashMap<ID, MembershipAxes> =
    added_membership_from_per_stage_diffs (
      staged_changes   . map ( |c| c . contains_diff . as_slice () ),
      unstaged_changes . map ( |c| c . contains_diff . as_slice () ) );
  mark_membership_on_existing_children (
    &mut node_mut, tree_node_id, &added_membership_by_id );
  // Net HEAD->worktree contains order (an LCS of the reconstructed HEAD and
  // worktree contains lists), so each removed-member phantom lands at its
  // correct HEAD position among surviving siblings -- even when contains changed
  // in both git stages.
  let net_contains : Vec<Diff_Item<ID>> =
    net_diff_from_per_stage (
      staged_changes   . map ( |c| c . contains_diff . as_slice () ),
      unstaged_changes . map ( |c| c . contains_diff . as_slice () ) );
  let removed_membership_by_id : HashMap<ID, MembershipAxes> =
    removed_membership_from_per_stage_diffs (
      staged_changes   . map ( |c| c . contains_diff . as_slice () ),
      unstaged_changes . map ( |c| c . contains_diff . as_slice () ) );
  insert_phantoms_for_missing_contains (
    &mut node_mut, tree_node_id, &net_contains, &removed_membership_by_id,
    source_diff, source_diffs,
    deleted_since_head_pid_src_map, tantivy_index, config ) ?;
  Ok (( )) }

/// Decide where each removed-member phantom belongs among its surviving
/// siblings: insert it immediately before the *next* surviving child in
/// net HEAD->worktree order, or append it (anchor = None) when no surviving
/// child follows. Returns (removed_id, anchor_id) pairs in the order the
/// phantoms should be created, so consecutive removals before the same
/// survivor keep their relative order.
fn phantom_insertion_plan (
  net_contains : &[Diff_Item<ID>],
) -> Vec<(ID, Option<ID>)> {
  let mut plan : Vec<(ID, Option<ID>)> = Vec::new ();
  let mut next_survivor : Option<ID> = None;
  for item in net_contains . iter () . rev () {
    match item {
      Diff_Item::Unchanged (id) | Diff_Item::New (id)
        => { next_survivor = Some ( id . clone () ); },
      Diff_Item::Removed (id)
        => { plan . push ( ( id . clone (), next_survivor . clone () )); }, }}
  plan . reverse ();
  plan }

/// True iff either stage's list-field diff actually added or removed an entry
/// (an Unchanged-only diff is no change). The cheap decision used to emit an
/// empty diff col; reconcile_id_col_children / reconcile_alias_col_children
/// then fills the col from the same per-stage diff.
fn list_diff_has_change<T> (
  staged   : Option<&[Diff_Item<T>]>,
  unstaged : Option<&[Diff_Item<T>]>,
) -> bool {
  let changed = | slice : Option<&[Diff_Item<T>]> | -> bool {
    slice . is_some_and ( |s| s . iter () . any (
      |item| matches! ( item,
        Diff_Item::New (_) | Diff_Item::Removed (_) ) )) };
  changed (staged) || changed (unstaged) }

/// True iff the node already has a child QualCol of KIND.
fn has_qualcol_child (
  node_mut     : &mut NodeMut<ViewNode>,
  tree_node_id : NodeId,
  kind         : QualCol,
) -> bool {
  let node_ref : NodeRef<ViewNode> =
    node_mut . tree () . get (tree_node_id) . unwrap ();
  node_ref . children () . any ( |c| matches! (
    & c . value () . kind,
    ViewNodeKind::QualCol (k) if *k == kind )) }

/// Prepend an EMPTY QualCol diff scaffold (an IDCol or AliasCol). Its per-entry
/// children -- each carrying its membership axes -- are filled when the BFS
/// reaches the col, by reconcile_id_col_children / reconcile_alias_col_children.
fn prepend_empty_diff_col (
  node_mut : &mut NodeMut<ViewNode>,
  kind     : QualCol,
) {
  node_mut . prepend (
    ViewNode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewNodeKind::QualCol (kind) } ); }

/// For each existing child in the worktree's contains list, copy any
/// per-stage ADDITION (Plus) axes from the added-membership map, so a member
/// added (or moved to a new slot) since HEAD renders 'newM'.
fn mark_membership_on_existing_children (
  node_mut     : &mut NodeMut<ViewNode>,
  tree_node_id : NodeId,
  by_id        : &HashMap<ID, MembershipAxes>,
) {
  let child_ids : Vec<NodeId> = {
    let node_ref : NodeRef<ViewNode> =
      node_mut . tree() . get (tree_node_id) . unwrap();
    node_ref . children() . map ( |c| c . id() ) . collect() };
  for child_id in child_ids {
    let mut child : NodeMut<ViewNode> =
      node_mut . tree() . get_mut (child_id) . unwrap();
    let child_id_and_membership : Option<(ID, &mut MembershipAxes)> =
      match &mut child . value() . kind {
        ViewNodeKind::Vognode (Vognode::Active (t)) =>
          Some ((t . id . clone (), &mut t . membership)),
        // No Inactive arm: diff mode requires the "all" source set
        // (diff_analysis.rs and source_sets.rs refuse otherwise), under
        // which no node is inactive, so inactive placeholders never
        // reach diff rendering.
        _ => None };
    if let Some ((id, membership)) = child_id_and_membership {
      if let Some (m) = by_id . get (&id) {
        // Only Plus signs are meaningful here (the child appears in
        // worktree.contains; '-' positions are phantoms, handled separately).
        if m . staged   == Some (Sign::Plus)
          { membership . staged   = Some (Sign::Plus); }
        if m . unstaged == Some (Sign::Plus)
          { membership . unstaged = Some (Sign::Plus); }}}} }

/// Insert a removed-member phantom for each net-Removed id in the parent's
/// contains diff (ids present in HEAD but absent from worktree.contains).
/// Each phantom is placed at its correct HEAD position among surviving
/// siblings (per 'phantom_insertion_plan'), carrying its M axes (from the
/// merged contains diff) and X axes (if its file is also gone in some stage).
fn insert_phantoms_for_missing_contains (
  node_mut                       : &mut NodeMut<ViewNode>,
  parent_node_id                 : NodeId,
  net_contains                   : &[Diff_Item<ID>],
  membership_by_id               : &HashMap<ID, MembershipAxes>,
  source_diff                    : &SourceDiff,
  source_diffs                   : &HashMap<SourceName, SourceDiff>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  tantivy_index                  : Option<&TantivyIndex>,
  config                         : &SkgConfig,
) -> Result<(), String> {
  let plan : Vec<(ID, Option<ID>)> =
    phantom_insertion_plan (net_contains);
  if plan . is_empty () { return Ok (( )); }
  // Map each surviving child's id to its NodeId, so an anchor id resolves
  // to the tree node we insert the phantom before.
  let child_node_by_id : HashMap<ID, NodeId> = {
    let node_ref : NodeRef<ViewNode> =
      node_mut . tree () . get (parent_node_id) . unwrap ();
    let mut m : HashMap<ID, NodeId> = HashMap::new ();
    for c in node_ref . children () {
      match &c . value () . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => { m . insert ( t . id . clone (), c . id () ); },
        ViewNodeKind::Phantom (Phantom::Diff (p))
          => { m . insert ( p . id . clone (), c . id () ); },
        // No Inactive arm: inactive placeholders never reach diff
        // rendering (diff mode requires the "all" source set).
        _ => {}, }}
    m };
  for (id, anchor) in plan {
    let membership : MembershipAxes =
      membership_by_id . get (&id) . copied () . unwrap_or_default ();
    // A removed-member diff-phantom is a *non-Active* viewnode. If its
    // source can't be determined -- e.g. a contains pointer at HEAD to a
    // node whose .skg file was deleted by an earlier commit and so exists
    // in no source -- fall back to the NOT_FOUND sentinel rather than
    // aborting the whole render (matching the PartnerCol removed-member
    // path; TODO/DONE/local-view-update/plan_v2.org §7.6).
    let child_source : SourceName =
      find_source_with_optional_tantivy (
        &id, deleted_since_head_pid_src_map,
        tantivy_index, config )
        . unwrap_or_else ( SourceName::not_found );
    let child_existence : ExistenceAxes =
      existence_axes_for_phantom (&id, &child_source, source_diff, source_diffs);
    let child_title : String =
      title_for_phantom (
        &id, &child_source,
        Some (source_diffs), config );
    let phantom : ViewNode =
      mk_phantom_viewnode (
        id . clone (), child_source, child_title,
        child_existence, membership );
    match anchor . and_then ( |a| child_node_by_id . get (&a) . copied () ) {
      Some (anchor_nid) =>
        { node_mut . tree () . get_mut (anchor_nid) . unwrap ()
            . insert_before (phantom); },
      None =>
        { node_mut . tree () . get_mut (parent_node_id) . unwrap ()
            . append (phantom); }, }}
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
  existence_axes_in_source_diff ( Some (resolved), &file_path ) }

#[cfg(test)]
#[path = "../../../tests/unit/render_diff.rs"]
mod phantom_insertion_plan_tests;
