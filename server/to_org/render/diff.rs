/// Per-node git-diff decoration for the git diff view.
/// process_unrestrictedVognode_diff decorates one Unrestricted vognode and generates its
/// diff-only children. TODO/DONE/local-view-update/plan_v2.org §9 reversal (#3): it is now called INLINE, at each
/// node's own BFS visit (server/update_buffer/complete.rs), for both the
/// post-save and de-novo paths.
///
/// Each UnrestrictedVognode and Non-vognode is decorated with per-stage diff axes:
///   N (node) describes whether the node's '.skg' file changed
///     between HEAD↔index (staged) or index↔worktree (unstaged).
///   R (relationship) describes whether the node's appearance at this
///     position in its parent's contains list changed in each stage.
/// Phantoms are inserted wherever some stage's parent.contains had the
/// child but the worktree's parent.contains lacks it.

use crate::types::env::find_skgrepo_with_optional_tantivy;
use crate::types::git::{NodeAxes, RelationshipAxes, Sign, SkgRepoDiff, GraphnodeDiff, GitDiffStatus, NodeChanges, added_relationship_axes_from_per_stage_diffs, node_axes_in_skgrepo_diff, net_diff_from_per_stage, removed_relationship_axes_from_per_stage_diffs};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, SkgConfig, SkgRepoName, TantivyIndex};
use crate::types::phantom::title_for_phantom;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, mk_phantom_viewnode };
use crate::types::viewnode::{Vognode, Phantom, PropertyFolder, Property};
use crate::types::tree::viewnode_graphnode::pid_and_skgrepo_from_viewnode_at;
use crate::dbs::in_rust_graph::InRustGraph;

use ego_tree::{NodeMut, NodeRef, NodeId};
use std::collections::HashMap;
use std::path::PathBuf;

/// Decorate an unrestricted vognode and generate any diff-only children
/// implied by staged and unstaged GraphnodeDiffs. Called inline per Unrestricted
/// node at its own BFS visit (for both de-novo and post-save), TODO/DONE/local-view-update/plan_v2.org §9 reversal / #3:
/// the node flips to a phantom here and its folders then self-deaden via their own
/// generalized-orphan check at their later visits.
pub(crate) fn process_unrestrictedVognode_diff (
  mut node_mut                   : NodeMut<Viewnode>,
  graph                          : &InRustGraph,
  skgrepo_diffs                  : &HashMap<SkgRepoName, SkgRepoDiff>,
  deleted_since_head_pid_src_map : &HashMap<ID, SkgRepoName>,
  tantivy_index                  : Option<&TantivyIndex>,
  config                         : &SkgConfig,
) -> Result<(), String> {
  let treeid : NodeId =
    node_mut . id();
  let (pid, skgrepo) : (ID, SkgRepoName) =
    pid_and_skgrepo_from_viewnode_at (
      node_mut . tree(), treeid, "process_unrestrictedVognode_diff"
    ) . map_err ( |e| e . to_string() ) ?;
  let skgrepo_diff : &SkgRepoDiff =
    match skgrepo_diffs . get (&skgrepo) {
      Some (d) => d,
      None => return Ok (( )) };
  if ! skgrepo_diff . is_gitrepo {
    if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
      = node_mut . value() . kind
      { t . not_in_git = true; }
    return Ok (( )); }
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  let staged   : Option<&GraphnodeDiff> =
    skgrepo_diff . staged   . get (&file_path);
  let unstaged : Option<&GraphnodeDiff> =
    skgrepo_diff . unstaged . get (&file_path);
  if staged . is_none () && unstaged . is_none ()
    { return Ok (( )); }
  // Stamp the node's N axes from the per-stage file statuses.
  let staged_n   : Option<Sign> =
    staged   . and_then ( |d| d . status . to_node_axis_sign ());
  let unstaged_n : Option<Sign> =
    unstaged . and_then ( |d| d . status . to_node_axis_sign ());
  if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
    = node_mut . value() . kind
    { t . node_axes . staged   = staged_n;
      t . node_axes . unstaged = unstaged_n; }
  node_mut . value() . normal_to_phantom ();
  let node_flipped_to_phantom : bool =
    matches! ( node_mut . value() . kind,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (_))) );
  // For an Added or Deleted file we don't read node_changes
  // (the comparison is degenerate). NewHere/RemovedHere on children
  // and IDFolder/textChanged properties only apply to Modified files.
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
      Viewnode {
        focused     : false,
        folded      : false,
        body_folded : false,
        kind        : ViewnodeKind::Property (
          Property::TextChanged {
            staged   : staged_text,
            unstaged : unstaged_text } ) } ); }
  // The IDFolder/AliasFolder diff-only properties are folders, so if this node flipped to a
  // phantom they would be generalized orphans (a folder requires an Unrestricted-vognode
  // ancestor) and get deadened + pruned at their own BFS visit -- i.e. emitted
  // here only to be destroyed before render. Skip creating them on a flipped
  // node: same final tree, without the wasted work. (The node's id/alias
  // sub-diffs are noise on a removed node anyway.)
  if ! node_flipped_to_phantom {
    // Emit an EMPTY IDFolder / AliasFolder when this node's id-list / alias-list
    // changed. Their per-id / per-alias children (each carrying its relationship
    // axes) are filled when the BFS later reaches the folder, by
    // reconcile_idFolder_children / reconcile_aliasFolder_children -- the single
    // place that derives them from the per-stage diff. So we only DECIDE here
    // (cheaply: did any entry get added or removed?), and do not duplicate the
    // per-stage merge. A folder the node ALREADY carries (the buffer being
    // completed can hold one -- e.g. a prior diff render, saved back) is
    // reused, never doubled: a second folder would also reconcile to the full
    // list, duplicating every entry in the view (TODO/more.org, "aliases
    // should be merged, not added").
    if list_diff_has_change (
         staged_changes   . map ( |c| c . ids_diff . as_slice () ),
         unstaged_changes . map ( |c| c . ids_diff . as_slice () ) )
      && ! has_propertyFolder_child ( &mut node_mut, treeid, PropertyFolder::ID ) {
      prepend_empty_diff_folder ( &mut node_mut, PropertyFolder::ID ); }
    if list_diff_has_change (
         staged_changes   . map ( |c| c . aliases_diff . as_slice () ),
         unstaged_changes . map ( |c| c . aliases_diff . as_slice () ) )
      && ! has_propertyFolder_child ( &mut node_mut, treeid, PropertyFolder::Alias ) {
      prepend_empty_diff_folder ( &mut node_mut, PropertyFolder::Alias ); } }
  // Per-stage contains diff for the parent, split into position-specific
  // relationship axes. A REORDERED id appears in one stage as both Removed (its
  // old slot) and New (its new slot); a single RelationshipAxes keyed by id
  // (axes_from_per_stage_diffs) collapses that pair to whichever it applies
  // last, so the moved member's two slots cannot both be labelled. Keeping the
  // added (Plus on New) and removed (Minus on Removed) maps apart lets the live
  // worktree child render 'addedR' at its new slot and the phantom 'removedR' at
  // its old slot -- a git-style move that round-trips (the old slot, carrying a
  // Minus, re-parses as a phantom, not a duplicate live vognode).
  let added_relationship_axes_by_skgid : HashMap<ID, RelationshipAxes> =
    added_relationship_axes_from_per_stage_diffs (
      staged_changes   . map ( |c| c . contains_diff . as_slice () ),
      unstaged_changes . map ( |c| c . contains_diff . as_slice () ) );
  mark_relationship_axes_on_existing_children (
    &mut node_mut, treeid, &added_relationship_axes_by_skgid );
  if matches! ( & node_mut . value () . kind,
                ViewnodeKind::Vognode (Vognode::Unrestricted (t))
                  if t . is_writeProtected () ) {
    // TODO/fork-fixes.org: no git phantoms under a write-protected node.
    // It draws none of its worktree children, so a removed-member
    // phantom under it would show the node's DELETED children while
    // its kept children go unshown. Its editable occurrence (or a
    // wider view) carries the contains diff.
    return Ok (( )); }
  // Net HEAD->worktree contains order (an LCS of the reconstructed HEAD and
  // worktree contains lists), so each removed-member phantom lands at its
  // correct HEAD position among surviving siblings -- even when contains changed
  // in both git stages.
  let net_contains : Vec<Diff_Item<ID>> =
    net_diff_from_per_stage (
      staged_changes   . map ( |c| c . contains_diff . as_slice () ),
      unstaged_changes . map ( |c| c . contains_diff . as_slice () ) );
  let removed_relationship_axes_by_skgid : HashMap<ID, RelationshipAxes> =
    removed_relationship_axes_from_per_stage_diffs (
      staged_changes   . map ( |c| c . contains_diff . as_slice () ),
      unstaged_changes . map ( |c| c . contains_diff . as_slice () ) );
  insert_phantoms_for_missing_contains (
    &mut node_mut, graph, treeid, &net_contains, &removed_relationship_axes_by_skgid,
    skgrepo_diff, skgrepo_diffs,
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
      Diff_Item::Unchanged (skgid) | Diff_Item::New (skgid)
        => { next_survivor = Some ( skgid . clone () ); },
      Diff_Item::Removed (skgid)
        => { plan . push ( ( skgid . clone (), next_survivor . clone () )); }, }}
  plan . reverse ();
  plan }

/// True iff either stage's list-field diff actually added or removed an entry
/// (an Unchanged-only diff is no change). The cheap decision used to emit an
/// empty diff folder; reconcile_idFolder_children / reconcile_aliasFolder_children
/// then fills the folder from the same per-stage diff.
fn list_diff_has_change<T> (
  staged   : Option<&[Diff_Item<T>]>,
  unstaged : Option<&[Diff_Item<T>]>,
) -> bool {
  let changed = | slice : Option<&[Diff_Item<T>]> | -> bool {
    slice . is_some_and ( |s| s . iter () . any (
      |item| matches! ( item,
        Diff_Item::New (_) | Diff_Item::Removed (_) ) )) };
  changed (staged) || changed (unstaged) }

/// True iff the node already has a child PropertyFolder of KIND.
fn has_propertyFolder_child (
  node_mut     : &mut NodeMut<Viewnode>,
  treeid       : NodeId,
  kind         : PropertyFolder,
) -> bool {
  let node_ref : NodeRef<Viewnode> =
    node_mut . tree () . get (treeid) . unwrap ();
  node_ref . children () . any ( |c| matches! (
    & c . value () . kind,
    ViewnodeKind::PropertyFolder (k) if *k == kind )) }

/// Prepend an EMPTY PropertyFolder diff-only property (an IDFolder or AliasFolder). Its per-entry
/// children -- each carrying its relationship axes -- are filled when the BFS
/// reaches the folder, by reconcile_idFolder_children / reconcile_aliasFolder_children.
fn prepend_empty_diff_folder (
  node_mut : &mut NodeMut<Viewnode>,
  kind     : PropertyFolder,
) {
  node_mut . prepend (
    Viewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewnodeKind::PropertyFolder (kind) } ); }

/// For each existing child in the worktree's contains list, copy any
/// per-stage ADDITION (Plus) axes from the added-relationship-axes map, so a member
/// added (or moved to a new slot) since HEAD renders 'addedR'.
fn mark_relationship_axes_on_existing_children (
  node_mut     : &mut NodeMut<Viewnode>,
  treeid       : NodeId,
  by_skgid     : &HashMap<ID, RelationshipAxes>,
) {
  let child_skgids : Vec<NodeId> = {
    let node_ref : NodeRef<Viewnode> =
      node_mut . tree() . get (treeid) . unwrap();
    node_ref . children() . map ( |c| c . id() ) . collect() };
  for child_skgid in child_skgids {
    let mut child : NodeMut<Viewnode> =
      node_mut . tree() . get_mut (child_skgid) . unwrap();
    let child_id_and_relationship_axes : Option<(ID, &mut RelationshipAxes)> =
      match &mut child . value() . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          Some ((t . skgid . clone (), &mut t . relationship_axes)),
        // No Restricted arm: diff mode requires the "all" skgrepo set
        // (diff_report.rs and repo_sets.rs refuse otherwise), under
        // which no node is restricted, so restricted vognodes never
        // reach diff rendering.
        _ => None };
    if let Some ((skgid, relationship_axes)) = child_id_and_relationship_axes {
      if let Some (m) = by_skgid . get (&skgid) {
        // Only Plus signs are meaningful here (the child appears in
        // worktree.contains; '-' positions are phantoms, handled separately).
        if m . staged   == Some (Sign::Plus)
          { relationship_axes . staged   = Some (Sign::Plus); }
        if m . unstaged == Some (Sign::Plus)
          { relationship_axes . unstaged = Some (Sign::Plus); }}}} }

/// Insert a removed-member phantom for each net-Removed id in the parent's
/// contains diff (ids present in HEAD but absent from worktree.contains).
/// Each phantom is placed at its correct HEAD position among surviving
/// siblings (per 'phantom_insertion_plan'), carrying its R axes (from the
/// merged contains diff) and N axes (if its file is also gone in some stage).
fn insert_phantoms_for_missing_contains (
  node_mut                       : &mut NodeMut<Viewnode>,
  graph                          : &InRustGraph,
  parent_treeid                  : NodeId,
  net_contains                   : &[Diff_Item<ID>],
  relationship_axes_by_skgid     : &HashMap<ID, RelationshipAxes>,
  skgrepo_diff                   : &SkgRepoDiff,
  skgrepo_diffs                  : &HashMap<SkgRepoName, SkgRepoDiff>,
  deleted_since_head_pid_src_map : &HashMap<ID, SkgRepoName>,
  tantivy_index                  : Option<&TantivyIndex>,
  config                         : &SkgConfig,
) -> Result<(), String> {
  let plan : Vec<(ID, Option<ID>)> =
    phantom_insertion_plan (net_contains);
  if plan . is_empty () { return Ok (( )); }
  // Map each surviving child's id to its NodeId, so an anchor id resolves
  // to the tree node we insert the phantom before.
  let child_node_by_skgid : HashMap<ID, NodeId> = {
    let node_ref : NodeRef<Viewnode> =
      node_mut . tree () . get (parent_treeid) . unwrap ();
    let mut m : HashMap<ID, NodeId> = HashMap::new ();
    for c in node_ref . children () {
      match &c . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => { m . insert ( t . skgid . clone (), c . id () ); },
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p)))
          => { m . insert ( p . skgid . clone (), c . id () ); },
        // No Restricted arm: restricted vognodes never reach diff
        // rendering (diff mode requires the "all" skgrepo set).
        _ => {}, }}
    m };
  for (skgid, anchor) in plan {
    let relationship_axes : RelationshipAxes =
      relationship_axes_by_skgid . get (&skgid) . copied () . unwrap_or_default ();
    // A removed-member diff-phantom is a *non-Unrestricted* viewnode. If its
    // skgrepo can't be determined -- e.g. a contains pointer at HEAD to a
    // node whose .skg file was deleted by an earlier commit and so exists
    // in no skgrepo -- fall back to the NOT_FOUND sentinel rather than
    // aborting the whole render (matching the PartnerFolder removed-member
    // path; TODO/DONE/local-view-update/plan_v2.org §7.6).
    let child_skgrepo : SkgRepoName =
      find_skgrepo_with_optional_tantivy (
        graph, &skgid, deleted_since_head_pid_src_map,
        tantivy_index, config )
        . unwrap_or_else ( SkgRepoName::not_found );
    let child_node_axes : NodeAxes =
      node_axes_for_phantom (&skgid, &child_skgrepo, skgrepo_diff, skgrepo_diffs);
    let child_title : String =
      title_for_phantom (
        graph, &skgid, &child_skgrepo,
        Some (skgrepo_diffs), config );
    let phantom : Viewnode =
      mk_phantom_viewnode (
        skgid . clone (), child_skgrepo, child_title,
        child_node_axes, relationship_axes );
    match anchor . and_then ( |a| child_node_by_skgid . get (&a) . copied () ) {
      Some (anchor_treeid) =>
        { node_mut . tree () . get_mut (anchor_treeid) . unwrap ()
            . insert_before (phantom); },
      None =>
        { node_mut . tree () . get_mut (parent_treeid) . unwrap ()
            . append (phantom); }, }}
  Ok (( )) }


/// Compute node axes for a phantom: derived from whether the
/// child's '.skg' file shows up as Deleted in either stage.
fn node_axes_for_phantom (
  skgid            : &ID,
  skgrepo       : &SkgRepoName,
  skgrepo_diff  : &SkgRepoDiff,
  skgrepo_diffs : &HashMap<SkgRepoName, SkgRepoDiff>,
) -> NodeAxes {
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", skgid . 0 ));
  // Prefer the repo_diff for the phantom's own skgrepo if available,
  // otherwise fall back to the parent's repo_diff.
  let resolved : &SkgRepoDiff =
    skgrepo_diffs . get (skgrepo) . unwrap_or (skgrepo_diff);
  node_axes_in_skgrepo_diff ( Some (resolved), &file_path ) }

#[cfg(test)]
#[path = "../../../tests/unit/render_diff.rs"]
mod phantom_insertion_plan_tests;
