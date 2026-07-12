/// Goal-list computers for sharing-scaffold rerender completers.
///
/// Each function returns `(Vec<ID>, HashSet<ID>)`: the ordered goal
/// list of children the scaffold should contain, and the set of IDs
/// that should appear as phantoms (present at HEAD but absent in the
/// worktree). Outside diff view, the second element is empty.

use crate::dbs::in_rust_graph::snapshot_global;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::git::{GitDiffStatus, MembershipAxes, NodeChanges, NodeCompleteDiff, Sign, SourceDiff, axes_from_per_stage_diffs, net_diff_from_per_stage, per_stage_node_changes_for_activeNode};
use crate::types::list::{compute_interleaved_diff, itemlist_and_removedset_from_diff, Diff_Item};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::misc::{ID, SkgConfig, SourceName, members_of};
use crate::types::nodes::complete::NodeComplete;
use crate::types::phantom::source_from_disk;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

/// Goal list for an OUTBOUND col -- one whose membership is a
/// relation list stored in the owner's own file (subscribeeCol,
/// overriddenCol, hiddenCol): the owner's worktree list, in diff
/// mode LCS-interleaved against the owner's HEAD-side list so
/// removed members appear as phantoms at their HEAD positions.
///
/// Reads no git objects: when neither stage map lists the owner's
/// file as Modified, HEAD = worktree for this list and the worktree
/// list is returned unchanged (the short-circuit); otherwise the
/// HEAD and worktree lists are reconstructed from the per-stage
/// relation diffs already in hand ('net_diff_from_per_stage').
/// An owner whose file is Added (not Modified) in some stage has no
/// HEAD side, so -- like content under a new file -- its members
/// carry no membership marks from here.
pub fn goal_list_for_outbound_col (
  owner_pid     : &ID,
  owner_source  : &SourceName,
  relation      : NodeRelation,
  source_diffs  : &Option<HashMap<SourceName, SourceDiff>>,
  worktree_list : &[ID],
) -> (Vec<ID>, HashSet<ID>) {
  if source_diffs . is_none () {
    return (worktree_list . to_vec (), HashSet::new ()); }
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_activeNode (
      source_diffs, owner_pid, owner_source );
  if staged_nc . is_none () && unstaged_nc . is_none () {
    return (worktree_list . to_vec (), HashSet::new ()); }
  let net : Vec<Diff_Item<ID>> =
    net_diff_from_per_stage (
      staged_nc   . and_then ( |c| relation . diff_in_nodechanges (c) ),
      unstaged_nc . and_then ( |c| relation . diff_in_nodechanges (c) ));
  itemlist_and_removedset_from_diff (&net) }

/// The per-stage membership axes of an outbound col's members, read
/// from the owner's per-stage diff of the named relation.  Used to
/// stamp 'newM' on PRESENT members; removed members are phantoms,
/// which carry their axes already.  Empty when the owner's file is
/// Modified in neither stage map.
pub fn outbound_member_axes (
  owner_pid    : &ID,
  owner_source : &SourceName,
  relation     : NodeRelation,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
) -> HashMap<ID, MembershipAxes> {
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_activeNode (
      source_diffs, owner_pid, owner_source );
  axes_from_per_stage_diffs (
    staged_nc   . and_then ( |c| relation . diff_in_nodechanges (c) ),
    unstaged_nc . and_then ( |c| relation . diff_in_nodechanges (c) ))
    . into_iter () . collect () }

/// The three snapshots -- HEAD, index, worktree -- of one node's
/// outbound relation list, reconstructed from the per-stage diffs
/// already in hand; no git reads:
/// - a file Modified in a stage: that stage's interleaved diff gives
///   its before-list (Unchanged + Removed) and after-list
///   (Unchanged + New);
/// - Deleted in a stage: before = the kept 'before_node's list,
///   after = empty;
/// - Added in a stage: before = empty, after = 'after_node's list;
/// - absent from a stage map: that stage changed nothing.
/// Used by the filter cols' three-snapshot derived-membership
/// comparison.
pub fn three_snapshots_of_relation_list (
  pid           : &ID,
  source        : &SourceName,
  relation      : NodeRelation,
  worktree_list : &[ID],
  source_diffs  : &Option<HashMap<SourceName, SourceDiff>>,
) -> [Vec<ID>; 3] {
  let file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  let sd : Option<&SourceDiff> =
    source_diffs . as_ref ()
    . and_then ( |d| d . get (source) )
    . filter ( |sd| sd . is_git_repo );
  let stage_before = | entry : Option<&NodeCompleteDiff>,
                       after_this_stage : &[ID] | -> Vec<ID> {
    match entry {
      None => after_this_stage . to_vec (), // stage changed nothing
      Some (ncd) => match ncd . status {
        GitDiffStatus::Modified =>
          match ncd . node_changes . as_ref ()
                . and_then ( |nc| relation . diff_in_nodechanges (nc) ) {
            None => after_this_stage . to_vec (),
            Some (diff_list) =>
              diff_list . iter () . filter_map ( |d| match d {
                Diff_Item::Unchanged (id) | Diff_Item::Removed (id)
                  => Some ( id . clone () ),
                Diff_Item::New (_) => None } )
              . collect () },
        GitDiffStatus::Deleted =>
          ncd . before_node . as_ref ()
          . map ( |nc| relation_list_of_nodecomplete (nc, relation) )
          . unwrap_or_default (),
        GitDiffStatus::Added =>
          Vec::new (), } } };
  let index : Vec<ID> =
    stage_before (
      sd . and_then ( |sd| sd . unstaged . get (&file) ),
      worktree_list );
  let head : Vec<ID> =
    stage_before (
      sd . and_then ( |sd| sd . staged . get (&file) ),
      &index );
  [ head, index, worktree_list . to_vec () ] }

/// The outbound list a NodeComplete holds for a relation, levels
/// dropped.  (The inverse scan has a private sibling; this one serves
/// the three-snapshot reconstruction.)
/// NOTE: was '&'a [ID]' before the leveled-list change; a borrow can
/// no longer be returned once the levels must be stripped, so this
/// now returns an owned 'Vec<ID>' (its one caller already called
/// '.to_vec()' on the result, so nothing downstream changed).
fn relation_list_of_nodecomplete (
  nc       : &NodeComplete,
  relation : NodeRelation,
) -> Vec<ID> {
  match relation {
    NodeRelation::Contains =>
      members_of ( & nc . contains ),
    NodeRelation::Subscribes =>
      members_of ( nc . subscribes_to . or_default () ),
    NodeRelation::HidesFromItsSubscriptions =>
      members_of ( nc . hides_from_its_subscriptions . or_default () ),
    NodeRelation::OverridesViewOf =>
      members_of ( nc . overrides_view_of . or_default () ),
    NodeRelation::TextlinksTo =>
      Vec::new (), } }

/// Exact per-stage membership axes from three derived-membership
/// snapshots: the staged signs are the HEAD-to-index changes and the
/// unstaged signs the index-to-worktree ones.  Members present (or
/// absent) in all three snapshots contribute nothing.
fn axes_from_three_snapshots (
  head     : &[ID],
  index    : &[ID],
  worktree : &[ID],
) -> HashMap<ID, MembershipAxes> {
  let head_set     : HashSet<&ID> = head     . iter () . collect ();
  let index_set    : HashSet<&ID> = index    . iter () . collect ();
  let worktree_set : HashSet<&ID> = worktree . iter () . collect ();
  let sign_between = | before : bool, after : bool | -> Option<Sign> {
    match (before, after) {
      (false, true) => Some (Sign::Plus),
      (true, false) => Some (Sign::Minus),
      _             => None } };
  let mut result : HashMap<ID, MembershipAxes> = HashMap::new ();
  for id in head_set . iter ()
            . chain ( index_set . iter () )
            . chain ( worktree_set . iter () ) {
    if result . contains_key (*id) { continue; }
    let axes : MembershipAxes = MembershipAxes {
      staged   : sign_between ( head_set  . contains (*id),
                                index_set . contains (*id) ),
      unstaged : sign_between ( index_set    . contains (*id),
                                worktree_set . contains (*id) ) };
    if ! axes . is_empty () {
      result . insert ( (*id) . clone (), axes ); }}
  result }

/// Goal list for a HiddenInSubscribeeCol: the intersection of the
/// subscriber's hides-list and the subscribee's contains-list.  In
/// diff mode, the DERIVED membership is compared at the three
/// snapshots (HEAD, index, worktree), so removed members phantom at
/// their HEAD positions with EXACT per-stage labels and added
/// members get per-stage 'newM' -- honest signs for a membership no
/// single relation's diff can express.  Returns (goal list,
/// removed-id set, per-member membership axes).
pub fn goal_list_for_hiddeninsubscribee_col (
  subscribee_pid      : &ID,
  subscribee_source   : &SourceName,
  subscriber_pid      : &ID,
  subscriber_source   : &SourceName,
  subscribee_contains : &[ID],
  subscriber_hides    : &[ID],
  source_diffs        : &Option<HashMap<SourceName, SourceDiff>>,
) -> (Vec<ID>, HashSet<ID>, HashMap<ID, MembershipAxes>) {
  let derived = | hides : &[ID], contains : &[ID] | -> Vec<ID> {
    // Intersection, preserving order from the hides list.
    let contains_set : HashSet<&ID> =
      contains . iter () . collect ();
    hides . iter ()
      . filter ( |id| contains_set . contains (id) )
      . cloned () . collect () };
  if source_diffs . is_none () {
    return ( derived (subscriber_hides, subscribee_contains),
             HashSet::new (), HashMap::new () ); }
  let hides3 : [Vec<ID>; 3] =
    three_snapshots_of_relation_list (
      subscriber_pid, subscriber_source,
      NodeRelation::HidesFromItsSubscriptions,
      subscriber_hides, source_diffs );
  let contains3 : [Vec<ID>; 3] =
    three_snapshots_of_relation_list (
      subscribee_pid, subscribee_source,
      NodeRelation::Contains,
      subscribee_contains, source_diffs );
  let derived3 : [Vec<ID>; 3] =
    [ derived (&hides3 [0], &contains3 [0]),
      derived (&hides3 [1], &contains3 [1]),
      derived (&hides3 [2], &contains3 [2]) ];
  let axes : HashMap<ID, MembershipAxes> =
    axes_from_three_snapshots (
      &derived3 [0], &derived3 [1], &derived3 [2] );
  let diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff ( &derived3 [0], &derived3 [2] );
  let (goal, removed) : (Vec<ID>, HashSet<ID>) =
    itemlist_and_removedset_from_diff (&diff);
  (goal, removed, axes) }

/// Goal list for a HiddenOutsideOfSubscribeeCol: the subscriber's
/// hides-list minus everything contained by any subscribee.  In
/// diff mode, the DERIVED membership is compared at the three
/// snapshots (HEAD, index, worktree) -- the hides list, the
/// subscribee list, and every involved subscribee's contains list
/// are each reconstructed per snapshot -- so removed members phantom
/// with exact per-stage labels and added members get per-stage
/// 'newM'.  Returns (goal list, removed-id set, per-member
/// membership axes).
pub fn goal_list_for_hiddenoutsideof_subscribeecol (
  subscriber_pid       : &ID,
  subscriber_source    : &SourceName,
  wt_subscriber_hides  : &[ID],
  wt_subscribees       : &[ID],
  source_diffs         : &Option<HashMap<SourceName, SourceDiff>>,
  config               : &SkgConfig,
) -> (Vec<ID>, HashSet<ID>, HashMap<ID, MembershipAxes>) {
  let derived = | hides : &[ID],
                  all_subscribee_content : &HashSet<ID> | -> Vec<ID> {
    hides . iter ()
      . filter ( |id| ! all_subscribee_content . contains (id) )
      . cloned () . collect () };
  let wt_subscribee_content_of = | pid : &ID | -> Vec<ID> {
    match snapshot_global_source (pid, config) {
      Some (src) =>
        nodecomplete_rustFirst_by_pid_and_source ( config, pid, &src )
          . ok ()
          . map ( |skg| members_of (& skg . contains) )
          . unwrap_or_default (),
      None => Vec::new () } };
  if source_diffs . is_none () {
    let wt_all_subscribee_content : HashSet<ID> =
      wt_subscribees . iter ()
        . flat_map ( |pid| wt_subscribee_content_of (pid) )
        . collect ();
    return ( derived (wt_subscriber_hides, &wt_all_subscribee_content),
             HashSet::new (), HashMap::new () ); }
  let hides3 : [Vec<ID>; 3] =
    three_snapshots_of_relation_list (
      subscriber_pid, subscriber_source,
      NodeRelation::HidesFromItsSubscriptions,
      wt_subscriber_hides, source_diffs );
  let subscribees3 : [Vec<ID>; 3] =
    three_snapshots_of_relation_list (
      subscriber_pid, subscriber_source,
      NodeRelation::Subscribes,
      wt_subscribees, source_diffs );
  let content3_by_subscribee : HashMap<ID, [Vec<ID>; 3]> = {
    // The contains snapshots of every subscribee involved in ANY
    // snapshot (a subscribee dropped since HEAD still shaped the
    // HEAD-side membership).
    let all_involved : HashSet<ID> =
      subscribees3 . iter () . flatten () . cloned () . collect ();
    all_involved . into_iter ()
      . map ( |pid| {
          let wt_contains : Vec<ID> =
            wt_subscribee_content_of (&pid);
          let source : Option<SourceName> =
            snapshot_global_source (&pid, config)
            . or_else ( || source_in_diffs_for_file (
                &pid, source_diffs ));
          let snapshots : [Vec<ID>; 3] = match source {
            Some (src) =>
              three_snapshots_of_relation_list (
                &pid, &src, NodeRelation::Contains,
                &wt_contains, source_diffs ),
            None => // no source anywhere: treat as empty throughout
              [ Vec::new (), Vec::new (), Vec::new () ] };
          (pid, snapshots) } )
      . collect () };
  let derived3 : [Vec<ID>; 3] =
    [0, 1, 2] . map ( |k| {
      let all_subscribee_content : HashSet<ID> =
        subscribees3 [k] . iter ()
          . flat_map ( |pid|
              content3_by_subscribee . get (pid)
                . map ( |snaps| snaps [k] . clone () )
                . unwrap_or_default () )
          . collect ();
      derived ( &hides3 [k], &all_subscribee_content ) } );
  let axes : HashMap<ID, MembershipAxes> =
    axes_from_three_snapshots (
      &derived3 [0], &derived3 [1], &derived3 [2] );
  let diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff ( &derived3 [0], &derived3 [2] );
  let (goal, removed) : (Vec<ID>, HashSet<ID>) =
    itemlist_and_removedset_from_diff (&diff);
  (goal, removed, axes) }

/// Locate a file's source by scanning the diff maps for it: serves
/// nodes whose file is gone from both the graph and the disk (e.g. a
/// subscribee deleted since HEAD), whose history nonetheless shaped
/// a filter col's HEAD-side membership.
fn source_in_diffs_for_file (
  pid          : &ID,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
) -> Option<SourceName> {
  let file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  source_diffs . as_ref () ? . iter ()
    . find ( |(_src, sd)|
        sd . staged   . contains_key (&file)
        || sd . unstaged . contains_key (&file) )
    . map ( |(src, _)| src . clone () ) }

#[cfg(test)]
#[path = "../../../../tests/unit/three_snapshots.rs"]
mod three_snapshot_tests;

/// Resolve a node's source: try the in-Rust graph snapshot first,
/// fall back to scanning source directories for the matching '.skg'
/// file. Tests that bypass
/// `init_global_handle_for_first_time_or_panic` rely on the disk
/// fallback.
fn snapshot_global_source (
  pid    : &ID,
  config : &SkgConfig,
) -> Option<SourceName> {
  if let Some (s) =
    snapshot_global ()
      . as_deref ()
      . and_then ( |g| g . pid_and_source (pid) . map ( |(_, s)| s ) )
  { return Some (s); }
  source_from_disk (pid, config) }
