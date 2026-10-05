/// Goal-list computers for sharing-non-vognode rerender completers.
///
/// Each function returns `(Vec<ID>, HashSet<ID>)`: the ordered goal
/// list of children the non-vognode should contain, and the set of IDs
/// that should appear as phantoms (present at HEAD but absent in the
/// worktree). Outside diff view, the second element is empty.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::git::{GitDiffStatus, RelationshipAxes, NodeChanges, GraphnodeDiff, Sign, SkgRepoDiff, axes_from_per_stage_diffs, net_diff_from_per_stage, per_stage_node_changes_for_activeVognode};
use crate::types::list::{compute_interleaved_diff, itemlist_and_removedset_from_diff, Diff_Item};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::misc::{ID, RelationshipMemberKey, SkgConfig, SkgRepoName, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::phantom::home_from_disk;

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

fn relationship_member_key (
  graph : &InRustGraph,
  skgid    : &ID,
) -> RelationshipMemberKey {
  graph . relationship_member_key (skgid)
}

/// Goal list for an OUTBOUND folder -- one whose membership is a
/// relation list stored in the recorder's own file (subscribeeFolder,
/// overriddenFolder, hiddenFolder): the recorder's worktree list, in diff
/// mode LCS-interleaved against the recorder's HEAD-side list so
/// removed members appear as phantoms at their HEAD positions.
///
/// Reads no git objects: when neither stage map lists the recorder's
/// file as Modified, HEAD = worktree for this list and the worktree
/// list is returned unchanged (the short-circuit); otherwise the
/// HEAD and worktree lists are reconstructed from the per-stage
/// relation diffs already in hand ('net_diff_from_per_stage').
/// An recorder whose file is Added (not Modified) in some stage has no
/// HEAD side, so -- like content under a new file -- its members
/// carry no membership marks from here.
pub fn goal_list_for_outbound_folder (
  recorder_pid     : &ID,
  recorder_skgrepo : &SkgRepoName,
  relation         : NodeRelation,
  skgrepo_diffs    : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
  worktree_list    : &[ID],
) -> (Vec<ID>, HashSet<ID>) {
  if skgrepo_diffs . is_none () {
    return (worktree_list . to_vec (), HashSet::new ()); }
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_activeVognode (
      skgrepo_diffs, recorder_pid, recorder_skgrepo );
  if staged_nc . is_none () && unstaged_nc . is_none () {
    return (worktree_list . to_vec (), HashSet::new ()); }
  let net : Vec<Diff_Item<ID>> =
    net_diff_from_per_stage (
      staged_nc   . and_then ( |c| relation . diff_in_nodechanges (c) ),
      unstaged_nc . and_then ( |c| relation . diff_in_nodechanges (c) ));
  itemlist_and_removedset_from_diff (&net) }

/// The per-stage relationship axes of an outbound folder's members, read
/// from the recorder's per-stage diff of the named relation.  Used to
/// stamp 'addedR' on PRESENT members; removed members are phantoms,
/// which carry their axes already.  Empty when the recorder's file is
/// Modified in neither stage map.
pub fn outbound_member_axes (
  recorder_pid     : &ID,
  recorder_skgrepo : &SkgRepoName,
  relation         : NodeRelation,
  skgrepo_diffs    : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
) -> HashMap<ID, RelationshipAxes> {
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_activeVognode (
      skgrepo_diffs, recorder_pid, recorder_skgrepo );
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
/// Used by the filter folders' three-snapshot derived-membership
/// comparison.
pub fn three_snapshots_of_relation_list (
  pid           : &ID,
  skgrepo       : &SkgRepoName,
  relation      : NodeRelation,
  worktree_list : &[ID],
  skgrepo_diffs : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
) -> [Vec<ID>; 3] {
  let file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  let sd : Option<&SkgRepoDiff> =
    skgrepo_diffs . as_ref ()
    . and_then ( |d| d . get (skgrepo) )
    . filter ( |sd| sd . is_gitrepo );
  let stage_before = | entry : Option<&GraphnodeDiff>,
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
          . map ( |nc| relation_list_of_graphnode (nc, relation) )
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

/// The outbound list a Graphnode holds for a relation, skgrepos
/// dropped.  (The inverse scan has a private sibling; this one serves
/// the three-snapshot reconstruction.)
/// NOTE: was '&'a [ID]' before the historical relation-partner change;
/// a borrow can no longer be returned once the skgrepos must be stripped, so this
/// now returns an owned 'Vec<ID>' (its one caller already called
/// '.to_vec()' on the result, so nothing downstream changed).
fn relation_list_of_graphnode (
  nc       : &Graphnode,
  relation : NodeRelation,
) -> Vec<ID> {
  match relation {
    NodeRelation::Contains =>
      members_of ( & nc . contains ),
    NodeRelation::SubscribesTo =>
      members_of ( nc . subscribes_to . or_default () ),
    NodeRelation::HidesFromItsSubscriptions =>
      members_of ( nc . hides_from_its_subscriptions . or_default () ),
    NodeRelation::OverridesViewOf =>
      members_of ( nc . overrides_view_of . or_default () ),
    NodeRelation::LinksTo =>
      Vec::new (), } }

/// Exact per-stage relationship axes from three derived-membership
/// snapshots: the staged signs are the HEAD-to-index changes and the
/// unstaged signs the index-to-worktree ones.  Members present (or
/// absent) in all three snapshots contribute nothing.
fn axes_from_three_snapshots (
  head     : &[ID],
  index    : &[ID],
  worktree : &[ID],
) -> HashMap<ID, RelationshipAxes> {
  let head_set     : HashSet<&ID> = head     . iter () . collect ();
  let index_set    : HashSet<&ID> = index    . iter () . collect ();
  let worktree_set : HashSet<&ID> = worktree . iter () . collect ();
  let sign_between = | before : bool, after : bool | -> Option<Sign> {
    match (before, after) {
      (false, true) => Some (Sign::Plus),
      (true, false) => Some (Sign::Minus),
      _             => None } };
  let mut result : HashMap<ID, RelationshipAxes> = HashMap::new ();
  for skgid in head_set . iter ()
            . chain ( index_set . iter () )
            . chain ( worktree_set . iter () ) {
    if result . contains_key (*skgid) { continue; }
    let axes : RelationshipAxes = RelationshipAxes {
      staged   : sign_between ( head_set  . contains (*skgid),
                                index_set . contains (*skgid) ),
      unstaged : sign_between ( index_set    . contains (*skgid),
                                worktree_set . contains (*skgid) ) };
    if ! axes . is_empty () {
      result . insert ( (*skgid) . clone (), axes ); }}
  result }

/// Goal list for a HiddenInSubscribeeFolder: the intersection of the
/// subscriber's hides-list and the subscribee's contains-list.  In
/// diff mode, the DERIVED membership is compared at the three
/// snapshots (HEAD, index, worktree), so removed members phantom at
/// their HEAD positions with EXACT per-stage labels and added
/// members get per-stage 'addedR' -- honest signs for a membership no
/// single relation's diff can express.  Returns (goal list,
/// removed-id set, per-member relationship axes).
pub fn goal_list_for_hiddenInSubscribee_folder (
  graph                : &InRustGraph,
  subscribee_pid      : &ID,
  subscribee_skgrepo  : &SkgRepoName,
  subscriber_pid      : &ID,
  subscriber_skgrepo  : &SkgRepoName,
  subscribee_contains : &[ID],
  subscriber_hides    : &[ID],
  skgrepo_diffs       : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
) -> (Vec<ID>, HashSet<ID>, HashMap<ID, RelationshipAxes>) {
  let derived = | hides : &[ID], contains : &[ID] | -> Vec<ID> {
    // Intersection, preserving order from the hides list.
    let contains_set : HashSet<RelationshipMemberKey> =
      contains . iter () . map (|skgid| relationship_member_key (graph, skgid)) . collect ();
    hides . iter ()
      . filter ( |skgid| contains_set . contains (&relationship_member_key (graph, skgid)) )
      . cloned () . collect () };
  if skgrepo_diffs . is_none () {
    return ( derived (subscriber_hides, subscribee_contains),
             HashSet::new (), HashMap::new () ); }
  let hides3 : [Vec<ID>; 3] =
    three_snapshots_of_relation_list (
      subscriber_pid, subscriber_skgrepo,
      NodeRelation::HidesFromItsSubscriptions,
      subscriber_hides, skgrepo_diffs );
  let contains3 : [Vec<ID>; 3] =
    three_snapshots_of_relation_list (
      subscribee_pid, subscribee_skgrepo,
      NodeRelation::Contains,
      subscribee_contains, skgrepo_diffs );
  let derived3 : [Vec<ID>; 3] =
    [ derived (&hides3 [0], &contains3 [0]),
      derived (&hides3 [1], &contains3 [1]),
      derived (&hides3 [2], &contains3 [2]) ];
  let axes : HashMap<ID, RelationshipAxes> =
    axes_from_three_snapshots (
      &derived3 [0], &derived3 [1], &derived3 [2] );
  let diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff ( &derived3 [0], &derived3 [2] );
  let (goal, removed) : (Vec<ID>, HashSet<ID>) =
    itemlist_and_removedset_from_diff (&diff);
  (goal, removed, axes) }

/// Goal list for a HiddenOutsideOfSubscribeeFolder: the subscriber's
/// hides-list minus everything contained by any subscribee.  In
/// diff mode, the DERIVED membership is compared at the three
/// snapshots (HEAD, index, worktree) -- the hides list, the
/// subscribee list, and every involved subscribee's contains list
/// are each reconstructed per snapshot -- so removed members phantom
/// with exact per-stage labels and added members get per-stage
/// 'addedR'.  Returns (goal list, removed-id set, per-member
/// relationship axes).
pub fn goal_list_for_hiddenOutsideOfSubscribee_folder (
  graph                : &InRustGraph,
  subscriber_pid       : &ID,
  subscriber_skgrepo   : &SkgRepoName,
  wt_subscriber_hides  : &[ID],
  wt_subscribees       : &[ID],
  skgrepo_diffs        : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
  config               : &SkgConfig,
) -> (Vec<ID>, HashSet<ID>, HashMap<ID, RelationshipAxes>) {
  let derived = | hides : &[ID],
                  all_subscribee_content : &HashSet<RelationshipMemberKey> | -> Vec<ID> {
    hides . iter ()
      . filter ( |skgid| ! all_subscribee_content
                . contains (&relationship_member_key (graph, skgid)) )
      . cloned () . collect () };
  let wt_subscribee_content_of = | pid : &ID | -> Vec<ID> {
    match graph_skgrepo (graph, pid, config) {
      Some (src) =>
        graphnode_graphFirst_by_pid_and_skgrepo ( graph, config, pid, &src )
          . ok ()
          . map ( |skg| members_of (& skg . contains) )
          . unwrap_or_default (),
      None => Vec::new () } };
  if skgrepo_diffs . is_none () {
    let wt_all_subscribee_content : HashSet<RelationshipMemberKey> =
      wt_subscribees . iter ()
        . flat_map ( |pid| wt_subscribee_content_of (pid) )
        . map (|skgid| relationship_member_key (graph, &skgid))
        . collect ();
    return ( derived (wt_subscriber_hides, &wt_all_subscribee_content),
             HashSet::new (), HashMap::new () ); }
  let hides3 : [Vec<ID>; 3] =
    three_snapshots_of_relation_list (
      subscriber_pid, subscriber_skgrepo,
      NodeRelation::HidesFromItsSubscriptions,
      wt_subscriber_hides, skgrepo_diffs );
  let subscribees3 : [Vec<ID>; 3] =
    three_snapshots_of_relation_list (
      subscriber_pid, subscriber_skgrepo,
      NodeRelation::SubscribesTo,
      wt_subscribees, skgrepo_diffs );
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
          let skgrepo : Option<SkgRepoName> =
            graph_skgrepo (graph, &pid, config)
            . or_else ( || skgrepo_in_diffs_for_file (
                &pid, skgrepo_diffs ));
          let snapshots : [Vec<ID>; 3] = match skgrepo {
            Some (src) =>
              three_snapshots_of_relation_list (
                &pid, &src, NodeRelation::Contains,
                &wt_contains, skgrepo_diffs ),
            None => // no skgrepo anywhere: treat as empty throughout
              [ Vec::new (), Vec::new (), Vec::new () ] };
          (pid, snapshots) } )
      . collect () };
  let derived3 : [Vec<ID>; 3] =
    [0, 1, 2] . map ( |k| {
      let all_subscribee_content : HashSet<RelationshipMemberKey> =
        subscribees3 [k] . iter ()
          . flat_map ( |pid|
              content3_by_subscribee . get (pid)
                . map ( |snaps| snaps [k] . clone () )
                . unwrap_or_default () )
          . map (|skgid| relationship_member_key (graph, &skgid))
          . collect ();
      derived ( &hides3 [k], &all_subscribee_content ) } );
  let axes : HashMap<ID, RelationshipAxes> =
    axes_from_three_snapshots (
      &derived3 [0], &derived3 [1], &derived3 [2] );
  let diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff ( &derived3 [0], &derived3 [2] );
  let (goal, removed) : (Vec<ID>, HashSet<ID>) =
    itemlist_and_removedset_from_diff (&diff);
  (goal, removed, axes) }

/// Locate a file's skgrepo by scanning the diff maps for it: serves
/// nodes whose file is gone from both the graph and the disk (e.g. a
/// subscribee deleted since HEAD), whose history nonetheless shaped
/// a filter folder's HEAD-side membership.
fn skgrepo_in_diffs_for_file (
  pid           : &ID,
  skgrepo_diffs : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
) -> Option<SkgRepoName> {
  let file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  skgrepo_diffs . as_ref () ? . iter ()
    . find ( |(_src, sd)|
        sd . staged   . contains_key (&file)
        || sd . unstaged . contains_key (&file) )
    . map ( |(src, _)| src . clone () ) }

#[cfg(test)]
#[path = "../../../../tests/unit/three_snapshots.rs"]
mod three_snapshot_tests;

/// Resolve a node's skgrepo: try the in-Rust graph snapshot first,
/// fall back to scanning skgrepo directories for the matching '.skg'
/// file. The disk fallback handles diff/deletion states absent from the current
/// graph snapshot.
fn graph_skgrepo (
  graph  : &InRustGraph,
  pid    : &ID,
  config : &SkgConfig,
) -> Option<SkgRepoName> {
  if let Some (s) =
    graph . pid_and_skgrepo (pid) . map ( |(_, s)| s )
  { return Some (s); }
  home_from_disk (pid, config) }
