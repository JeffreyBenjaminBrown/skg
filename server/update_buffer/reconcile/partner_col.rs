use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_col::child_data::{
  build_child_data,
  ChildData,
  apply_membership_axes_to_col_members,
  reconcile_partnerCol_children_against_goal_list,
};
use crate::to_org::complete::partner_col::goal_list::{
  goal_list_for_outbound_col,
  outbound_member_axes,
};
use crate::to_org::complete::partner_col::inverse_scan::inverse_scan_for_inbound_col;
use crate::types::env::SkgEnv;
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, file_existence_axes_from_source_diff};
use crate::types::misc::{ID, SourceName};
use crate::source_sets::ActiveSourceSet;
use crate::types::phantom::{phantom_axes, source_from_disk};
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::update_buffer::reconcile::omit_inactive_members;
use crate::update_buffer::util::RepairSummary;
use crate::update_buffer::warnings::{CompletionWarning, RepairKind};
use crate::types::viewnode::{ColPolicy, ParentIs, PartnerCol, ViewNode, ViewNodeKind, Vognode};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

/// Reconciles one PartnerCol (TODO/DONE/local-view-update/plan_v2.org §19 terminology: a col = a collecting scaffold)
/// from a node in the view tree with the current in-Rust graph snapshot's data
/// about that node.
/// Makes the col's ActiveNode children marked parentIs=affected match a goal list,
/// preserving reusable children and creating missing ones,
/// then demotes stale children marked parentIs=affected to 'parentIs=independent'.
pub fn reconcile_partnerCol_children (
  node         : NodeId, // The PartnerCol. Its parent is an ActiveNode.
  tree         : &mut Tree<ViewNode>,
  kind         : PartnerCol,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  env          : &SkgEnv,
  graph_snap   : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set : Option<&ActiveSourceSet>,
  warning_sink : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  kind . error_unless_node_is_this_kind (tree, node) ?;
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: read the owner Active vognode *through* the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table
  // (index 0 = the parent), so this can never read an ancestor the table
  // does not list, and the death-check and this read share one spec.
  let (owner_pid, owner_source) : (ID, SourceName) =
    pid_and_source_from_required_ancestor (
      tree, node, 0, kind . caller_label () ) ?;
  let Some (member_role) = kind . relation_member_role () else {
    return Err (format!(
      "{} called for PartnerCol {:?}, which has no relation member role",
      kind . caller_label (), kind) . into ()); };
  let owner_role =
    member_role . opposite_role ();
  let source_resolver = |id : &ID| -> Option<SourceName> {
    graph_snap . pid_and_source (id)
      . map ( |(_pid, src)| src )
      . or_else ( || source_from_disk (id, &env . config) ) };
  let outbound : bool = // the col shows a list in the OWNER's file
    owner_role . is_first_role ();
  let inbound_scan : HashMap<ID, MembershipAxes> =
    // Inbound cols' edges live in the MEMBERS' files; the inverse
    // scan reads those files' diffs (Modified relation diffs,
    // Deleted before_node lists, Added after_node lists). Empty
    // outside diff mode and for outbound cols.
    if ! outbound && source_diffs . is_some () {
      inverse_scan_for_inbound_col (
        &owner_pid, member_role . relation, source_diffs )
    } else { HashMap::new () };
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) = {
    let graph_members : Vec<ID> =
      // TODO/full-schema/9-2_source-set-safety.org: these cols omit
      // inactive members, with no retention (a stale InactiveNode
      // child gets the reconciler's delete-leaf / deaden-branch rule).
      omit_inactive_members (
        graph_snap . other_member_pids (&owner_pid, owner_role),
        active_source_set,
        source_resolver );
    if outbound && source_diffs . is_some () {
      // Diff mode, outbound col: the owner's per-stage relation diff
      // interleaves members removed since HEAD (phantom positions)
      // into the worktree list. This diff-derived order supersedes
      // the ReadOnlySet view-local order (the hiddenCol) while diff
      // mode is on: a phantom belongs at its HEAD position, which a
      // view-local reordering cannot express.
      let (goal, removed) : (Vec<ID>, HashSet<ID>) =
        goal_list_for_outbound_col (
          &owner_pid, &owner_source, member_role . relation,
          source_diffs, &graph_members );
      let goal : Vec<ID> = // phantoms can be inactive too
        omit_inactive_members (
          goal, active_source_set, source_resolver );
      (goal, removed)
    } else {
      let mut goal : Vec<ID> = match kind . policy () {
        ColPolicy::WritableSet =>
          // Graph (disk) order is meaningful here: the user's own
          // save defines it.
          graph_members,
        ColPolicy::ReadOnlySet =>
          // The user may have reordered this generated col; the
          // order is view-local and respected (metaplan_2.org,
          // "preserve user-visible order in read-only sets").
          view_order_preserving_goal_list (tree, node, &graph_members) ?,
        ColPolicy::ReadOnlyFilter =>
          // Unreachable: the let-else above already returned,
          // because the filter cols have no relation member role.
          graph_members, };
      let removed : HashSet<ID> = {
        // Inbound phantom tail: members the inverse scan says are
        // gone from the worktree. Inbound cols have no meaningful
        // HEAD order (display order is view-local), so phantoms
        // append after the real members in sorted-ID order -- the
        // stage-8 deterministic tail rule. Appended AFTER the
        // view-order pass above, which filters to graph-real
        // members and so cannot carry them.
        let mut tail : Vec<ID> = {
          let goal_set : HashSet<&ID> =
            goal . iter () . collect ();
          inbound_scan . iter ()
            . filter ( |(_, axes)| ! axes . net_is_present () )
            . map ( |(id, _)| id . clone () )
            . filter ( |id| ! goal_set . contains (id) )
            . collect () };
        tail = // phantoms of inactive members are omitted too
          omit_inactive_members (
            tail, active_source_set,
            |id : &ID| env . find_source (
              id, deleted_since_head_pid_src_map ));
        tail . sort_by ( |a, b| a . 0 . cmp (&b . 0) );
        let removed : HashSet<ID> =
          tail . iter () . cloned () . collect ();
        goal . extend (tail);
        removed };
      (goal, removed) }};
  let outbound_axes = // the owner's own relation diff
    |child : &ID, child_src : &SourceName|
    -> (ExistenceAxes, MembershipAxes) {
    phantom_axes ( child, child_src,
                   &owner_pid, &owner_source,
                   member_role . relation,
                   source_diffs . as_ref () ) };
  let inbound_axes = // membership from the inverse scan; existence
                     // from the member's own file statuses
    |child : &ID, child_src : &SourceName|
    -> (ExistenceAxes, MembershipAxes) {
    ( file_existence_axes_from_source_diff (
        source_diffs, child, child_src ),
      inbound_scan . get (child) . copied ()
        . unwrap_or ( MembershipAxes {
            staged : None, unstaged : Some (Sign::Minus) } )) };
  let axes_for_removed // the relation this col represents
    : &dyn Fn (&ID, &SourceName) -> (ExistenceAxes, MembershipAxes) =
    if outbound { &outbound_axes } else { &inbound_axes };
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a col fills its members WHOLE and is budget-neutral -- the owning
  // vognode already spent its budget unit when it expanded, so drawing all the
  // relation members here costs nothing and never truncates the group. (The
  // budget bounds how many vognodes EXPAND, not how big one group is.)
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node,
      &goal_list, &removed_ids, axes_for_removed,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §6.0/§16: the reconciler deletes a stale member that is a view-leaf and
  // demotes one that is a branch, so a read-only PartnerCol
  // drops a stale leaf member instead of preserving it.
  let summary : RepairSummary<ID> =
    reconcile_partnerCol_children_against_goal_list (
      tree, node, kind, &goal_list, &child_data ) ?;
  if source_diffs . is_some () {
    // Present members whose edge is New in some stage get that
    // stage's 'newM'; removed members are the phantoms above.
    let axes_by_id : HashMap<ID, MembershipAxes> =
      if outbound {
        outbound_member_axes (
          &owner_pid, &owner_source, member_role . relation,
          source_diffs )
      } else { inbound_scan . clone () };
    apply_membership_axes_to_col_members (
      tree, node, &axes_by_id ) ?; }
  if kind . policy () != ColPolicy::WritableSet {
    // Repairs to a writable col are not repairs: its membership IS
    // whatever the user saved. Read-only cols warn (when there is a
    // sink, i.e. when this completion serves the just-saved view).
    if let Some (sink) = warning_sink {
      push_repair_warnings (sink, kind, &owner_pid, summary); }}
  Ok (( )) }

/// Translate a RepairSummary into per-repair-kind CompletionWarnings.
/// Empty categories contribute nothing.
pub fn push_repair_warnings (
  sink    : &mut Vec<CompletionWarning>,
  col     : PartnerCol,
  owner   : &ID,
  summary : RepairSummary<ID>,
) {
  let categories : [ (RepairKind, Vec<ID>); 4 ] = [
    (RepairKind::RestoredMember,   summary . created),
    (RepairKind::DemotedNonMember, summary . demoted),
    (RepairKind::RemovedStaleLeaf, summary . deleted_stale),
    (RepairKind::RemovedDuplicate, summary . deleted_duplicates) ];
  for (repair, children) in categories {
    if ! children . is_empty () {
      sink . push ( CompletionWarning::ColRepair {
        col,
        owner : owner . clone (),
        repair,
        children } ); }}}

/// The effective goal list for a ColPolicy::ReadOnlySet col:
/// the col's existing Active parentIs=Affected children, in their
/// current view order, filtered to graph-real members (first
/// occurrence of a duplicate wins; the reconciler detaches the
/// duplicates themselves), then every graph member not yet listed,
/// appended in the deterministic order 'other_member_pids' returns
/// (sorted by ID, for the inbound relations these cols hold).
/// The resulting order is view-local: it is never written to disk,
/// and two open views of the same col may disagree.
fn view_order_preserving_goal_list (
  tree          : &Tree<ViewNode>,
  col           : NodeId,
  graph_members : &[ID],
) -> Result<Vec<ID>, Box<dyn Error>> {
  let member_set : HashSet<&ID> =
    graph_members . iter () . collect ();
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut goal : Vec<ID> = Vec::new ();
  { let col_ref : NodeRef<ViewNode> =
      tree . get (col)
      . ok_or ("view_order_preserving_goal_list: col not found") ?;
    for child in col_ref . children () {
      if let ViewNodeKind::Vognode (Vognode::Active (t))
        = & child . value () . kind
      { if t . parentIs == ParentIs::Affected
          && member_set . contains (&t . id)
          && seen . insert (t . id . clone ())
        { goal . push (t . id . clone ()); }}}}
  for member in graph_members {
    if seen . insert (member . clone ()) {
      goal . push (member . clone ()); }}
  Ok (goal) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../../tests/unit/reconcile_partner_col.rs"]
mod tests;
