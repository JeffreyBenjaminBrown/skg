use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::graph_straddle::types::{GraphStraddle, Partner, Presence};
use crate::to_org::expand::role_tree::{
  RoleTreePartners, attach_full_containerward_role_trees_at_treeids_with_skgrepo_set,
  build_and_integrate_role_tree_with_skgrepo_set};
use crate::to_org::render::change_viewnodes::{
  mk_historical_viewnode, mk_relRepo_move_viewnode};
use crate::to_org::render::fresh_drawing::{
  is_unrestricted_removed_member, match_key,
  reconcile_children_with_fresh_drawing};
use crate::to_org::complete::partner_folder::child_data::{
  build_child_data,
  ChildData,
  FolderGoalItem,
  FolderGoalShape,
  reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds,
};
use crate::types::env::RuntimeGeneration;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgConfig, SkgrepoName};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::phantom::home_from_disk;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::util::RepairSummary;
use crate::update_buffer::warnings::{CompletionWarning, RepairKind};
use crate::types::tree::moves::arrange_last_children;
use crate::types::viewnode::{DiffPresence, FolderPolicy, AffectsParent, PartnerFolder, Phantom, Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

/// Where a partner folder's goal list comes from.
#[derive(Clone, Copy)]
pub enum PartnerFolderGoalSource<'a> {
  /// The live graph.
  LiveGraph,
  /// The union of a graph straddle: the members of either graph, each
  /// tagged with the presence of its node and of its relationship.
  GraphStraddle (&'a GraphStraddle),
}

/// Reconciles one PartnerFolder (a folder is a collecting non-vognode)
/// from a node in the viewforest with the current in-Rust graph snapshot's data
/// about that node, or with a graph straddle's ('goal_source').
/// Makes the folder's UnrestrictedVognode children marked affectsParent=true match a goal list,
/// preserving reusable children and creating missing ones,
/// then demotes stale children marked affectsParent=true to 'affectsParent=false'.
pub fn reconcile_partnerFolder_children (
  node         : NodeId, // The PartnerFolder. Its parent is an UnrestrictedVognode.
  tree         : &mut Tree<Viewnode>,
  kind         : PartnerFolder,
  goal_source  : PartnerFolderGoalSource,
  runtime      : &RuntimeGeneration,
  graph_snap   : &Arc<InRustGraph>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  warning_sink : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  kind . error_unless_node_is_this_kind (tree, node) ?;
  // Read the recorder Unrestricted vognode *through* the required-ancestry table of 'update_buffer/ancestry.rs'
  // (index 0 = the parent), so this can never read an ancestor the table
  // does not list, and the death-check and this read share one spec.
  let (recorder_pid, recorder_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor (
      tree, node, 0, kind . caller_label () ) ?;
  let Some (member_role) = kind . relation_member_role () else {
    return Err (format!(
      "{} called for PartnerFolder {:?}, which has no relation member role",
      kind . caller_label (), kind) . into ()); };
  if let PartnerFolderGoalSource::GraphStraddle (straddle) = goal_source {
    return reconcile_partnerFolder_children_from_graph_straddle (
      node, tree, kind, straddle, &recorder_pid, member_role, runtime,
      deleted_by_this_save_extra_ids, warning_sink ); }
  let goal_list : Vec<ID> = {
    let graph_members : Vec<ID> =
      partnerFolder_members_in_graph (
        graph_snap, &runtime . config, &recorder_pid, kind, skgrepo_restriction )
      . ok_or ( "partnerFolder_members_in_graph: no relation member role" ) ?;
    match kind . policy () {
      FolderPolicy::EditableSet =>
        // Graph (disk) order is meaningful here: the user's own
        // save defines it.
        graph_members,
      FolderPolicy::WriteProtectedSet =>
        // The user may have reordered this generated folder; the
        // order is view-local and respected (metaplan_2.org,
        // "preserve user-visible order in write-protected sets").
        view_order_preserving_goal_list (
          tree, node, &graph_members, &live_member_skgid ) ?,
      FolderPolicy::WriteProtectedFilter =>
        // Unreachable: the let-else above already returned,
        // because the filter folders have no relation member role.
        graph_members,
      FolderPolicy::EditableFilter =>
        // Likewise unreachable here: HiddenOutside has its own
        // hide-derived reconciler, rather than a relation role.
        graph_members, }};
  // The node budget: a folder fills its members WHOLE and is budget-neutral -- the owning
  // vognode already spent its budget unit when it expanded, so drawing all the
  // relation members here costs nothing and never truncates the group. (The
  // budget bounds how many vognodes EXPAND, not how big one group is.)
  let relRepos : HashMap<ID, SkgrepoName> = // of unknown members, for their heralds
    raw_outbound_members (graph_snap, &recorder_pid, member_role, skgrepo_restriction)
      . into_iter ()
      . filter (|member| member . member . is_unresolved ())
      . filter (|member| member . relRepo != recorder_skgrepo)
      . map (|member| (member . member . into_id (), member . relRepo))
      . collect ();
  let goal_items : Vec<FolderGoalItem> =
    FolderGoalItem::live_members (&goal_list);
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &goal_items, &relRepos, runtime ) ?;
  // The stale-member rule of 'complete_managed_children': the reconciler deletes a stale member that is a view-leaf and
  // demotes one that is a branch, so a write-protected PartnerFolder
  // drops a stale leaf member instead of preserving it.
  let summary : RepairSummary<ID> =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind, &goal_items, &child_data,
      deleted_by_this_save_extra_ids ) ?;
  if kind . policy () != FolderPolicy::EditableSet {
    // Repairs to an editable folder are not repairs: its membership IS
    // whatever the user saved. Write-protected folders warn (when there is a
    // sink, i.e. when this completion serves the just-saved view).
    if let Some (sink) = warning_sink {
      push_repair_warnings (sink, kind, &recorder_pid, summary); }}
  Ok (( )) }

/// The members a folder of this kind under the recorder should hold,
/// from 'graph': its partners in the folder's relation, each extra ID
/// resolved to its node's pid (an unresolved ID kept as it is), less
/// the members the skgrepo restriction hides. In graph order. None if
/// the kind has no relation member role. The folder reconciler draws
/// these, and the save's write-protected-occurrence check compares a
/// present folder with them.
pub fn partnerFolder_members_in_graph (
  graph               : &InRustGraph,
  config              : &SkgConfig,
  recorder_pid        : &ID,
  kind                : PartnerFolder,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Option<Vec<ID>> {
  let member_role : RelationRole = kind . relation_member_role () ?;
  let recorder_role : RelationRole =
    member_role . opposite_role ();
  let skgrepo_resolver = |skgid : &ID| -> Option<SkgrepoName> {
    graph . pid_and_skgrepo (skgid)
      . map ( |(_pid, src)| src )
      . or_else ( || home_from_disk (skgid, config) ) };
  let members : Vec<ID> =
    if recorder_role . is_first_role () { // the folder shows a list in the RECORDER's file
      raw_outbound_members (graph, recorder_pid, member_role, skgrepo_restriction)
        . into_iter ()
        . map ( |member| member . member . into_id () )
        . collect ()
    } else { graph . other_member_pids_gated (
      recorder_pid, recorder_role, skgrepo_restriction ) };
  Some ( // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): these folders omit
         // restricted members, with no retention (a stale RestrictedVognode
         // child gets the reconciler's delete-leaf / deaden-branch rule).
    omit_restricted_members (
      members, skgrepo_restriction, skgrepo_resolver )) }

/// The recorder's partners in the member role's relation, as its file
/// lists them, each tagged by whether the graph resolves it, if the
/// folder shows a list in the RECORDER's file; else none.
fn raw_outbound_members (
  graph               : &InRustGraph,
  recorder_pid        : &ID,
  member_role         : RelationRole,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Vec<RelPartner<RelationshipMemberKey>> {
  if member_role . opposite_role () . is_first_role () {
    // Preserve an unresolved ID through this outbound surface.  The old
    // canonical-PID accessor is still right for inverse/write-protected folders, but
    // would erase an Unknown from the writable OverriddenFolder.
    graph . outbound_member_keys_for_relation_gated (
      recorder_pid, member_role . relation, skgrepo_restriction )
  } else { Vec::new () }}

/// 'reconcile_partnerFolder_children' with its goal list from the union
/// of a graph straddle: the partners in either graph, each item tagged
/// with its presence and shaped as 'straddle_goal_item' says. A child
/// standing for an item is kept whatever it stood as before, and
/// takes the item's shape.
fn reconcile_partnerFolder_children_from_graph_straddle (
  node         : NodeId,
  tree         : &mut Tree<Viewnode>,
  kind         : PartnerFolder,
  straddle     : &GraphStraddle,
  recorder_pid : &ID,
  member_role  : RelationRole,
  runtime      : &RuntimeGeneration,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  warning_sink : Option<&mut Vec<CompletionWarning>>,
) -> Result<(), Box<dyn Error>> {
  let partners : Vec<Partner> =
    straddle . partners (recorder_pid, member_role);
  let editable_set : bool = kind . policy () == FolderPolicy::EditableSet;
  let items : Vec<FolderGoalItem> =
    partners . iter ()
    . filter_map ( |partner| straddle_goal_item (straddle, partner, editable_set) )
    . collect ();
  let goal_items : Vec<FolderGoalItem> = match kind . policy () {
    FolderPolicy::WriteProtectedSet => {
      let members : Vec<ID> =
        items . iter () . map ( |item| item . skgid . clone () ) . collect ();
      let ordered : Vec<ID> = view_order_preserving_goal_list (
        tree, node, &members, &straddle_member_skgid ) ?;
      let by_skgid : HashMap<&ID, &FolderGoalItem> =
        items . iter () . map ( |item| (&item . skgid, item) ) . collect ();
      ordered . iter ()
        . filter_map ( |skgid| by_skgid . get (skgid) . map ( |item| (*item) . clone () ))
        . collect () },
    _ => items, };
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &goal_items, &HashMap::new (), runtime ) ?;
  let summary : RepairSummary<ID> =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind, &goal_items, &child_data,
      deleted_by_this_save_extra_ids ) ?;
  attach_memberRoleTrees_under_straddle_folder_members (
    tree, node, kind, member_role, straddle, &partners, runtime ) ?;
  if kind . policy () != FolderPolicy::EditableSet {
    if let Some (sink) = warning_sink {
      push_repair_warnings (sink, kind, recorder_pid, summary); }}
  Ok (( )) }

/// The goal item of a partner of a straddle-drawn folder: a node only
/// in the before-graph is its historical phantom; in a folder whose
/// membership the save reads (an EditableSet), a member whose
/// relationship is only in the before-graph is removed, and not drawn
/// at all when it is dangling, so that saving the buffer does not
/// restore it; any other is a member.
fn straddle_goal_item (
  straddle     : &GraphStraddle,
  partner      : &Partner,
  editable_set : bool,
) -> Option<FolderGoalItem> {
  let node_presence : Option<Presence> = straddle . node_presence (&partner . id);
  let presence : DiffPresence = DiffPresence {
    node : node_presence . and_then ( |presence| presence . diff_sign () ),
    rel  : partner . presence . diff_sign (), };
  let shape : FolderGoalShape = match node_presence {
    Some (Presence::OnlyBefore) => FolderGoalShape::Historical (
      mk_historical_viewnode (straddle, &partner . id, presence . rel) ? ),
    _ if editable_set && partner . presence == Presence::OnlyBefore => {
      node_presence ?; // dangling
      FolderGoalShape::Removed },
    _ => FolderGoalShape::Member, };
  Some ( FolderGoalItem {
    skgid : partner . id . clone (), presence : Some (presence), shape } ) }

/// The ID a member of a live-graph folder stands for.
fn live_member_skgid (
  viewnode : &Viewnode,
) -> Option<ID> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if t . affectsParent == AffectsParent::True => Some ( t . skgid . clone () ),
    _ => None, }}

/// The ID a member of a straddle-drawn folder stands for: a live
/// member, a removed one, or a historical phantom.
fn straddle_member_skgid (
  viewnode : &Viewnode,
) -> Option<ID> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
      Some ( h . skgid . clone () ),
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if is_unrestricted_removed_member (viewnode) => Some ( t . skgid . clone () ),
    _ => live_member_skgid (viewnode), }}

/// Under each member of a straddle-drawn folder, its member role tree,
/// drawn by the role-tree engine over the union of the straddle: under
/// a container, its full containerward tree; under any other member,
/// the role tree of the folder's role, with a full containerward tree
/// under each graft. In a folder of the folder viewparent's own
/// relationships, only under the members whose relationship changed.
/// A member whose relationship moved to another skgrepo gets that
/// move. Each member role tree is drawn afresh in a scratch tree and
/// reconciled with what the member holds, by skgid (and the move by
/// its kind) at each level, so that a member role tree that stays
/// keeps its viewnodes and their fold state, in the order of the walk.
fn attach_memberRoleTrees_under_straddle_folder_members (
  tree        : &mut Tree<Viewnode>,
  folder      : NodeId,
  kind        : PartnerFolder,
  member_role : RelationRole,
  straddle    : &GraphStraddle,
  partners    : &[Partner],
  runtime     : &RuntimeGeneration,
) -> Result<(), Box<dyn Error>> {
  let by_id : HashMap<&ID, &Partner> =
    partners . iter () . map ( |partner| (&partner . id, partner) ) . collect ();
  let outbound : bool = ! member_role . is_first_role ();
  let members : Vec<(NodeId, &Partner)> =
    tree . get (folder) . ok_or ("attach_memberRoleTrees_under_straddle_folder_members: folder not found") ?
    . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        by_id . get (&t . skgid) . map ( |partner| (child . id (), *partner) ),
      _ => None, } )
    . collect ();
  for (member, partner) in members {
    let mut fresh : Tree<Viewnode> = // its root stands in for the member
      Tree::new ( tree . get (member) . unwrap () . value () . clone () );
    let fresh_member : NodeId = fresh . root () . id ();
    if ! outbound || partner . changed () {
      if let Some (moved) = mk_relRepo_move_viewnode (partner) {
        fresh . root_mut () . prepend (moved); }
      if kind == PartnerFolder::Container {
        attach_full_containerward_role_trees_at_treeids_with_skgrepo_set (
          &mut fresh, &[fresh_member], &runtime . graph, &runtime . config, None,
          RoleTreePartners::GraphStraddle (straddle) ) ?;
      } else {
        build_and_integrate_role_tree_with_skgrepo_set (
          &mut fresh, fresh_member, &runtime . graph, member_role,
          &runtime . config, None, RoleTreePartners::GraphStraddle (straddle) ) ?; }}
    let kept : Vec<NodeId> = reconcile_children_with_fresh_drawing (
      tree, member, &fresh, fresh_member,
      &|viewnode| match_key (viewnode) . is_some (), &|_| true ) ?;
    arrange_last_children (tree, member, &kept) ?; }
  Ok (( )) }

/// Translate a RepairSummary into per-repair-kind CompletionWarnings.
/// Empty categories contribute nothing.
pub fn push_repair_warnings (
  sink    : &mut Vec<CompletionWarning>,
  folder     : PartnerFolder,
  recorder   : &ID,
  summary : RepairSummary<ID>,
) {
  let categories : [ (RepairKind, Vec<ID>); 4 ] = [
    (RepairKind::RestoredMember,   summary . created),
    (RepairKind::DemotedNonMember, summary . demoted),
    (RepairKind::RemovedStaleLeaf, summary . deleted_stale),
    (RepairKind::RemovedDuplicate, summary . deleted_duplicates) ];
  for (repair, children) in categories {
    if ! children . is_empty () {
      sink . push ( CompletionWarning::FolderRepair {
        folder,
        recorder : recorder . clone (),
        repair,
        children } ); }}}

/// The effective goal list for a FolderPolicy::WriteProtectedSet folder:
/// the folder's existing members ('member_skgid' says which children
/// are, and which ID each stands for), in their
/// current view order, filtered to graph-real members (first
/// occurrence of a duplicate wins; the reconciler detaches the
/// duplicates themselves), then every graph member not yet listed,
/// appended in the deterministic order 'other_member_pids' returns
/// (sorted by ID, for the inbound relations these folders hold).
/// The resulting order is view-local: it is never written to disk,
/// and two open views of the same folder may disagree.
fn view_order_preserving_goal_list (
  tree          : &Tree<Viewnode>,
  folder           : NodeId,
  graph_members : &[ID],
  member_skgid  : &dyn Fn (&Viewnode) -> Option<ID>,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let member_set : HashSet<&ID> =
    graph_members . iter () . collect ();
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut goal : Vec<ID> = Vec::new ();
  { let folder_ref : NodeRef<Viewnode> =
      tree . get (folder)
      . ok_or ("view_order_preserving_goal_list: folder not found") ?;
    for child in folder_ref . children () {
      if let Some (skgid) = member_skgid ( child . value () ) {
        if member_set . contains (&skgid)
          && seen . insert (skgid . clone ())
        { goal . push (skgid); }}}}
  for member in graph_members {
    if seen . insert (member . clone ()) {
      goal . push (member . clone ()); }}
  Ok (goal) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../../tests/unit/reconcile_partner_folder.rs"]
mod tests;
