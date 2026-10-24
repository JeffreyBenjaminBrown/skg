use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::diff_view::partner_folders::reconcile_partnerFolder_children_from_graph_straddle;
use crate::graph_straddle::types::GraphStraddle;
use crate::to_org::complete::partner_folder::child_data::{
  build_child_data,
  ChildData,
  FolderGoalItem,
  reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds,
  skgid_a_folder_child_stands_for,
};
use crate::types::env::RuntimeGeneration;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgConfig, SkgrepoName};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::phantom::home_from_disk;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::util::RepairSummary;
use crate::update_buffer::warnings::{CompletionWarning, RepairKind};
use crate::types::viewnode::{FolderPolicy, PartnerFolder, ViewnodeId, Viewnode};

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
  /// tagged with the change of its node and of its relationship.
  GraphStraddle (&'a GraphStraddle),
}

/// Reconciles one PartnerFolder (a folder is a collecting non-vognode)
/// from a node in the viewforest with the current in-Rust graph snapshot's data
/// about that node, or with a graph straddle's ('goal_source';
/// 'diff_view/partner_folders.rs').
/// Makes the folder's UnrestrictedVognode children that are members match a goal list,
/// preserving reusable children and creating missing ones,
/// then makes stale member children non-members ('nonMember').
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
  // Read the viewparent Unrestricted vognode *through* the required-ancestry table of 'update_buffer/ancestry.rs'
  // (index 0 = the parent), so this can never read an ancestor the table
  // does not list, and the death-check and this read share one spec.
  let (viewparent_pid, viewparent_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor (
      tree, node, 0, kind . caller_label () ) ?;
  let Some (member_role) = kind . relation_member_role () else {
    return Err (format!(
      "{} called for PartnerFolder {:?}, which has no relation member role",
      kind . caller_label (), kind) . into ()); };
  if let PartnerFolderGoalSource::GraphStraddle (straddle) = goal_source {
    return reconcile_partnerFolder_children_from_graph_straddle (
      node, tree, kind, straddle, &viewparent_pid, member_role, runtime,
      deleted_by_this_save_extra_ids, warning_sink ); }
  let goal_list : Vec<ID> = {
    let graph_members : Vec<ID> =
      partnerFolder_members_in_graph (
        graph_snap, &runtime . config, &viewparent_pid, kind, skgrepo_restriction )
      . ok_or ( "partnerFolder_members_in_graph: no relation member role" ) ?;
    match kind . policy () {
      FolderPolicy::EditableSet =>
        // Graph (disk) order is meaningful here: the user's own
        // save defines it.
        graph_members,
      FolderPolicy::WriteProtectedSet =>
        // The user may have reordered this generated folder; the
        // order is view-local and respected ('docs/sharing-model.org',
        // "The write-protected folders: who subscribes to, overrides, or
        // hides this node").
        view_order_preserving_goal_list (
          tree, node, &graph_members, graph_snap ) ?,
      FolderPolicy::WriteProtectedFilter =>
        // Unreachable: the let-else above already returned,
        // because the filter folders have no relation member role.
        graph_members,
      FolderPolicy::EditableFilter =>
        // Likewise unreachable here: HiddenOutside has its own
        // hide-derived reconciler, rather than a relation role.
        graph_members, }};
  // The node budget: a folder fills its members whole and is budget-neutral -- the owning
  // vognode already spent its budget unit when it extended, so adding all the
  // relation members here costs nothing and never truncates the group. (The
  // budget bounds how many vognodes extend, not how big one group is.)
  let relRepos : HashMap<ID, SkgrepoName> = // of unknown members, for their heralds
    raw_outbound_members (graph_snap, &viewparent_pid, member_role, skgrepo_restriction)
      . into_iter ()
      . filter (|member| member . member . is_unresolved ())
      . filter (|member| member . relRepo != viewparent_skgrepo)
      . map (|member| (member . member . into_id (), member . relRepo))
      . collect ();
  let goal_items : Vec<FolderGoalItem> =
    FolderGoalItem::live_members (&goal_list);
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &goal_items, &relRepos, runtime ) ?;
  // The one rule ('docs/glossary.org'): the reconciler converts a stale
  // member, a leaf to a tombstone the prune removes and a branch
  // retained as non-member, so a write-protected PartnerFolder drops a
  // stale leaf member instead of preserving it.
  let summary : RepairSummary<ID> =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind, &goal_items, &child_data, graph_snap,
      deleted_by_this_save_extra_ids ) ?;
  if kind . policy () != FolderPolicy::EditableSet {
    // Repairs to an editable folder are not repairs: its membership is
    // whatever the user saved. Write-protected folders warn (when there is a
    // sink, i.e. when this completion serves the just-saved view).
    if let Some (sink) = warning_sink {
      push_repair_warnings (
        sink, kind, &viewparent_pid,
        folder_viewnode_id_of (tree, node) . as_ref (), summary ); }}
  Ok (( )) }

/// The members a folder of this kind under the viewparent should hold,
/// from 'graph': its partners in the folder's relation, each extra ID
/// resolved to its node's pid (an unresolved ID kept as it is), less
/// the members the skgrepo restriction hides. In graph order. None if
/// the kind has no relation member role. The folder reconciler shows
/// these, and the save's write-protected-occurrence check compares a
/// present folder with them.
pub fn partnerFolder_members_in_graph (
  graph               : &InRustGraph,
  config              : &SkgConfig,
  viewparent_pid      : &ID,
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
    if recorder_role . is_first_role () { // the folder shows a list in the recorder's file
      raw_outbound_members (graph, viewparent_pid, member_role, skgrepo_restriction)
        . into_iter ()
        . map ( |member| member . member . into_id () )
        . collect ()
    } else { graph . other_member_pids_gated (
      viewparent_pid, recorder_role, skgrepo_restriction ) };
  Some ( // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): these folders omit
         // restricted members. A RestrictedVognode child already in the
         // folder is unmanaged, so an ordinary rerender never removes it.
    omit_restricted_members (
      members, skgrepo_restriction, skgrepo_resolver )) }

/// The viewparent's partners in the member role's relation, as its file
/// lists them, each tagged by whether the graph resolves it, if the
/// folder shows a list in the viewparent's file (the viewparent is the
/// recorder); else none.
fn raw_outbound_members (
  graph               : &InRustGraph,
  viewparent_pid      : &ID,
  member_role         : RelationRole,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Vec<RelPartner<RelationshipMemberKey>> {
  if member_role . opposite_role () . is_first_role () {
    // Preserve an unresolved ID through this outbound surface.  The old
    // canonical-PID accessor is still right for inverse/write-protected folders, but
    // would erase an Unknown from the writable OverriddenFolder.
    graph . outbound_member_keys_for_relation_gated (
      viewparent_pid, member_role . relation, skgrepo_restriction )
  } else { Vec::new () }}

/// Translate a RepairSummary into per-repair-kind CompletionWarnings.
/// Empty categories contribute nothing.
pub fn push_repair_warnings (
  sink    : &mut Vec<CompletionWarning>,
  folder     : PartnerFolder,
  recorder   : &ID,
  folder_viewnode_id : Option<&ViewnodeId>,
  summary : RepairSummary<ID>,
) {
  let categories : [ (RepairKind, Vec<ID>); 4 ] = [
    (RepairKind::ReinstatedMember,   summary . created),
    (RepairKind::Retained,    summary . retained),
    (RepairKind::EntombstonedStaleLeaf, summary . entombstoned_stale),
    (RepairKind::EntombstonedDuplicate, summary . entombstoned_duplicates) ];
  for (repair, children) in categories {
    if ! children . is_empty () {
      sink . push ( CompletionWarning::FolderRepair {
        folder,
        recorder : recorder . clone (),
        folder_viewnode_id : folder_viewnode_id . cloned (),
        repair,
        children } ); }}}

/// The viewnode ID of the folder at 'node', for its repair warnings.
pub fn folder_viewnode_id_of (
  tree : &Tree<Viewnode>,
  node : NodeId,
) -> Option<ViewnodeId> {
  tree . get (node) . and_then ( |folder| folder . value () . viewnode_id . clone () ) }

/// The effective goal list for a FolderPolicy::WriteProtectedSet folder:
/// the folder's existing members (the children for which
/// 'skgid_a_folder_child_stands_for' over 'graph' says which ID each
/// stands for), in their
/// current view order, filtered to graph-real members (first
/// occurrence of a duplicate wins; the reconciler converts the
/// duplicates themselves), then every graph member not yet listed,
/// appended in the deterministic order 'other_member_pids' returns
/// (sorted by ID, for the inbound relations these folders hold).
/// The resulting order is view-local: it is never written to disk,
/// and two open views of the same folder may disagree.
pub(crate) fn view_order_preserving_goal_list (
  tree          : &Tree<Viewnode>,
  folder           : NodeId,
  graph_members : &[ID],
  graph         : &InRustGraph,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let member_set : HashSet<&ID> =
    graph_members . iter () . collect ();
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut goal : Vec<ID> = Vec::new ();
  { let folder_ref : NodeRef<Viewnode> =
      tree . get (folder)
      . ok_or ("view_order_preserving_goal_list: folder not found") ?;
    for child in folder_ref . children () {
      if let Some (skgid) =
        skgid_a_folder_child_stands_for ( child . value (), graph ) {
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
