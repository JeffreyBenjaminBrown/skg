use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::graph_straddle::types::{GraphStraddle, Partner, Presence};
use crate::to_org::expand::role_tree::{
  RoleTreePartners, attach_full_containerward_role_trees_at_treeids_with_skgrepo_set,
  build_and_integrate_role_tree_with_skgrepo_set};
use crate::to_org::render::change_viewnodes::{
  mk_historical_viewnode, mk_relRepo_move_viewnode};
use crate::to_org::complete::partner_folder::child_data::{
  build_child_data,
  ChildData,
  apply_presence_to_folder_members,
  reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds,
};
use crate::types::env::RuntimeGeneration;
use crate::types::git::Sign;
use crate::types::misc::{ID, RelPartner, SkgrepoName};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::phantom::home_from_disk;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::util::RepairSummary;
use crate::update_buffer::warnings::{CompletionWarning, RepairKind};
use crate::types::viewnode::{DiffPresence, Editability, FolderPolicy, AffectsParent, PartnerFolder, Phantom, Viewnode, ViewnodeKind, Vognode, mk_unknown_viewnode};

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

/// Reconciles one PartnerFolder (TODO/DONE/local-view-update/plan_v2.org §19 terminology: a folder = a collecting non-vognode)
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
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: read the recorder Unrestricted vognode *through* the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table
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
  let recorder_role =
    member_role . opposite_role ();
  let skgrepo_resolver = |skgid : &ID| -> Option<SkgrepoName> {
    graph_snap . pid_and_skgrepo (skgid)
      . map ( |(_pid, src)| src )
      . or_else ( || home_from_disk (skgid, &runtime . config) ) };
  let outbound : bool = // the folder shows a list in the RECORDER's file
    recorder_role . is_first_role ();
  let raw_outbound_members : Vec<RelPartner<ID>> = if outbound {
    // Preserve an unresolved ID through this outbound surface.  The old
    // canonical-PID accessor is still right for inverse/write-protected folders, but
    // would erase an Unknown from the writable OverriddenFolder.
    graph_snap . outbound_rel_partners_for_relation_gated (
      &recorder_pid, member_role . relation, skgrepo_restriction )
  } else { Vec::new () };
  let goal_list : Vec<ID> = {
    let graph_members : Vec<ID> =
      // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: these folders omit
      // restricted members, with no retention (a stale RestrictedVognode
      // child gets the reconciler's delete-leaf / deaden-branch rule).
      omit_restricted_members (
        if outbound {
          raw_outbound_members . iter ()
            . map ( |member| graph_snap . pid_of (&member . member)
                   . unwrap_or_else ( || member . member . clone () ) )
            . collect ()
        } else { graph_snap . other_member_pids_gated (
          &recorder_pid, recorder_role, skgrepo_restriction ) },
        skgrepo_restriction,
        skgrepo_resolver );
    match kind . policy () {
      FolderPolicy::EditableSet =>
        // Graph (disk) order is meaningful here: the user's own
        // save defines it.
        graph_members,
      FolderPolicy::WriteProtectedSet =>
        // The user may have reordered this generated folder; the
        // order is view-local and respected (metaplan_2.org,
        // "preserve user-visible order in write-protected sets").
        view_order_preserving_goal_list (tree, node, &graph_members) ?,
      FolderPolicy::WriteProtectedFilter =>
        // Unreachable: the let-else above already returned,
        // because the filter folders have no relation member role.
        graph_members,
      FolderPolicy::EditableFilter =>
        // Likewise unreachable here: HiddenOutside has its own
        // hide-derived reconciler, rather than a relation role.
        graph_members, }};
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a folder fills its members WHOLE and is budget-neutral -- the owning
  // vognode already spent its budget unit when it expanded, so drawing all the
  // relation members here costs nothing and never truncates the group. (The
  // budget bounds how many vognodes EXPAND, not how big one group is.)
  let relRepos : HashMap<ID, SkgrepoName> =
    raw_outbound_members . iter ()
      . filter (|member| graph_snap . pid_of (&member . member) . is_none ())
      . filter (|member| member . relRepo != recorder_skgrepo)
      . map (|member| (member . member . clone (), member . relRepo . clone ()))
      . collect ();
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &goal_list, &relRepos, runtime ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §6.0/§16: the reconciler deletes a stale member that is a view-leaf and
  // demotes one that is a branch, so a write-protected PartnerFolder
  // drops a stale leaf member instead of preserving it.
  let summary : RepairSummary<ID> =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind, &goal_list, &child_data,
      deleted_by_this_save_extra_ids ) ?;
  if kind . policy () != FolderPolicy::EditableSet {
    // Repairs to an editable folder are not repairs: its membership IS
    // whatever the user saved. Write-protected folders warn (when there is a
    // sink, i.e. when this completion serves the just-saved view).
    if let Some (sink) = warning_sink {
      push_repair_warnings (sink, kind, &recorder_pid, summary); }}
  Ok (( )) }

/// 'reconcile_partnerFolder_children' with its goal list from the union
/// of a graph straddle: the partners in either graph, each member tagged
/// with its presence. A member whose node is only in the before-graph
/// is drawn as an unknown phantom.
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
  restore_removed_members_of_straddle_folder (tree, node, kind) ?;
  let partners : Vec<Partner> =
    straddle . partners (recorder_pid, member_role);
  let members : Vec<ID> =
    partners . iter () . map ( |partner| partner . id . clone () ) . collect ();
  let goal_list : Vec<ID> = match kind . policy () {
    FolderPolicy::WriteProtectedSet =>
      view_order_preserving_goal_list (tree, node, &members) ?,
    _ => members, };
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &goal_list, &HashMap::new (), runtime ) ?;
  let summary : RepairSummary<ID> =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind, &goal_list, &child_data,
      deleted_by_this_save_extra_ids ) ?;
  let presence_by_skgid : HashMap<ID, DiffPresence> =
    partners . iter ()
    . map ( |partner| ( partner . id . clone (), DiffPresence {
      node : straddle . node_presence (&partner . id)
             . and_then ( |presence| presence . diff_sign () ),
      rel  : partner . presence . diff_sign (), } ))
    . collect ();
  apply_presence_to_folder_members (tree, node, &presence_by_skgid) ?;
  redraw_removed_members_of_straddle_folder (tree, node, kind, straddle) ?;
  attach_continuations_under_straddle_folder_members (
    tree, node, kind, member_role, straddle, &partners, runtime ) ?;
  if kind . policy () != FolderPolicy::EditableSet {
    if let Some (sink) = warning_sink {
      push_repair_warnings (sink, kind, recorder_pid, summary); }}
  Ok (( )) }

/// Undo what 'redraw_removed_members_of_straddle_folder' did when the
/// folder was last drawn: a historical phantom becomes the unknown
/// phantom the reconciler drew it as, and a demoted member a member
/// again. The reconciler then matches them where they stand, instead
/// of drawing them a second time, and the redraw redoes the rest.
fn restore_removed_members_of_straddle_folder (
  tree   : &mut Tree<Viewnode>,
  folder : NodeId,
  kind   : PartnerFolder,
) -> Result<(), Box<dyn Error>> {
  let editable_set : bool = kind . policy () == FolderPolicy::EditableSet;
  let members : Vec<NodeId> =
    tree . get (folder) . ok_or ("restore_removed_members_of_straddle_folder: folder not found") ?
    . children () . map ( |child| child . id () ) . collect ();
  for member in members {
    let mut member_mut = tree . get_mut (member) . unwrap ();
    let restored : Option<ViewnodeKind> = match &mut member_mut . value () . kind {
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
        Some ( mk_unknown_viewnode ( h . skgid . clone () ) . kind ),
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if editable_set && t . affectsParent == AffectsParent::False
           && t . change_facts . presence . rel == Some (Sign::Minus) => {
        t . affectsParent = AffectsParent::True;
        t . change_facts . presence = DiffPresence::default ();
        None },
      _ => None, };
    if let Some (restored) = restored {
      member_mut . value () . kind = restored; }}
  Ok (( )) }

/// The members of a straddle-drawn folder whose relationship is only in
/// the before-graph: one whose node the after-graph lacks becomes a
/// historical phantom; in a folder whose membership the save reads
/// (an EditableSet), a surviving one is demoted to a write-protected
/// non-member and a dangling one is dropped, so saving the buffer
/// does not restore them.
fn redraw_removed_members_of_straddle_folder (
  tree     : &mut Tree<Viewnode>,
  folder   : NodeId,
  kind     : PartnerFolder,
  straddle : &GraphStraddle,
) -> Result<(), Box<dyn Error>> {
  let editable_set : bool = kind . policy () == FolderPolicy::EditableSet;
  let members : Vec<NodeId> =
    tree . get (folder) . ok_or ("redraw_removed_members_of_straddle_folder: folder not found") ?
    . children () . map ( |child| child . id () ) . collect ();
  for member in members {
    let (skgid, rel) : (ID, Option<Sign>) =
      match &tree . get (member) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          ( t . skgid . clone (), t . change_facts . presence . rel ),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
          ( u . skgid . clone (), u . change_facts . presence . rel ),
        _ => continue, };
    if straddle . node_presence (&skgid) == Some (Presence::OnlyBefore) {
      if let Some (historical) = mk_historical_viewnode (straddle, &skgid, rel) {
        tree . get_mut (member) . unwrap () . value () . kind = historical . kind; }
      continue; }
    if ! editable_set || rel != Some (Sign::Minus) { continue; }
    let mut member_mut = tree . get_mut (member) . unwrap ();
    match &mut member_mut . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => {
        t . affectsParent = AffectsParent::False;
        t . editability = Editability::WriteProtected; },
      _ => { member_mut . detach (); }, }}
  Ok (( )) }

/// Under each member of a straddle-drawn folder, the role-tree engine's
/// continuation over the union of the straddle: under a container, its
/// full containerward tree; under any other member, the role tree of
/// the folder's role, with a full containerward tree under each
/// graft. In a folder of the anchor's own relationships, only under
/// the members whose relationship changed. A member whose
/// relationship moved to another skgrepo gets that move. What an
/// earlier drawing hung under the members goes first, so that drawing
/// again is idempotent.
fn attach_continuations_under_straddle_folder_members (
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
  let all_members : Vec<(NodeId, &Partner)> =
    tree . get (folder) . ok_or ("attach_continuations_under_straddle_folder_members: folder not found") ?
    . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        by_id . get (&t . skgid) . map ( |partner| (child . id (), *partner) ),
      _ => None, } )
    . collect ();
  { // The previous drawing's continuations and moves.
    let stale : Vec<NodeId> =
      all_members . iter ()
      . flat_map ( |(member, _)| tree . get (*member) . unwrap () . children ()
                   . map ( |child| child . id () ) . collect::<Vec<NodeId>> () )
      . collect ();
    for treeid in stale {
      tree . get_mut (treeid) . unwrap () . detach (); }}
  let members : Vec<(NodeId, &Partner)> =
    all_members . into_iter ()
    . filter ( |(_, partner)| ! outbound || partner . changed () )
    . collect ();
  for (member, partner) in &members {
    if let Some (moved) = mk_relRepo_move_viewnode (partner) {
      tree . get_mut (*member) . unwrap () . prepend (moved); }}
  let member_treeids : Vec<NodeId> =
    members . iter () . map ( |(member, _)| *member ) . collect ();
  if kind == PartnerFolder::Container {
    attach_full_containerward_role_trees_at_treeids_with_skgrepo_set (
      tree, &member_treeids, &runtime . graph, &runtime . config, None,
      RoleTreePartners::GraphStraddle (straddle) ) ?;
  } else {
    for member in member_treeids {
      build_and_integrate_role_tree_with_skgrepo_set (
        tree, member, &runtime . graph, member_role, &runtime . config,
        None, RoleTreePartners::GraphStraddle (straddle) ) ?; }}
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
/// the folder's existing Unrestricted affectsParent=true children, in their
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
) -> Result<Vec<ID>, Box<dyn Error>> {
  let member_set : HashSet<&ID> =
    graph_members . iter () . collect ();
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut goal : Vec<ID> = Vec::new ();
  { let folder_ref : NodeRef<Viewnode> =
      tree . get (folder)
      . ok_or ("view_order_preserving_goal_list: folder not found") ?;
    for child in folder_ref . children () {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        = & child . value () . kind
      { if t . affectsParent == AffectsParent::True
          && member_set . contains (&t . skgid)
          && seen . insert (t . skgid . clone ())
        { goal . push (t . skgid . clone ()); }}}}
  for member in graph_members {
    if seen . insert (member . clone ()) {
      goal . push (member . clone ()); }}
  Ok (goal) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../../tests/unit/reconcile_partner_folder.rs"]
mod tests;
