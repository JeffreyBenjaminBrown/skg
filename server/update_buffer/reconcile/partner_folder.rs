use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_folder::child_data::{
  build_child_data,
  ChildData,
  apply_relationship_axes_to_folder_members,
  reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds,
};
use crate::to_org::complete::partner_folder::goal_list::{
  goal_list_for_outbound_folder,
  outbound_member_axes,
};
use crate::to_org::complete::partner_folder::inverse_scan::inverse_scan_for_inbound_folder;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::git::{NodeAxes, RelationshipAxes, Sign, SkgRepoDiff, file_node_axes_from_skgrepo_diff};
use crate::types::misc::{ID, RelPartner, SkgRepoName};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::phantom::{phantom_axes, home_from_disk};
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::util::RepairSummary;
use crate::update_buffer::warnings::{CompletionWarning, RepairKind};
use crate::types::viewnode::{FolderPolicy, AffectsParent, PartnerFolder, Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

/// Reconciles one PartnerFolder (TODO/DONE/local-view-update/plan_v2.org §19 terminology: a folder = a collecting non-vognode)
/// from a node in the viewforest with the current in-Rust graph snapshot's data
/// about that node.
/// Makes the folder's UnrestrictedVognode children marked affectsParent=true match a goal list,
/// preserving reusable children and creating missing ones,
/// then demotes stale children marked affectsParent=true to 'affectsParent=false'.
pub fn reconcile_partnerFolder_children (
  node         : NodeId, // The PartnerFolder. Its parent is an UnrestrictedVognode.
  tree         : &mut Tree<Viewnode>,
  kind         : PartnerFolder,
  skgrepo_diffs : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
  runtime      : &RuntimeGeneration,
  graph_snap   : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SkgRepoName>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  warning_sink : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  kind . error_unless_node_is_this_kind (tree, node) ?;
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: read the recorder Unrestricted vognode *through* the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table
  // (index 0 = the parent), so this can never read an ancestor the table
  // does not list, and the death-check and this read share one spec.
  let (recorder_pid, recorder_skgrepo) : (ID, SkgRepoName) =
    pid_and_skgrepo_from_required_ancestor (
      tree, node, 0, kind . caller_label () ) ?;
  let Some (member_role) = kind . relation_member_role () else {
    return Err (format!(
      "{} called for PartnerFolder {:?}, which has no relation member role",
      kind . caller_label (), kind) . into ()); };
  let recorder_role =
    member_role . opposite_role ();
  let skgrepo_resolver = |skgid : &ID| -> Option<SkgRepoName> {
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
  let inbound_scan : HashMap<ID, RelationshipAxes> =
    // Inbound folders' relationships live in the MEMBERS' files; the inverse
    // scan reads those files' diffs (Modified relation diffs,
    // Deleted before_node lists, Added after_node lists). Empty
    // outside diff mode and for outbound folders.
    if ! outbound && skgrepo_diffs . is_some () {
      inverse_scan_for_inbound_folder (
        &recorder_pid, member_role . relation, skgrepo_diffs,
        skgrepo_restriction )
    } else { HashMap::new () };
  let (goal_list, removed_skgids) : (Vec<ID>, HashSet<ID>) = {
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
    if outbound && skgrepo_diffs . is_some () {
      // Diff mode, outbound folder: the recorder's per-stage relation diff
      // interleaves members removed since HEAD (phantom positions)
      // into the worktree list. This diff-derived order supersedes
      // the WriteProtectedSet view-local order (the hiddenFolder) while diff
      // mode is on: a phantom belongs at its HEAD position, which a
      // view-local reordering cannot express.
      let (goal, removed) : (Vec<ID>, HashSet<ID>) =
        goal_list_for_outbound_folder (
          &recorder_pid, &recorder_skgrepo, member_role . relation,
          skgrepo_diffs, &graph_members );
      let goal : Vec<ID> = // phantoms can be restricted too
        omit_restricted_members (
          goal, skgrepo_restriction, skgrepo_resolver );
      (goal, removed)
    } else {
      let mut goal : Vec<ID> = match kind . policy () {
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
          graph_members, };
      let removed : HashSet<ID> = {
        // Inbound phantom tail: members the inverse scan says are
        // gone from the worktree. Inbound folders have no meaningful
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
            . map ( |(skgid, _)| skgid . clone () )
            . filter ( |skgid| ! goal_set . contains (skgid) )
            . collect () };
        tail = // phantoms of restricted members are omitted too
          omit_restricted_members (
            tail, skgrepo_restriction,
            |skgid : &ID| SkgEnv::find_skgrepo_in_generation (
              runtime, skgid, deleted_since_head_pid_src_map ));
        tail . sort_by ( |a, b| a . 0 . cmp (&b . 0) );
        let removed : HashSet<ID> =
          tail . iter () . cloned () . collect ();
        goal . extend (tail);
        removed };
      (goal, removed) }};
  let outbound_axes = // the recorder's own relation diff
    |child : &ID, child_src : &SkgRepoName|
    -> (NodeAxes, RelationshipAxes) {
    phantom_axes ( child, child_src,
                   &recorder_pid, &recorder_skgrepo,
                   member_role . relation,
                   skgrepo_diffs . as_ref () ) };
  let inbound_axes = // relationship axes from the inverse scan; node
                     // axes from the member's own file statuses
    |child : &ID, child_src : &SkgRepoName|
    -> (NodeAxes, RelationshipAxes) {
    ( file_node_axes_from_skgrepo_diff (
        skgrepo_diffs, child, child_src ),
      inbound_scan . get (child) . copied ()
        . unwrap_or ( RelationshipAxes {
            staged : None, unstaged : Some (Sign::Minus) } )) };
  let axes_for_removed // the relation this folder represents
    : &dyn Fn (&ID, &SkgRepoName) -> (NodeAxes, RelationshipAxes) =
    if outbound { &outbound_axes } else { &inbound_axes };
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a folder fills its members WHOLE and is budget-neutral -- the owning
  // vognode already spent its budget unit when it expanded, so drawing all the
  // relation members here costs nothing and never truncates the group. (The
  // budget bounds how many vognodes EXPAND, not how big one group is.)
  let relRepos : HashMap<ID, SkgRepoName> =
    raw_outbound_members . iter ()
      . filter (|member| graph_snap . pid_of (&member . member) . is_none ())
      . filter (|member| member . relRepo != recorder_skgrepo)
      . map (|member| (member . member . clone (), member . relRepo . clone ()))
      . collect ();
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node,
      &goal_list, &removed_skgids, axes_for_removed,
      skgrepo_diffs, deleted_since_head_pid_src_map,
      &relRepos, runtime ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §6.0/§16: the reconciler deletes a stale member that is a view-leaf and
  // demotes one that is a branch, so a write-protected PartnerFolder
  // drops a stale leaf member instead of preserving it.
  let summary : RepairSummary<ID> =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind, &goal_list, &child_data,
      deleted_by_this_save_extra_ids ) ?;
  if skgrepo_diffs . is_some () {
    // Present members whose relationship is New in some stage get that
    // stage's 'addedR'; removed members are the phantoms above.
    let axes_by_skgid : HashMap<ID, RelationshipAxes> =
      if outbound {
        outbound_member_axes (
          &recorder_pid, &recorder_skgrepo, member_role . relation,
          skgrepo_diffs )
      } else { inbound_scan . clone () };
    apply_relationship_axes_to_folder_members (
      tree, node, &axes_by_skgid ) ?; }
  if kind . policy () != FolderPolicy::EditableSet {
    // Repairs to an editable folder are not repairs: its membership IS
    // whatever the user saved. Write-protected folders warn (when there is a
    // sink, i.e. when this completion serves the just-saved view).
    if let Some (sink) = warning_sink {
      push_repair_warnings (sink, kind, &recorder_pid, summary); }}
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
