use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::diff_view::partner_folders::reconcile_partnerFolder_children_from_graph_straddle;
use crate::diff_view::shallow_model_subtree::reconcile_children_with_shallowModelSubtree;
use crate::to_org::complete::partner_folder::child_data::skgid_a_folder_child_stands_for;
use crate::to_org::extend::containerward_chain::{
  ContainerwardChainPartners, attach_containerward_chains_under_members,
  is_drawn_chain_node};
use crate::types::env::RuntimeGeneration;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgConfig, SkgrepoName};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::phantom::home_from_disk;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::util::RepairSummary;
use crate::update_buffer::warnings::{CompletionWarning, RepairKind};
use crate::types::viewnode::{Folder, ChangeViewnode, ViewnodeId, Viewnode, ViewnodeKind};
use crate::update_buffer::completion_mode::CompletionMode;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashSet;
use std::error::Error;

/// Reconciles one PartnerFolder (a folder is a collecting non-vognode)
/// of a buffer of the diff view with the union of the mode's graph
/// straddle ('mode'; 'diff_view/partner_folders.rs'). (A content view
/// is refreshed: 'server/refresh/'.)
pub fn reconcile_partnerFolder_children (
  node         : NodeId, // The PartnerFolder. Its parent is an UnrestrictedVognode.
  tree         : &mut Tree<Viewnode>,
  kind         : &Folder,
  mode         : CompletionMode,
  runtime      : &RuntimeGeneration,
  warning_sink : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  kind . error_unless_node_is_this_kind (tree, node) ?;
  // Read the viewparent Unrestricted vognode *through* the required-ancestry table of 'update_buffer/ancestry.rs'
  // (index 0 = the parent), so this can never read an ancestor the table
  // does not list, and the death-check and this read share one spec.
  let (viewparent_pid, _viewparent_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor (
      tree, node, 0, kind . caller_label () ) ?;
  let Some (member_role) = kind . relation_member_role () else {
    return Err (format!(
      "{} called for PartnerFolder {:?}, which has no relation member role",
      kind . caller_label (), kind) . into ()); };
  let CompletionMode::DiffView { straddle, .. } = mode else {
    return Err ( "reconcile_partnerFolder_children: a content view is refreshed, not completed"
                 . into () ); };
  reconcile_partnerFolder_children_from_graph_straddle (
    node, tree, kind, straddle, &viewparent_pid, member_role, runtime,
    warning_sink ) }

/// Under 'member', an unrestricted vognode in a partner folder, make
/// what the folder's reconciler hangs there match a shallow model
/// subtree built afresh: 'other_model_children' (the diff view's
/// relRepo move), then the member's containerward chain, walked over
/// 'partners'. Matched by key at each level
/// ('reconcile_children_with_shallowModelSubtree'), every child of the
/// member but its members a candidate, whoever drew it, so a chain node
/// the user copied is a duplicate, converted by the glossary's "retain
/// as non-member or tombstone, then prune". Of the stale children, this
/// converts only what it draws, known by its shape: a chain node as the
/// walk draws it ('is_drawn_chain_node') and a relRepo move. What the
/// user added stays where it stands, and so does what the member's own
/// visit adds (its folders; in the diff view its old text and removed
/// members, which carry change facts). 'graph' builds the viewnodes and
/// 'key_graph' resolves the match keys ('match_key').
pub fn reconcile_containerward_chain_under_member (
  tree                 : &mut Tree<Viewnode>,
  member               : NodeId,
  other_model_children : Vec<Viewnode>,
  graph                : &InRustGraph,
  key_graph            : &InRustGraph,
  config               : &SkgConfig,
  skgrepo_restriction  : Option<&SkgrepoRestriction>,
  partners             : ContainerwardChainPartners,
) -> Result<(), Box<dyn Error>> {
  let mut model : Tree<Viewnode> = // its root stands in for the member
    Tree::new ( tree . get (member)
                . ok_or ("reconcile_containerward_chain_under_member: member not found") ?
                . value () . clone () );
  let model_member : NodeId = model . root () . id ();
  for child in other_model_children . into_iter () . rev () {
    model . root_mut () . prepend (child); }
  attach_containerward_chains_under_members (
    &mut model, &[model_member], graph, config, skgrepo_restriction,
    partners ) ?;
  reconcile_children_with_shallowModelSubtree (
    tree, member, &model, model_member, key_graph,
    &|node| is_drawn_chain_node (node)
      || matches! ( node . value () . kind,
                    ViewnodeKind::ChangeViewnode (ChangeViewnode::RelRepoMove { .. }) ),
    &|viewnode| ! viewnode . is_unrestrictedVognode_and_member (),
    &|_| true ) ?;
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
  kind                : &Folder,
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
         // hidden members. A tombstone child already in the folder is
         // unmanaged, so an ordinary rerender never removes it.
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
  folder     : &Folder,
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
        folder   : folder . clone (),
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

/// The effective goal list for a folder whose goal is not ordered:
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
