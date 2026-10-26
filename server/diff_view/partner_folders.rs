//! The partner folders of the diff view: the straddle half of the
//! partner-folder reconciler ('update_buffer/reconcile/partner_folder.rs'
//! holds the live-graph half and dispatches here). A partner folder
//! over the graph straddle holds the union of its members in
//! both graphs, each tagged with its change, and under each member
//! the containerward chain, walked over the union
//! ('docs/diff-view.org', "the in-folders and out-folders").

use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::diff_view::change_viewnodes::{
  mk_historical_viewnode, mk_relRepo_move_viewnode};
use crate::graph_straddle::types::{GraphStraddle, Partner};
use crate::to_org::complete::partner_folder::child_data::{
  build_child_data,
  ChildData,
  FolderGoalItem,
  FolderGoalShape,
  reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds,
};
use crate::to_org::extend::containerward_chain::ContainerwardChainPartners;
use crate::types::env::RuntimeGeneration;
use crate::types::misc::ID;
use crate::types::viewnode::{
  DiffChange, Existence, Folder, Viewnode,
  ViewnodeKind, Vognode};
use crate::update_buffer::reconcile::partner_folder::{
  folder_viewnode_id_of, push_repair_warnings,
  reconcile_containerward_chain_under_member, view_order_preserving_goal_list};
use crate::update_buffer::util::RepairSummary;
use crate::update_buffer::warnings::CompletionWarning;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// 'reconcile_partnerFolder_children' with its goal list from the union
/// of a graph straddle: the partners in either graph, each item tagged
/// with its change and shaped as 'straddle_goal_item' says. A child
/// standing for an item is kept whatever it stood as before, and
/// takes the item's shape.
pub fn reconcile_partnerFolder_children_from_graph_straddle (
  node         : NodeId,
  tree         : &mut Tree<Viewnode>,
  kind         : &Folder,
  straddle     : &GraphStraddle,
  viewparent_pid : &ID,
  member_role  : RelationRole,
  runtime      : &RuntimeGeneration,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  warning_sink : Option<&mut Vec<CompletionWarning>>,
) -> Result<(), Box<dyn Error>> {
  let partners : Vec<Partner> =
    straddle . partners (viewparent_pid, member_role);
  let editable_set : bool = // the save reads the membership
    kind . goal_is_ordered ();
  let items : Vec<FolderGoalItem> =
    partners . iter ()
    . filter_map ( |partner| straddle_goal_item (straddle, partner, editable_set) )
    . collect ();
  let goal_items : Vec<FolderGoalItem> = match kind . goal_is_ordered () {
    false => {
      let members : Vec<ID> =
        items . iter () . map ( |item| item . skgid . clone () ) . collect ();
      let ordered : Vec<ID> = view_order_preserving_goal_list (
        tree, node, &members, &straddle . after ) ?;
      let by_skgid : HashMap<&ID, &FolderGoalItem> =
        items . iter () . map ( |item| (&item . skgid, item) ) . collect ();
      ordered . iter ()
        . filter_map ( |skgid| by_skgid . get (skgid) . map ( |item| (*item) . clone () ))
        . collect () },
    true => items, };
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &goal_items, &HashMap::new (), runtime ) ?;
  let summary : RepairSummary<ID> =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind, &goal_items, &child_data, &straddle . after,
      deleted_by_this_save_extra_ids ) ?;
  attach_chains_under_straddle_folder_members (
    tree, node, kind, member_role, straddle, &partners, runtime ) ?;
  if ! kind . goal_is_ordered () {
    if let Some (sink) = warning_sink {
      push_repair_warnings (
        sink, kind, viewparent_pid,
        folder_viewnode_id_of (tree, node) . as_ref (), summary ); }}
  Ok (( )) }

/// The goal item of a partner of a folder over the straddle: a node only
/// in the before-graph is its historical phantom; a member whose
/// relationship is only in the before-graph is removed (a non-member
/// with '(rel removed)', the one encoding of a removed relationship);
/// any other is a member. In a folder whose membership the save reads
/// (one whose goal is ordered), a removed member that is dangling is not shown at
/// all, since an unknown phantom there would read back as a member.
fn straddle_goal_item (
  straddle     : &GraphStraddle,
  partner      : &Partner,
  editable_set : bool,
) -> Option<FolderGoalItem> {
  let change : DiffChange = // as a content member's
    straddle . node_change ( &partner . id, partner . existence );
  let shape : FolderGoalShape =
    if change . node == Some (Existence::Removed) {
      FolderGoalShape::Historical (
        mk_historical_viewnode (straddle, &partner . id, change . rel) ? )
    } else if partner . existence == Some (Existence::Removed) {
      if editable_set && ! straddle . node_in_either_graph (&partner . id) {
        return None; } // dangling
      FolderGoalShape::Removed
    } else { FolderGoalShape::Member };
  Some ( FolderGoalItem {
    skgid : partner . id . clone (), change : Some (change), shape } ) }

/// Under each member of a folder over the straddle, its containerward
/// chain, walked over the union of the straddle, and, where the
/// member's relationship moved to another skgrepo, that move
/// ('reconcile_containerward_chain_under_member'). In a folder of the
/// folder viewparent's own relationships, the move is shown only under
/// the members whose relationship changed. A chain node carries no
/// change facts. Of the folder's members, a removed member carries its
/// chain too; a historical phantom and an unknown one carry nothing.
fn attach_chains_under_straddle_folder_members (
  tree        : &mut Tree<Viewnode>,
  folder      : NodeId,
  kind        : &Folder,
  member_role : RelationRole,
  straddle    : &GraphStraddle,
  partners    : &[Partner],
  runtime     : &RuntimeGeneration,
) -> Result<(), Box<dyn Error>> {
  if ! kind . members_carry_containerward_chain () { return Ok (( )); }
  let by_id : HashMap<&ID, &Partner> =
    partners . iter () . map ( |partner| (&partner . id, partner) ) . collect ();
  let outbound : bool = ! member_role . is_first_role ();
  let members : Vec<(NodeId, &Partner)> =
    tree . get (folder) . ok_or ("attach_chains_under_straddle_folder_members: folder not found") ?
    . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        by_id . get (&t . skgid) . map ( |partner| (child . id (), *partner) ),
      _ => None, } )
    . collect ();
  for (member, partner) in members {
    let moves : Vec<Viewnode> =
      if ! outbound || partner . changed () {
        mk_relRepo_move_viewnode (partner) . into_iter () . collect ()
      } else { Vec::new () };
    reconcile_containerward_chain_under_member (
      tree, member, moves, &runtime . graph, &straddle . after,
      &runtime . config, None,
      ContainerwardChainPartners::GraphStraddle (straddle) ) ?; }
  Ok (( )) }
