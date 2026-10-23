//! The partner folders of the diff view: the straddle half of the
//! partner-folder reconciler ('update_buffer/reconcile/partner_folder.rs'
//! holds the live-graph half and dispatches here). A partner folder
//! over the graph straddle holds the union of its members in
//! both graphs, each tagged with its presence, and under each member
//! its member role tree, extended over the union ('docs/diff-view.org',
//! "the in-folders and out-folders").

use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::diff_view::change_viewnodes::{
  mk_historical_viewnode, mk_relRepo_move_viewnode};
use crate::diff_view::shallow_model_subtree::{
  is_the_diffViews_viewnode,
  reconcile_children_with_shallowModelSubtree};
use crate::graph_straddle::types::{GraphStraddle, Partner, Presence};
use crate::to_org::complete::partner_folder::child_data::{
  build_child_data,
  ChildData,
  FolderGoalItem,
  FolderGoalShape,
  reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds,
};
use crate::to_org::extend::role_tree::{
  RoleTreePartners, attach_full_containerward_role_trees_at_treeids_with_skgrepo_set,
  build_and_integrate_role_tree_with_skgrepo_set};
use crate::types::env::RuntimeGeneration;
use crate::types::git::NodeSign;
use crate::types::misc::ID;
use crate::types::viewnode::{DiffPresence, FolderPolicy, PartnerFolder, Viewnode, ViewnodeKind, Vognode};
use crate::update_buffer::reconcile::partner_folder::{
  folder_vid_of, push_repair_warnings, view_order_preserving_goal_list};
use crate::update_buffer::util::RepairSummary;
use crate::update_buffer::warnings::CompletionWarning;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// 'reconcile_partnerFolder_children' with its goal list from the union
/// of a graph straddle: the partners in either graph, each item tagged
/// with its presence and shaped as 'straddle_goal_item' says. A child
/// standing for an item is kept whatever it stood as before, and
/// takes the item's shape.
pub fn reconcile_partnerFolder_children_from_graph_straddle (
  node         : NodeId,
  tree         : &mut Tree<Viewnode>,
  kind         : PartnerFolder,
  straddle     : &GraphStraddle,
  viewparent_pid : &ID,
  member_role  : RelationRole,
  runtime      : &RuntimeGeneration,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  warning_sink : Option<&mut Vec<CompletionWarning>>,
) -> Result<(), Box<dyn Error>> {
  let partners : Vec<Partner> =
    straddle . partners (viewparent_pid, member_role);
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
        tree, node, &members, &straddle . after ) ?;
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
      tree, node, kind, &goal_items, &child_data, &straddle . after,
      deleted_by_this_save_extra_ids ) ?;
  attach_memberRoleTrees_under_straddle_folder_members (
    tree, node, kind, member_role, straddle, &partners, runtime ) ?;
  if kind . policy () != FolderPolicy::EditableSet {
    if let Some (sink) = warning_sink {
      push_repair_warnings (
        sink, kind, viewparent_pid,
        folder_vid_of (tree, node) . as_ref (), summary ); }}
  Ok (( )) }

/// The goal item of a partner of a folder over the straddle: a node only
/// in the before-graph is its historical phantom; a member whose
/// relationship is only in the before-graph is removed (a non-member
/// with '(presence rel-)', the one encoding of a removed relationship);
/// any other is a member. In a folder whose membership the save reads
/// (an EditableSet), a removed member that is dangling is not shown at
/// all, since an unknown phantom there would read back as a member.
fn straddle_goal_item (
  straddle     : &GraphStraddle,
  partner      : &Partner,
  editable_set : bool,
) -> Option<FolderGoalItem> {
  let node_presence : Option<Presence> = straddle . node_presence (&partner . id);
  let presence : DiffPresence = DiffPresence {
    node : node_presence . and_then ( |presence| presence . diff_sign () )
           . map (NodeSign::from), // a folder member is never '~'
    rel  : partner . presence . diff_sign (), };
  let shape : FolderGoalShape = match node_presence {
    Some (Presence::OnlyBefore) => FolderGoalShape::Historical (
      mk_historical_viewnode (straddle, &partner . id, presence . rel) ? ),
    _ if partner . presence == Presence::OnlyBefore => {
      if editable_set { node_presence ?; } // dangling
      FolderGoalShape::Removed },
    _ => FolderGoalShape::Member, };
  Some ( FolderGoalItem {
    skgid : partner . id . clone (), presence : Some (presence), shape } ) }

/// Under each member of a folder over the straddle, its member role tree,
/// extended by the role-tree engine over the union of the straddle: under
/// a container, its full containerward tree; under any other member,
/// the role tree of the folder's role, with a full containerward tree
/// under each graft. In a folder of the folder viewparent's own
/// relationships, only under the members whose relationship changed.
/// A member whose relationship moved to another skgrepo gets that
/// move. Each member role tree is built afresh in a scratch tree and
/// reconciled with what the member holds, by skgid (and the move by
/// its kind) at each level, so that a member role tree that stays
/// keeps its viewnodes, where they stand, and their fold state; what
/// the user added under the member stays where it stands.
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
    let mut model : Tree<Viewnode> = // its root stands in for the member
      Tree::new ( tree . get (member) . unwrap () . value () . clone () );
    let model_member : NodeId = model . root () . id ();
    if ! outbound || partner . changed () {
      if let Some (moved) = mk_relRepo_move_viewnode (partner) {
        model . root_mut () . prepend (moved); }
      if kind == PartnerFolder::Container {
        attach_full_containerward_role_trees_at_treeids_with_skgrepo_set (
          &mut model, &[model_member], &runtime . graph, &runtime . config, None,
          RoleTreePartners::GraphStraddle (straddle) ) ?;
      } else {
        build_and_integrate_role_tree_with_skgrepo_set (
          &mut model, model_member, &runtime . graph, member_role,
          &runtime . config, None, RoleTreePartners::GraphStraddle (straddle) ) ?; }}
    reconcile_children_with_shallowModelSubtree (
      tree, member, &model, model_member, &straddle . after,
      &|viewnode| is_the_diffViews_viewnode (viewnode, false, &straddle . after),
      &|_| true ) ?; }
  Ok (( )) }
