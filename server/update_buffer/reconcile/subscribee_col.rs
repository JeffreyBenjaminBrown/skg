use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::to_org::complete::partner_col::child_data::{ChildData, build_child_data, apply_membership_axes_to_col_members, reconcile_partnerCol_children_against_goal_list};
use crate::update_buffer::reconcile::{omit_inactive_members, retained_inactive_children};
use crate::to_org::complete::partner_col::goal_list::{goal_list_for_outbound_col, outbound_member_axes};
use crate::types::git::{ExistenceAxes, MembershipAxes, SourceDiff};
use crate::types::phantom::phantom_axes;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::viewnode_nodecomplete::{ unique_scaffold_child_of_viewnode, insert_scaffold_as_child};
use crate::update_buffer::ancestry::required_ancestor;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, PartnerCol};
use crate::types::viewnode::Vognode;
use crate::update_buffer::util::move_child_to_end;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct SubscribeeColContext {
  parent_pid             : ID,
  parent_source          : SourceName,
  parent_indefinitive    : bool,
  worktree_subscribees   : Vec<ID>,
}

/// SubscribeeCol completion. Called at this col's own visit in the level-order
/// BFS (after its Normal parent has been visited and created it).
///
/// WHAT IT DOES:
/// - Error unless it's a SubscribeeCol.
/// - Read parent's skg ID and indefinitive flag.
/// - Look up parent's subscribees.
/// - If no subscribees: transfer focus if needed, then delete.
/// - If parent is indefinitive: reconcile subscribee children.
/// - Ensure HiddenOutsideOfSubscribeeCol exists and is last.
pub async fn reconcile_subscribee_col_children (
  node                           : NodeId,
  tree                           : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  env                            : &SkgEnv,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set              : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerCol = PartnerCol::Subscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : SubscribeeColContext =
    read_subscribee_col_context (tree, node, env) ?;
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_outbound_col (
      &context . parent_pid, &context . parent_source,
      NodeRelation::Subscribes,
      source_diffs, &context . worktree_subscribees );
  let goal_list : Vec<ID> =
    // TODO/full-schema/9-2_source-set-safety.org: omit inactive
    // subscribees, except retained placeholders (InactiveNode
    // children with view-children), which are positional members.
    omit_inactive_members (
      goal_list, active_source_set,
      &retained_inactive_children (tree, node),
      |id : &ID| env . find_source (id, deleted_since_head_pid_src_map) );

  // TODO/DONE/local-view-update/plan_v2.org §3.4/§6.7 exception: an *empty* SubscribeeCol is PRESERVED, not
  // self-deleted. It is the editable interface onto the origin's outgoing
  // subscriptions; if it vanished when emptied, the user would lose the place
  // to add one back. (A SubscribeeCol is only *created* when subscribes_to is
  // non-empty -- to_org/complete/sharing/mod.rs gates on that -- so an empty one
  // here means the subscriber lost all its subscriptions, and we keep the
  // headline so the user can re-add.) Its children still reconcile to empty
  // below, and the HiddenOutsideOfSubscribeeCol is still ensured last.
  if context . parent_indefinitive || source_diffs . is_some() {
    // TODO/DONE/local-view-update/plan_v2.org §5.5: a col fills its members WHOLE and is budget-neutral -- the owning
    // subscriber already spent its budget unit when it expanded, so drawing all
    // its subscribees here costs nothing and never truncates the group.
    let axes_for_removed = // the relation this col represents
      |child : &ID, child_src : &SourceName|
      -> (ExistenceAxes, MembershipAxes) {
      phantom_axes ( child, child_src,
                     &context . parent_pid, &context . parent_source,
                     NodeRelation::Subscribes,
                     source_diffs . as_ref () ) };
    let child_data : HashMap<ID, ChildData> =
      build_child_data (
        tree, node,
        &goal_list, &removed_ids, &axes_for_removed,
        source_diffs, deleted_since_head_pid_src_map, env ) ?;
    reconcile_partnerCol_children_against_goal_list(
      tree, node, kind,
      &goal_list, &child_data ) ?;
    if source_diffs . is_some () {
      // Present members whose edge is New in some stage get that
      // stage's 'newM'; removed members are the phantoms above.
      apply_membership_axes_to_col_members (
        tree, node,
        & outbound_member_axes (
          &context . parent_pid, &context . parent_source,
          NodeRelation::Subscribes, source_diffs )) ?; }}

  ensure_hiddenoutsideofsubscribeecol_is_last (tree, node) ?;
  Ok(( )) }

fn read_subscribee_col_context (
  tree : &Tree<ViewNode>,
  node : NodeId,
  env  : &SkgEnv,
) -> Result<SubscribeeColContext, Box<dyn Error>> {
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: read the subscriber Active vognode through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table
  // (index 0 = the parent), rather than at a hard-coded generation.
  let subscriber : NodeId =
    required_ancestor (tree, node, 0) ?
    . ok_or ("reconcile_subscribee_col_children: \
              subscriber ancestor absent (generalized orphan)") ?;
  let (parent_pid, parent_source, parent_indefinitive)
    : (ID, SourceName, bool)
    = read_at_node_in_tree(
      tree, subscriber,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => Some(( t . id . clone(),
                    t . source . clone(),
                    t . is_indefinitive () )),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
    . ok_or ("reconcile_subscribee_col_children: parent is not a TrueNode") ?;
  let worktree_subscribees : Vec<ID> =
    nodecomplete_rustFirst_by_pid_and_source (
      &env . config, &parent_pid, &parent_source )
      . ok ()
      . map ( |skg| skg . subscribes_to . or_default () . to_vec () )
      . unwrap_or_default ();
  Ok (SubscribeeColContext {
    parent_pid,
    parent_source,
    parent_indefinitive,
    worktree_subscribees }) }

fn ensure_hiddenoutsideofsubscribeecol_is_last (
  tree : &mut Tree<ViewNode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let hidden_outside : Option<NodeId> =
    unique_scaffold_child_of_viewnode(
      tree, node,
      &ViewNodeKind::PartnerCol (PartnerCol::HiddenOutsideOfSubscribee) ) ?;
  match hidden_outside {
    Some (child) => { move_child_to_end( tree, node, child ) ?; },
    None => {
      insert_scaffold_as_child(
        tree, node,
        ViewNodeKind::PartnerCol (PartnerCol::HiddenOutsideOfSubscribee),
        false ) ?; }}
  Ok (( )) }
