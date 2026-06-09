use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::sharing::child_data::{
  build_child_data,
  ChildData,
  reconcile_sharing_col_children,
};
use crate::types::env::SkgEnv;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::types::viewnode::{RoleCol, ViewNode};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

/// Reconciles one relation col (TODO/DONE/local-view-update/plan_v2.org §19 terminology: a col = a collecting scaffold)
/// from a node in the view tree with the current in-Rust graph snapshot's data
/// about that node.
/// Makes the col's TrueNode children marked parentIs=affected match a goal list,
/// preserving reusable children and creating missing ones,
/// then demotes stale children marked parentIs=affected to 'parentIs=independent'.
pub fn reconcile_relation_col_children (
  node         : NodeId, // The relation col. Its parent is a TrueNode.
  tree         : &mut Tree<ViewNode>,
  kind         : RoleCol,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  env          : &SkgEnv,
  graph_snap   : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
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
      "{} called for non-relation collection {:?}",
      kind . caller_label (), kind) . into ()); };
  let owner_role =
    member_role . opposite_role ();
  let goal_list : Vec<ID> =
    graph_snap . other_member_pids (&owner_pid, owner_role);
  let removed_ids : HashSet<ID> = HashSet::new ();
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a col fills its members WHOLE and is budget-neutral -- the owning
  // vognode already spent its budget unit when it expanded, so drawing all the
  // relation members here costs nothing and never truncates the group. (The
  // budget bounds how many vognodes EXPAND, not how big one group is.)
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &owner_pid, &owner_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §6.0/§16: the reconciler deletes a stale member that is a view-leaf and
  // demotes one that is a branch, so a relation col (read-only from this side)
  // drops a stale leaf member instead of preserving it.
  reconcile_sharing_col_children (
    tree, node, kind, &goal_list, &child_data ) ?;
  Ok (( )) }
