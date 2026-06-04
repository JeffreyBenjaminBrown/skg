use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::sharing::child_data::{
  build_child_data,
  ChildData,
  reconcile_sharing_scaffold_children,
};
use crate::types::env::SkgEnv;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::update_buffer::complete_preorder::truenode::cap_goal_list_to_budget;
use crate::types::viewnode::{RoleCol, ViewNode};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

/// Reconciles one relation collection branch from a node in the view tree
/// with the current in-Rust graph snapshot's data about that node.
/// Makes the scaffold's TrueNode children marked parentIs=affected match a goal list,
/// preserving reusable children and creating missing ones,
/// then demotes stale children marked parentIs=affected to 'parentIs=independent'.
pub fn reconcile_relation_col_children (
  node         : NodeId, // The relation collection scaffold. Its parent is a TrueNode.
  tree         : &mut Tree<ViewNode>,
  kind         : RoleCol,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  env          : &SkgEnv,
  graph_snap   : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  node_budget  : &mut usize,
) -> Result<(), Box<dyn Error>> {
  kind . error_unless_node_is_this_kind (tree, node) ?;
  // §4: read the owner Normal vognode *through* the §3 ancestry table
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
  // §5.5/§18: a relation member is a new TrueNode, so spend the per-buffer
  // node budget on it -- a node thousands of people relate to (e.g. a famous
  // note's subscribers) must truncate here, not draw every member.
  let goal_list : Vec<ID> =
    cap_goal_list_to_budget (
      tree, node, &goal_list, &removed_ids, node_budget );
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &owner_pid, &owner_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  // §6.0/§16: no longer pre-mark non-goal members Independent. The reconciler
  // now deletes a stale member that is a view-leaf and demotes one that is a
  // branch, so a relation col (read-only from this side) correctly drops a
  // stale leaf member instead of preserving it.
  reconcile_sharing_scaffold_children (
    tree, node, kind, &goal_list, &child_data ) ?;
  Ok (( )) }
