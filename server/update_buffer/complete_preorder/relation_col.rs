use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::sharing::child_data::{
  build_child_data,
  ChildData,
  reconcile_sharing_scaffold_children,
};
use crate::types::viewnode::RoleCol;
use crate::types::env::SkgEnv;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::pid_and_source_from_ancestor;
use crate::types::viewnode::{ParentIs, ViewNode};
use crate::update_buffer::util::mark_managed_children_outside_goal_independent;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

/// Reconciles one relation collection branch from a node in the view tree
/// with the current in-Rust graph snapshot's data about that node.
/// Makes the scaffold's TrueNode children marked parentIs=collector match a goal list,
/// preserving reusable children and creating missing ones,
/// then demotes stale children marked parentIs=collector to 'parentIs=independent'.
pub fn reconcile_relation_col_children (
  node         : NodeId, // The relation collection scaffold. Its parent is a TrueNode.
  tree         : &mut Tree<ViewNode>,
  kind         : RoleCol,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  env          : &SkgEnv,
  graph_snap   : &Arc<InRustGraph>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  kind . error_unless_node_is_this_kind (tree, node) ?;
  let (owner_pid, owner_source) : (ID, SourceName) =
    pid_and_source_from_ancestor (
      tree, node, 1, kind . caller_label () ) ?;
  let Some (member_role) = kind . relation_member_role () else {
    return Err (format!(
      "{} called for non-relation collection {:?}",
      kind . caller_label (), kind) . into ()); };
  let owner_role =
    member_role . opposite_role ();
  let goal_list : Vec<ID> =
    graph_snap . other_member_pids (&owner_pid, owner_role);
  let removed_ids : HashSet<ID> = HashSet::new ();
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &owner_pid, &owner_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  mark_managed_children_outside_goal_independent (
    // Do this before reconciliation -- otherwise non-goal children marked parentIs=collector are treated as relevant collection members and discarded.
    tree, node, &goal_list,
    |t| t . parentIs == ParentIs::Collector ) ?;
  reconcile_sharing_scaffold_children (
    tree, node, kind, &goal_list, &child_data ) ?;
  Ok (( )) }
