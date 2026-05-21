use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_scaffold_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_subscribee_col;
use crate::to_org::complete::sharing::kind::SharingScaffoldKind;
use crate::types::git::SourceDiff;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::read_at_ancestor_in_tree;
use crate::types::tree::viewnode_nodecomplete::{ unique_scaffold_child_of_viewnode, insert_scaffold_as_child};
use crate::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold};
use crate::update_buffer::util::{ detach_scaffold_transferring_focus, move_child_to_end};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct SubscribeeColContext {
  parent_pid             : ID,
  parent_source          : SourceName,
  parent_indefinitive    : bool,
  worktree_subscribees   : Vec<ID>,
}

/// SubscribeeCol completion.
///
/// INTENDED USE: Use in the first, DFS preorder (parent-first)
/// buffer-update pass through the tree.
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
) -> Result<(), Box<dyn Error>> {
  let kind : SharingScaffoldKind = SharingScaffoldKind::SubscribeeCol;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : SubscribeeColContext =
    read_subscribee_col_context (tree, node, kind, env) ?;
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    compute_subscribee_col_goal (
      &context, source_diffs, env );

  if goal_list . is_empty() {
    detach_scaffold_transferring_focus (tree, node) ?;
    return Ok(( )); }

  if context . parent_indefinitive || source_diffs . is_some() {
    let child_data : HashMap<ID, ChildData> =
      build_subscribee_col_child_data (
        tree, node, &context,
        &goal_list, &removed_ids,
        source_diffs, deleted_since_head_pid_src_map, env ) ?;
    reconcile_sharing_scaffold_children(
      tree, node, kind,
      &goal_list, &child_data ) ?; }

  ensure_hiddenoutsideofsubscribeecol_is_last (tree, node) ?;
  Ok(( )) }

fn read_subscribee_col_context (
  tree : &Tree<ViewNode>,
  node : NodeId,
  kind : SharingScaffoldKind,
  env  : &SkgEnv,
) -> Result<SubscribeeColContext, Box<dyn Error>> {
  let (parent_pid, parent_source, parent_indefinitive)
    : (ID, SourceName, bool)
    = read_at_ancestor_in_tree(
      tree, node, kind . correct_subscriber_ancestor_distance (),
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) =>
          Some(( t . id . clone(),
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

fn compute_subscribee_col_goal (
  context      : &SubscribeeColContext,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  env          : &SkgEnv,
) -> (Vec<ID>, HashSet<ID>) {
  goal_list_for_subscribee_col(
    &context . parent_pid, &context . parent_source,
    source_diffs, &context . worktree_subscribees, &env . config ) }

fn build_subscribee_col_child_data (
  tree                           : &Tree<ViewNode>,
  node                           : NodeId,
  context                        : &SubscribeeColContext,
  goal_list                      : &[ID],
  removed_ids                    : &HashSet<ID>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  env                            : &SkgEnv,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  // Build child data after the read phase and before mutation, so the
  // completion steps stay auditable as read, compute, then reconcile.
  build_child_data(
    tree, node, &context . parent_pid, &context . parent_source,
    goal_list, removed_ids,
    source_diffs, deleted_since_head_pid_src_map, env ) }

fn ensure_hiddenoutsideofsubscribeecol_is_last (
  tree : &mut Tree<ViewNode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let hidden_outside : Option<NodeId> =
    unique_scaffold_child_of_viewnode(
      tree, node, &Scaffold::HiddenOutsideOfSubscribeeCol ) ?;
  match hidden_outside {
    Some (child) => { move_child_to_end(
                         tree, node, child ) ?; },
    None => { insert_scaffold_as_child(
                tree, node,
                Scaffold::HiddenOutsideOfSubscribeeCol,
                false ) ?; }, }
  Ok (( )) }
