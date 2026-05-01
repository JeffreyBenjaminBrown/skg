use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_scaffold_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_subscribee_col;
use crate::to_org::complete::sharing::kind::SharingScaffoldKind;
use crate::types::git::SourceDiff;
use crate::types::views_state::nodecomplete_from_inrustgraph_or_disk;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::read_at_ancestor_in_tree;
use crate::types::tree::viewnode_nodecomplete::{ unique_scaffold_child, insert_scaffold_as_child};
use crate::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold};
use crate::update_buffer::util::{ detach_scaffold_transferring_focus, move_child_to_end};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

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
pub async fn complete_subscribee_col_preorder (
  node                           : NodeId,
  tree                           : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  env                            : &SkgEnv,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let kind : SharingScaffoldKind = SharingScaffoldKind::SubscribeeCol;
  kind . error_unless_node_is_this_kind (tree, node) ?;
  let (parent_skgid, parent_source, parent_indefinitive)
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
    . ok_or ("complete_subscribee_col_preorder: parent is not a TrueNode") ?;
  let worktree_subscribees : Vec<ID> =
    nodecomplete_from_inrustgraph_or_disk (
      &env . config, &parent_skgid, &parent_source )
      . ok ()
      . map ( |skg| skg . subscribes_to . or_default () . to_vec () )
      . unwrap_or_default ();
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_subscribee_col(
      &parent_skgid, &parent_source,
      source_diffs, &worktree_subscribees, &env . config );
  if goal_list . is_empty() { // delete SubscribeeCol, handling focus
    detach_scaffold_transferring_focus (tree, node) ?;
    return Ok(( )); }
  if parent_indefinitive || source_diffs . is_some() { // If the parent is indefinitive, its SubscribeeCol might not reflect the truth, so we complete its children. If source_diffs is present (diff view), even a definitive parent needs reconciliation, so that removed subscribees appear as phantoms. (If the parent is definitive and the diff view is off, though, then the parent just *defined* what its children should be, so they don't need completion.)
    let child_data : HashMap<ID, ChildData> =
      // Pre-compute child creation data so that the create_child closure inside reconcile_sharing_scaffold_children captures only owned data and does not conflict with the &mut Tree borrow.
      build_child_data(
        tree, node, &parent_skgid, &parent_source,
        &goal_list, &removed_ids,
        source_diffs, deleted_since_head_pid_src_map, env ) ?;
    reconcile_sharing_scaffold_children(
      tree, node, kind,
      &goal_list, &child_data ) ?; }
  { // Ensure HiddenOutsideOfSubscribeeCol exists and is last.
    let hidden_outside : Option<NodeId> =
      unique_scaffold_child(
        tree, node, &Scaffold::HiddenOutsideOfSubscribeeCol ) ?;
    match hidden_outside {
      Some (child) => { move_child_to_end(
                           tree, node, child ) ?; },
      None => { insert_scaffold_as_child(
                  tree, node,
                  Scaffold::HiddenOutsideOfSubscribeeCol,
                  false ) ?; }, }}
  Ok(( )) }
