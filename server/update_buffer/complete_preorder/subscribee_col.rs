use crate::git_ops::read_repo::nodecomplete_from_git_head;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_scaffold_children};
use crate::types::git::SourceDiff;
use crate::types::list::{compute_interleaved_diff, itemlist_and_removedset_from_diff, Diff_Item};
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::tree::generic::{ error_unless_node_satisfies, read_at_ancestor_in_tree};
use crate::types::tree::viewnode_nodecomplete::{ unique_scaffold_child, insert_scaffold_as_child};
use crate::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold};
use crate::update_buffer::util::{ detach_scaffold_transferring_focus, move_child_to_end};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

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
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  _driver            : &TypeDBDriver,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
      tree, node,
      |vn : &ViewNode| matches!( &vn . kind,
                                 ViewNodeKind::Scaff(
                                   Scaffold::SubscribeeCol )),
      "complete_subscribee_col_preorder: expected SubscribeeCol"
    ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
  let (parent_skgid, parent_source, parent_indefinitive)
    : (ID, SourceName, bool)
    = read_at_ancestor_in_tree(
      tree, node, 1,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) =>
          Some(( t . id . clone(),
                 t . source . clone(),
                 t . is_indefinitive () )),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
    . ok_or ("complete_subscribee_col_preorder: parent is not a TrueNode") ?;
  let worktree_subscribees : Vec<ID> =
    nodecomplete_from_memory_or_disk (
      config, &parent_skgid, &parent_source )
      . ok ()
      . map ( |skg| skg . subscribes_to . or_default () . to_vec () )
      . unwrap_or_default ();
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    diff_aware_goal_list(
      source_diffs, &parent_source, &parent_skgid,
      config, &worktree_subscribees );
  if goal_list . is_empty() { // delete SubscribeeCol, handling focus
    detach_scaffold_transferring_focus (tree, node) ?;
    return Ok(( )); }
  if parent_indefinitive || source_diffs . is_some() { // If the parent is indefinitive, its SubscribeeCol might not reflect the truth, so we complete its children. If source_diffs is present (diff view), even a definitive parent needs reconciliation, so that removed subscribees appear as phantoms. (If the parent is definitive and the diff view is off, though, then the parent just *defined* what its children should be, so they don't need completion.)
    let child_data : HashMap<ID, ChildData> =
      // Pre-compute child creation data so that the create_child closure inside reconcile_sharing_scaffold_children captures only owned data and does not conflict with the &mut Tree borrow.
      build_child_data(
        tree, node, &parent_skgid, &parent_source,
        &goal_list, &removed_ids,
        source_diffs, deleted_since_head_pid_src_map, config ) ?;
    reconcile_sharing_scaffold_children(
      tree, node, &goal_list, &child_data,
      "complete_subscribee_col_preorder" ) ?; }
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

/// Compute the goal list and removed set for subscribee children.
/// In diff view, diffs HEAD subscribees against worktree subscribees
/// so that removed subscriptions appear as phantoms.
/// Outside diff view, returns the worktree subscribees unchanged.
fn diff_aware_goal_list (
  source_diffs          : &Option<HashMap<SourceName, SourceDiff>>,
  parent_source         : &SourceName,
  parent_skgid          : &ID,
  config                : &SkgConfig,
  worktree_subscribees  : &[ID],
) -> (Vec<ID>, HashSet<ID>) {
  let head_subscribees : Option<Vec<ID>> =
    source_diffs . as_ref()
      . and_then( |diffs| diffs . get (parent_source) )
      . filter( |sd| sd . is_git_repo )
      . and_then(
         |_| { let skg : NodeComplete =
                 nodecomplete_from_git_head(
                   parent_skgid, parent_source, config )
                 . ok() ?;
               if skg . subscribes_to . is_unspecified() { None }
               else { Some (
                 skg . subscribes_to . into_vec() ) }} );
  match head_subscribees {
    None =>
      (worktree_subscribees . to_vec(), HashSet::new()),
    Some (head) => {
      let diff : Vec<Diff_Item<ID>> =
        compute_interleaved_diff( &head, worktree_subscribees );
      itemlist_and_removedset_from_diff (&diff) } } }

