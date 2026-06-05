use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_col_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_subscribee_col;
use crate::types::git::SourceDiff;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::viewnode_nodecomplete::{ unique_scaffold_child_of_viewnode, insert_scaffold_as_child};
use crate::update_buffer::ancestry::required_ancestor;
use crate::update_buffer::util::cap_goal_list_to_budget;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, RoleCol};
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
  node_budget                    : &mut usize,
) -> Result<(), Box<dyn Error>> {
  let kind : RoleCol = RoleCol::Subscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : SubscribeeColContext =
    read_subscribee_col_context (tree, node, env) ?;
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_subscribee_col (
      &context . parent_pid, &context . parent_source,
      source_diffs, &context . worktree_subscribees, &env . config );

  // §3.4/§6.7 exception: an *empty* SubscribeeCol is PRESERVED, not
  // self-deleted. It is the editable interface onto the origin's outgoing
  // subscriptions; if it vanished when emptied, the user would lose the place
  // to add one back. (A SubscribeeCol is only *created* when subscribes_to is
  // non-empty -- to_org/complete/sharing/mod.rs gates on that -- so an empty one
  // here means the subscriber lost all its subscriptions, and we keep the
  // headline so the user can re-add.) Its children still reconcile to empty
  // below, and the HiddenOutsideOfSubscribeeCol is still ensured last.
  if context . parent_indefinitive || source_diffs . is_some() {
    // §5.5/§18: each genuinely-new subscribee is a new TrueNode; spend the
    // per-buffer budget so a node with thousands of subscriptions truncates
    // here rather than drawing every one (removed-member phantoms are free).
    let goal_list : Vec<ID> =
      cap_goal_list_to_budget (
        tree, node, &goal_list, &removed_ids, node_budget );
    let child_data : HashMap<ID, ChildData> =
      build_child_data (
        tree, node, &context . parent_pid, &context . parent_source,
        &goal_list, &removed_ids,
        source_diffs, deleted_since_head_pid_src_map, env ) ?;
    reconcile_sharing_col_children(
      tree, node, kind,
      &goal_list, &child_data ) ?; }

  ensure_hiddenoutsideofsubscribeecol_is_last (tree, node) ?;
  Ok(( )) }

fn read_subscribee_col_context (
  tree : &Tree<ViewNode>,
  node : NodeId,
  env  : &SkgEnv,
) -> Result<SubscribeeColContext, Box<dyn Error>> {
  // §4: read the subscriber Normal vognode through the §3 ancestry table
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
        ViewNodeKind::Vognode (Vognode::Normal (t))
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
      &ViewNodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee) ) ?;
  match hidden_outside {
    Some (child) => { move_child_to_end( tree, node, child ) ?; },
    None => {
      insert_scaffold_as_child(
        tree, node,
        ViewNodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee),
        false ) ?; }}
  Ok (( )) }
