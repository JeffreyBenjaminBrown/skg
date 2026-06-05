use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_col_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_hiddenoutsideof_subscribeecol;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::update_buffer::util::cap_goal_list_to_budget;
use crate::types::viewnode::{ViewNode, RoleCol};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct HiddenOutsideContext {
  subscriber_pid      : ID,
  subscriber_source   : SourceName,
  subscriber_hides    : Vec<ID>,
  subscribees         : Vec<ID>,
}

/// HiddenOutsideOfSubscribeeCol completion (called at this col's own BFS visit).
///
/// Tree structure:
///   Subscriber (TrueNode)                       <- ancestor 2
///     └─ SubscribeeCol (Scaffold)               <- ancestor 1 = parent
///          ├─ Subscribee_A (TrueNode)           <- sibling
///          ├─ Subscribee_B (TrueNode)           <- sibling
///          └─ HiddenOutsideOfSubscribeeCol      <- self
///               └─ [hidden TrueNode children]
///
/// Collects nodes that the subscriber hides from its subscriptions
/// but that are NOT top-level content of any subscribee.
pub fn reconcile_hiddenoutside_subscribee_col_children (
  node                           : NodeId,
  tree                           : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  env                            : &SkgEnv,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  node_budget                    : &mut usize,
) -> Result<(), Box<dyn Error>> {
  let kind : RoleCol =
    RoleCol::HiddenOutsideOfSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  // §4: the parent-is-SubscribeeCol check is subsumed by reading the
  // subscriber through the §3 ancestry table (index 1 validates the
  // [SubscribeeCol, Normal] prefix), so a separate validation is unneeded.
  let context : HiddenOutsideContext =
    read_hiddenoutside_context (tree, node, kind, env) ?;
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_hiddenoutsideof_subscribeecol (
      &context . subscriber_pid, &context . subscriber_source,
      &context . subscriber_hides, &context . subscribees,
      source_diffs, &env . config );
  // §5.5/§18: each newly-hidden member is a new TrueNode -- spend the
  // per-buffer budget (existing children and removed-member phantoms are free).
  let goal_list : Vec<ID> =
    cap_goal_list_to_budget (
      tree, node, &goal_list, &removed_ids, node_budget );
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &context . subscriber_pid, &context . subscriber_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  reconcile_sharing_col_children(
    // §6.0: a stale member of this read-only col is removed when a view-leaf
    // (the common case) and demoted to Independent only if it has a user
    // subtree. Handled uniformly by the reconciler; no mark_managed pre-pass.
    tree, node, kind,
    &goal_list, &child_data ) ?;
  // §3.4: an emptied HiddenOutsideOfSubscribeeCol is removed by the single
  // postorder prune sweep (prune_self_deletable_when_empty), not here.
  Ok(( )) }

fn read_hiddenoutside_context (
  tree : &Tree<ViewNode>,
  node : NodeId,
  kind : RoleCol,
  env  : &SkgEnv,
) -> Result<HiddenOutsideContext, Box<dyn Error>> {
  // §4: subscriber = ancestry-table index 1 (the [SubscribeeCol, Normal] chain).
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_required_ancestor(
      tree, node, 1, kind . caller_label () ) ?;
  let wt_subscriber_nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_source (
      &env . config, &subscriber_pid, &subscriber_source ) ?;
  let wt_subscriber_hides : Vec<ID> =
    wt_subscriber_nodecomplete . hides_from_its_subscriptions
      . or_default() . to_vec();
  let wt_subscribees : Vec<ID> =
    wt_subscriber_nodecomplete . subscribes_to
      . or_default() . to_vec();
  Ok (HiddenOutsideContext {
    subscriber_pid,
    subscriber_source,
    subscriber_hides : wt_subscriber_hides,
    subscribees      : wt_subscribees }) }

