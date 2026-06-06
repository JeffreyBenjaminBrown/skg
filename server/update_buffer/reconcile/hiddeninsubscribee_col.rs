use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_col_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_hiddeninsubscribee_col;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::types::viewnode::{ViewNode, RoleCol};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct HiddenInContext {
  subscriber_pid      : ID,
  subscriber_source   : SourceName,
  subscribee_pid      : ID,
  subscribee_source   : SourceName,
  subscribee_contains : Vec<ID>,
  subscriber_hides    : Vec<ID>,
}

/// HiddenInSubscribeeCol completion (called at this col's own BFS visit).
///
/// Tree structure:
///   Subscriber (TrueNode)            <- ancestor 3
///     └─ SubscribeeCol (Scaffold)    <- ancestor 2
///          └─ Subscribee (TrueNode)  <- ancestor 1
///               └─ HiddenInSubscribeeCol (Scaffold) <- self
///                    └─ [hidden TrueNode children]
///
/// The HiddenInSubscribeeCol collects nodes that the subscriber
/// hides from its subscriptions AND that are top-level content
/// of the subscribee.
pub fn reconcile_hiddenin_subscribee_col_children (
  node                           : NodeId,
  tree                           : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  env                            : &SkgEnv,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let kind : RoleCol =
    RoleCol::HiddenInSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : HiddenInContext =
    read_hiddenin_context (tree, node, kind, env) ?;
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_hiddeninsubscribee_col (
      &context . subscribee_pid, &context . subscribee_source,
      &context . subscriber_pid, &context . subscriber_source,
      &context . subscribee_contains, &context . subscriber_hides,
      source_diffs, &env . config );
  // §5.5: a col fills its members WHOLE and is budget-neutral -- the owning
  // subscribee already spent its budget unit when it expanded, so drawing all
  // the hidden members here costs nothing and never truncates the group.
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &context . subscribee_pid, &context . subscribee_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  reconcile_sharing_col_children(
    // §6.0: a HiddenInSubscribeeCol child that becomes stale (e.g. the user
    // moved it into the subscribee-as-such, 'unhiding' it) is removed when it
    // is a view-leaf -- the common case for hidden members -- and demoted to
    // Independent only if it has a user subtree to preserve. The reconciler
    // applies this uniformly.
    tree, node, kind,
    &goal_list, &child_data ) ?;
  // §3.4: an emptied HiddenInSubscribeeCol is removed by the single postorder
  // prune sweep (prune_self_deletable_when_empty), not self-deleted here.
  Ok(( )) }

fn read_hiddenin_context (
  tree : &Tree<ViewNode>,
  node : NodeId,
  kind : RoleCol,
  env  : &SkgEnv,
) -> Result<HiddenInContext, Box<dyn Error>> {
  // §4: ancestry table indices -- subscribee = index 0 (parent), subscriber =
  // index 2 (the full [Normal, SubscribeeCol, Normal] chain), read through the
  // helper so this multi-level read shares the death-check's spec.
  let (subscribee_pid, subscribee_source) : (ID, SourceName) =
    pid_and_source_from_required_ancestor(
      tree, node, 0, kind . caller_label () ) ?;
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_required_ancestor(
      tree, node, 2, kind . caller_label () ) ?;
  let subscribee_contains : Vec<ID> = {
    let subscribee_nodecomplete : NodeComplete =
      nodecomplete_rustFirst_by_pid_and_source (
        &env . config, &subscribee_pid, &subscribee_source ) ?;
    subscribee_nodecomplete . contains . clone() };
  let subscriber_hides : Vec<ID> = {
    let subscriber_nodecomplete : NodeComplete =
      nodecomplete_rustFirst_by_pid_and_source (
        &env . config, &subscriber_pid, &subscriber_source ) ?;
    subscriber_nodecomplete . hides_from_its_subscriptions
      . or_default() . to_vec() };
  Ok (HiddenInContext {
    subscriber_pid,
    subscriber_source,
    subscribee_pid,
    subscribee_source,
    subscribee_contains,
    subscriber_hides }) }

