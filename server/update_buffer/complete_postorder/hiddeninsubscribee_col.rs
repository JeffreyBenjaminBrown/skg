use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_scaffold_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_hiddeninsubscribee_col;
use crate::to_org::complete::sharing::kind::SharingScaffoldKind;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::types::tree::generic::pid_and_source_from_ancestor;
use crate::types::viewnode::ViewNode;
use crate::update_buffer::util::detach_scaffold_if_empty;

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

/// HiddenInSubscribeeCol completion (child-first / postorder pass).
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
  let kind : SharingScaffoldKind =
    SharingScaffoldKind::HiddenInSubscribeeCol;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : HiddenInContext =
    read_hiddenin_context (tree, node, kind, env) ?;
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    compute_hiddenin_goal (&context, source_diffs, env);
  let child_data : HashMap<ID, ChildData> =
    build_hiddenin_child_data (
      tree, node, &context,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  reconcile_sharing_scaffold_children(
    // PITFALL: ('reconcile_hiddenoutside_subscribee_col_children' has a similar note.) Unlike other reconciliation steps, in a HiddenInSubscribeeCol we do not call mark_managed_children_outside_goal_independent before reconciliation. If a user moves a node into a subscribee-as-such, 'unhiding' it from the corresponding subscriber, that would cause one of the children of the HiddenInSubscribeeCol to become stale (because it is no longer hidden). We do not want to mark it independent; we actually want to remove it from the HiddenInSubscribeeCol during reconciliation.
    tree, node, kind,
    &goal_list, &child_data ) ?;
  detach_scaffold_if_empty (tree, node) ?;
  Ok(( )) }

fn read_hiddenin_context (
  tree : &Tree<ViewNode>,
  node : NodeId,
  kind : SharingScaffoldKind,
  env  : &SkgEnv,
) -> Result<HiddenInContext, Box<dyn Error>> {
  let (subscribee_pid, subscribee_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, 1, kind . caller_label () ) ?;
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, kind . correct_subscriber_ancestor_distance (),
      kind . caller_label () ) ?;
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

fn compute_hiddenin_goal (
  context      : &HiddenInContext,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  env          : &SkgEnv,
) -> (Vec<ID>, HashSet<ID>) {
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_hiddeninsubscribee_col(
      &context . subscribee_pid, &context . subscribee_source,
      &context . subscriber_pid, &context . subscriber_source,
      &context . subscribee_contains, &context . subscriber_hides,
      source_diffs, &env . config );
  (goal_list, removed_ids) }

fn build_hiddenin_child_data (
  tree                           : &Tree<ViewNode>,
  node                           : NodeId,
  context                        : &HiddenInContext,
  goal_list                      : &[ID],
  removed_ids                    : &HashSet<ID>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  env                            : &SkgEnv,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  // Build child data after the read phase and before mutation, so the
  // completion steps stay auditable as read, compute, then reconcile.
  build_child_data(
    tree, node, &context . subscribee_pid, &context . subscribee_source,
    goal_list, removed_ids,
    source_diffs, deleted_since_head_pid_src_map, env ) }
