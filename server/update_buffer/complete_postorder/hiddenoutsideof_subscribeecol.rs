use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_scaffold_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_hiddenoutsideof_subscribeecol;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::types::tree::generic::{pid_and_source_from_ancestor, read_at_ancestor_in_tree};
use crate::types::viewnode::{ViewNode, ViewNodeKind, RoleCol};
use crate::update_buffer::util::detach_scaffold_if_empty;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct HiddenOutsideContext {
  subscriber_pid      : ID,
  subscriber_source   : SourceName,
  subscriber_hides    : Vec<ID>,
  subscribees         : Vec<ID>,
}

/// HiddenOutsideOfSubscribeeCol completion (child-first / postorder pass).
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
) -> Result<(), Box<dyn Error>> {
  let kind : RoleCol =
    RoleCol::HiddenOutsideOfSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  validate_hiddenoutside_parent (tree, node) ?;
  let context : HiddenOutsideContext =
    read_hiddenoutside_context (tree, node, kind, env) ?;
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    compute_hiddenoutside_goal (&context, source_diffs, env);
  let child_data : HashMap<ID, ChildData> =
    build_hiddenoutside_child_data (
      tree, node, &context,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  reconcile_sharing_scaffold_children(
    // §6.0: a stale member of this read-only col is removed when a view-leaf
    // (the common case) and demoted to Independent only if it has a user
    // subtree. Handled uniformly by the reconciler; no mark_managed pre-pass.
    tree, node, kind,
    &goal_list, &child_data ) ?;
  detach_scaffold_if_empty (tree, node) ?;
  Ok(( )) }

fn validate_hiddenoutside_parent (
  tree : &Tree<ViewNode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  read_at_ancestor_in_tree(
      tree, node, 1,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::PartnerCol (RoleCol::Subscribee)
          => Ok(( )),
        _ => Err( "reconcile_hiddenoutside_subscribee_col_children: \
                   ancestor 1 is not a SubscribeeCol" ) } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }

fn read_hiddenoutside_context (
  tree : &Tree<ViewNode>,
  node : NodeId,
  kind : RoleCol,
  env  : &SkgEnv,
) -> Result<HiddenOutsideContext, Box<dyn Error>> {
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, kind . correct_subscriber_ancestor_distance (),
      kind . caller_label () ) ?;
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

fn compute_hiddenoutside_goal (
  context      : &HiddenOutsideContext,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  env          : &SkgEnv,
) -> (Vec<ID>, HashSet<ID>) {
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_hiddenoutsideof_subscribeecol(
      &context . subscriber_pid, &context . subscriber_source,
      &context . subscriber_hides, &context . subscribees,
      source_diffs, &env . config );
  (goal_list, removed_ids) }

fn build_hiddenoutside_child_data (
  tree                           : &Tree<ViewNode>,
  node                           : NodeId,
  context                        : &HiddenOutsideContext,
  goal_list                      : &[ID],
  removed_ids                    : &HashSet<ID>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  env                            : &SkgEnv,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  // Build child data after the read phase and before mutation, so the
  // completion steps stay auditable as read, compute, then reconcile.
  build_child_data(
    tree, node, &context . subscriber_pid, &context . subscriber_source,
    goal_list, removed_ids,
    source_diffs, deleted_since_head_pid_src_map, env ) }
