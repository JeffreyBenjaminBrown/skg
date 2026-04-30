use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_scaffold_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_hiddenoutsideof_subscribeecol;
use crate::to_org::complete::sharing::kind::SharingScaffoldKind;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::types::nodes::complete::NodeComplete;
use crate::types::tree::generic::{error_unless_node_satisfies, pid_and_source_from_ancestor, read_at_ancestor_in_tree};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, Birth};
use crate::update_buffer::util::{detach_scaffold_if_empty, treat_certain_children};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

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
pub fn complete_hiddenoutsideofsubscribeecol (
  node                           : NodeId,
  tree                           : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  env                            : &SkgEnv,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  { // verify node and parent are the right kinds
    error_unless_node_satisfies(
        tree, node,
        |vn : &ViewNode| matches!(
          &vn . kind,
          ViewNodeKind::Scaff(
            Scaffold::HiddenOutsideOfSubscribeeCol )),
        "complete_hiddenoutsideofsubscribeecol: \
         expected HiddenOutsideOfSubscribeeCol"
      ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    read_at_ancestor_in_tree(
        tree, node, 1,
        |vn : &ViewNode| match &vn . kind {
          ViewNodeKind::Scaff (Scaffold::SubscribeeCol) => Ok(( )),
          _ => Err( "complete_hiddenoutsideofsubscribeecol: \
                     ancestor 1 is not a SubscribeeCol" ) } )
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?; }
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, 2,
      "complete_hiddenoutsideofsubscribeecol" ) ?;
  let wt_subscriber_nodecomplete : NodeComplete =
    nodecomplete_from_memory_or_disk (
      &env . config, &subscriber_pid, &subscriber_source ) ?;
  let wt_subscriber_hides : Vec<ID> =
    wt_subscriber_nodecomplete . hides_from_its_subscriptions
      . or_default() . to_vec();
  let wt_subscribees : Vec<ID> =
    wt_subscriber_nodecomplete . subscribes_to
      . or_default() . to_vec();
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_hiddenoutsideof_subscribeecol(
      &subscriber_pid, &subscriber_source,
      &wt_subscriber_hides, &wt_subscribees,
      source_diffs, &env . config );
  let child_data : HashMap<ID, ChildData> = // Pre-compute this, so that the create_child closure inside reconcile_sharing_scaffold_children captures only owned data and does not conflict with the &mut Tree borrow.
    build_child_data(
      tree, node, &subscriber_pid, &subscriber_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  reconcile_sharing_scaffold_children(
    tree, node, SharingScaffoldKind::HiddenOutsideOfSubscribeeCol,
    &goal_list, &child_data ) ?;
  { // Mark erroneous content children birth=Independent. A child is erroneous if it is a non-phantom TrueNode marked birth=Content but not in the goal_list.
    let goal_set : HashSet<ID> =
      goal_list . iter() . cloned() . collect();
    treat_certain_children(
      tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) =>
          !t . parent_ignores_it()
          && !goal_set . contains( &t . id )
          && !t . is_phantom(),
        _ => false },
      |vn : &mut ViewNode| {
        if let ViewNodeKind::True( ref mut t ) = vn . kind {
          t . birth = Birth::Independent; } },
    ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?; }
  detach_scaffold_if_empty (tree, node) ?;
  Ok(( )) }

