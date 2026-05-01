use crate::types::env::SkgEnv;
use crate::to_org::complete::sharing::child_data::{ChildData, build_child_data, reconcile_sharing_scaffold_children};
use crate::to_org::complete::sharing::goal_list::goal_list_for_hiddeninsubscribee_col;
use crate::to_org::complete::sharing::kind::SharingScaffoldKind;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::types::views_state::nodecomplete_from_inrustgraph_or_disk;
use crate::types::nodes::complete::NodeComplete;
use crate::types::tree::generic::pid_and_source_from_ancestor;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Birth};
use crate::update_buffer::util::{detach_scaffold_if_empty, treat_certain_children};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

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
pub fn complete_hiddeninsubscribee_col (
  node                           : NodeId,
  tree                           : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  env                            : &SkgEnv,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let kind : SharingScaffoldKind =
    SharingScaffoldKind::HiddenInSubscribeeCol;
  kind . error_unless_node_is_this_kind (tree, node) ?;
  let (subscribee_pid, subscribee_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, 1, kind . caller_label () ) ?;
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, node, kind . correct_subscriber_ancestor_distance (),
      kind . caller_label () ) ?;
  let subscribee_contains : Vec<ID> = {
    let subscribee_nodecomplete : NodeComplete =
      nodecomplete_from_inrustgraph_or_disk (
        &env . config, &subscribee_pid, &subscribee_source ) ?;
    subscribee_nodecomplete . contains . clone() };
  let subscriber_hides : Vec<ID> = {
    let subscriber_nodecomplete : NodeComplete =
      nodecomplete_from_inrustgraph_or_disk (
        &env . config, &subscriber_pid, &subscriber_source ) ?;
    subscriber_nodecomplete . hides_from_its_subscriptions
      . or_default() . to_vec() };
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_hiddeninsubscribee_col(
      &subscribee_pid, &subscribee_source,
      &subscriber_pid, &subscriber_source,
      &subscribee_contains, &subscriber_hides,
      source_diffs, &env . config );
  let child_data : HashMap<ID, ChildData> = // Pre-compute this, so that the create_child closure inside reconcile_sharing_scaffold_children captures only owned data and does not conflict with the &mut Tree borrow.
    build_child_data(
      tree, node, &subscribee_pid, &subscribee_source,
      &goal_list, &removed_ids,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  reconcile_sharing_scaffold_children(
    tree, node, kind,
    &goal_list, &child_data ) ?;
  { // Change birth of erroneous content children to Independent. "Erroneous content" are TrueNode children marked birth=ContentOf that are not part of its content.
    let subscribee_contains_set : HashSet<ID> =
      subscribee_contains . iter() . cloned() . collect();
    treat_certain_children(
      tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) =>
          ! t . parent_ignores_it()
          && ! subscribee_contains_set . contains( &t . id )
          && ! t . is_phantom(),
        _ => false },
      |vn : &mut ViewNode| {
        if let ViewNodeKind::True( ref mut t ) = vn . kind {
          t . birth = Birth::Independent; } },
    ) . map_err( |e| -> Box<dyn Error> { e . into() } ) ?; }
  detach_scaffold_if_empty (tree, node) ?;
  Ok(( )) }
