use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::to_org::complete::partner_col::child_data::{ChildData, apply_membership_axes_to_col_members, build_child_data, reconcile_partnerCol_children_against_goal_list};
use crate::to_org::complete::partner_col::goal_list::goal_list_for_hiddeninsubscribee_col;
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, file_existence_axes_from_source_diff};
use crate::types::misc::{ID, SourceName};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::update_buffer::reconcile::omit_inactive_members;
use crate::update_buffer::reconcile::partner_col::push_repair_warnings;
use crate::update_buffer::warnings::CompletionWarning;
use crate::types::viewnode::{ViewNode, PartnerCol};

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
///   Subscriber (ActiveNode)            <- ancestor 3
///     └─ SubscribeeCol (Scaffold)    <- ancestor 2
///          └─ Subscribee (ActiveNode)  <- ancestor 1
///               └─ HiddenInSubscribeeCol (Scaffold) <- self
///                    └─ [hidden ActiveNode children]
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
  active_source_set              : Option<&ActiveSourceSet>,
  warning_sink                   : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerCol =
    PartnerCol::HiddenInSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : HiddenInContext =
    read_hiddenin_context (tree, node, kind, env) ?;
  let (goal_list, removed_ids, member_axes)
    : (Vec<ID>, HashSet<ID>, HashMap<ID, MembershipAxes>) =
    goal_list_for_hiddeninsubscribee_col (
      &context . subscribee_pid, &context . subscribee_source,
      &context . subscriber_pid, &context . subscriber_source,
      &context . subscribee_contains, &context . subscriber_hides,
      source_diffs );
  let goal_list : Vec<ID> =
    // TODO/full-schema/9-2_source-set-safety.org: omit inactive
    // members; no retention for this filter col.
    omit_inactive_members (
      goal_list, active_source_set,
      |id : &ID| env . find_source (id, deleted_since_head_pid_src_map) );
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a col fills its members WHOLE and is budget-neutral -- the owning
  // subscribee already spent its budget unit when it expanded, so drawing all
  // the hidden members here costs nothing and never truncates the group.
  let axes_for_removed =
    // This col's membership is DERIVED (subscriber hides ∩ subscribee
    // contains), so no single relation diff is authoritative: the
    // membership signs come from the three-snapshot comparison;
    // existence from the member's own file statuses.
    |child : &ID, child_src : &SourceName|
    -> (ExistenceAxes, MembershipAxes) {
    ( file_existence_axes_from_source_diff (
        source_diffs, child, child_src ),
      member_axes . get (child) . copied ()
        . unwrap_or ( MembershipAxes {
            staged : None, unstaged : Some (Sign::Minus) } )) };
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node,
      &goal_list, &removed_ids, &axes_for_removed,
      source_diffs, deleted_since_head_pid_src_map, env ) ?;
  let summary =
    reconcile_partnerCol_children_against_goal_list(
      // TODO/DONE/local-view-update/plan_v2.org §6.0: a HiddenInSubscribeeCol child that becomes stale (e.g. the user
      // moved it into the subscribee-as-such, 'unhiding' it) is removed when it
      // is a view-leaf -- the common case for hidden members -- and demoted to
      // Independent only if it has a user subtree to preserve. The reconciler
      // applies this uniformly.
      tree, node, kind,
      &goal_list, &child_data ) ?;
  if source_diffs . is_some () {
    // Present members newly derived-in in some stage get that
    // stage's 'newM'; removed members are the phantoms above.
    apply_membership_axes_to_col_members (
      tree, node, &member_axes ) ?; }
  if let Some (sink) = warning_sink {
    push_repair_warnings (
      sink, kind, &context . subscribee_pid, summary ); }
  // TODO/DONE/local-view-update/plan_v2.org §3.4: an emptied HiddenInSubscribeeCol is removed by the single postorder
  // prune sweep (prune_self_deletable_when_empty), not self-deleted here.
  Ok(( )) }

fn read_hiddenin_context (
  tree : &Tree<ViewNode>,
  node : NodeId,
  kind : PartnerCol,
  env  : &SkgEnv,
) -> Result<HiddenInContext, Box<dyn Error>> {
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: ancestry table indices -- subscribee = index 0 (parent), subscriber =
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

