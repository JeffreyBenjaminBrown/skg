use crate::source_sets::ActiveSourceSet;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::to_org::complete::partner_folder::child_data::{ChildData, apply_membership_axes_to_folder_members, build_child_data, reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds};
use crate::to_org::complete::partner_folder::goal_list::goal_list_for_hiddenOutsideOfSubscribee_folder;
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff, file_existence_axes_from_source_diff};
use crate::types::misc::{ID, SourceName};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::update_buffer::reconcile::omit_inactive_members;
use crate::update_buffer::reconcile::partner_folder::push_repair_warnings;
use crate::update_buffer::util::fold_members_of_newborn_folder;
use crate::update_buffer::warnings::CompletionWarning;
use crate::types::viewnode::{ViewNode, PartnerFolder};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct HiddenOutsideContext {
  subscriber_pid      : ID,
  subscriber_source   : SourceName,
  subscriber_hides    : Vec<ID>,
  relSources : HashMap<ID, SourceName>,
  subscribees         : Vec<ID>,
}

/// HiddenOutsideOfSubscribeeFolder completion (called at this folder's own BFS visit).
///
/// Tree structure:
///   Subscriber (ActiveNode)                       <- ancestor 2
///     └─ SubscribeeFolder (Scaffold)               <- ancestor 1 = parent
///          ├─ Subscribee_A (ActiveNode)           <- sibling
///          ├─ Subscribee_B (ActiveNode)           <- sibling
///          └─ HiddenOutsideOfSubscribeeFolder      <- self
///               └─ [hidden ActiveNode children]
///
/// Collects nodes that the subscriber hides from its subscriptions
/// but that are NOT top-level content of any subscribee.
pub fn reconcile_hiddenoutsideSubscribeeFolder_children (
  node                           : NodeId,
  tree                           : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  runtime                        : &RuntimeGeneration,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  active_source_set              : Option<&ActiveSourceSet>,
  warning_sink                   : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerFolder =
    PartnerFolder::HiddenOutsideOfSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: the parent-is-SubscribeeFolder check is subsumed by reading the
  // subscriber through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table (index 1 validates the
  // [SubscribeeFolder, Normal] prefix), so a separate validation is unneeded.
  let context : HiddenOutsideContext =
    read_hiddenoutside_context (
      tree, node, kind, runtime, active_source_set) ?;
  let (goal_list, removed_ids, member_axes)
    : (Vec<ID>, HashSet<ID>, HashMap<ID, MembershipAxes>) =
    goal_list_for_hiddenOutsideOfSubscribee_folder (
      &runtime . graph,
      &context . subscriber_pid, &context . subscriber_source,
      &context . subscriber_hides, &context . subscribees,
      source_diffs, &runtime . config );
  let goal_list : Vec<ID> =
    // TODO/full-schema/9-2_source-set-safety.org: omit inactive
    // members; no retention for this filter folder.
    omit_inactive_members (
      goal_list, active_source_set,
      |id : &ID| SkgEnv::find_source_in_generation (
        runtime, id, deleted_since_head_pid_src_map) );
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a folder fills its members WHOLE and is budget-neutral -- the owning
  // subscriber already spent its budget unit when it expanded, so drawing all
  // these hidden members here costs nothing and never truncates the group.
  let axes_for_removed =
    // This folder's membership is DERIVED (subscriber hides minus
    // subscribee content), so no single relation diff is
    // authoritative: the membership signs come from the
    // three-snapshot comparison; existence from the member's own
    // file statuses.
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
      source_diffs, deleted_since_head_pid_src_map,
      &context . relSources, runtime ) ?;
  let summary =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      // TODO/DONE/local-view-update/plan_v2.org §6.0: a stale member of this read-only folder is removed when a view-leaf
      // (the common case) and demoted to Independent only if it has a user
      // subtree. Handled uniformly by the reconciler.
      tree, node, kind,
      &goal_list, &child_data, deleted_by_this_save_extra_ids ) ?;
  if source_diffs . is_some () {
    // Present members newly derived-in in some stage get that
    // stage's 'newM'; removed members are the phantoms above.
    apply_membership_axes_to_folder_members (
      tree, node, &member_axes ) ?; }
  if let Some (sink) = warning_sink {
    push_repair_warnings (
      sink, kind, &context . subscriber_pid, summary ); }
  fold_members_of_newborn_folder (tree, node)
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §3.4: an emptied HiddenOutsideOfSubscribeeFolder is removed by the single
  // postorder prune sweep (prune_self_deletable_when_empty), not here.
  Ok(( )) }

fn read_hiddenoutside_context (
  tree               : &Tree<ViewNode>,
  node               : NodeId,
  kind               : PartnerFolder,
  runtime            : &RuntimeGeneration,
  active_source_set  : Option<&ActiveSourceSet>,
) -> Result<HiddenOutsideContext, Box<dyn Error>> {
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: subscriber = ancestry-table index 1 (the [SubscribeeFolder, Normal] chain).
  let (subscriber_pid, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_required_ancestor(
      tree, node, 1, kind . caller_label () ) ?;
  let wt_subscriber_nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_source (
      &runtime . graph, &runtime . config,
      &subscriber_pid, &subscriber_source ) ?;
  // relSource gating (render-and-gating, 5_plan.org): both are the
  // subscriber's own outbound lists (hides_from_its_subscriptions,
  // subscribes_to); a membership recorded in an inactive source must
  // not feed this derived folder.
  let source_active = |source : &SourceName| match active_source_set {
    None      => true,
    Some (a)  => a . is_all () || a . contains_source (source) };
  let wt_subscriber_hide_members =
    wt_subscriber_nodecomplete . hides_from_its_subscriptions
    . or_default () . iter ()
    . filter ( |m| source_active (& m . relSource) )
    . collect::<Vec<_>> ();
  let wt_subscriber_hides : Vec<ID> = wt_subscriber_hide_members . iter ()
    . map ( |m| m . member . clone () ) . collect ();
  let relSources : HashMap<ID, SourceName> =
    wt_subscriber_hide_members . iter ()
    . filter ( |m| m . relSource != subscriber_source )
    . map ( |m| (m . member . clone (), m . relSource . clone ()) )
    . collect ();
  let wt_subscribees : Vec<ID> =
    wt_subscriber_nodecomplete . subscribes_to
    . or_default () . iter ()
    . filter ( |m| source_active (& m . relSource) )
    . map ( |m| m . member . clone () )
    . collect ();
  Ok (HiddenOutsideContext {
    subscriber_pid,
    subscriber_source,
    subscriber_hides : wt_subscriber_hides,
    relSources,
    subscribees      : wt_subscribees }) }
