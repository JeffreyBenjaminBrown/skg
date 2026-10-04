use crate::repo_sets::ActiveRepoSet;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::to_org::complete::partner_folder::child_data::{ChildData, apply_relationship_axes_to_folder_members, build_child_data, reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds};
use crate::to_org::complete::partner_folder::goal_list::goal_list_for_hiddenInSubscribee_folder;
use crate::types::git::{NodeAxes, RelationshipAxes, Sign, RepoDiff, file_node_axes_from_repo_diff};
use crate::types::misc::{ID, RepoName};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_repo;
use crate::types::nodes::complete::NodeComplete;
use crate::update_buffer::ancestry::pid_and_repo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_inactive_members;
use crate::update_buffer::reconcile::partner_folder::push_repair_warnings;
use crate::update_buffer::util::fold_members_of_newborn_folder;
use crate::update_buffer::warnings::CompletionWarning;
use crate::types::viewnode::{ViewNode, PartnerFolder};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct HiddenInContext {
  subscriber_pid      : ID,
  subscriber_repo   : RepoName,
  subscribee_pid      : ID,
  subscribee_repo   : RepoName,
  subscribee_contains : Vec<ID>,
  subscriber_hides    : Vec<ID>,
  relRepos : HashMap<ID, RepoName>,
}

/// HiddenInSubscribeeFolder completion (called at this folder's own BFS visit).
///
/// Tree structure:
///   Subscriber (ActiveNode)            <- ancestor 3
///     └─ SubscribeeFolder (Scaffold)    <- ancestor 2
///          └─ Subscribee (ActiveNode)  <- ancestor 1
///               └─ HiddenInSubscribeeFolder (Scaffold) <- self
///                    └─ [hidden ActiveNode children]
///
/// The HiddenInSubscribeeFolder collects nodes that the subscriber
/// hides from its subscriptions AND that are top-level content
/// of the subscribee.
pub fn reconcile_hiddenInSubscribeeFolder_children (
  node                           : NodeId,
  tree                           : &mut Tree<ViewNode>,
  repo_diffs                   : &Option<HashMap<RepoName, RepoDiff>>,
  runtime                        : &RuntimeGeneration,
  deleted_since_head_pid_src_map : &HashMap<ID, RepoName>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  active_repo_set              : Option<&ActiveRepoSet>,
  warning_sink                   : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerFolder =
    PartnerFolder::HiddenInSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : HiddenInContext =
    read_hiddenin_context (tree, node, kind, runtime, active_repo_set) ?;
  let (goal_list, removed_ids, member_axes)
    : (Vec<ID>, HashSet<ID>, HashMap<ID, RelationshipAxes>) =
    goal_list_for_hiddenInSubscribee_folder (
      &runtime . graph,
      &context . subscribee_pid, &context . subscribee_repo,
      &context . subscriber_pid, &context . subscriber_repo,
      &context . subscribee_contains, &context . subscriber_hides,
      repo_diffs );
  let goal_list : Vec<ID> =
    // TODO/full-schema/9-2_repo-set-safety.org: omit inactive
    // members; no retention for this filter folder.
    omit_inactive_members (
      goal_list, active_repo_set,
      |id : &ID| SkgEnv::find_repo_in_generation (
        runtime, id, deleted_since_head_pid_src_map) );
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a folder fills its members WHOLE and is budget-neutral -- the owning
  // subscribee already spent its budget unit when it expanded, so drawing all
  // the hidden members here costs nothing and never truncates the group.
  let axes_for_removed =
    // This folder's membership is DERIVED (subscriber hides ∩ subscribee
    // contains), so no single relation diff is authoritative: the
    // relationship signs come from the three-snapshot comparison;
    // node signs from the member's own file statuses.
    |child : &ID, child_src : &RepoName|
    -> (NodeAxes, RelationshipAxes) {
    ( file_node_axes_from_repo_diff (
        repo_diffs, child, child_src ),
      member_axes . get (child) . copied ()
        . unwrap_or ( RelationshipAxes {
            staged : None, unstaged : Some (Sign::Minus) } )) };
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node,
      &goal_list, &removed_ids, &axes_for_removed,
      repo_diffs, deleted_since_head_pid_src_map,
      &context . relRepos, runtime ) ?;
  let summary =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      // TODO/DONE/local-view-update/plan_v2.org §6.0: a HiddenInSubscribeeFolder child that becomes stale (e.g. the user
      // moved it into the subscribee-as-such, 'unhiding' it) is removed when it
      // is a view-leaf -- the common case for hidden members -- and demoted to
      // Independent only if it has a user subtree to preserve. The reconciler
      // applies this uniformly.
      tree, node, kind,
      &goal_list, &child_data, deleted_by_this_save_extra_ids ) ?;
  if repo_diffs . is_some () {
    // Present members newly derived-in in some stage get that
    // stage's 'addedR'; removed members are the phantoms above.
    apply_relationship_axes_to_folder_members (
      tree, node, &member_axes ) ?; }
  if let Some (sink) = warning_sink {
    push_repair_warnings (
      sink, kind, &context . subscribee_pid, summary ); }
  fold_members_of_newborn_folder (tree, node)
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §3.4: an emptied HiddenInSubscribeeFolder is removed by the single postorder
  // prune sweep (prune_self_deletable_when_empty), not self-deleted here.
  Ok(( )) }

fn read_hiddenin_context (
  tree               : &Tree<ViewNode>,
  node               : NodeId,
  kind               : PartnerFolder,
  runtime            : &RuntimeGeneration,
  active_repo_set  : Option<&ActiveRepoSet>,
) -> Result<HiddenInContext, Box<dyn Error>> {
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: ancestry table indices -- subscribee = index 0 (parent), subscriber =
  // index 2 (the full [Normal, SubscribeeFolder, Normal] chain), read through the
  // helper so this multi-level read shares the death-check's spec.
  let (subscribee_pid, subscribee_repo) : (ID, RepoName) =
    pid_and_repo_from_required_ancestor(
      tree, node, 0, kind . caller_label () ) ?;
  let (subscriber_pid, subscriber_repo) : (ID, RepoName) =
    pid_and_repo_from_required_ancestor(
      tree, node, 2, kind . caller_label () ) ?;
  // relRepo gating (render-and-gating, 5_plan.org): these are the
  // subscribee's and subscriber's own outbound lists (contains,
  // hides_from_its_subscriptions), read here to compute a DERIVED
  // membership for a third node (the HiddenInSubscribeeFolder) -- like
  // 'content_goal_list's grandparent subtrahends. A membership
  // recorded in an inactive repo must not participate, in either
  // direction, or a private containment/hide would leak by omission
  // or by appearance.
  let repo_active = |repo : &RepoName| match active_repo_set {
    None      => true,
    Some (a)  => a . is_all () || a . contains_repo (repo) };
  let subscribee_contains : Vec<ID> = {
    let subscribee_nodecomplete : NodeComplete =
      nodecomplete_rustFirst_by_pid_and_repo (
        &runtime . graph, &runtime . config,
        &subscribee_pid, &subscribee_repo ) ?;
    subscribee_nodecomplete . contains . iter ()
      . filter ( |m| repo_active (& m . relRepo) )
      . map ( |m| m . member . clone () )
      . collect () };
  let (subscriber_hides, relRepos) : (Vec<ID>, HashMap<ID, RepoName>) = {
    let subscriber_nodecomplete : NodeComplete =
      nodecomplete_rustFirst_by_pid_and_repo (
        &runtime . graph, &runtime . config,
        &subscriber_pid, &subscriber_repo ) ?;
    let members = subscriber_nodecomplete . hides_from_its_subscriptions
      . or_default () . iter ()
      . filter ( |m| repo_active (& m . relRepo) )
      . collect::<Vec<_>> ();
    ( members . iter () . map ( |m| m . member . clone () ) . collect (),
      members . iter () . filter ( |m| m . relRepo != subscriber_repo )
        . map ( |m| (m . member . clone (), m . relRepo . clone ()) )
        . collect () ) };
  Ok (HiddenInContext {
    subscriber_pid,
    subscriber_repo,
    subscribee_pid,
    subscribee_repo,
    subscribee_contains,
    subscriber_hides,
    relRepos }) }
