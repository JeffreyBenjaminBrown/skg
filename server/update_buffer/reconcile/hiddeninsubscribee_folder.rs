use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::to_org::complete::partner_folder::child_data::{ChildData, apply_relationship_axes_to_folder_members, build_child_data, reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds};
use crate::to_org::complete::partner_folder::goal_list::goal_list_for_hiddenInSubscribee_folder;
use crate::types::git::{NodeAxes, RelationshipAxes, Sign, SkgRepoDiff, file_node_axes_from_skgrepo_diff};
use crate::types::misc::{ID, SkgRepoName};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::nodes::complete::Graphnode;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::reconcile::partner_folder::push_repair_warnings;
use crate::update_buffer::util::fold_members_of_newborn_folder;
use crate::update_buffer::warnings::CompletionWarning;
use crate::types::viewnode::{Viewnode, PartnerFolder};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct HiddenInContext {
  subscriber_pid      : ID,
  subscriber_skgrepo  : SkgRepoName,
  subscribee_pid      : ID,
  subscribee_skgrepo  : SkgRepoName,
  subscribee_contains : Vec<ID>,
  subscriber_hides    : Vec<ID>,
  relRepos            : HashMap<ID, SkgRepoName>,
}

/// HiddenInSubscribeeFolder completion (called at this folder's own BFS visit).
///
/// Tree structure:
///   Subscriber (UnrestrictedVognode)            <- ancestor 3
///     └─ SubscribeeFolder (Non-vognode)    <- ancestor 2
///          └─ Subscribee (UnrestrictedVognode)  <- ancestor 1
///               └─ HiddenInSubscribeeFolder (Non-vognode) <- self
///                    └─ [hidden UnrestrictedVognode children]
///
/// The HiddenInSubscribeeFolder collects nodes that the subscriber
/// hides from its subscriptions AND that are top-level content
/// of the subscribee.
pub fn reconcile_hiddenInSubscribeeFolder_children (
  node                           : NodeId,
  tree                           : &mut Tree<Viewnode>,
  skgrepo_diffs                  : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
  runtime                        : &RuntimeGeneration,
  deleted_since_head_pid_src_map : &HashMap<ID, SkgRepoName>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  skgrepo_restriction            : Option<&SkgrepoRestriction>,
  warning_sink                   : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerFolder =
    PartnerFolder::HiddenInSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : HiddenInContext =
    read_hiddenin_context (tree, node, kind, runtime, skgrepo_restriction) ?;
  let (goal_list, removed_skgids, member_axes)
    : (Vec<ID>, HashSet<ID>, HashMap<ID, RelationshipAxes>) =
    goal_list_for_hiddenInSubscribee_folder (
      &runtime . graph,
      &context . subscribee_pid, &context . subscribee_skgrepo,
      &context . subscriber_pid, &context . subscriber_skgrepo,
      &context . subscribee_contains, &context . subscriber_hides,
      skgrepo_diffs );
  let goal_list : Vec<ID> =
    // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: omit restricted
    // members; no retention for this filter folder.
    omit_restricted_members (
      goal_list, skgrepo_restriction,
      |skgid : &ID| SkgEnv::find_skgrepo_in_generation (
        runtime, skgid, deleted_since_head_pid_src_map) );
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a folder fills its members WHOLE and is budget-neutral -- the owning
  // subscribee already spent its budget unit when it expanded, so drawing all
  // the hidden members here costs nothing and never truncates the group.
  let axes_for_removed =
    // This folder's membership is DERIVED (subscriber hides ∩ subscribee
    // contains), so no single relation diff is authoritative: the
    // relationship signs come from the three-snapshot comparison;
    // node signs from the member's own file statuses.
    |child : &ID, child_src : &SkgRepoName|
    -> (NodeAxes, RelationshipAxes) {
    ( file_node_axes_from_skgrepo_diff (
        skgrepo_diffs, child, child_src ),
      member_axes . get (child) . copied ()
        . unwrap_or ( RelationshipAxes {
            staged : None, unstaged : Some (Sign::Minus) } )) };
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node,
      &goal_list, &removed_skgids, &axes_for_removed,
      skgrepo_diffs, deleted_since_head_pid_src_map,
      &context . relRepos, runtime ) ?;
  let summary =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      // TODO/DONE/local-view-update/plan_v2.org §6.0: a HiddenInSubscribeeFolder child that becomes stale (e.g. the user
      // moved it into the subscribee-as-such, 'unhiding' it) is removed when it
      // is a view-leaf -- the common case for hidden members -- and demoted to
      // non-member only if it has a user subtree to preserve. The reconciler
      // applies this uniformly.
      tree, node, kind,
      &goal_list, &child_data, deleted_by_this_save_extra_ids ) ?;
  if skgrepo_diffs . is_some () {
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
  tree               : &Tree<Viewnode>,
  node               : NodeId,
  kind               : PartnerFolder,
  runtime            : &RuntimeGeneration,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<HiddenInContext, Box<dyn Error>> {
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: ancestry table indices -- subscribee = index 0 (parent), subscriber =
  // index 2 (the full [Unrestricted, SubscribeeFolder, Unrestricted] chain), read through the
  // helper so this multi-level read shares the death-check's spec.
  let (subscribee_pid, subscribee_skgrepo) : (ID, SkgRepoName) =
    pid_and_skgrepo_from_required_ancestor(
      tree, node, 0, kind . caller_label () ) ?;
  let (subscriber_pid, subscriber_skgrepo) : (ID, SkgRepoName) =
    pid_and_skgrepo_from_required_ancestor(
      tree, node, 2, kind . caller_label () ) ?;
  // relRepo gating (render-and-gating, 5_plan.org): these are the
  // subscribee's and subscriber's own outbound lists (contains,
  // hidesFromSubs), read here to compute a DERIVED
  // membership for a third node (the HiddenInSubscribeeFolder) -- like
  // 'content_goal_list's grandparent subtrahends. A membership
  // recorded in a restricted skgrepo must not participate, in either
  // direction, or a private containment/hide would leak by omission
  // or by appearance.
  let skgrepo_unrestricted = |skgrepo : &SkgRepoName| match skgrepo_restriction {
    None      => true,
    Some (a)  => a . is_all () || a . contains_skgrepo (skgrepo) };
  let subscribee_contains : Vec<ID> = {
    let subscribee_graphnode : Graphnode =
      graphnode_graphFirst_by_pid_and_skgrepo (
        &runtime . graph, &runtime . config,
        &subscribee_pid, &subscribee_skgrepo ) ?;
    subscribee_graphnode . contains . iter ()
      . filter ( |m| skgrepo_unrestricted (& m . relRepo) )
      . map ( |m| m . member . clone () )
      . collect () };
  let (subscriber_hides, relRepos) : (Vec<ID>, HashMap<ID, SkgRepoName>) = {
    let subscriber_graphnode : Graphnode =
      graphnode_graphFirst_by_pid_and_skgrepo (
        &runtime . graph, &runtime . config,
        &subscriber_pid, &subscriber_skgrepo ) ?;
    let members = subscriber_graphnode . hidesFromSubs
      . or_default () . iter ()
      . filter ( |m| skgrepo_unrestricted (& m . relRepo) )
      . collect::<Vec<_>> ();
    ( members . iter () . map ( |m| m . member . clone () ) . collect (),
      members . iter () . filter ( |m| m . relRepo != subscriber_skgrepo )
        . map ( |m| (m . member . clone (), m . relRepo . clone ()) )
        . collect () ) };
  Ok (HiddenInContext {
    subscriber_pid,
    subscriber_skgrepo,
    subscribee_pid,
    subscribee_skgrepo,
    subscribee_contains,
    subscriber_hides,
    relRepos }) }
