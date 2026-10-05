use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::to_org::complete::partner_folder::child_data::{ChildData, apply_relationship_axes_to_folder_members, build_child_data, reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds};
use crate::to_org::complete::partner_folder::goal_list::goal_list_for_hiddenOutsideOfSubscribee_folder;
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

struct HiddenOutsideContext {
  subscriber_pid      : ID,
  subscriber_skgrepo  : SkgRepoName,
  subscriber_hides    : Vec<ID>,
  relRepos            : HashMap<ID, SkgRepoName>,
  subscribees         : Vec<ID>,
}

/// HiddenOutsideOfSubscribeeFolder completion (called at this folder's own BFS visit).
///
/// Tree structure:
///   Subscriber (UnrestrictedVognode)                       <- ancestor 2
///     └─ SubscribeeFolder (Non-vognode)               <- ancestor 1 = parent
///          ├─ Subscribee_A (UnrestrictedVognode)           <- sibling
///          ├─ Subscribee_B (UnrestrictedVognode)           <- sibling
///          └─ HiddenOutsideOfSubscribeeFolder      <- self
///               └─ [hidden UnrestrictedVognode children]
///
/// Collects nodes that the subscriber hides from its subscriptions
/// but that are NOT top-level content of any subscribee.
pub fn reconcile_hiddenoutsideSubscribeeFolder_children (
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
    PartnerFolder::HiddenOutsideOfSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: the parent-is-SubscribeeFolder check is subsumed by reading the
  // subscriber through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table (index 1 validates the
  // [SubscribeeFolder, Unrestricted] prefix), so a separate validation is unneeded.
  let context : HiddenOutsideContext =
    read_hiddenoutside_context (
      tree, node, kind, runtime, skgrepo_restriction) ?;
  let (goal_list, removed_skgids, member_axes)
    : (Vec<ID>, HashSet<ID>, HashMap<ID, RelationshipAxes>) =
    goal_list_for_hiddenOutsideOfSubscribee_folder (
      &runtime . graph,
      &context . subscriber_pid, &context . subscriber_skgrepo,
      &context . subscriber_hides, &context . subscribees,
      skgrepo_diffs, &runtime . config );
  let goal_list : Vec<ID> =
    // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: omit restricted
    // members; no retention for this filter folder.
    omit_restricted_members (
      goal_list, skgrepo_restriction,
      |skgid : &ID| SkgEnv::find_skgrepo_in_generation (
        runtime, skgid, deleted_since_head_pid_src_map) );
  // TODO/DONE/local-view-update/plan_v2.org §5.5: a folder fills its members WHOLE and is budget-neutral -- the owning
  // subscriber already spent its budget unit when it expanded, so drawing all
  // these hidden members here costs nothing and never truncates the group.
  let axes_for_removed =
    // This folder's membership is DERIVED (subscriber hides minus
    // subscribee content), so no single relation diff is
    // authoritative: the relationship signs come from the
    // three-snapshot comparison; node signs from the member's own
    // file statuses.
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
      // TODO/DONE/local-view-update/plan_v2.org §6.0: a stale member of this write-protected folder is removed when a view-leaf
      // (the common case) and demoted to non-member only if it has a user
      // subtree. Handled uniformly by the reconciler.
      tree, node, kind,
      &goal_list, &child_data, deleted_by_this_save_extra_ids ) ?;
  if skgrepo_diffs . is_some () {
    // Present members newly derived-in in some stage get that
    // stage's 'addedR'; removed members are the phantoms above.
    apply_relationship_axes_to_folder_members (
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
  tree               : &Tree<Viewnode>,
  node               : NodeId,
  kind               : PartnerFolder,
  runtime            : &RuntimeGeneration,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<HiddenOutsideContext, Box<dyn Error>> {
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: subscriber = ancestry-table index 1 (the [SubscribeeFolder, Unrestricted] chain).
  let (subscriber_pid, subscriber_skgrepo) : (ID, SkgRepoName) =
    pid_and_skgrepo_from_required_ancestor(
      tree, node, 1, kind . caller_label () ) ?;
  let wt_subscriber_graphnode : Graphnode =
    graphnode_graphFirst_by_pid_and_skgrepo (
      &runtime . graph, &runtime . config,
      &subscriber_pid, &subscriber_skgrepo ) ?;
  // relRepo gating (render-and-gating, 5_plan.org): both are the
  // subscriber's own outbound lists (hidesFromSubs,
  // subscribesTo); a membership recorded in a restricted skgrepo must
  // not feed this derived folder.
  let skgrepo_unrestricted = |skgrepo : &SkgRepoName| match skgrepo_restriction {
    None      => true,
    Some (a)  => a . is_all () || a . contains_skgrepo (skgrepo) };
  let wt_subscriber_hide_members =
    wt_subscriber_graphnode . hidesFromSubs
    . or_default () . iter ()
    . filter ( |m| skgrepo_unrestricted (& m . relRepo) )
    . collect::<Vec<_>> ();
  let wt_subscriber_hides : Vec<ID> = wt_subscriber_hide_members . iter ()
    . map ( |m| m . member . clone () ) . collect ();
  let relRepos : HashMap<ID, SkgRepoName> =
    wt_subscriber_hide_members . iter ()
    . filter ( |m| m . relRepo != subscriber_skgrepo )
    . map ( |m| (m . member . clone (), m . relRepo . clone ()) )
    . collect ();
  let wt_subscribees : Vec<ID> =
    wt_subscriber_graphnode . subscribesTo
    . or_default () . iter ()
    . filter ( |m| skgrepo_unrestricted (& m . relRepo) )
    . map ( |m| m . member . clone () )
    . collect ();
  Ok (HiddenOutsideContext {
    subscriber_pid,
    subscriber_skgrepo,
    subscriber_hides : wt_subscriber_hides,
    relRepos,
    subscribees      : wt_subscribees }) }
