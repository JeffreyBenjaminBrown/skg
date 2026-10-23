use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::to_org::complete::partner_folder::child_data::{ChildData, FolderGoalItem, build_child_data, reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds};
use crate::to_org::complete::partner_folder::goal_list::goal_list_for_hiddenOutsideOfSubscribee_folder;
use crate::types::misc::{ID, SkgrepoName};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::nodes::complete::Graphnode;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::reconcile::partner_folder::{folder_vid_of, push_repair_warnings};
use crate::update_buffer::util::fold_members_of_newborn_folder;
use crate::update_buffer::warnings::CompletionWarning;
use crate::types::viewnode::{Viewnode, PartnerFolder};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct HiddenOutsideContext {
  subscriber_pid      : ID,
  subscriber_hides    : Vec<ID>,
  relRepos            : HashMap<ID, SkgrepoName>,
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
/// but that are not top-level content of any subscribee.
pub fn reconcile_hiddenoutsideSubscribeeFolder_children (
  node                           : NodeId,
  tree                           : &mut Tree<Viewnode>,
  runtime                        : &RuntimeGeneration,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  skgrepo_restriction            : Option<&SkgrepoRestriction>,
  warning_sink                   : Option<&mut Vec<CompletionWarning>>, // Some only when completing the view the user just saved.
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerFolder =
    PartnerFolder::HiddenOutsideOfSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  // The parent-is-SubscribeeFolder check is subsumed by reading the
  // subscriber through the required-ancestry table of 'update_buffer/ancestry.rs' (index 1 validates the
  // [SubscribeeFolder, Unrestricted] prefix), so a separate validation is unneeded.
  let context : HiddenOutsideContext =
    read_hiddenoutside_context (
      tree, node, kind, runtime, skgrepo_restriction) ?;
  let goal_list : Vec<ID> =
    goal_list_for_hiddenOutsideOfSubscribee_folder (
      &runtime . graph,
      &context . subscriber_hides, &context . subscribees,
      &runtime . config );
  let goal_list : Vec<ID> =
    // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): omit restricted
    // members; no retention for this filter folder.
    omit_restricted_members (
      goal_list, skgrepo_restriction,
      |skgid : &ID| SkgEnv::find_skgrepo_in_generation (runtime, skgid) );
  // The node budget: a folder fills its members whole and is budget-neutral -- the owning
  // subscriber already spent its budget unit when it extended, so adding all
  // these hidden members here costs nothing and never truncates the group.
  let goal_items : Vec<FolderGoalItem> =
    FolderGoalItem::live_members (&goal_list);
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &goal_items, &context . relRepos, runtime ) ?;
  let summary =
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      // The stale-member rule of 'complete_managed_children': a stale member of this write-protected folder is removed when a view-leaf
      // (the common case) and made a non-member only if it has a user
      // subtree. Handled uniformly by the reconciler.
      tree, node, kind,
      &goal_items, &child_data, &runtime . graph,
      deleted_by_this_save_extra_ids ) ?;
  if let Some (sink) = warning_sink {
    push_repair_warnings (
      sink, kind, &context . subscriber_pid,
      folder_vid_of (tree, node) . as_ref (), summary ); }
  fold_members_of_newborn_folder (tree, node)
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  // An emptied HiddenOutsideOfSubscribeeFolder is removed by the single
  // postorder prune sweep (prune_self_deletable_when_empty), not here.
  Ok(( )) }

fn read_hiddenoutside_context (
  tree               : &Tree<Viewnode>,
  node               : NodeId,
  kind               : PartnerFolder,
  runtime            : &RuntimeGeneration,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<HiddenOutsideContext, Box<dyn Error>> {
  // The subscriber = required-ancestry-table index 1 (the [SubscribeeFolder, Unrestricted] chain).
  let (subscriber_pid, subscriber_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor(
      tree, node, 1, kind . caller_label () ) ?;
  let wt_subscriber_graphnode : Graphnode =
    graphnode_graphFirst_by_pid_and_skgrepo (
      &runtime . graph, &runtime . config,
      &subscriber_pid, &subscriber_skgrepo ) ?;
  // relRepo gating ('docs/telescopes.org', "What lives where"): both are the
  // subscriber's own outbound lists (hidesFromSubs,
  // subscribesTo); a membership recorded in a restricted skgrepo must
  // not feed this derived folder.
  let skgrepo_unrestricted = |skgrepo : &SkgrepoName| match skgrepo_restriction {
    None      => true,
    Some (a)  => a . is_all () || a . contains_skgrepo (skgrepo) };
  let wt_subscriber_hide_members =
    wt_subscriber_graphnode . hidesFromSubs
    . or_default () . iter ()
    . filter ( |m| skgrepo_unrestricted (& m . relRepo) )
    . collect::<Vec<_>> ();
  let wt_subscriber_hides : Vec<ID> = wt_subscriber_hide_members . iter ()
    . map ( |m| m . member . clone () ) . collect ();
  let relRepos : HashMap<ID, SkgrepoName> =
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
    subscriber_hides : wt_subscriber_hides,
    relRepos,
    subscribees      : wt_subscribees }) }
