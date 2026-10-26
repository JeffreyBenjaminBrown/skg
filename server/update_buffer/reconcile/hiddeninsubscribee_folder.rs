use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::to_org::complete::partner_folder::child_data::{ChildData, FolderGoalItem, build_child_data, reconcile_partnerFolder_children_against_goal_list};
use crate::to_org::complete::partner_folder::goal_list::goal_list_for_hiddenInSubscribee_folder;
use crate::types::misc::{ID, SkgrepoName};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::nodes::complete::Graphnode;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::util::fold_members_of_newborn_folder;
use crate::types::viewnode::{Viewnode, Folder};

use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

struct HiddenInContext {
  subscribee_contains : Vec<ID>,
  subscriber_hides    : Vec<ID>,
  relRepos            : HashMap<ID, SkgrepoName>,
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
/// hides from its subscriptions and that are top-level content
/// of the subscribee.
pub fn reconcile_hiddenInSubscribeeFolder_children (
  node                           : NodeId,
  tree                           : &mut Tree<Viewnode>,
  runtime                        : &RuntimeGeneration,
  skgrepo_restriction            : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  let kind : &Folder =
    &Folder::HiddenInSubscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : HiddenInContext =
    read_hiddenin_context (tree, node, kind, runtime, skgrepo_restriction) ?;
  let goal_list : Vec<ID> =
    goal_list_for_hiddenInSubscribee_folder (
      &runtime . graph,
      &context . subscribee_contains, &context . subscriber_hides );
  let goal_list : Vec<ID> =
    // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): omit restricted
    // members; no retention for this compound partner folder.
    omit_restricted_members (
      goal_list, skgrepo_restriction,
      |skgid : &ID| SkgEnv::find_skgrepo_in_generation (runtime, skgid) );
  // The node budget: a folder fills its members whole and is budget-neutral -- the owning
  // subscribee already spent its budget unit when it extended, so adding all
  // the hidden members here costs nothing and never truncates the group.
  let goal_items : Vec<FolderGoalItem> =
    FolderGoalItem::live_members (&goal_list);
  let child_data : HashMap<ID, ChildData> =
    build_child_data (
      tree, node, &goal_items, &context . relRepos, runtime ) ?;
  reconcile_partnerFolder_children_against_goal_list (
      // Retain as non-member or tombstone, then prune
      // ('docs/glossary.org'): a HiddenInSubscribeeFolder child
      // that becomes stale (e.g. the user moved it into the subscribee-as-such,
      // 'unhiding' it) is converted: a leaf -- the common case for hidden
      // members -- to a tombstone the prune removes, one holding a subtree
      // retained as non-member.
      tree, node, kind,
      &goal_items, &child_data, &runtime . graph ) ?;
  fold_members_of_newborn_folder (tree, node)
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  // An emptied HiddenInSubscribeeFolder is removed by the single postorder
  // prune sweep (prune_childless_tombstones), not self-deleted here.
  Ok(( )) }

fn read_hiddenin_context (
  tree               : &Tree<Viewnode>,
  node               : NodeId,
  kind               : &Folder,
  runtime            : &RuntimeGeneration,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<HiddenInContext, Box<dyn Error>> {
  // Required-ancestry table ('update_buffer/ancestry.rs') indices -- subscribee = index 0 (parent), subscriber =
  // index 2 (the full [Unrestricted, SubscribeeFolder, Unrestricted] chain), read through the
  // helper so this multi-level read shares the death-check's spec.
  let (subscribee_pid, subscribee_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor(
      tree, node, 0, kind . caller_label () ) ?;
  let (subscriber_pid, subscriber_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor(
      tree, node, 2, kind . caller_label () ) ?;
  // relRepo gating ('docs/telescopes.org', "What lives where"): these are the
  // subscribee's and subscriber's own outbound lists (contains,
  // hidesFromSubs), read here to compute a derived
  // membership for a third node (the HiddenInSubscribeeFolder) -- like
  // 'content_goal_list's grandparent subtrahends. A membership
  // recorded in a restricted skgrepo must not participate, in either
  // direction, or a private containment/hide would leak by omission
  // or by appearance.
  let skgrepo_unrestricted = |skgrepo : &SkgrepoName| match skgrepo_restriction {
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
  let (subscriber_hides, relRepos) : (Vec<ID>, HashMap<ID, SkgrepoName>) = {
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
    subscribee_contains,
    subscriber_hides,
    relRepos }) }
