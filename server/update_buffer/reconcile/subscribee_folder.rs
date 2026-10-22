use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::to_org::complete::partner_folder::child_data::{ChildData, FolderGoalItem, build_child_data, reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds};
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::misc::{ID, RelPartner, SkgrepoName};
use crate::types::tree::generic::{read_at_node_in_tree, with_node_mut};
use crate::types::tree::moves::move_to_last_child;
use crate::types::tree::viewnode_graphnode::{ unique_non_vognode_child_of_viewnode, insert_non_vognode_as_child};
use crate::update_buffer::ancestry::required_ancestor;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, PartnerFolder};
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct SubscribeeFolderContext {
  worktree_subscribees   : Vec<ID>,
  relRepos               : HashMap<ID, SkgrepoName>,
}

/// SubscribeeFolder completion. Called at this folder's own visit in the level-order
/// BFS (after its Unrestricted parent has been visited and created it).
///
/// WHAT IT DOES:
/// - Error unless it's a SubscribeeFolder.
/// - Read parent's skg ID and write-protected flag.
/// - Look up parent's subscribees.
/// - If no subscribees: transfer focus if needed, then delete.
/// - Reconcile the subscribee children from the graph.
/// - Ensure HiddenOutsideOfSubscribeeFolder exists and is last.
pub fn reconcile_subscribeeFolder_children (
  node                           : NodeId,
  tree                           : &mut Tree<Viewnode>,
  runtime                        : &RuntimeGeneration,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  skgrepo_restriction            : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerFolder = PartnerFolder::Subscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : SubscribeeFolderContext =
    read_subscribeeFolder_context (
      tree, node, runtime, skgrepo_restriction) ?;
  let goal_list : Vec<ID> =
    // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): omit every restricted
    // subscribee from the goal (the weave preserves them at save). A
    // retained restricted vognode already in the tree survives anyway
    // -- the folder reconciler treats it as unmanaged, not goal-matched.
    omit_restricted_members (
      context . worktree_subscribees . clone (), skgrepo_restriction,
      |skgid : &ID| SkgEnv::find_skgrepo_in_generation (runtime, skgid) );

  // An exception to the prune sweep ('prune_self_deletable_when_empty'): an *empty* SubscribeeFolder is preserved, not
  // self-deleted. It is the editable interface onto the origin's outgoing
  // subscriptions; if it vanished when emptied, the user would lose the place
  // to add one back. (A SubscribeeFolder is only *created* when subscribesTo is
  // non-empty -- to_org/complete/sharing/mod.rs gates on that -- so an empty one
  // here means the subscriber lost all its subscriptions, and we keep the
  // headline so the user can re-add.) Its children still reconcile to empty
  // below, and the HiddenOutsideOfSubscribeeFolder is still ensured last.
  //
  // The subscribeeFolder's children always reconcile from the graph -- like every
  // other PartnerFolder (reconcile_partnerFolder_children is unconditional). A
  // generated folder states "these nodes stand in this relationship to the
  // recorder" and must not lie, including in the saved view: reconciliation runs
  // strictly after extraction and the graph update, so the graph already holds
  // the user's just-saved subscriptions and reconciling reproduces them rather
  // than clobbering them. This also refreshes a editable subscriber's
  // subscribeeFolder during a collateral rerender, which would otherwise
  // go stale.
  { // The node budget: a folder fills its members whole and is budget-neutral -- the owning
    // subscriber already spent its budget unit when it extended, so adding all
    // its subscribees here costs nothing and never truncates the group.
    let goal_items : Vec<FolderGoalItem> =
      FolderGoalItem::live_members (&goal_list);
    let child_data : HashMap<ID, ChildData> =
      build_child_data (
        tree, node, &goal_items, &context . relRepos, runtime ) ?;
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind,
      &goal_items, &child_data, &runtime . graph,
      deleted_by_this_save_extra_ids ) ?; }

  ensure_hiddenOutsideOfSubscribeeFolder_is_last (tree, node) ?;
  Ok(( )) }

fn read_subscribeeFolder_context (
  tree               : &Tree<Viewnode>,
  node               : NodeId,
  runtime            : &RuntimeGeneration,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<SubscribeeFolderContext, Box<dyn Error>> {
  // Read the subscriber Unrestricted vognode through the required-ancestry table of 'update_buffer/ancestry.rs'
  // (index 0 = the parent), rather than at a hard-coded generation.
  let subscriber : NodeId =
    required_ancestor (tree, node, 0) ?
    . ok_or ("reconcile_subscribeeFolder_children: \
              subscriber ancestor absent (generalized orphan)") ?;
  let (parent_pid, parent_skgrepo)
    : (ID, SkgrepoName)
    = read_at_node_in_tree(
      tree, subscriber,
      |vn : &Viewnode| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => Some(( t . skgid . clone(),
                    t . home_skgrepo . clone() )),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
    . ok_or ("reconcile_subscribeeFolder_children: parent is not an UnrestrictedVognode") ?;
  let worktree_members : Vec<RelPartner<ID>> =
    // relRepo gating ('docs/telescopes.org', "What lives where"): this is the
    // recorder's own outbound list (like 'contains' in
    // reconcile/content.rs), so a subscription recorded at an
    // restricted level must not appear here even though the
    // subscribee node itself may be unrestricted.
    graphnode_graphFirst_by_pid_and_skgrepo (
      &runtime . graph, &runtime . config, &parent_pid, &parent_skgrepo )
      . ok ()
      . map ( |skg| skg . subscribesTo . or_default () . iter ()
              . filter ( |m| match skgrepo_restriction {
                  None      => true,
                  Some (a)  => a . is_all ()
                    || a . contains_skgrepo (& m . relRepo) } )
              . cloned ()
              . collect () )
      . unwrap_or_default ();
  let worktree_subscribees : Vec<ID> =
    worktree_members . iter () . map ( |m| m . member . clone () ) . collect ();
  let relRepos : HashMap<ID, SkgrepoName> =
    worktree_members . into_iter ()
      // An unresolved destination has no home of its own, so the
      // relationship default is the subscriber's home.  Only retain an
      // off-default fact for the Unknown's display metadata.
      . filter ( |m| m . relRepo != parent_skgrepo )
      . map ( |m| (m . member, m . relRepo) ) . collect ();
  Ok (SubscribeeFolderContext {
    worktree_subscribees,
    relRepos }) }

fn ensure_hiddenOutsideOfSubscribeeFolder_is_last (
  tree : &mut Tree<Viewnode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let hidden_outside : Option<NodeId> =
    unique_non_vognode_child_of_viewnode(
      tree, node,
      &ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) ) ?;
  match hidden_outside {
    Some (child) => { move_to_last_child( tree, node, child ) ?; },
    None => {
      let new_folder : NodeId =
        insert_non_vognode_as_child(
          tree, node,
          ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee),
          false ) ?;
      with_node_mut ( tree, new_folder,
        |mut n| {
          // A new hidden folder begins folded. The
          // stamp moves to the members at the folder's own BFS visit
          // ('fold_members_of_newborn_folder'), which reconciles them in.
          n . value () . org_folded = true; } )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }}
  Ok (( )) }
