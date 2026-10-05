use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::to_org::complete::partner_folder::child_data::{ChildData, build_child_data, apply_relationship_axes_to_folder_members, reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds};
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::to_org::complete::partner_folder::goal_list::{goal_list_for_outbound_folder, outbound_member_axes};
use crate::types::git::{NodeAxes, RelationshipAxes, SkgRepoDiff};
use crate::types::phantom::phantom_axes;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::misc::{ID, RelPartner, SkgRepoName};
use crate::types::tree::generic::{read_at_node_in_tree, with_node_mut};
use crate::types::tree::viewnode_graphnode::{ unique_non_vognode_child_of_viewnode, insert_non_vognode_as_child};
use crate::update_buffer::ancestry::required_ancestor;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, PartnerFolder};
use crate::types::viewnode::Vognode;
use crate::update_buffer::util::move_child_to_end;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct SubscribeeFolderContext {
  parent_pid             : ID,
  parent_skgrepo         : SkgRepoName,
  worktree_subscribees   : Vec<ID>,
  relRepos               : HashMap<ID, SkgRepoName>,
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
  skgrepo_diffs                  : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
  runtime                        : &RuntimeGeneration,
  deleted_since_head_pid_src_map : &HashMap<ID, SkgRepoName>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  skgrepo_restriction            : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerFolder = PartnerFolder::Subscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : SubscribeeFolderContext =
    read_subscribeeFolder_context (
      tree, node, runtime, skgrepo_restriction) ?;
  let (goal_list, removed_skgids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_outbound_folder (
      &context . parent_pid, &context . parent_skgrepo,
      NodeRelation::SubscribesTo,
      skgrepo_diffs, &context . worktree_subscribees );
  let goal_list : Vec<ID> =
    // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: omit every restricted
    // subscribee from the goal (the weave preserves them at save). A
    // retained restricted vognode already in the tree survives anyway
    // -- the folder reconciler treats it as irrelevant, not goal-matched.
    omit_restricted_members (
      goal_list, skgrepo_restriction,
      |skgid : &ID| SkgEnv::find_skgrepo_in_generation (
        runtime, skgid, deleted_since_head_pid_src_map) );

  // TODO/DONE/local-view-update/plan_v2.org §3.4/§6.7 exception: an *empty* SubscribeeFolder is PRESERVED, not
  // self-deleted. It is the editable interface onto the origin's outgoing
  // subscriptions; if it vanished when emptied, the user would lose the place
  // to add one back. (A SubscribeeFolder is only *created* when subscribesTo is
  // non-empty -- to_org/complete/sharing/mod.rs gates on that -- so an empty one
  // here means the subscriber lost all its subscriptions, and we keep the
  // headline so the user can re-add.) Its children still reconcile to empty
  // below, and the HiddenOutsideOfSubscribeeFolder is still ensured last.
  //
  // The subscribeeFolder's children ALWAYS reconcile from the graph -- like every
  // other PartnerFolder (reconcile_partnerFolder_children is unconditional). A
  // generated folder states "these nodes stand in this relationship to the
  // recorder" and must not lie, including in the saved view: reconciliation runs
  // strictly AFTER extraction and the graph update, so the graph already holds
  // the user's just-saved subscriptions and reconciling reproduces them rather
  // than clobbering them. This also refreshes a EDITABLE subscriber's
  // subscribeeFolder during a collateral rerender -- the latent staleness
  // subscribeeFolder-maybe-todo.org flagged (forks plan.org: "Collateral-rerender
  // staleness fix"). The old gate (`if parent_write-protected ||
  // repo_diffs.is_some()`) wrongly skipped an editable subscriber outside
  // diff mode; 'parent_write-protected' is no longer read.
  { // TODO/DONE/local-view-update/plan_v2.org §5.5: a folder fills its members WHOLE and is budget-neutral -- the owning
    // subscriber already spent its budget unit when it expanded, so drawing all
    // its subscribees here costs nothing and never truncates the group.
    let axes_for_removed = // the relation this folder represents
      |child : &ID, child_src : &SkgRepoName|
      -> (NodeAxes, RelationshipAxes) {
      phantom_axes ( child, child_src,
                     &context . parent_pid, &context . parent_skgrepo,
                     NodeRelation::SubscribesTo,
                     skgrepo_diffs . as_ref () ) };
    let child_data : HashMap<ID, ChildData> =
      build_child_data (
        tree, node,
        &goal_list, &removed_skgids, &axes_for_removed,
        skgrepo_diffs, deleted_since_head_pid_src_map,
        &context . relRepos, runtime ) ?;
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind,
      &goal_list, &child_data, deleted_by_this_save_extra_ids ) ?;
    if skgrepo_diffs . is_some () {
      // Present members whose relationship is New in some stage get that
      // stage's 'addedR'; removed members are the phantoms above.
      apply_relationship_axes_to_folder_members (
        tree, node,
        & outbound_member_axes (
          &context . parent_pid, &context . parent_skgrepo,
          NodeRelation::SubscribesTo, skgrepo_diffs )) ?; }}

  ensure_hiddenOutsideOfSubscribeeFolder_is_last (tree, node) ?;
  Ok(( )) }

fn read_subscribeeFolder_context (
  tree               : &Tree<Viewnode>,
  node               : NodeId,
  runtime            : &RuntimeGeneration,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<SubscribeeFolderContext, Box<dyn Error>> {
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: read the subscriber Unrestricted vognode through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table
  // (index 0 = the parent), rather than at a hard-coded generation.
  let subscriber : NodeId =
    required_ancestor (tree, node, 0) ?
    . ok_or ("reconcile_subscribeeFolder_children: \
              subscriber ancestor absent (generalized orphan)") ?;
  let (parent_pid, parent_skgrepo)
    : (ID, SkgRepoName)
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
    // relRepo gating (render-and-gating, 5_plan.org): this is the
    // RECORDER's own outbound list (like 'contains' in
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
  let relRepos : HashMap<ID, SkgRepoName> =
    worktree_members . into_iter ()
      // An unresolved destination has no home of its own, so the
      // relationship default is the subscriber's home.  Only retain an
      // off-default fact for the Unknown's display metadata.
      . filter ( |m| m . relRepo != parent_skgrepo )
      . map ( |m| (m . member, m . relRepo) ) . collect ();
  Ok (SubscribeeFolderContext {
    parent_pid,
    parent_skgrepo,
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
    Some (child) => { move_child_to_end( tree, node, child ) ?; },
    None => {
      let new_folder : NodeId =
        insert_non_vognode_as_child(
          tree, node,
          ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee),
          false ) ?;
      with_node_mut ( tree, new_folder,
        |mut n| {
          // TODO/fork-fixes.org: a new hidden folder begins folded. The
          // stamp moves to the members at the folder's own BFS visit
          // ('fold_members_of_newborn_folder'), which reconciles them in.
          n . value () . folded = true; } )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }}
  Ok (( )) }
