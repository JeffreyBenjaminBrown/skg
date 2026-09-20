use crate::source_sets::ActiveSourceSet;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::to_org::complete::partner_folder::child_data::{ChildData, build_child_data, apply_membership_axes_to_folder_members, reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds};
use crate::update_buffer::reconcile::omit_inactive_members;
use crate::to_org::complete::partner_folder::goal_list::{goal_list_for_outbound_folder, outbound_member_axes};
use crate::types::git::{ExistenceAxes, MembershipAxes, SourceDiff};
use crate::types::phantom::phantom_axes;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::misc::{ID, RelPartner, SourceName};
use crate::types::tree::generic::{read_at_node_in_tree, with_node_mut};
use crate::types::tree::viewnode_nodecomplete::{ unique_scaffold_child_of_viewnode, insert_scaffold_as_child};
use crate::update_buffer::ancestry::required_ancestor;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, PartnerFolder};
use crate::types::viewnode::Vognode;
use crate::update_buffer::util::move_child_to_end;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct SubscribeeFolderContext {
  parent_pid             : ID,
  parent_source          : SourceName,
  worktree_subscribees   : Vec<ID>,
  relSources   : HashMap<ID, SourceName>,
}

/// SubscribeeFolder completion. Called at this folder's own visit in the level-order
/// BFS (after its Normal parent has been visited and created it).
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
  tree                           : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  runtime                        : &RuntimeGeneration,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  deleted_by_this_save_extra_ids : &HashMap<ID, HashSet<ID>>,
  active_source_set              : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let kind : PartnerFolder = PartnerFolder::Subscribee;
  kind . error_unless_node_is_this_kind (tree, node) ?;

  let context : SubscribeeFolderContext =
    read_subscribeeFolder_context (
      tree, node, runtime, active_source_set) ?;
  let (goal_list, removed_ids) : (Vec<ID>, HashSet<ID>) =
    goal_list_for_outbound_folder (
      &context . parent_pid, &context . parent_source,
      NodeRelation::Subscribes,
      source_diffs, &context . worktree_subscribees );
  let goal_list : Vec<ID> =
    // TODO/full-schema/9-2_source-set-safety.org: omit every inactive
    // subscribee from the goal (the weave preserves them at save). A
    // retained inactive placeholder already in the tree survives anyway
    // -- the folder reconciler treats it as irrelevant, not goal-matched.
    omit_inactive_members (
      goal_list, active_source_set,
      |id : &ID| SkgEnv::find_source_in_generation (
        runtime, id, deleted_since_head_pid_src_map) );

  // TODO/DONE/local-view-update/plan_v2.org §3.4/§6.7 exception: an *empty* SubscribeeFolder is PRESERVED, not
  // self-deleted. It is the editable interface onto the origin's outgoing
  // subscriptions; if it vanished when emptied, the user would lose the place
  // to add one back. (A SubscribeeFolder is only *created* when subscribes_to is
  // non-empty -- to_org/complete/sharing/mod.rs gates on that -- so an empty one
  // here means the subscriber lost all its subscriptions, and we keep the
  // headline so the user can re-add.) Its children still reconcile to empty
  // below, and the HiddenOutsideOfSubscribeeFolder is still ensured last.
  //
  // The subscribeeFolder's children ALWAYS reconcile from the graph -- like every
  // other PartnerFolder (reconcile_partnerFolder_children is unconditional). A
  // generated folder states "these nodes stand in this relationship to the
  // owner" and must not lie, including in the saved view: reconciliation runs
  // strictly AFTER extraction and the graph update, so the graph already holds
  // the user's just-saved subscriptions and reconciling reproduces them rather
  // than clobbering them. This also refreshes a DEFINITIVE subscriber's
  // subscribeeFolder during a collateral rerender -- the latent staleness
  // subscribeeFolder-maybe-todo.org flagged (forks plan.org: "Collateral-rerender
  // staleness fix"). The old gate (`if parent_write-protected ||
  // source_diffs.is_some()`) wrongly skipped a definitive subscriber outside
  // diff mode; 'parent_write-protected' is no longer read.
  { // TODO/DONE/local-view-update/plan_v2.org §5.5: a folder fills its members WHOLE and is budget-neutral -- the owning
    // subscriber already spent its budget unit when it expanded, so drawing all
    // its subscribees here costs nothing and never truncates the group.
    let axes_for_removed = // the relation this folder represents
      |child : &ID, child_src : &SourceName|
      -> (ExistenceAxes, MembershipAxes) {
      phantom_axes ( child, child_src,
                     &context . parent_pid, &context . parent_source,
                     NodeRelation::Subscribes,
                     source_diffs . as_ref () ) };
    let child_data : HashMap<ID, ChildData> =
      build_child_data (
        tree, node,
        &goal_list, &removed_ids, &axes_for_removed,
        source_diffs, deleted_since_head_pid_src_map,
        &context . relSources, runtime ) ?;
    reconcile_partnerFolder_children_against_goal_list_with_deleted_extraIds (
      tree, node, kind,
      &goal_list, &child_data, deleted_by_this_save_extra_ids ) ?;
    if source_diffs . is_some () {
      // Present members whose edge is New in some stage get that
      // stage's 'newM'; removed members are the phantoms above.
      apply_membership_axes_to_folder_members (
        tree, node,
        & outbound_member_axes (
          &context . parent_pid, &context . parent_source,
          NodeRelation::Subscribes, source_diffs )) ?; }}

  ensure_hiddenOutsideOfSubscribeeFolder_is_last (tree, node) ?;
  Ok(( )) }

fn read_subscribeeFolder_context (
  tree               : &Tree<ViewNode>,
  node               : NodeId,
  runtime            : &RuntimeGeneration,
  active_source_set  : Option<&ActiveSourceSet>,
) -> Result<SubscribeeFolderContext, Box<dyn Error>> {
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: read the subscriber Active vognode through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table
  // (index 0 = the parent), rather than at a hard-coded generation.
  let subscriber : NodeId =
    required_ancestor (tree, node, 0) ?
    . ok_or ("reconcile_subscribeeFolder_children: \
              subscriber ancestor absent (generalized orphan)") ?;
  let (parent_pid, parent_source)
    : (ID, SourceName)
    = read_at_node_in_tree(
      tree, subscriber,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => Some(( t . id . clone(),
                    t . source . clone() )),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
    . ok_or ("reconcile_subscribeeFolder_children: parent is not an ActiveNode") ?;
  let worktree_members : Vec<RelPartner<ID>> =
    // relSource gating (render-and-gating, 5_plan.org): this is the
    // OWNER's own outbound list (like 'contains' in
    // reconcile/content.rs), so a subscription recorded at an
    // inactive level must not appear here even though the
    // subscribee node itself may be active.
    nodecomplete_rustFirst_by_pid_and_source (
      &runtime . graph, &runtime . config, &parent_pid, &parent_source )
      . ok ()
      . map ( |skg| skg . subscribes_to . or_default () . iter ()
              . filter ( |m| match active_source_set {
                  None      => true,
                  Some (a)  => a . is_all ()
                    || a . contains_source (& m . relSource) } )
              . cloned ()
              . collect () )
      . unwrap_or_default ();
  let worktree_subscribees : Vec<ID> =
    worktree_members . iter () . map ( |m| m . member . clone () ) . collect ();
  let relSources : HashMap<ID, SourceName> =
    worktree_members . into_iter ()
      // An unresolved destination has no home of its own, so the
      // relationship default is the subscriber's home.  Only retain an
      // off-default fact for the Unknown's display metadata.
      . filter ( |m| m . relSource != parent_source )
      . map ( |m| (m . member, m . relSource) ) . collect ();
  Ok (SubscribeeFolderContext {
    parent_pid,
    parent_source,
    worktree_subscribees,
    relSources }) }

fn ensure_hiddenOutsideOfSubscribeeFolder_is_last (
  tree : &mut Tree<ViewNode>,
  node : NodeId,
) -> Result<(), Box<dyn Error>> {
  let hidden_outside : Option<NodeId> =
    unique_scaffold_child_of_viewnode(
      tree, node,
      &ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) ) ?;
  match hidden_outside {
    Some (child) => { move_child_to_end( tree, node, child ) ?; },
    None => {
      let new_folder : NodeId =
        insert_scaffold_as_child(
          tree, node,
          ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee),
          false ) ?;
      with_node_mut ( tree, new_folder,
        |mut n| {
          // TODO/fork-fixes.org: a new hidden folder begins folded. The
          // stamp moves to the members at the folder's own BFS visit
          // ('fold_members_of_newborn_folder'), which reconciles them in.
          n . value () . folded = true; } )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }}
  Ok (( )) }
