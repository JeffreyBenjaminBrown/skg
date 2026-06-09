// MANUAL RECURSION:
// This uses manual async recursions, rather than calls to
// `do_everywhere_in_tree_dfs`, because some dispatch targets
// are async, and `do_everywhere_in_tree_dfs` takes a sync
// `FnMut` closure which cannot `.await`.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::source_sets::ActiveSourceSet;
use crate::to_org::expand::definitive::{ apply_definitive_draw_rule, DrawOutcome};
use crate::to_org::util::DefinitiveMap;
use crate::types::env::SkgEnv;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName, TantivyIndex};
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs_readonly, read_at_node_in_tree, read_at_ancestor_in_tree, write_at_node_in_tree};
use crate::to_org::complete::sharing::maybe_add_relation_col_branches;
use crate::update_buffer::ancestry::{ col_is_generalized_orphan, deaden_generalized_orphan_col, is_col_kind};
use crate::update_buffer::util::detach_scaffold_transferring_focus;
use crate::to_org::render::diff::process_truenode_diff;
use crate::types::tree::viewnode_nodecomplete::write_at_truenode_in_tree;
use crate::types::viewnode::{ViewNode, ViewNodeKind, RoleCol, ViewRequest, IndefOrDef, DeletedNode};
use crate::types::viewnode::{Vognode, QualCol};
use super::reconcile::hiddeninsubscribee_col::reconcile_hiddenin_subscribee_col_children;
use super::reconcile::hiddenoutsideof_subscribeecol::reconcile_hiddenoutside_subscribee_col_children;
use super::reconcile::relation_col::reconcile_relation_col_children;
use super::reconcile::subscribee_col::reconcile_subscribee_col_children;
use super::reconcile::content::expand_true_content_at_truenode;

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::sync::Arc;

pub(super) struct CompletionContext<'a> {
  pub(super) defmap                         : &'a mut DefinitiveMap,
  /// The per-source git diffs (Some in diff mode, None otherwise). This is the
  /// single diff handle: it drives the per-node process_truenode_diff (content
  /// axes, the phantom flip, TextChanged/IDCol/AliasCol), the diff-aware QualCol
  /// reconcilers, and the sharing cols' removed-member phantoms. The content
  /// reconcile itself produces only the pure worktree view; process_truenode_diff
  /// applies every content diff effect afterward at the node's own visit (TODO/DONE/local-view-update/plan_v2.org §9
  /// reversal / #3).
  pub(super) source_diffs                   : &'a Option<HashMap<SourceName, SourceDiff>>,
  pub(super) env                            : &'a SkgEnv,
  pub(super) graph_snap                     : &'a Arc<InRustGraph>,
  pub(super) errors                         : &'a mut Vec<String>,
  pub(super) deleted_since_head_pid_src_map : &'a HashMap<ID, SourceName>,
  pub(super) deleted_by_this_save_pids      : &'a HashSet<ID>,
  pub(super) active_source_set              : Option<&'a ActiveSourceSet>,
  /// TODO/DONE/local-view-update/plan_v2.org §5.5 per-buffer node limit: the remaining budget of *new* ViewNodes
  /// the ordinary update pass may create. Initialized once per rerender to
  /// `config.initial_node_limit` and decremented as content children are
  /// created; when it reaches 0 no further new content is drawn (and the
  /// cascade stops). The inline diff is exempt (TODO/DONE/local-view-update/plan_v2.org §5.5): every diff phantom and
  /// diff scaffold is created regardless of the budget.
  pub(super) node_budget                    : usize,
  /// Phase 8 (TODO/DONE/local-view-update/plan_v2.org §13): true only for a DE-NOVO (initial) render driven through
  /// view completion. When set, a fresh CONTENT node (parent is not a PartnerCol)
  /// gets its relation cols created at its visit, the way
  /// build_node_branch_minus_content does at node birth.
  /// FALSE for post-save rerender, where relation cols are already present in
  /// the saved buffer and re-creating them would change the buffer and break the
  /// save round-trip (TODO/DONE/local-view-update/plan_v2.org §18). So post-save stays byte-identical.
  pub(super) create_relation_cols_for_fresh_nodes : bool,
  /// TODO/DONE/local-view-update/plan_v2.org §9 reversal (#3): the tantivy index for the inline diff's phantom-source
  /// resolution. None on the post-save path (the deleted-id map + disk scan
  /// suffice); Some on the de-novo path.
  pub(super) diff_tantivy_index : Option<&'a TantivyIndex>,
}

pub(super) async fn complete_viewforest (
  viewforest : &mut Tree<ViewNode>,
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = viewforest . root () . id ();
  // The single TODO/DONE/local-view-update/plan_v2.org §3 level-order BFS: each node's visit
  // (dispatch_node_update) does ALL of its own update -- content
  // reconcile, the TODO/DONE/local-view-update/plan_v2.org §5.2 draw rule + TODO/DONE/local-view-update/plan_v2.org §5.3 cascade for definitive-view
  // requests, the inline diff, the other view requests, ensuring a definitive
  // subscribee's HiddenInSubscribeeCol, and per-col reconciliation. Cols and
  // content children created during a node's visit are reached later in the
  // same BFS.
  complete_nodes_in_level_order (
    viewforest, root_treeid, context ) . await ?;
  prune_self_deletable_when_empty (viewforest) ?;
  Ok(( )) }

/// Visit every node in *level order* (BFS), completing each at its own visit.
///
/// This is the core of view completion: a single top-down level-order traversal
/// (TODO/DONE/local-view-update/plan_v2.org §3) in which each node, when visited, may create view-descendants
/// (content children, subscribee/relation members) that are then visited later
/// in the same traversal. A FIFO queue (not depth-first recursion) is what makes
/// the TODO/DONE/local-view-update/plan_v2.org §5.5 node-limit and TODO/DONE/local-view-update/plan_v2.org §5.2 draw-rule semantics order-correct: the first
/// occurrence of a duplicate id reached in BFS order -- not in depth-first order
/// -- is the one that wins, and the Finalizable draw rule makes that safe. A
/// node's expansion depends only on its ancestors and the graph, never on
/// siblings or descendants, so it can be processed the moment it is dequeued.
async fn complete_nodes_in_level_order (
  tree     : &mut Tree<ViewNode>,
  root_treeid : NodeId,
  context  : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let mut queue : VecDeque<NodeId> = VecDeque::new ();
  queue . push_back (root_treeid);
  while let Some (treeid) = queue . pop_front () {
    dispatch_node_update (tree, treeid, context) . await ?;
    // Enqueue the node's *current* children -- including any this visit
    // just created -- so they are completed after every node already in
    // their level. (See 'MANUAL RECURSION' comment at top of file: the
    // dispatch is async, so we hand-roll the traversal.)
    if let Some (node) = tree . get (treeid) {
      for child in node . children () {
        queue . push_back (child . id ()); }}}
  Ok(( )) }

/// One BFS visit: dispatch on (kind, parent-kind) to the node's update rule
/// (TODO/DONE/local-view-update/plan_v2.org §3/§4). Cols are reconciled at their own visit (the BFS reaches a
/// col after its Normal parent created it).
async fn dispatch_node_update (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get (treeid) . unwrap () . value () . kind . clone ();
  // Death-leafward (TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5): before reconciling any
  // col, self-check its required ancestry. If it is a generalized orphan (some
  // required ancestor is the wrong viewnode kind -- full chain, not just the
  // parent), deaden it and dispose its children instead of reconciling, so the
  // reconcile never reads a since-deleted ancestor's missing NodeComplete.
  // The BFS visits every col, and the check walks the col's whole required
  // ancestry, so a col whose grandparent (not just its parent) died is deadened.
  if is_col_kind (&kind)
    && col_is_generalized_orphan (tree, treeid) ? {
    deaden_generalized_orphan_col (tree, treeid) ?;
    return Ok (( )); }
  match &kind {
    ViewNodeKind::Vognode (Vognode::Normal (_)) =>
      visit_normal_node (tree, treeid, context) . await ?,
    ViewNodeKind::PartnerCol (RoleCol::Subscribee) =>
      // A col fills its members WHOLE and is budget-neutral (TODO/DONE/local-view-update/plan_v2.org §5.5): the owning
      // vognode already spent its 1 budget unit when it expanded, so drawing all
      // the members here costs nothing more and never truncates a group.
      reconcile_subscribee_col_children (
        treeid, tree, context . source_diffs, context . env,
        context . deleted_since_head_pid_src_map ) . await ?,
    ViewNodeKind::PartnerCol (RoleCol::HiddenInSubscribee) =>
      reconcile_hiddenin_subscribee_col_children (
        treeid, tree, context . source_diffs, context . env,
        context . deleted_since_head_pid_src_map ) ?,
    ViewNodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee) =>
      reconcile_hiddenoutside_subscribee_col_children (
        treeid, tree, context . source_diffs, context . env,
        context . deleted_since_head_pid_src_map ) ?,
    ViewNodeKind::PartnerCol (role)
      if role . relation_member_role () . is_some () =>
      reconcile_relation_col_children (
        treeid, tree, *role, context . source_diffs,
        context . env, context . graph_snap,
        context . deleted_since_head_pid_src_map ) ?,
    // TODO/DONE/local-view-update/plan_v2.org §9 reversal (#3): the IDCol/AliasCol diff scaffolds are created inline by
    // process_truenode_diff at the owner's BFS visit, so their reconcilers must
    // see the real diffs (source_diffs) or they would clobber the just-created
    // diff entries. Diffs flow inline for both de-novo and post-save.
    ViewNodeKind::QualCol (QualCol::Alias) =>
      super::reconcile::aliascol::reconcile_alias_col_children (
        tree, treeid, context . source_diffs, &context . env . config ) ?,
    ViewNodeKind::QualCol (QualCol::ID) =>
      super::reconcile::id_col::reconcile_id_col_children (
        treeid, tree, context . source_diffs, &context . env . config ) ?,
    ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
      // TODO/DONE/local-view-update/plan_v2.org §6.4/§6.6/§16: an Inactive node deleted by this save becomes Deleted,
      // parallel to the Normal-node deletion in expand_true_content_at_truenode.
      convert_inactive_to_deleted_if_deleted (
        tree, treeid, context . deleted_by_this_save_pids ) ?,
    _ => {
      // No-op for: Unknown (unresolvable-id placeholder), Deleted,
      // DeadScaffold, Qual leaves, BufferRoot, and DiffPhantom (a diff-only
      // placeholder, inert here, TODO/DONE/local-view-update/plan_v2.org §6.3). The prune sweep handles empty/dead nodes.
    } }
  Ok(( )) }

/// The Normal-node visit (TODO/DONE/local-view-update/plan_v2.org §6.1/§6.2): settle the Finalizable state
/// for a definitive-view request *before* drawing content (so a Final node
/// can cascade), reconcile content, run the remaining (non-Definitive) view
/// requests, ensure a definitive subscribee's HiddenInSubscribeeCol, and
/// finally (in diff mode) compute this node's diff inline. The BFS reaches
/// every col/child this creates and reconciles it in turn.
async fn visit_normal_node (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let had_dvr : bool =
    read_at_node_in_tree ( tree, treeid,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t)) =>
          t . view_requests . contains (& ViewRequest::Definitive),
        _ => false } ) ?;
  let mut settled : bool = false; // TODO/DONE/local-view-update/plan_v2.org §5.2 draw rule already ran
  let mut cascade : bool = false; // node is Final -> hand DVRs to children
  // TODO/DONE/local-view-update/plan_v2.org §5.5: the budget counts vognode *expansions* (each costs 1, charged in
  // expand_true_content_at_truenode); once it hits 0 every later vognode is left
  // indefinitive -- a visible, collapsed headline. We never truncate a group
  // mid-way (whole groups already drawn keep all their members); we only stop
  // STARTING new expansions. EXCEPTION: a view root (child of the BufferRoot) is
  // the node the user explicitly opened, so it always expands -- never truncated.
  // (The ancestor read is short-circuited to only run when the budget is spent.)
  if context . node_budget == 0
    && ! read_at_ancestor_in_tree ( tree, treeid, 1,
           |vn : &ViewNode| matches! ( &vn . kind, ViewNodeKind::BufferRoot ) )
         . unwrap_or (false) {
    // Budget spent and this is not a view root: draw it indefinitive and expand
    // nothing under it; strip any DVR so it is not treated as Final. The content
    // engine (settled) then clobbers+returns.
    write_at_truenode_in_tree (
      tree, treeid,
      |t| { t . view_requests . remove (& ViewRequest::Definitive);
            t . indef_or_def = IndefOrDef::Indefinitive; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    settled = true;
  } else if had_dvr {
    match apply_definitive_draw_rule (
      tree, treeid, &context . env . config, context . defmap ) ? {
      DrawOutcome::Deferred => {
        // Deferred to an existing Final occurrence: the node is now
        // indefinitive; the content engine (settled) will clobber+return.
        settled = true; }
      DrawOutcome::MadeFinal => {
        settled = true; cascade = true; } } }
  expand_true_content_at_truenode (
    treeid, tree, context . defmap,
    &context . env . config, context . graph_snap,
    context . deleted_since_head_pid_src_map,
    context . deleted_by_this_save_pids,
    context . active_source_set,
    settled, cascade, &mut context . node_budget ) ?;
  // The steps below apply only while the node is still a Normal vognode:
  // content reconcile may have converted it to Deleted (a node this save
  // deleted). The flip to a DiffPhantom happens at the END of this visit
  // (process_truenode_diff, below), after content + cols + view requests.
  let still_normal : bool =
    read_at_node_in_tree ( tree, treeid,
      |vn : &ViewNode| matches! ( &vn . kind,
        ViewNodeKind::Vognode (Vognode::Normal (_)) ) ) ?;
  if ! still_normal { return Ok (( )); }
  // Phase 8 (TODO/DONE/local-view-update/plan_v2.org §13): for a DE-NOVO render, create this node's relation cols the
  // way build_node_branch_minus_content does at node birth, so the one view
  // completion can expand a bare stub. Only CONTENT nodes/roots get them (a sharing-col
  // MEMBER is rendered bare), so gate on "parent is not a PartnerCol";
  // maybe_add_relation_col_branches is itself idempotent + skips empties. OFF
  // for post-save (flag false), keeping that path byte-identical.
  if context . create_relation_cols_for_fresh_nodes {
    let parent_is_partner_col : bool =
      read_at_ancestor_in_tree ( tree, treeid, 1,
        |vn : &ViewNode| matches! ( &vn . kind,
          ViewNodeKind::PartnerCol (_) ) )
      . unwrap_or (false);
    if ! parent_is_partner_col {
      maybe_add_relation_col_branches (
        tree, treeid, &context . env . config,
        &context . env . driver ) . await ?; } }
  // Remaining view requests (Aliases / Containerward / Sourceward); the
  // Definitive request was already consumed by apply_definitive_draw_rule.
  super::reconcile::view_requests::execute_truenode_view_requests (
    treeid, tree, &context . env . config, &context . env . driver,
    context . errors, context . active_source_set ) . await ?;
  // Ensure a definitive subscribee's HiddenInSubscribeeCol exists; the BFS
  // reconciles it on reaching it.
  super::reconcile::view_requests::ensure_hiddenin_col_under_definitive_subscribee (
    tree, treeid, &context . env . config, &context . env . driver ) . await ?;
  // TODO/DONE/local-view-update/plan_v2.org §9 reversal (#3 / Jeff): compute this node's content+scaffold diff LOCALLY,
  // at its own BFS visit. Runs last, after the node is fully completed as a
  // worktree Normal node (content, cols, view requests), so process_truenode_diff
  // sees its final children. The flip to a phantom happens here; the node's cols
  // (visited later, level-order) self-deaden via their own generalized-orphan
  // check. Gated on diff mode (source_diffs = Some); both de-novo and post-save
  // feed the real diffs here, so the diff is computed inline for both.
  if let Some (real_diffs) = context . source_diffs {
    let node_mut : NodeMut<ViewNode> =
      tree . get_mut (treeid) . unwrap ();
    process_truenode_diff (
      node_mut, real_diffs,
      context . deleted_since_head_pid_src_map,
      context . diff_tantivy_index,
      &context . env . config )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  Ok(( )) }

/// Whether a node of this kind is *self-deletable when empty* (TODO/DONE/local-view-update/plan_v2.org §3.4): the
/// single postorder prune sweep removes it once it is childless. This is the
/// one place self-deletion lives -- per-kind reconcilers no longer detach
/// themselves when empty.
/// - DeadScaffold and a childless Vognode::Deleted (TODO/DONE/local-view-update/plan_v2.org §6.6).
/// - the read-only relation cols Subscriber/Overrider/Hider/Hidden (a graph
///   relationship the user cannot edit from this side, so an emptied one is
///   just noise) -- but NOT Overridden (the editable interface for adding
///   overrides, preserved when empty like AliasCol).
/// - HiddenInSubscribeeCol / HiddenOutsideOfSubscribeeCol.
/// - QualCol::ID (largely moot -- an IDCol always holds at least the PID).
/// PRESERVED when empty (so NOT here): PartnerCol(Subscribee), PartnerCol
/// (Overridden), QualCol(Alias) -- each an *editable interface onto the origin*
/// that the user must be able to refill, removed only by hand (TODO/DONE/local-view-update/plan_v2.org §3.4 exception).
fn is_self_deletable_when_empty (
  kind : &ViewNodeKind,
) -> bool {
  matches! ( kind,
    ViewNodeKind::DeadScaffold
    | ViewNodeKind::Vognode (Vognode::Deleted (_))
    | ViewNodeKind::PartnerCol (RoleCol::Subscriber)
    | ViewNodeKind::PartnerCol (RoleCol::Overrider)
    | ViewNodeKind::PartnerCol (RoleCol::Hider)
    | ViewNodeKind::PartnerCol (RoleCol::Hidden)
    | ViewNodeKind::PartnerCol (RoleCol::HiddenInSubscribee)
    | ViewNodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee)
    | ViewNodeKind::QualCol (QualCol::ID) ) }

/// The TODO/DONE/local-view-update/plan_v2.org §3.4 postorder prune sweep -- the *one* place self-deletion happens.
/// Removes, bottom-up, every childless `is_self_deletable_when_empty` node:
/// deadened cols whose members all died, a TODO/DONE/local-view-update/plan_v2.org §6.6 childless Deleted, and empty
/// read-only relation / Hidden* / ID cols. Postorder so a chain (e.g.
/// Dead -> Deleted, or a col emptied by the removal of its last member)
/// collapses completely in one pass. Focus is transferred to the surviving
/// parent if the removed node held it. Never prunes a *view root* (child of the
/// invisible BufferRoot): the user should still see a deleted root.
fn prune_self_deletable_when_empty (
  tree : &mut Tree<ViewNode>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_nodeids (
      tree,
      |vn| is_self_deletable_when_empty (&vn . kind) ) ?;
  for treeid in nodes {
    let node = match tree . get (treeid) {
      Some (node) => node,
      None => continue };
    let has_children : bool =
      node . children () . next () . is_some ();
    let parent_is_buffer_root : bool =
      node . parent ()
      . map ( |p| matches! ( &p . value () . kind,
                             ViewNodeKind::BufferRoot ) )
      . unwrap_or (false);
    if ! has_children && ! parent_is_buffer_root {
      detach_scaffold_transferring_focus (tree, treeid) ?; }
  }
  Ok (( )) }

/// TODO/DONE/local-view-update/plan_v2.org §6.4/§6.6/§16: convert an Inactive node whose pid is in
/// `deleted_by_this_save_pids` into a Deleted node, mirroring the Normal-node
/// conversion in expand_true_content_at_truenode. An InactiveNode carries no
/// title/body of its own (its .skg is now gone), so the Deleted placeholder is
/// built with an empty title. Inert thereafter; the TODO/DONE/local-view-update/plan_v2.org §6.6 prune removes it if it
/// ends childless.
fn convert_inactive_to_deleted_if_deleted (
  tree                      : &mut Tree<ViewNode>,
  treeid                    : NodeId,
  deleted_by_this_save_pids : &HashSet<ID>,
) -> Result<(), Box<dyn Error>> {
  let to_delete : Option<(ID, SourceName)> =
    read_at_node_in_tree ( tree, treeid,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Inactive (i))
          if deleted_by_this_save_pids . contains (&i . id) =>
            Some (( i . id . clone (), i . source . clone () )),
        _ => None } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  if let Some ((id, source)) = to_delete {
    write_at_node_in_tree ( tree, treeid,
      |vn : &mut ViewNode| {
        vn . kind = ViewNodeKind::Vognode ( Vognode::Deleted (
          DeletedNode { id, source,
                        title : String::new (), body : None } )); } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  Ok (( )) }

fn collect_matching_nodeids<Predicate> (
  tree      : &Tree<ViewNode>,
  predicate : Predicate,
) -> Result<Vec<NodeId>, Box<dyn Error>>
where Predicate : Fn (&ViewNode) -> bool {
  let root_treeid : NodeId = tree . root () . id ();
  let mut result : Vec<NodeId> = Vec::new ();
  do_everywhere_in_tree_dfs_readonly (
    tree, root_treeid,
    false, // postorder: a chain (Dead -> Deleted, or a col emptied by the
           // removal of its last child) collapses bottom-up in one sweep.
    &mut |node| {
      if predicate (node . value ()) {
        result . push (node . id ()); }
      Ok (( )) } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (result) }

#[cfg(test)]
#[path = "../../tests/unit/complete.rs"]
mod tests;
