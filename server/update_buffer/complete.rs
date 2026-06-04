// MANUAL RECURSION:
// This uses manual async recursions, rather than calls to
// `do_everywhere_in_tree_dfs`, because some dispatch targets
// are async, and `do_everywhere_in_tree_dfs` takes a sync
// `FnMut` closure which cannot `.await`.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::source_sets::ActiveSourceSet;
use crate::to_org::expand::definitive::{ apply_definitive_draw_rule, extendDefinitiveSubtree_fromGit, DrawOutcome};
use crate::to_org::util::DefinitiveMap;
use crate::types::env::SkgEnv;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs_readonly, read_at_node_in_tree, read_at_ancestor_in_tree, write_at_node_in_tree};
use crate::to_org::complete::sharing::maybe_add_relation_col_branches;
use crate::update_buffer::ancestry::{ col_is_generalized_orphan, deaden_generalized_orphan_col, is_col_kind};
use crate::update_buffer::util::detach_scaffold_transferring_focus;
use crate::types::viewnode::{ViewNode, ViewNodeKind, RoleCol, ViewRequest, IndefOrDef, DeletedNode};
use crate::types::viewnode::{Vognode, QualCol};
use super::complete_postorder::hiddeninsubscribee_col::reconcile_hiddenin_subscribee_col_children;
use super::complete_postorder::hiddenoutsideof_subscribeecol::reconcile_hiddenoutside_subscribee_col_children;
use super::complete_preorder::relation_col::reconcile_relation_col_children;
use super::complete_preorder::subscribee_col::reconcile_subscribee_col_children;
use super::complete_preorder::truenode::expand_true_content_at_truenode;

use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::sync::Arc;

pub(super) struct CompletionContext<'a> {
  pub(super) defmap                         : &'a mut DefinitiveMap,
  /// Diffs for the *content/scaffold* path. Phase 5 (§9) runs the main BFS
  /// content+scaffold update with this set to None, so the BFS produces the
  /// pure worktree view and the diff overlay (apply_diff_to_viewforest) adds
  /// content axes, content phantoms, and TextChanged/IDCol/AliasCol scaffolds
  /// afterward.
  pub(super) source_diffs                   : &'a Option<HashMap<SourceName, SourceDiff>>,
  /// Diffs for the *sharing cols* (Subscribee / HiddenIn / HiddenOut /
  /// relation). These reconcile their removed-member phantoms *inline*, while
  /// the col's members are still Normal (before the overlay flips any to
  /// DiffPhantom) -- the overlay does not descend through cols (§9 gap, see
  /// plan_v2 §18). Set to the real diffs even when `source_diffs` is None.
  pub(super) sharing_diffs                  : &'a Option<HashMap<SourceName, SourceDiff>>,
  pub(super) env                            : &'a SkgEnv,
  pub(super) graph_snap                     : &'a Arc<InRustGraph>,
  pub(super) errors                         : &'a mut Vec<String>,
  pub(super) deleted_since_head_pid_src_map : &'a HashMap<ID, SourceName>,
  pub(super) deleted_by_this_save_pids      : &'a HashSet<ID>,
  pub(super) active_source_set              : Option<&'a ActiveSourceSet>,
  pub(super) is_saved_view                  : bool,
  /// §5.5 per-buffer node limit: the remaining budget of *new* ViewNodes
  /// the ordinary update pass may create. Initialized once per rerender to
  /// `config.initial_node_limit` and decremented as content children are
  /// created; when it reaches 0 no further new content is drawn (and the
  /// cascade stops). Unifies what was extendDefinitiveSubtreeFromLeaf's
  /// per-DVR limit. The diff overlay is exempt (§5.5).
  pub(super) node_budget                    : usize,
  /// Phase 8 (§13): true only for a DE-NOVO (initial) render driven through
  /// this driver. When set, a fresh CONTENT node (parent is not a PartnerCol)
  /// gets its relation cols created at its visit, the way
  /// build_node_branch_minus_content does at node birth in the old de-novo BFS.
  /// FALSE for post-save rerender, where relation cols are already present in
  /// the saved buffer and re-creating them would change the buffer and break the
  /// save round-trip (plan_v2 §18). So post-save stays byte-identical.
  pub(super) create_relation_cols_for_fresh_nodes : bool,
}

pub(super) async fn complete_viewforest (
  viewforest : &mut Tree<ViewNode>,
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = viewforest . root () . id ();
  // The single plan_v2 §3 level-order BFS: each node's visit
  // (expand_true_content_at_node) does ALL of its own update -- content
  // reconcile, the §5.2 draw rule + §5.3 cascade for definitive-view
  // requests, diff scaffolds, the other view requests, ensuring a definitive
  // subscribee's HiddenInSubscribeeCol, and per-col reconciliation -- so the
  // former fixed sequence of postorder passes is gone. Cols and content
  // children created during a node's visit are reached later in the same BFS.
  expand_true_content_until_stable (
    viewforest, root_treeid, context ) . await ?;
  prune_self_deletable_when_empty (viewforest) ?;
  Ok(( )) }

/// Parent-first content expansion, in *level order* (BFS).
///
/// This is the first step of the plan_v2 §3 driver: a single top-down
/// level-order traversal in which each node, when visited, may create
/// view-descendants (content children, subscribee/relation members) that
/// are then visited later in the same traversal. A FIFO queue (rather than
/// the previous DFS-preorder recursion) is what makes the §5.5 node-limit
/// and §5.2 draw-rule semantics order-correct: the first occurrence of a
/// duplicate id reached in BFS order -- not in depth-first order -- is the
/// one that wins. The Finalizable draw rule (commit fb7786b) makes this
/// reorder safe (plan_v2 §18). A node's expansion depends only on its
/// ancestors and the graph, never on siblings or descendants, so it can be
/// processed the moment it is dequeued.
async fn expand_true_content_until_stable (
  tree     : &mut Tree<ViewNode>,
  root_treeid : NodeId,
  context  : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let mut queue : VecDeque<NodeId> = VecDeque::new ();
  queue . push_back (root_treeid);
  while let Some (treeid) = queue . pop_front () {
    expand_true_content_at_node (tree, treeid, context) . await ?;
    // Enqueue the node's *current* children -- including any this visit
    // just created -- so they are completed after every node already in
    // their level. (See 'MANUAL RECURSION' comment at top of file: the
    // dispatch is async, so we hand-roll the traversal.)
    if let Some (node) = tree . get (treeid) {
      for child in node . children () {
        queue . push_back (child . id ()); }}}
  Ok(( )) }

/// One BFS visit: dispatch on (kind, parent-kind) to the node's update rule
/// (plan_v2 §3/§4). Cols are reconciled at their own visit (the BFS reaches a
/// col after its Normal parent created it), so the former separate col-sweep
/// passes are gone.
async fn expand_true_content_at_node (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get (treeid) . unwrap () . value () . kind . clone ();
  // Death-leafward (plan propagate-death-leafward §5): before reconciling any
  // col, self-check its required ancestry. If it is a generalized orphan (some
  // required ancestor is the wrong viewnode kind -- full chain, not just the
  // parent), deaden it and dispose its children instead of reconciling, so the
  // reconcile never reads a since-deleted ancestor's missing NodeComplete.
  // This subsumes the old scaffolds_with_deadOrDeleted_parents_become_dead
  // sweep (immediate-parent only): the BFS visits every col, and the ancestry
  // check is strictly more powerful.
  if is_col_kind (&kind)
    && col_is_generalized_orphan (tree, treeid) ? {
    deaden_generalized_orphan_col (tree, treeid) ?;
    return Ok (( )); }
  match &kind {
    ViewNodeKind::Vognode (Vognode::Normal (_)) =>
      visit_normal_node (tree, treeid, context) . await ?,
    ViewNodeKind::PartnerCol (RoleCol::Subscribee) =>
      reconcile_subscribee_col_children (
        treeid, tree, context . sharing_diffs, context . env,
        context . deleted_since_head_pid_src_map ) . await ?,
    ViewNodeKind::PartnerCol (RoleCol::HiddenInSubscribee) =>
      reconcile_hiddenin_subscribee_col_children (
        treeid, tree, context . sharing_diffs, context . env,
        context . deleted_since_head_pid_src_map ) ?,
    ViewNodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee) =>
      reconcile_hiddenoutside_subscribee_col_children (
        treeid, tree, context . sharing_diffs, context . env,
        context . deleted_since_head_pid_src_map ) ?,
    ViewNodeKind::PartnerCol (role)
      if role . relation_member_role () . is_some () =>
      reconcile_relation_col_children (
        treeid, tree, *role, context . sharing_diffs,
        context . env, context . graph_snap,
        context . deleted_since_head_pid_src_map ) ?,
    ViewNodeKind::QualCol (QualCol::Alias) =>
      super::complete_postorder::aliascol::reconcile_alias_col_children (
        tree, treeid, context . source_diffs, &context . env . config ) ?,
    ViewNodeKind::QualCol (QualCol::ID) =>
      super::complete_postorder::id_col::reconcile_id_col_children (
        treeid, tree, context . source_diffs, &context . env . config ) ?,
    ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
      // §6.4/§6.6/§16: an Inactive node deleted by this save becomes Deleted
      // (current code converted only Normal nodes).
      convert_inactive_to_deleted_if_deleted (
        tree, treeid, context . deleted_by_this_save_pids ) ?,
    _ => {
      // No-op for: Unknown (unresolvable-id placeholder), Deleted,
      // DeadScaffold, Qual leaves, BufferRoot, DiffPhantom (owned by the
      // diff overlay, §6.3). The prune sweep handles empty/dead nodes.
    } }
  Ok(( )) }

/// The Normal-node visit (plan_v2 §6.1/§6.2): settle the Finalizable state
/// for a definitive-view request *before* drawing content (so a Final node
/// can cascade), reconcile content, then -- while still a Normal node --
/// emit diff scaffolds, run the remaining (non-Definitive) view requests,
/// and ensure a definitive subscribee's HiddenInSubscribeeCol. The BFS
/// reaches every col/child this creates and reconciles it in turn.
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
  let mut settled    : bool = false; // §5.2 draw rule already ran
  let mut cascade    : bool = false; // node is Final -> hand DVRs to children
  let mut do_content : bool = true;
  if had_dvr && context . node_budget == 0 {
    // §5.5: budget exhausted -- strip the (user or cascade) DVR and leave the
    // node indefinitive instead of making it Final. The content engine
    // (settled) then clobbers+returns, drawing no further content. This is
    // the BFS-order truncation: earlier siblings expand, later ones stay
    // indefinitive once the budget is spent.
    crate::types::tree::viewnode_nodecomplete::write_at_truenode_in_tree (
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
      DrawOutcome::MadeFinal { is_removed_node : true, hidden_ids } => {
        // Removed-file DVR: keep the git-phantom self-expansion (diff
        // overlay territory, §9). Effectively unreachable in the post-save
        // pass -- removed-file nodes arrive as DiffPhantoms -- but handled
        // defensively; the content engine is skipped.
        extendDefinitiveSubtree_fromGit (
          tree, treeid, context . env . config . initial_node_limit,
          context . defmap, context . source_diffs, &context . env . config,
          &hidden_ids, &context . env . driver,
          context . deleted_since_head_pid_src_map,
          context . active_source_set ) . await ?;
        settled = true; do_content = false; }
      DrawOutcome::MadeFinal { is_removed_node : false, .. } => {
        settled = true; cascade = true; } } }
  if do_content {
    expand_true_content_at_truenode (
      treeid, tree, context . defmap, context . source_diffs,
      &context . env . config, context . graph_snap,
      context . deleted_since_head_pid_src_map,
      context . deleted_by_this_save_pids,
      context . active_source_set, context . is_saved_view,
      settled, cascade, &mut context . node_budget ) ?; }
  // The steps below apply only while the node is still a Normal vognode:
  // content reconcile may have converted it to Deleted (a node this save
  // deleted). (It is no longer flipped to a DiffPhantom here -- that is the
  // post-BFS diff overlay's job now, §9.)
  let still_normal : bool =
    read_at_node_in_tree ( tree, treeid,
      |vn : &ViewNode| matches! ( &vn . kind,
        ViewNodeKind::Vognode (Vognode::Normal (_)) ) ) ?;
  if ! still_normal { return Ok (( )); }
  // Phase 8 (§13): for a DE-NOVO render, create this node's relation cols the
  // way build_node_branch_minus_content does at node birth, so the one driver
  // can expand a bare stub. Only CONTENT nodes/roots get them (a sharing-col
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
  // (Diff scaffolds -- TextChanged / IDCol / AliasCol -- are no longer emitted
  // here: phase 5 (§9) moved them onto the post-BFS diff overlay.)
  // Remaining view requests (Aliases / Containerward / Sourceward); the
  // Definitive request was already consumed by apply_definitive_draw_rule.
  super::complete_postorder::truenode::execute_truenode_view_requests (
    treeid, tree, context . defmap, context . source_diffs,
    &context . env . config, &context . env . driver,
    context . errors, context . deleted_since_head_pid_src_map,
    context . active_source_set ) . await ?;
  // Ensure a definitive subscribee's HiddenInSubscribeeCol (was
  // ensure_hiddenin_cols_under_definitive_subscribees); the BFS reconciles
  // it on reaching it.
  super::complete_postorder::truenode::ensure_hiddenin_col_under_definitive_subscribee (
    tree, treeid, &context . env . config, &context . env . driver ) . await ?;
  Ok(( )) }

/// Whether a node of this kind is *self-deletable when empty* (plan §3.4): the
/// single postorder prune sweep removes it once it is childless. This is the
/// one place self-deletion lives -- per-kind reconcilers no longer detach
/// themselves when empty.
/// - DeadScaffold and a childless Vognode::Deleted (§6.6).
/// - the read-only relation cols Subscriber/Overrider/Hider/Hidden (a graph
///   relationship the user cannot edit from this side, so an emptied one is
///   just noise) -- but NOT Overridden (the editable interface for adding
///   overrides, preserved when empty like AliasCol).
/// - HiddenInSubscribeeCol / HiddenOutsideOfSubscribeeCol.
/// - QualCol::ID (largely moot -- an IDCol always holds at least the PID).
/// PRESERVED when empty (so NOT here): PartnerCol(Subscribee), PartnerCol
/// (Overridden), QualCol(Alias) -- each an *editable interface onto the origin*
/// that the user must be able to refill, removed only by hand (§3.4 exception).
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

/// The §3.4 postorder prune sweep -- the *one* place self-deletion happens.
/// Removes, bottom-up, every childless `is_self_deletable_when_empty` node:
/// deadened cols whose members all died, a §6.6 childless Deleted, and empty
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
      tree, false,
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

/// §6.4/§6.6/§16: convert an Inactive node whose pid is in
/// `deleted_by_this_save_pids` into a Deleted node, mirroring the Normal-node
/// conversion in expand_true_content_at_truenode (which only ever handled
/// Normal). An InactiveNode carries no title/body of its own (its .skg is now
/// gone), so the Deleted placeholder is built with an empty title. Inert
/// thereafter; the §6.6 prune removes it if it ends childless.
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
  preorder  : bool,
  predicate : Predicate,
) -> Result<Vec<NodeId>, Box<dyn Error>>
where Predicate : Fn (&ViewNode) -> bool {
  let root_treeid : NodeId = tree . root () . id ();
  let mut result : Vec<NodeId> = Vec::new ();
  do_everywhere_in_tree_dfs_readonly (
    tree, root_treeid, preorder,
    &mut |node| {
      if predicate (node . value ()) {
        result . push (node . id ()); }
      Ok (( )) } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (result) }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::SourceName;
  use crate::types::viewnode::{ mk_inactive_viewnode, viewforest_root_viewnode };
  use crate::types::git::MembershipAxes;
  use ego_tree::Tree;

  // §6.4/§6.6/§16: an Inactive node whose pid this save deleted converts to a
  // Deleted node (so the prune sweep can later remove it if childless), while
  // an Inactive node NOT deleted by this save is left untouched.
  #[test]
  fn inactive_deleted_by_save_becomes_deleted () {
    let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = t . root () . id ();
    let inact : NodeId = t . get_mut (root) . unwrap () . append (
      mk_inactive_viewnode ( ID::from ("x"), SourceName::from ("main"),
                             MembershipAxes::default () ) ) . id ();
    let kept : NodeId = t . get_mut (root) . unwrap () . append (
      mk_inactive_viewnode ( ID::from ("y"), SourceName::from ("main"),
                             MembershipAxes::default () ) ) . id ();
    let deleted : HashSet<ID> =
      [ ID::from ("x") ] . into_iter () . collect ();

    convert_inactive_to_deleted_if_deleted (&mut t, inact, &deleted) . unwrap ();
    convert_inactive_to_deleted_if_deleted (&mut t, kept,  &deleted) . unwrap ();

    assert! ( matches! ( &t . get (inact) . unwrap () . value () . kind,
      ViewNodeKind::Vognode (Vognode::Deleted (d)) if d . id == ID::from ("x") ),
      "an Inactive node deleted by this save must become Deleted" );
    assert! ( matches! ( &t . get (kept) . unwrap () . value () . kind,
      ViewNodeKind::Vognode (Vognode::Inactive (_)) ),
      "an Inactive node NOT deleted by this save must be left untouched" ); }
}
