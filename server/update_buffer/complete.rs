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
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs, do_everywhere_in_tree_dfs_readonly, read_at_node_in_tree};
use crate::types::viewnode::{ViewNode, ViewNodeKind, RoleCol, ViewRequest, IndefOrDef};
use crate::types::viewnode::{Vognode, QualCol};
use super::complete_postorder::hiddeninsubscribee_col::reconcile_hiddenin_subscribee_col_children;
use super::complete_postorder::hiddenoutsideof_subscribeecol::reconcile_hiddenoutside_subscribee_col_children;
use super::complete_preorder::relation_col::reconcile_relation_col_children;
use super::complete_preorder::subscribee_col::reconcile_subscribee_col_children;
use super::complete_preorder::truenode::{ expand_true_content_at_truenode, maybe_prepend_diff_view_scaffolds};

use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::sync::Arc;

pub(super) struct CompletionContext<'a> {
  pub(super) defmap                         : &'a mut DefinitiveMap,
  pub(super) source_diffs                   : &'a Option<HashMap<SourceName, SourceDiff>>,
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
}

pub(super) async fn complete_viewforest (
  viewforest : &mut Tree<ViewNode>,
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = viewforest . root () . id ();
  scaffolds_with_deadOrDeleted_parents_become_dead (viewforest) ?;
  // The single plan_v2 §3 level-order BFS: each node's visit
  // (expand_true_content_at_node) does ALL of its own update -- content
  // reconcile, the §5.2 draw rule + §5.3 cascade for definitive-view
  // requests, diff scaffolds, the other view requests, ensuring a definitive
  // subscribee's HiddenInSubscribeeCol, and per-col reconciliation -- so the
  // former fixed sequence of postorder passes is gone. Cols and content
  // children created during a node's visit are reached later in the same BFS.
  expand_true_content_until_stable (
    viewforest, root_treeid, context ) . await ?;
  scaffolds_with_deadOrDeleted_parents_become_dead (viewforest) ?;
  remove_empty_deleted_scaffolds (viewforest) ?;
  Ok(( )) }

/// Convert to DeletedScaff every scaffold node
/// whose parent is deleted (be it scaffold or truenode).
///
/// Completion can encounter this in two ways:
/// - The incoming view already had scaffolds
///   beneath a node that has since become Deleted.
/// - An earlier completion phase may add or preserve scaffolds
///   before a later pass has cleaned up the deleted subtree.
fn scaffolds_with_deadOrDeleted_parents_become_dead (
  tree : &mut Tree<ViewNode>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = tree . root () . id ();
  do_everywhere_in_tree_dfs (
    tree, root_treeid, true,
    &mut |mut node| -> Result<(), String> {
      let is_scaffold : bool =
        ! matches! ( &node . value () . kind,
                      ViewNodeKind::Vognode(_));
      if is_scaffold {
        let parent_is_dead_or_deleted : bool =
          node . parent ()
          . map ( |mut p| matches! ( &p . value () . kind,
            ViewNodeKind::Vognode (Vognode::Deleted (_)) |
            ViewNodeKind::DeadScaffold ))
          . unwrap_or (false);
        if parent_is_dead_or_deleted {
          node . value () . kind =
            ViewNodeKind::DeadScaffold; }}
      Ok (( )) } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) }

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
  match &kind {
    ViewNodeKind::Vognode (Vognode::Normal (_)) =>
      visit_normal_node (tree, treeid, context) . await ?,
    ViewNodeKind::PartnerCol (RoleCol::Subscribee) =>
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
    ViewNodeKind::QualCol (QualCol::Alias) =>
      super::complete_postorder::aliascol::reconcile_alias_col_children (
        tree, treeid, context . source_diffs, &context . env . config ) ?,
    ViewNodeKind::QualCol (QualCol::ID) =>
      super::complete_postorder::id_col::reconcile_id_col_children (
        treeid, tree, context . source_diffs, &context . env . config ) ?,
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
  // content reconcile may have converted it to Deleted, or set_diff_status
  // to a DiffPhantom.
  let still_normal : bool =
    read_at_node_in_tree ( tree, treeid,
      |vn : &ViewNode| matches! ( &vn . kind,
        ViewNodeKind::Vognode (Vognode::Normal (_)) ) ) ?;
  if ! still_normal { return Ok (( )); }
  // Diff scaffolds for a definitive, non-phantom node (was ensure_diff_scaffolds).
  let ready : Option<(ID, SourceName)> =
    read_at_node_in_tree ( tree, treeid,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t))
          if ! t . should_be_phantom () && ! t . is_indefinitive () =>
            Some (( t . id . clone (), t . source . clone () )),
        _ => None } ) ?;
  if let Some ((pid, source)) = ready {
    maybe_prepend_diff_view_scaffolds (
      tree, treeid, context . source_diffs, &pid, &source ) ?; }
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

fn remove_empty_deleted_scaffolds (
  tree : &mut Tree<ViewNode>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_nodeids (
      tree, false,
      |vn| matches! ( &vn . kind,
        ViewNodeKind::DeadScaffold )) ?;
  for treeid in nodes {
    let has_children : bool =
      match tree . get (treeid) {
        Some (node) => node . children () . next () . is_some (),
        None => continue };
    if ! has_children {
      tree . get_mut (treeid) . unwrap () . detach (); }
  }
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
