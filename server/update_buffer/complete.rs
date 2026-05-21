// MANUAL RECURSION:
// This uses manual async recursions, rather than calls to
// `do_everywhere_in_tree_dfs`, because some dispatch targets
// are async, and `do_everywhere_in_tree_dfs` takes a sync
// `FnMut` closure which cannot `.await`.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::util::DefinitiveMap;
use crate::types::env::SkgEnv;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs, do_everywhere_in_tree_dfs_readonly};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, ScaffoldKind};
use super::complete_preorder::subscribee_col::reconcile_subscribee_col_children;
use super::complete_preorder::truenode::expand_true_content_at_truenode;

use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

pub(super) struct CompletionContext<'a> {
  pub(super) defmap                         : &'a mut DefinitiveMap,
  pub(super) source_diffs                   : &'a Option<HashMap<SourceName, SourceDiff>>,
  pub(super) env                            : &'a SkgEnv,
  pub(super) graph_snap                     : &'a Arc<InRustGraph>,
  pub(super) errors                         : &'a mut Vec<String>,
  pub(super) deleted_since_head_pid_src_map : &'a HashMap<ID, SourceName>,
  pub(super) deleted_by_this_save_pids      : &'a HashSet<ID>,
  pub(super) is_saved_view                  : bool,
}

pub(super) async fn complete_viewforest (
  viewforest : &mut Tree<ViewNode>,
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = viewforest . root () . id ();
  scaffolds_with_deleted_parents_become_deletedScaff (viewforest) ?;
  expand_true_content_until_stable (
    viewforest, root_treeid, context ) . await ?;
  scaffolds_with_deleted_parents_become_deletedScaff (viewforest) ?;
  ensure_diff_scaffolds (viewforest, context) ?;
  let postorder_true_nodes : Vec<NodeId> =
    collect_matching_nodeids (
      viewforest, false,
      |vn| matches! ( &vn . kind, ViewNodeKind::True (_) )) ?;
  execute_truenode_view_requests (
    viewforest, context, &postorder_true_nodes ) . await ?;
  ensure_hiddenin_cols_under_definitive_subscribees (
    viewforest, context, &postorder_true_nodes ) . await ?;
  reconcile_alias_cols (viewforest, context) ?;
  reconcile_id_cols (viewforest, context) ?;
  reconcile_hiddenin_cols (viewforest, context) ?;
  reconcile_hiddenoutside_cols (viewforest, context) ?;
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
fn scaffolds_with_deleted_parents_become_deletedScaff (
  tree : &mut Tree<ViewNode>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = tree . root () . id ();
  do_everywhere_in_tree_dfs (
    tree, root_treeid, true,
    &mut |mut node| -> Result<(), String> {
      let scaffold_kind : Option<ScaffoldKind> =
        match &node . value () . kind {
          ViewNodeKind::Scaff (s) => Some (s . kind ()),
          _ => None };
      if let Some (kind) = scaffold_kind {
        let parent_is_deleted : bool =
          node . parent ()
          . map ( |mut p| matches! ( &p . value () . kind,
            ViewNodeKind::Deleted (_) |
            ViewNodeKind::DeletedScaff (_) ))
          . unwrap_or (false);
        if parent_is_deleted {
          node . value () . kind =
            ViewNodeKind::DeletedScaff (kind); }}
      Ok (( )) } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) }

/// Parent-first content expansion. This remains a narrow manual async
/// recursion because a parent can insert content or subscribee children
/// that must be completed before later passes run.
fn expand_true_content_until_stable<'a> (
  tree    : &'a mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &'a mut CompletionContext<'_>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin ( async move {
    expand_true_content_at_node (tree, treeid, context) . await ?;
    { // Recursion boilerplate. See 'MANUAL RECURSION' comment at top of file.
      let child_treeids : Vec<NodeId> =
        tree . get (treeid) . unwrap ()
        . children () . map ( |c| c . id () ) . collect ();
      for child_treeid in child_treeids {
        expand_true_content_until_stable (
          tree, child_treeid, context ) . await ?; }}
    Ok(( )) }) }

async fn expand_true_content_at_node (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get (treeid) . unwrap () . value () . kind . clone ();
  if matches!( kind, ViewNodeKind::True (_)) {
    expand_true_content_at_truenode (
      treeid, tree, context . defmap, context . source_diffs,
      &context . env . config, context . graph_snap,
      context . deleted_since_head_pid_src_map,
      context . deleted_by_this_save_pids,
      context . is_saved_view ) ?;
  } else if matches!( kind,
      ViewNodeKind::Scaff (Scaffold::SubscribeeCol) ) {
        reconcile_subscribee_col_children (
          treeid, tree, context . source_diffs, context . env,
          context . deleted_since_head_pid_src_map
        ) . await ?;
  } else if matches!( kind, ViewNodeKind::Unknown (_) ) {
    // No-op: Unknown is a placeholder for an unresolvable id and
    // has no completion to do. Listed explicitly so a reader sees
    // the variant covered.
  }
  // No-op for: Deleted, DeletedScaff (whose parent is not
  // Deleted/DeletedScaff -- occurs on re-save of a buffer
  // containing them).
  Ok(( )) }

fn ensure_diff_scaffolds (
  tree    : &mut Tree<ViewNode>,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let true_nodes : Vec<NodeId> =
    collect_matching_nodeids (
      tree, true,
      |vn| matches! ( &vn . kind, ViewNodeKind::True (_) )) ?;
  for treeid in true_nodes {
    super::complete_preorder::truenode::
    ensure_diff_scaffolds_for_truenode (
      treeid, tree, context . source_diffs ) ?; }
  Ok (( )) }

async fn execute_truenode_view_requests (
  tree       : &mut Tree<ViewNode>,
  context    : &mut CompletionContext<'_>,
  true_nodes : &[NodeId],
) -> Result<(), Box<dyn Error>> {
  for treeid in true_nodes {
    super::complete_postorder::truenode::
    execute_truenode_view_requests (
      *treeid, tree, context . defmap, context . source_diffs,
      &context . env . config, &context . env . driver,
      context . errors, context . deleted_since_head_pid_src_map )
    . await ?; }
  Ok (( )) }

async fn ensure_hiddenin_cols_under_definitive_subscribees (
  tree       : &mut Tree<ViewNode>,
  context    : &CompletionContext<'_>,
  true_nodes : &[NodeId],
) -> Result<(), Box<dyn Error>> {
  for treeid in true_nodes {
    super::complete_postorder::truenode::
    ensure_hiddenin_col_under_definitive_subscribee (
      tree, *treeid, &context . env . config,
      &context . env . driver ) . await ?; }
  Ok (( )) }

fn reconcile_alias_cols (
  tree    : &mut Tree<ViewNode>,
  context : &CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_nodeids (
      tree, false,
      |vn| matches! ( &vn . kind,
        ViewNodeKind::Scaff (Scaffold::AliasCol) )) ?;
  for treeid in nodes {
      super::complete_postorder::aliascol::
      reconcile_alias_col_children (
        tree, treeid, context . source_diffs, &context . env . config ) ?;
  }
  Ok (( )) }

fn reconcile_id_cols (
  tree    : &mut Tree<ViewNode>,
  context : &CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_nodeids (
      tree, false,
      |vn| matches! ( &vn . kind,
        ViewNodeKind::Scaff (Scaffold::IDCol) )) ?;
  for treeid in nodes {
    super::complete_postorder::id_col::
    reconcile_id_col_children (
      treeid, tree, context . source_diffs,
      &context . env . config ) ?; }
  Ok (( )) }

fn reconcile_hiddenin_cols (
  tree    : &mut Tree<ViewNode>,
  context : &CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_nodeids (
      tree, false,
      |vn| matches! ( &vn . kind,
        ViewNodeKind::Scaff (Scaffold::HiddenInSubscribeeCol) )) ?;
  for treeid in nodes {
      super::complete_postorder::hiddeninsubscribee_col::
      reconcile_hiddenin_subscribee_col_children (
        treeid, tree, context . source_diffs, context . env,
        context . deleted_since_head_pid_src_map ) ?; }
  Ok (( )) }

fn reconcile_hiddenoutside_cols (
  tree    : &mut Tree<ViewNode>,
  context : &CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_nodeids (
      tree, false,
      |vn| matches! ( &vn . kind,
        ViewNodeKind::Scaff (
          Scaffold::HiddenOutsideOfSubscribeeCol) )) ?;
  for treeid in nodes {
    super::complete_postorder::hiddenoutsideof_subscribeecol::
    reconcile_hiddenoutside_subscribee_col_children (
      treeid, tree, context . source_diffs, context . env,
      context . deleted_since_head_pid_src_map ) ?; }
  Ok (( )) }

fn remove_empty_deleted_scaffolds (
  tree : &mut Tree<ViewNode>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_nodeids (
      tree, false,
      |vn| matches! ( &vn . kind,
        ViewNodeKind::DeletedScaff (_) )) ?;
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
