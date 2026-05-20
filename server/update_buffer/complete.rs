// MANUAL RECURSION:
// This uses manual async recursions, rather than calls to
// `do_everywhere_in_tree_dfs`, because some dispatch targets
// are async, and `do_everywhere_in_tree_dfs` takes a sync
// `FnMut` closure which cannot `.await`.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SourceName};
use crate::types::git::SourceDiff;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, ScaffoldKind};
use crate::to_org::util::DefinitiveMap;
use crate::types::tree::generic::{
  do_everywhere_in_tree_dfs,
  do_everywhere_in_tree_dfs_readonly};

use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

pub async fn complete_viewforest (
  viewforest                     : &mut Tree<ViewNode>,
  defmap                         : &mut DefinitiveMap,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  env                            : &SkgEnv,
  graph_snap                     : &Arc<InRustGraph>,
  errors                         : &mut Vec<String>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  deleted_by_this_save_pids      : &HashSet<ID>,
  is_saved_view                  : bool,
) -> Result<(), Box<dyn Error>> {
  let mut context : CompletionContext = CompletionContext {
    defmap,
    source_diffs,
    env,
    graph_snap,
    errors,
    deleted_since_head_pid_src_map,
    deleted_by_this_save_pids,
    is_saved_view,
  };
  complete_viewforest_with_context (viewforest, &mut context) . await }

struct CompletionContext<'a> {
  defmap                         : &'a mut DefinitiveMap,
  source_diffs                   : &'a Option<HashMap<SourceName, SourceDiff>>,
  env                            : &'a SkgEnv,
  graph_snap                     : &'a Arc<InRustGraph>,
  errors                         : &'a mut Vec<String>,
  deleted_since_head_pid_src_map : &'a HashMap<ID, SourceName>,
  deleted_by_this_save_pids      : &'a HashSet<ID>,
  is_saved_view                  : bool,
}

async fn complete_viewforest_with_context (
  viewforest : &mut Tree<ViewNode>,
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = viewforest . root () . id ();
  mark_scaffolds_under_deleted_branches (viewforest) ?;
  expand_true_content_until_stable (
    viewforest, root_treeid, context ) . await ?;
  mark_scaffolds_under_deleted_branches (viewforest) ?;
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

fn mark_scaffolds_under_deleted_branches (
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
          node . value () . kind = ViewNodeKind::DeletedScaff (kind); }}
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
  // See the 'MANUAL RECURSION' comment at the top of this file.
  Box::pin ( async move {
    expand_true_content_at_node (tree, treeid, context) . await ?;
    let child_treeids : Vec<NodeId> =
      tree . get (treeid) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_treeid in child_treeids {
      expand_true_content_until_stable (
        tree, child_treeid, context ) . await ?; }
    Ok(( )) }) }

async fn expand_true_content_at_node (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get (treeid) . unwrap () . value () . kind . clone ();
  if matches!( kind, ViewNodeKind::True (_)) {
    super::complete_preorder::truenode::
    complete_truenode_preorder (
      treeid, tree, context . defmap, context . source_diffs,
      &context . env . config, context . graph_snap,
      context . deleted_since_head_pid_src_map,
      context . deleted_by_this_save_pids,
      context . is_saved_view ) ?;
  } else if matches!( kind,
      ViewNodeKind::Scaff (Scaffold::SubscribeeCol) ) {
        super::complete_preorder::subscribee_col::
        complete_subscribee_col_preorder (
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
      complete_alias_col (
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
    complete_id_col (
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
      complete_hiddeninsubscribee_col (
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
    complete_hiddenoutsideofsubscribeecol (
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
