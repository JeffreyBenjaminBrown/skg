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
use crate::types::tree::generic::read_at_ancestor_in_tree;

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
  resolve_truenode_state_and_expand_content (
    viewforest, root_treeid, context ) . await ?;
  insert_and_reconcile_generated_scaffolds (
    viewforest, root_treeid, context ) . await ?;
  Ok(( )) }

/// Parent-first pass. Completes TrueNodes while their parent context
/// is fresh, expands definitive content, performs saved-view request
/// effects, and reconciles SubscribeeCol direct children.
fn resolve_truenode_state_and_expand_content<'a> (
  tree    : &'a mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &'a mut CompletionContext<'_>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  // See the 'MANUAL RECURSION' comment at the top of this file.
  Box::pin ( async move {
    complete_parent_first_at_node (tree, treeid, context) . await ?;
    let child_treeids : Vec<NodeId> =
      tree . get (treeid) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_treeid in child_treeids {
      resolve_truenode_state_and_expand_content (
        tree, child_treeid, context ) . await ?; }
    Ok(( )) }) }

/// Child-first pass. Once generated content is present, populate and
/// order display scaffolds, reconcile sharing scaffolds, and detach
/// empty deleted scaffolds.
fn insert_and_reconcile_generated_scaffolds<'a> (
  tree    : &'a mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &'a mut CompletionContext<'_>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  // See the 'MANUAL RECURSION' comment at the top of this file.
  Box::pin ( async move {
    let child_treeids : Vec<NodeId> =
      tree . get (treeid) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_treeid in child_treeids {
      insert_and_reconcile_generated_scaffolds (
        tree, child_treeid, context ) . await ?; }
    complete_child_first_at_node (tree, treeid, context) . await ?;
    Ok(( )) }) }

/// Dispatches to functions that might descend a few layers --
/// e.g. to complete a truenode, its content must all be children.
/// But this dispatcher cannot call itself.
async fn complete_parent_first_at_node (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get (treeid) . unwrap () . value () . kind . clone ();
  if let ViewNodeKind::Scaff (ref s) = kind {
    // Scaff under Deleted or DeletedScaff parent becomes DeletedScaff.
    let scaffold_kind : ScaffoldKind = s . kind ();
    let parent_is_deleted : bool =
      read_at_ancestor_in_tree ( tree, treeid, 1,
        |vn : &ViewNode| matches! ( &vn . kind,
          ViewNodeKind::Deleted (_) |
          ViewNodeKind::DeletedScaff (_) ))
      . unwrap_or (false);
    if parent_is_deleted {
      tree . get_mut (treeid) . unwrap () . value () . kind =
        ViewNodeKind::DeletedScaff (scaffold_kind);
      return Ok(( )); }}
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

/// Dispatches to functions that might descend a few layers --
/// e.g. to complete a truenode, its content must all be children.
/// But this dispatcher cannot call itself.
async fn complete_child_first_at_node (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get (treeid) . unwrap () . value () . kind . clone ();
  if matches!( kind, ViewNodeKind::True (_)) {
    super::complete_postorder::truenode::
    complete_truenode ( treeid,
                        tree,
                        context . defmap,
                        context . source_diffs,
                        &context . env . config,
                        &context . env . driver,
                        context . errors,
                        context . deleted_since_head_pid_src_map )
    . await ?;
  } else if matches!(
    kind, ViewNodeKind::Scaff (Scaffold::AliasCol)) {
      super::complete_postorder::aliascol::
      complete_alias_col (
        tree, treeid, context . source_diffs, &context . env . config ) ?;
  } else if matches!(
      kind, ViewNodeKind::Scaff (Scaffold::IDCol)) {
        super::complete_postorder::id_col::
        complete_id_col (
          treeid, tree, context . source_diffs,
          &context . env . config ) ?;
  } else if matches!(
    kind, ViewNodeKind::Scaff (Scaffold::HiddenInSubscribeeCol)) {
      super::complete_postorder::hiddeninsubscribee_col::
      complete_hiddeninsubscribee_col (
        treeid, tree, context . source_diffs, context . env,
        context . deleted_since_head_pid_src_map ) ?;
  } else if matches!( kind,
      ViewNodeKind::Scaff (Scaffold::HiddenOutsideOfSubscribeeCol)) {
        super::complete_postorder::hiddenoutsideof_subscribeecol::
        complete_hiddenoutsideofsubscribeecol (
          treeid, tree, context . source_diffs, context . env,
          context . deleted_since_head_pid_src_map ) ?;
  } else if matches!( kind, ViewNodeKind::Deleted (_)) { // no-op
  } else if matches!( kind, ViewNodeKind::DeletedScaff (_) ) {
    // Detach self if no children remain.
    let has_children : bool =
      tree . get (treeid) . unwrap ()
      . children () . next () . is_some ();
    if ! has_children {
      tree . get_mut (treeid) . unwrap () . detach (); }
  } else if matches!( kind, ViewNodeKind::Unknown (_) ) {
    // No-op: Unknown is a placeholder for an unresolvable id and
    // has nothing to populate post-order. Listed explicitly so a
    // reader sees the variant covered.
  }
  // No-op for: BufferRoot, TextChanged, Alias { .. },
  // ID { .. }, SubscribeeCol.
  // These nodes' correctness depends on their parent
  // having been processed (or, for SubscribeeCol, on the
  // preorder pass).
  Ok(( )) }
