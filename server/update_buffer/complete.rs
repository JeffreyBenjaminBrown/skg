// MANUAL RECURSION:
// This uses manual async recursions, rather than calls to
// `do_everywhere_in_tree_dfs`, because some dispatch targets
// are async, and `do_everywhere_in_tree_dfs` takes a sync
// `FnMut` closure which cannot `.await`.

use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::git::SourceDiff;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold};
use crate::types::skgnodemap::SkgNodeMap;
use crate::to_org::util::DefinitiveMap;

use ego_tree::{Tree, NodeId};
use std::collections::HashMap;
use std::error::Error;
use std::future::Future;
use std::pin::Pin;
use neo4rs::Graph;

pub async fn complete_viewtree (
  forest             : &mut Tree<ViewNode>,
  map                : &mut SkgNodeMap,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  graph              : &Graph,
  errors             : &mut Vec<String>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = forest . root () . id ();
  complete_preorder_recursive (
    forest, root_treeid,
    map, defmap, source_diffs, config, graph,
    deleted_id_src_map ) . await ?;
  complete_postorder_recursive (
    forest, root_treeid,
    map, defmap, source_diffs, config, graph,
    errors, deleted_id_src_map ) . await ?;
  Ok(( )) }

fn complete_preorder_recursive<'a> (
  tree               : &'a mut Tree<ViewNode>,
  treeid             : NodeId,
  map                : &'a mut SkgNodeMap,
  defmap             : &'a mut DefinitiveMap,
  source_diffs       : &'a Option<HashMap<SourceName, SourceDiff>>,
  config             : &'a SkgConfig,
  graph              : &'a Graph,
  deleted_id_src_map : &'a HashMap<ID, SourceName>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  // See the 'MANUAL RECURSION' comment at the top of this file.
  Box::pin ( async move {
    complete_preorder_for_one_node (
      tree, treeid, map, defmap, source_diffs, config, graph,
      deleted_id_src_map
    ) . await ?;
    let child_treeids : Vec<NodeId> =
      tree . get ( treeid ) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_treeid in child_treeids {
      complete_preorder_recursive (
        tree, child_treeid,
        map, defmap, source_diffs, config, graph,
        deleted_id_src_map
      ) . await ?; }
    Ok(( )) }) }

fn complete_postorder_recursive<'a> (
  tree               : &'a mut Tree<ViewNode>,
  treeid             : NodeId,
  map                : &'a mut SkgNodeMap,
  defmap             : &'a mut DefinitiveMap,
  source_diffs       : &'a Option<HashMap<SourceName, SourceDiff>>,
  config             : &'a SkgConfig,
  graph              : &'a Graph,
  errors             : &'a mut Vec<String>,
  deleted_id_src_map : &'a HashMap<ID, SourceName>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  // See the 'MANUAL RECURSION' comment at the top of this file.
  Box::pin ( async move {
    let child_treeids : Vec<NodeId> =
      tree . get ( treeid ) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_treeid in child_treeids {
      complete_postorder_recursive (
        tree, child_treeid,
        map, defmap, source_diffs, config, graph,
        errors, deleted_id_src_map
      ) . await ?; }
    complete_postorder_for_one_node (
      tree, treeid, map, defmap, source_diffs, config, graph,
      errors, deleted_id_src_map
    ) . await ?;
    Ok(( )) }) }

async fn complete_preorder_for_one_node (
  tree               : &mut Tree<ViewNode>,
  treeid             : NodeId,
  map                : &mut SkgNodeMap,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  graph              : &Graph,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get ( treeid ) . unwrap () . value () . kind . clone ();
  if matches!( kind, ViewNodeKind::True( _ )) {
    super::complete_parent_first::truenode::
    complete_truenode_preorder (
      treeid, tree, map, defmap, source_diffs, config,
      deleted_id_src_map ) ?;
  } else if matches!( kind,
      ViewNodeKind::Scaff( Scaffold::SubscribeeCol ) ) {
        super::complete_parent_first::subscribee_col::
        complete_subscribee_col_preorder (
          treeid, tree, map, source_diffs, config, graph,
          deleted_id_src_map
        ). await ?; }
  Ok(( )) }

async fn complete_postorder_for_one_node (
  tree               : &mut Tree<ViewNode>,
  treeid             : NodeId,
  map                : &mut SkgNodeMap,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  graph              : &Graph,
  errors             : &mut Vec<String>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get ( treeid ) . unwrap () . value () . kind . clone ();
  if matches!( kind, ViewNodeKind::True( _ )) {
    super::complete_child_first::truenode::
    complete_truenode (
      treeid, tree, map, defmap, config, graph,
      errors, deleted_id_src_map ) . await ?;
  } else if matches!(
    kind, ViewNodeKind::Scaff( Scaffold::AliasCol )) {
      super::complete_child_first::aliascol::
      completeAliasCol ( tree, map, treeid, source_diffs ) ?;
  } else if matches!(
      kind, ViewNodeKind::Scaff( Scaffold::IDCol )) {
        super::complete_child_first::id_col::
        completeIDCol ( treeid, tree, map, source_diffs ) ?;
  } else if matches!(
    kind, ViewNodeKind::Scaff( Scaffold::HiddenInSubscribeeCol )) {
      super::complete_child_first::hiddeninsubscribee_col::
      complete_hiddeninsubscribee_col (
        treeid, tree, map, source_diffs, config,
        deleted_id_src_map ) ?;
  } else if matches!( kind,
      ViewNodeKind::Scaff( Scaffold::HiddenOutsideOfSubscribeeCol )) {
        super::complete_child_first::hiddenoutsideof_subscribeecol::
        complete_hiddenoutsideofsubscribeecol (
          treeid, tree, map, source_diffs, config,
          deleted_id_src_map ) ?; }
  // No-op for: BufferRoot, TextChanged, Alias { .. },
  // ID { .. }, SubscribeeCol.
  // These nodes' correctness depends on their parent
  // having been processed (or, for SubscribeeCol, on the
  // preorder pass).
  Ok(( )) }
