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
use crate::types::tree::generic::read_at_ancestor_in_tree;

use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::future::Future;
use std::pin::Pin;
use typedb_driver::TypeDBDriver;

pub async fn complete_viewtree (
  forest             : &mut Tree<ViewNode>,
  map                : &mut SkgNodeMap,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
  errors             : &mut Vec<String>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  complete_viewtree_with_deleted_pids (
    forest, map, defmap, source_diffs, config, driver,
    errors, deleted_id_src_map,
    &HashSet::new () ) . await }

pub async fn complete_viewtree_with_deleted_pids (
  forest             : &mut Tree<ViewNode>,
  map                : &mut SkgNodeMap,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
  errors             : &mut Vec<String>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  deleted_pids       : &HashSet<ID>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = forest . root () . id ();
  complete_preorder_recursive (
    forest, root_treeid,
    map, defmap, source_diffs, config, driver,
    deleted_id_src_map, deleted_pids ) . await ?;
  complete_postorder_recursive (
    forest, root_treeid,
    map, defmap, source_diffs, config, driver,
    errors, deleted_id_src_map ) . await ?;
  Ok(( )) }

fn complete_preorder_recursive<'a> (
  tree               : &'a mut Tree<ViewNode>,
  treeid             : NodeId,
  map                : &'a mut SkgNodeMap,
  defmap             : &'a mut DefinitiveMap,
  source_diffs       : &'a Option<HashMap<SourceName, SourceDiff>>,
  config             : &'a SkgConfig,
  driver             : &'a TypeDBDriver,
  deleted_id_src_map : &'a HashMap<ID, SourceName>,
  deleted_pids       : &'a HashSet<ID>,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  // See the 'MANUAL RECURSION' comment at the top of this file.
  Box::pin ( async move {
    complete_preorder_for_one_node (
      tree, treeid, map, defmap, source_diffs, config, driver,
      deleted_id_src_map, deleted_pids
    ) . await ?;
    let child_treeids : Vec<NodeId> =
      tree . get ( treeid ) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_treeid in child_treeids {
      complete_preorder_recursive (
        tree, child_treeid,
        map, defmap, source_diffs, config, driver,
        deleted_id_src_map, deleted_pids
      ) . await ?; }
    Ok(( )) }) }

fn complete_postorder_recursive<'a> (
  tree               : &'a mut Tree<ViewNode>,
  treeid             : NodeId,
  map                : &'a mut SkgNodeMap,
  defmap             : &'a mut DefinitiveMap,
  source_diffs       : &'a Option<HashMap<SourceName, SourceDiff>>,
  config             : &'a SkgConfig,
  driver             : &'a TypeDBDriver,
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
        map, defmap, source_diffs, config, driver,
        errors, deleted_id_src_map
      ) . await ?; }
    complete_postorder_for_one_node (
      tree, treeid, map, defmap, source_diffs, config, driver,
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
  driver             : &TypeDBDriver,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  deleted_pids       : &HashSet<ID>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get ( treeid ) . unwrap () . value () . kind . clone ();
  // Guard: Scaff under Deleted or DeletedScaff parent becomes DeletedScaff.
  if matches!( kind, ViewNodeKind::Scaff( _ )) {
    let parent_is_deleted : bool =
      read_at_ancestor_in_tree ( tree, treeid, 1,
        |vn : &ViewNode| matches! ( &vn.kind,
          ViewNodeKind::Deleted ( _ ) |
          ViewNodeKind::DeletedScaff ))
      . unwrap_or ( false );
    if parent_is_deleted {
      tree . get_mut ( treeid ) . unwrap () . value () . kind =
        ViewNodeKind::DeletedScaff;
      return Ok(( )); }}
  if matches!( kind, ViewNodeKind::True( _ )) {
    super::complete_parent_first::truenode::
    complete_truenode_preorder (
      treeid, tree, map, defmap, source_diffs, config,
      deleted_id_src_map, deleted_pids ) ?;
  } else if matches!( kind,
      ViewNodeKind::Scaff( Scaffold::SubscribeeCol ) ) {
        super::complete_parent_first::subscribee_col::
        complete_subscribee_col_preorder (
          treeid, tree, map, source_diffs, config, driver,
          deleted_id_src_map
        ). await ?; }
  // No-op for: Deleted, DeletedScaff (whose parent is not
  // Deleted/DeletedScaff -- occurs on re-save of a buffer
  // containing them).
  Ok(( )) }

async fn complete_postorder_for_one_node (
  tree               : &mut Tree<ViewNode>,
  treeid             : NodeId,
  map                : &mut SkgNodeMap,
  defmap             : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
  errors             : &mut Vec<String>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewNodeKind =
    tree . get ( treeid ) . unwrap () . value () . kind . clone ();
  if matches!( kind, ViewNodeKind::True( _ )) {
    super::complete_child_first::truenode::
    complete_truenode (
      treeid, tree, map, defmap, config, driver,
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
          deleted_id_src_map ) ?;
  } else if matches!( kind, ViewNodeKind::Deleted( _ )) {
    // No-op: Deleted nodes are inert.
  } else if matches!( kind, ViewNodeKind::DeletedScaff ) {
    // Detach self if no children remain.
    let has_children : bool =
      tree . get ( treeid ) . unwrap ()
      . children () . next () . is_some ();
    if ! has_children {
      tree . get_mut ( treeid ) . unwrap () . detach (); }
  }
  // No-op for: BufferRoot, TextChanged, Alias { .. },
  // ID { .. }, SubscribeeCol.
  // These nodes' correctness depends on their parent
  // having been processed (or, for SubscribeeCol, on the
  // preorder pass).
  Ok(( )) }
