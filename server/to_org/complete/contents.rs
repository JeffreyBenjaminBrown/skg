/// SINGLE ENTRY POINT: 'completeAndRestoreForest'.

use crate::to_org::util::{ DefinitiveMap, get_pid_in_pairtree, truenode_in_tree_is_indefinitive, collect_child_treeids, mark_if_visited_or_repeat_or_cycle };
use crate::to_org::complete::aliascol::completeAliasCol;
use crate::to_org::complete::content_children::completeAndReorder_childrenOf_definitiveOrgnode;
use crate::to_org::complete::sharing::{
  maybe_add_subscribeeCol_branch };
use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNodeKind, Scaffold};
use crate::types::tree::PairTree;
use crate::types::tree::generic::{
  read_at_node_in_tree, write_at_node_in_tree };

use ego_tree::NodeId;
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// TRIVIAL: Just wraps 'complete_or_restore_each_node_in_branch',
/// calling it on each "tree root" (child of the ForestRoot),
/// but threading 'visited' through that sequence of calls.
pub async fn completeAndRestoreForest (
  forest        : &mut PairTree,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
) -> Result < DefinitiveMap, Box<dyn Error> > {
  let mut visited : DefinitiveMap = DefinitiveMap::new ();
  let tree_root_ids : Vec < NodeId > =
    forest . root () . children ()
    . map ( |c| c . id () )
    . collect ();
  for tree_root_id in tree_root_ids {
    complete_or_restore_each_node_in_branch (
      forest, tree_root_id, config, typedb_driver,
      &mut visited ) . await ?; }
  Ok ( visited ) }

/// PURPOSE: Complete or restore a node,
/// and then its children (a preorder DFS traversal).
/// - "complete": Since another buffer might have already saved,
///   - Definitive nodes can be missing branches.
///     - TODO: Was I wrong about this? Saving a definitive node will cause whatever was on disk to be clobbered by its content children.
///   - An AliasCol might be missing aliases.
///   TODO: Complete other kinds of branches.
/// - "restore": Because indefinitive nodes may have had their titles or bodies edited.
///   TODO ? Maybe look for edits to indefinitive nodes and throw an error, as is done for foreign nodes.
fn complete_or_restore_each_node_in_branch<'a> (
  tree          : &'a mut PairTree,
  node_id       : NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  visited       : &'a mut DefinitiveMap,
) -> Pin<Box<dyn Future<Output =
                        Result<(), Box<dyn Error>>> + 'a>> {
  fn recurse<'b> (
    tree          : &'b mut PairTree,
    node_id       : NodeId,
    config        : &'b SkgConfig,
    typedb_driver : &'b TypeDBDriver,
    visited       : &'b mut DefinitiveMap,
  ) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'b>>
  { Box::pin(async move {
      let child_treeids : Vec < NodeId > =
        collect_child_treeids ( tree, node_id ) ?;
      for child_treeid in child_treeids {
        complete_or_restore_each_node_in_branch (
          tree, child_treeid, config, typedb_driver,
          visited ) . await ?; }
      Ok (( )) } ) }
  Box::pin(async move {
    if read_at_node_in_tree(tree, node_id, |node| {
        matches!(&node.orgnode.kind,
                 OrgNodeKind::Scaff(Scaffold::AliasCol)) })? {
      // Don't recurse; completeAliasCol handles the whole subtree.
      completeAliasCol (
        tree, node_id, config, typedb_driver ). await ?;
    } else if read_at_node_in_tree(tree, node_id, |node| {
        matches!( &node.orgnode.kind,
                  OrgNodeKind::Scaff(_)) } )? {
      // Skip, but recurse into children.
      recurse ( tree, node_id, config, typedb_driver, visited
              ) . await ?;
    } else {
      ensure_skgnode (
        tree, node_id, config, typedb_driver ). await ?;
      mark_if_visited_or_repeat_or_cycle (
        tree, node_id, visited ) ?;
      { if truenode_in_tree_is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveOrgnode (
            tree, node_id ) ?;
        } else { // futz with the orgnode and its content children
          maybe_add_subscribeeCol_branch (
            tree, node_id, config, typedb_driver ) . await ?;
          completeAndReorder_childrenOf_definitiveOrgnode (
            tree, node_id, config, typedb_driver ). await ?; }
        recurse ( // Recurse to children even for indefinitive nodes, since they may have children from (for instance) view requests.
          tree, node_id, config, typedb_driver, visited
        ). await ?; }}
    Ok (( )) } ) }

/// PURPOSE: Given an indefinitive node N:
/// - Reset title.
/// - Reset source.
/// - Set body to None.
///
/// ASSUMES: The SkgNode at that tree node is accurate.
/// ASSUMES: The input is indefinitive.
pub fn clobberIndefinitiveOrgnode (
  tree    : &mut PairTree,
  treeid : NodeId,
) -> Result < (), Box<dyn Error> > {
  write_at_node_in_tree ( tree, treeid, |pair| {
    let (title, source) : (String, String) = {
      let skgnode : &SkgNode =
        pair . mskgnode . as_ref ()
          . ok_or ("SkgNode should exist after fetch" . to_string() )?;
      ( skgnode . title . clone (),
        skgnode . source . clone () ) };
    let OrgNodeKind::True ( t ) = &mut pair.orgnode.kind
      else { return Err ( "clobberIndefinitiveOrgnode: expected TrueNode" . into () ) };
    t . title = title;
    t . source_opt = Some ( source );
    t . body = None;
    Ok::<(), String>(( ))
  } )? // before the '?' it's a nested Result: R<R<(),String>,String>
    . map_err( |e| -> Box<dyn Error> { e.into() } ) }

/// Ensure a node in a PairTree has a SkgNode.
/// If the node already has Some(skgnode), does nothing.
/// Otherwise fetches from disk and stores it.
pub async fn ensure_skgnode (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  let has_skgnode : bool =
    read_at_node_in_tree (
      tree, node_id,
      |np| np . mskgnode . is_some () ) ?;
  if ! has_skgnode {
    let skgnode : SkgNode =
      skgnode_from_id (
        config, driver, &node_pid ) . await ?;
    write_at_node_in_tree (
      tree, node_id,
      |np| np . mskgnode = Some ( skgnode ) ) ?; }
  Ok (( )) }

/// Noop for Scaffolds. Otherwise, ensures the node has a source.
/// If needed, fetches the data from TypeDB.
pub async fn ensure_source (
  tree    : &mut PairTree,
  node_id : NodeId,
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let needs_source : bool =
    read_at_node_in_tree (
      tree, node_id,
      |np| matches! ( &np . orgnode . kind,
                      OrgNodeKind::True(t)
                      if t . source_opt . is_none () )) ?;
  if needs_source {
    let node_pid : ID =
      get_pid_in_pairtree ( tree, node_id ) ?;
    let (_pid, source) : (ID, String) =
      pid_and_source_from_id (
        db_name, driver, &node_pid ) . await ?
      . ok_or_else ( || format! (
        "ensure_source: could not find source for ID {:?}",
        node_pid ) ) ?;
    write_at_node_in_tree (
      tree, node_id,
      |np| {
        let OrgNodeKind::True ( t ) = &mut np.orgnode.kind
          else { panic! ( "ensure_source: expected TrueNode" ) };
        t . source_opt = Some ( source ); } ) ?; }
  Ok (( )) }
