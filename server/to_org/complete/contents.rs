/// SINGLE ENTRY POINT: 'complete_or_restore_each_node_in_branch'.

use crate::to_org::util::{
  DefinitiveMap, get_id_from_treenode, truenode_in_tree_is_indefinitive, collect_child_treeids, detect_and_mark_cycle,
  make_indef_if_repeat_then_extend_defmap,
};
use crate::to_org::complete::aliascol::completeAliasCol;
use crate::to_org::complete::sharing::maybe_add_subscribeeCol_branch;
use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::types::maps::add_v_to_map_if_absent;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold};
use crate::types::tree::generic::{
  read_at_node_in_tree, write_at_node_in_tree };
use crate::types::tree::viewnode_skgnode::pid_and_source_from_treenode;

use ego_tree::{NodeId, Tree};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// PURPOSE: Complete or restore a node,
/// and then its children (a preorder DFS traversal).
/// - "complete": Since another buffer might have already saved,
///   - Definitive nodes can be missing branches.
///     - TODO: Was I wrong about this? Saving a definitive node will cause whatever was on disk to be clobbered by its content children.
///   - An AliasCol might be missing aliases.
///   TODO: Complete other kinds of branches.
/// - "restore": Because indefinitive nodes may have had their titles or bodies edited.
///   TODO ? Maybe look for edits to indefinitive nodes and throw an error, as is done for foreign nodes.
pub fn complete_or_restore_each_node_in_branch<'a> (
  tree          : &'a mut Tree<ViewNode>,
  map           : &'a mut SkgNodeMap,
  node_id       : NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  visited       : &'a mut DefinitiveMap,
) -> Pin<Box<dyn Future<Output =
                        Result<(), Box<dyn Error>>> + 'a>> {
  fn recurse<'b> (
    tree          : &'b mut Tree<ViewNode>,
    map           : &'b mut SkgNodeMap,
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
          tree, map, child_treeid, config, typedb_driver,
          visited ) . await ?; }
      Ok (( )) } ) }

  Box::pin(async move {
    if read_at_node_in_tree(tree, node_id, |node| {
        matches!(&node.kind,
                 ViewNodeKind::Scaff(Scaffold::AliasCol)) })
        . map_err ( |e| -> Box<dyn Error> { e.into() } ) ? {
      // Don't recurse; completeAliasCol handles the whole subtree.
      completeAliasCol (
        tree, map, node_id ). await ?;
    } else if read_at_node_in_tree(tree, node_id, |node| {
        matches!( &node.kind, ViewNodeKind::Scaff(_)) } )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ? {
      // Skip, but recurse into children.
      recurse ( tree, map, node_id, config, typedb_driver, visited
              ) . await ?;
    } else { // it's a TrueNode
      let node_pid : ID = get_id_from_treenode ( tree, node_id ) ?;
      add_v_to_map_if_absent (
        &node_pid, map,
        |id| { let id : ID = id.clone();
               async move {
               skgnode_from_id ( config, typedb_driver, &id
                               ). await }} ). await ?;

      detect_and_mark_cycle ( tree, node_id ) ?;
      make_indef_if_repeat_then_extend_defmap (
        tree, node_id, visited ) ?;

      { if truenode_in_tree_is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveViewnode (
            tree, map, node_id, config ) ?;
        } else { // futz with the viewnode and its content children
          maybe_add_subscribeeCol_branch (
            tree, map, node_id, config, typedb_driver ) . await ?; }
        recurse ( // Recurse to children even for indefinitive nodes, since they may have children from (for instance) view requests.
          tree, map, node_id, config, typedb_driver, visited
        ). await ?; }}
    Ok (( )) } ) }

/// PURPOSE: Given an indefinitive node N,
/// uses SkgNodeMap or lookup on disk to:
/// - Reset title.
/// - Reset source.
/// - Set body to None.
///
/// EXPECTS: The input node is indefinitive.
pub fn clobberIndefinitiveViewnode (
  tree    : &mut Tree<ViewNode>,
  map     : &mut SkgNodeMap,
  treeid  : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (node_id, source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      tree, treeid, "clobberIndefinitiveViewnode" ) ?;
  let skgnode : &SkgNode =
    skgnode_from_map_or_disk ( &node_id, &source, map, config ) ?;
  let title : String = skgnode . title . clone();
  let source : SourceName = skgnode . source . clone();
  write_at_node_in_tree ( tree, treeid, |viewnode| {
    let ViewNodeKind::True ( t ) : &mut ViewNodeKind =
      &mut viewnode.kind
      else { panic! (
             "clobberIndefinitiveViewnode: expected TrueNode" ) };
    t . title = title;
    t . source = source;
    t . body = None; }
  ). map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;

  Ok (( )) }
