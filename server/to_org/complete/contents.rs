/// SINGLE ENTRY POINT: 'complete_or_restore_each_node_in_branch'.

use crate::to_org::util::{
  DefinitiveMap, get_pid_in_tree, truenode_is_indefinitive, collect_child_treeids, detect_and_mark_cycle,
  make_indef_if_repeat_then_extend_defmap,
};
use crate::to_org::complete::aliascol::completeAliasCol;
use crate::to_org::complete::sharing::maybe_add_subscribeeCol_branch;
use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::{SkgNode, SkgNodeMap};
use crate::types::maps::add_v_to_map_if_absent;
use crate::types::orgnode::{OrgNode, OrgNodeKind, Scaffold};
use crate::types::tree::generic::{
  read_at_node_in_tree, write_at_node_in_tree };

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
  tree          : &'a mut Tree<OrgNode>,
  map           : &'a mut SkgNodeMap,
  node_id       : NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  visited       : &'a mut DefinitiveMap,
) -> Pin<Box<dyn Future<Output =
                        Result<(), Box<dyn Error>>> + 'a>> {
  fn recurse<'b> (
    tree          : &'b mut Tree<OrgNode>,
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
                 OrgNodeKind::Scaff(Scaffold::AliasCol)) })
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ? {
      // AliasCol: use v2 version working directly with tree+map
      completeAliasCol (
        tree, map, node_id, config, typedb_driver ). await ?;
    } else if read_at_node_in_tree(tree, node_id, |node| {
        matches!( &node.kind, OrgNodeKind::Scaff(_)) } )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ? {
      // Skip, but recurse into children.
      recurse ( tree, map, node_id, config, typedb_driver, visited
              ) . await ?;
    } else {
      // TrueNode: work directly with tree+map
      let node_pid : ID = get_pid_in_tree ( tree, node_id ) ?;
      add_v_to_map_if_absent (
        &node_pid, map,
        |id| {
          let id = id.clone();
          async move {
            skgnode_from_id ( config, typedb_driver, &id ) . await
          }
        } ) . await ?;

      detect_and_mark_cycle ( tree, node_id ) ?;
      make_indef_if_repeat_then_extend_defmap (
        tree, node_id, visited ) ?;

      { if truenode_is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveOrgnode (
            tree, map, node_id ) ?;
        } else {
          // Definitive: use v2 version working directly with tree+map
          maybe_add_subscribeeCol_branch (
            tree, map, node_id, config, typedb_driver ) . await ?; }
        recurse (
          tree, map, node_id, config, typedb_driver, visited
        ). await ?; }}
    Ok (( )) } ) }

/// Clobber an indefinitive OrgNode with data from the map.
/// Resets title, source, and sets body to None.
pub fn clobberIndefinitiveOrgnode (
  tree    : &mut Tree<OrgNode>,
  map     : &SkgNodeMap,
  treeid  : NodeId,
) -> Result < (), Box<dyn Error> > {
  // First get the ID from the node
  let node_id : ID =
    read_at_node_in_tree ( tree, treeid, |orgnode| {
      match &orgnode.kind {
        OrgNodeKind::True(t) => t . id_opt . clone()
          . ok_or("clobberIndefinitiveOrgnode: node has no ID"),
        OrgNodeKind::Scaff(_) => Err (
          "clobberIndefinitiveOrgnode: expected TrueNode" ),
      }
    } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ??;

  // Look up SkgNode in map
  let skgnode : &SkgNode =
    map . get ( &node_id )
    . ok_or ( "clobberIndefinitiveOrgnode: SkgNode should exist in map" ) ?;
  let title : String = skgnode . title . clone();
  let source : String = skgnode . source . clone();

  // Update the OrgNode
  write_at_node_in_tree ( tree, treeid, |orgnode| {
    let OrgNodeKind::True ( t ) = &mut orgnode.kind
      else { panic! ( "clobberIndefinitiveOrgnode: expected TrueNode" ) };
    t . title = title;
    t . source_opt = Some ( source );
    t . body = None;
  } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;

  Ok (( )) }

/// Noop for Scaffolds. Otherwise, ensures the node has a source.
/// If needed, fetches the data from TypeDB.
pub async fn ensure_source (
  tree    : &mut Tree<OrgNode>,
  node_id : NodeId,
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let needs_source : bool =
    read_at_node_in_tree (
      tree, node_id,
      |orgnode| matches! ( &orgnode . kind,
                      OrgNodeKind::True(t)
                      if t . source_opt . is_none () ))
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  if needs_source {
    let node_pid : ID =
      get_pid_in_tree ( tree, node_id ) ?;
    let (_pid, source) : (ID, String) =
      pid_and_source_from_id (
        db_name, driver, &node_pid ) . await ?
      . ok_or_else ( || format! (
        "ensure_source: could not find source for ID {:?}",
        node_pid ) ) ?;
    write_at_node_in_tree (
      tree, node_id,
      |orgnode| {
        let OrgNodeKind::True ( t ) = &mut orgnode.kind
          else { panic! ( "ensure_source: expected TrueNode" ) };
        t . source_opt = Some ( source ); } )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?; }
  Ok (( )) }
