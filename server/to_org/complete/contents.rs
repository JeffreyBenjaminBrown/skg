/// SINGLE ENTRY POINT: 'complete_or_restore_each_node_in_branch'.

use crate::to_org::util::{
  DefinitiveMap, get_pid_in_pairtree, get_pid_in_tree,
  truenode_in_tree_is_indefinitive, truenode_in_orgtree_is_indefinitive,
  collect_child_treeids, collect_child_treeids_in_orgtree,
  detect_and_mark_cycle, detect_and_mark_cycle_in_orgtree,
  make_indef_if_repeat_then_extend_defmap,
  make_indef_if_repeat_then_extend_defmap_in_orgtree,
};
use crate::to_org::complete::aliascol::{completeAliasCol, completeAliasCol_v2};
use crate::to_org::complete::sharing::{
  maybe_add_subscribeeCol_branch,
  maybe_add_subscribeeCol_branch_v2 };
use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::{SkgNode, SkgNodeMap};
use crate::types::maps::add_v_to_map_if_absent;
use crate::types::orgnode::{OrgNode, OrgNodeKind, Scaffold};
use crate::types::tree::PairTree;
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
      detect_and_mark_cycle ( tree, node_id ) ?;
      make_indef_if_repeat_then_extend_defmap (
        tree, node_id, visited ) ?;
      { if truenode_in_tree_is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveOrgnode (
            tree, node_id ) ?;
        } else { // futz with the orgnode and its content children
          maybe_add_subscribeeCol_branch (
            tree, node_id, config, typedb_driver ) . await ?; }
        recurse ( // Recurse to children even for indefinitive nodes, since they may have children from (for instance) view requests.
          tree, node_id, config, typedb_driver, visited
        ). await ?; }}
    Ok (( )) } ) }

/// V2: Same as complete_or_restore_each_node_in_branch,
/// but works with separate Tree<OrgNode> and SkgNodeMap.
/// Refactored to work directly with tree+map for most operations.
pub fn complete_or_restore_each_node_in_branch_v2<'a> (
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
        collect_child_treeids_in_orgtree ( tree, node_id ) ?;
      for child_treeid in child_treeids {
        complete_or_restore_each_node_in_branch_v2 (
          tree, map, child_treeid, config, typedb_driver,
          visited ) . await ?; }
      Ok (( )) } ) }

  Box::pin(async move {
    if read_at_node_in_tree(tree, node_id, |node| {
        matches!(&node.kind,
                 OrgNodeKind::Scaff(Scaffold::AliasCol)) })
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ? {
      // AliasCol: use v2 version working directly with tree+map
      completeAliasCol_v2 (
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

      detect_and_mark_cycle_in_orgtree ( tree, node_id ) ?;
      make_indef_if_repeat_then_extend_defmap_in_orgtree (
        tree, node_id, visited ) ?;

      { if truenode_in_orgtree_is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveOrgnode_v2 (
            tree, map, node_id ) ?;
        } else {
          // Definitive: use v2 version working directly with tree+map
          maybe_add_subscribeeCol_branch_v2 (
            tree, map, node_id, config, typedb_driver ) . await ?; }
        recurse (
          tree, map, node_id, config, typedb_driver, visited
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

/// V2: clobber an indefinitive OrgNode with data from the map.
/// Tree<OrgNode> + SkgNodeMap version.
pub fn clobberIndefinitiveOrgnode_v2 (
  tree    : &mut Tree<OrgNode>,
  map     : &SkgNodeMap,
  treeid  : NodeId,
) -> Result < (), Box<dyn Error> > {
  // First get the ID from the node
  let node_id : ID =
    read_at_node_in_tree ( tree, treeid, |orgnode| {
      match &orgnode.kind {
        OrgNodeKind::True(t) => t . id_opt . clone()
          . ok_or("clobberIndefinitiveOrgnode_v2: node has no ID"),
        OrgNodeKind::Scaff(_) => Err (
          "clobberIndefinitiveOrgnode_v2: expected TrueNode" ),
      }
    } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ??;

  // Look up SkgNode in map
  let skgnode : &SkgNode =
    map . get ( &node_id )
    . ok_or ( "clobberIndefinitiveOrgnode_v2: SkgNode should exist in map" ) ?;
  let title : String = skgnode . title . clone();
  let source : String = skgnode . source . clone();

  // Update the OrgNode
  write_at_node_in_tree ( tree, treeid, |orgnode| {
    let OrgNodeKind::True ( t ) = &mut orgnode.kind
      else { panic! ( "clobberIndefinitiveOrgnode_v2: expected TrueNode" ) };
    t . title = title;
    t . source_opt = Some ( source );
    t . body = None;
  } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;

  Ok (( )) }

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

/// V2: Tree<OrgNode> version of ensure_source.
pub async fn ensure_source_v2 (
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
        "ensure_source_v2: could not find source for ID {:?}",
        node_pid ) ) ?;
    write_at_node_in_tree (
      tree, node_id,
      |orgnode| {
        let OrgNodeKind::True ( t ) = &mut orgnode.kind
          else { panic! ( "ensure_source_v2: expected TrueNode" ) };
        t . source_opt = Some ( source ); } )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?; }
  Ok (( )) }
