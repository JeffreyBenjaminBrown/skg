/// TODO ? Maybe this module could be simpler,
/// by reusing the existing code for executing a
/// containerward view request.

use crate::dbs::tantivy::title_and_source_by_id;
use crate::dbs::typedb::ancestry::AncestryNode;
use crate::types::memory::skgnode_from_map_or_disk;
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::{
  ViewNode, ViewNodeKind, Scaffold, GraphNodeStats};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashMap;

/// Insert full containerward ancestry trees into the search forest,
/// under each level-2 result node.
/// Also populates the pool with SkgNodes for each ancestor node.
pub(crate) fn insert_ancestry_into_search_view (
  forest         : &mut Tree<ViewNode>,
  result_ids     : &[ID],
  ancestry_by_id : &HashMap<ID, AncestryNode>,
  tantivy_index  : &TantivyIndex,
  pool           : &mut HashMap<ID, SkgNode>,
  config         : &SkgConfig,
) {
  let level1_id : NodeId = { // the unique level-1 headline
    let root_ref : NodeRef<ViewNode> =
      forest . root ();
    match root_ref . children () . next () {
      Some (child) => child . id (),
      None => return } };
  let level2_ids : Vec<NodeId> = // the matches
    forest . get (level1_id) . unwrap ()
    . children ()
    . map ( |c| c . id () )
    . collect ();
  for (block_idx, &level2_nid)
    in level2_ids . iter () . enumerate ()
  { if block_idx >= result_ids . len () { break; }
    let id : &ID = &result_ids [block_idx];
    if let Some (ancestry) = ancestry_by_id . get (id) {
      // The ancestry root is the result node itself;
      // its children (containers) go under the level-2 node.
      if let AncestryNode::Inner ( _, children ) = ancestry {
        for child in children {
          insert_ancestry_node_recursive (
            child, level2_nid,
            forest, tantivy_index, pool, config ); } } } } }

/// Recursively insert an AncestryNode and its children
/// as SearchResult scaffold children under the given parent.
fn insert_ancestry_node_recursive(
  node          : &AncestryNode,
  parent_nid    : NodeId,
  forest        : &mut Tree<ViewNode>,
  tantivy_index : &TantivyIndex,
  pool          : &mut HashMap<ID, SkgNode>,
  config        : &SkgConfig,
) {
  let child_nid : NodeId =
    append_search_result_child_from_tantivy (
      node . id (), parent_nid,
      forest, tantivy_index, pool, config );
  if let AncestryNode::Inner ( _, children ) = node {
    for child in children {
      insert_ancestry_node_recursive (
        child, child_nid,
        forest, tantivy_index, pool, config ); } } }

/// Looks up a node's title and source from Tantivy,
/// populates the SkgNode pool, and
/// appends a SearchResult scaffold child under the given parent.
/// Returns the new child's NodeId.
fn append_search_result_child_from_tantivy (
  node_id       : &ID,
  parent_nid    : NodeId,
  forest        : &mut Tree<ViewNode>,
  tantivy_index : &TantivyIndex,
  pool          : &mut HashMap<ID, SkgNode>,
  config        : &SkgConfig,
) -> NodeId {
  let (title, source) : (String, SourceName) =
    title_and_source_by_id ( tantivy_index, node_id )
    . unwrap_or_else ( ||
      ( node_id . as_str () . to_string (),
        SourceName::from ("search") ) );
  let _ = skgnode_from_map_or_disk (
    node_id, &source, pool, config );
  let mut parent_mut : NodeMut<ViewNode> =
    forest . get_mut (parent_nid) . unwrap ();
  parent_mut . append ( ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::Scaff (
      Scaffold::SearchResult {
        id         : node_id . clone (),
        source,
        title,
        graphStats : GraphNodeStats::default () } ) } )
  . id () }
