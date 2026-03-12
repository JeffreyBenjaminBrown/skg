use crate::dbs::tantivy::title_and_source_by_id;
use crate::dbs::typedb::ancestry::AncestryTree;
use crate::types::memory::skgnode_from_map_or_disk;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::{ViewNode, ViewNodeKind, mk_indefinitive_viewnode};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashMap;

/// Insert full containerward ancestry trees into the search forest,
/// under each level-1 result TrueNode.
/// Ancestry children are prepended (inserted first among siblings).
/// Also populates the pool with SkgNodes for each ancestor node.
pub(crate) fn insert_ancestry_into_search_view (
  forest         : &mut Tree<ViewNode>,
  search_results     : &[ID],
  ancestry_by_id : &HashMap<ID, AncestryTree>,
  tantivy_index  : &TantivyIndex,
  pool           : &mut HashMap<ID, SkgNode>,
  config         : &SkgConfig,
) {
  // Search results ("hits") are level-1 BufferRoot children.
  // Match them by ID from search_results.
  let level1_ids : Vec<(NodeId, ID)> = {
    let root_ref : NodeRef<ViewNode> = forest . root ();
    root_ref . children ()
    . filter_map ( |c| match &c . value () . kind {
      ViewNodeKind::True (t) =>
        Some (( c . id (), t . id . clone () )),
      _ => None } )
    . collect () };
  for (node_nid, node_id) in &level1_ids {
    if ! search_results . contains (node_id) { continue; }
    if let Some (ancestry) = ancestry_by_id . get (node_id) {
      // The ancestry root is the result node itself;
      // its children (containers) go under the level-1 node.
      if let AncestryTree::Inner ( _, children ) = ancestry {
        for child in children . iter () . rev () {
          // Insert in reverse so the first child in
          // the ancestry ends up first among siblings.
          insert_ancestry_node_recursive (
            child, *node_nid,
            forest, tantivy_index, pool, config ); } } } } }

/// Recursively insert an AncestryTree and its children
/// as indefinitive parent_ignores TrueNode children
/// under the given parent. Ancestry nodes are prepended.
fn insert_ancestry_node_recursive(
  node          : &AncestryTree,
  parent_nid    : NodeId,
  forest        : &mut Tree<ViewNode>,
  tantivy_index : &TantivyIndex,
  pool          : &mut HashMap<ID, SkgNode>,
  config        : &SkgConfig,
) {
  let child_nid : NodeId =
    prepend_truenode_child_from_tantivy (
      node . id (), parent_nid,
      forest, tantivy_index, pool, config );
  if let AncestryTree::Inner ( _, children ) = node {
    for child in children {
      insert_ancestry_node_recursive (
        child, child_nid,
        forest, tantivy_index, pool, config ); } } }

/// Looks up a node's title and source from Tantivy,
/// populates the SkgNode pool, and
/// prepends an indefinitive parent_ignores TrueNode child
/// under the given parent.
/// Returns the new child's NodeId.
fn prepend_truenode_child_from_tantivy (
  node_id       : &ID, // what to prepend
  parent_treeid : NodeId, // where to prepend
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
  let _ = skgnode_from_map_or_disk ( // updates the map
    node_id, &source, pool, config );
  let viewnode : ViewNode =
    mk_indefinitive_viewnode ( node_id . clone (), source, title,
                               true ); // parent_ignores
  let mut parent_mut : NodeMut<ViewNode> =
    forest . get_mut (parent_treeid) . unwrap ();
  parent_mut . prepend (viewnode)
    . id () }
