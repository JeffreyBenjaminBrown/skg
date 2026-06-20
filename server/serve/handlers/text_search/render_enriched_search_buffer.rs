use crate::dbs::tantivy::title_and_source_by_id;
use crate::dbs::typedb::ancestry::AncestryTree;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::viewnode::{Birth, ViewNode, ViewNodeKind, ParentIs, mk_indefinitive_viewnode_with_birth};
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashMap;

/// Insert full containerward ancestry trees into the search viewforest,
/// under each level-1 result ActiveNode.
/// Ancestry children are prepended (inserted first among siblings).
pub(crate) fn insert_containerward_ancestries_into_search_view (
  viewforest     : &mut Tree<ViewNode>,
  search_results : &[ID],
  ancestry_by_id : &HashMap<ID, AncestryTree>,
  tantivy_index  : &TantivyIndex,
  config         : &SkgConfig,
  active         : &ActiveSourceSet,
) {
  // Search results ("hits") are forest roots.
  // Match them by ID from search_results.
  let level1_ids : Vec<(NodeId, ID)> = {
    let root_ref : NodeRef<ViewNode> = viewforest . root ();
    root_ref . children ()
    . filter_map ( |c| match &c . value () . kind {
      ViewNodeKind::Vognode (Vognode::Active (t))
        => Some (( c . id (), t . id . clone () )),
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
          insert_containerward_ancestry_tree (
            child, *node_nid,
            viewforest, tantivy_index, config, active ); } } } } }

/// Recursively insert an AncestryTree and its children
/// as indefinitive non-content ActiveNode children
/// under the given parent. Ancestry nodes are prepended.
fn insert_containerward_ancestry_tree(
  node          : &AncestryTree,
  parent_nid    : NodeId,
  viewforest        : &mut Tree<ViewNode>,
  tantivy_index : &TantivyIndex,
  config        : &SkgConfig,
  active        : &ActiveSourceSet,
) {
  let child_nid : NodeId = match
    prepend_containing_child_from_tantivy (
      node . id (), parent_nid,
      viewforest, tantivy_index, config, active ) {
        Some (child_nid) => child_nid,
        None => return, };
  if let AncestryTree::Inner ( _, children ) = node {
    for child in children {
      insert_containerward_ancestry_tree (
        child, child_nid,
        viewforest, tantivy_index, config, active ); } } }

/// Looks up a node's title and source from Tantivy,
/// prepends an indefinitive independent ActiveNode child
/// under the given parent.
/// Returns the new child's NodeId.
fn prepend_containing_child_from_tantivy (
  node_id       : &ID, // what to prepend
  parent_treeid : NodeId, // where to prepend
  viewforest        : &mut Tree<ViewNode>,
  tantivy_index : &TantivyIndex,
  _config       : &SkgConfig,
  active        : &ActiveSourceSet,
) -> Option<NodeId> {
  let viewnode : ViewNode =
    match title_and_source_by_id ( tantivy_index, node_id ) {
      Some ((title, source)) => {
        if ! active . contains_source (&source) {
          return None;
        } else {
          mk_indefinitive_viewnode_with_birth (
            node_id . clone (), source, title,
            ParentIs::Independent, Birth::Backpath (RelationRole::CONTAINER) ) }},
      None =>
        mk_indefinitive_viewnode_with_birth (
          node_id . clone (), SourceName::from ("search"),
          node_id . as_str () . to_string (),
          ParentIs::Independent, Birth::Backpath (RelationRole::CONTAINER) ) };
  let mut parent_mut : NodeMut<ViewNode> =
    viewforest . get_mut (parent_treeid) . unwrap ();
  Some (parent_mut . prepend (viewnode) . id ()) }
