use crate::dbs::tantivy::title_and_source_by_id;
use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::typedb::ancestry::AncestryTree;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::viewnode::{Birth, ViewNode, ViewNodeKind, ParentIs, mk_indefinitive_viewnode_with_birth};
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashMap, HashSet};

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
            child, node_id, *node_nid,
            viewforest, tantivy_index, config, active ); } } } } }

/// Recursively insert an AncestryTree and its children
/// as indefinitive non-content ActiveNode children
/// under the given parent. Ancestry nodes are prepended.
fn insert_containerward_ancestry_tree(
  node          : &AncestryTree,
  contained_id  : &ID, // the node this ancestry step CONTAINS
  parent_nid    : NodeId,
  viewforest        : &mut Tree<ViewNode>,
  tantivy_index : &TantivyIndex,
  config        : &SkgConfig,
  active        : &ActiveSourceSet,
) {
  if ! active . is_all () {
    // Edge-level gating (render-and-gating, 5_plan.org): a private
    // MEMBERSHIP must not surface through enrichment ancestry even
    // when both nodes are public. The edge's owner is the
    // container (this ancestry step).
    let edge_visible : bool =
      snapshot_global ()
      . and_then ( |snap| snap . edge_level (
        node . id (), NodeRelation::Contains, contained_id ))
      . map ( |level| active . contains_source (&level) )
      . unwrap_or (true); // unknown edge: fall through to the
                          // node-source gate below, as before
    if ! edge_visible { return; }}
  let child_nid : NodeId = match
    prepend_containing_child_from_tantivy (
      node . id (), parent_nid,
      viewforest, tantivy_index, config, active ) {
        Some (child_nid) => child_nid,
        None => return, };
  if let AncestryTree::Inner ( _, children ) = node {
    for child in children {
      insert_containerward_ancestry_tree (
        child, node . id (), child_nid,
        viewforest, tantivy_index, config, active ); } } }

/// Which way an override graft walks from a node, and the backpath
/// birth role it stamps on each grafted relative.
#[derive(Clone, Copy)]
enum OverrideDir {
  /// The nodes this node OVERRIDES (outbound `overrides_view_of`).
  /// Birth OVERRIDDEN; renders herald "aO" (parent overrides child).
  Overriddenward,
  /// The nodes that OVERRIDE this node (inbound). Birth OVERRIDER;
  /// renders herald "Oa" (child overrides parent).
  Overriderward,
}

/// Graft each result's override relatives -- BOTH directions -- as
/// inverted read-only indefinitive org-descendants, so an overridden
/// node and the node(s) overriding it are navigable straight from the
/// search results
/// (TODO/override-ancestry-in-search-results.org). Each direction is
/// its own one-directional chain hanging under the result, recursive
/// and cycle-guarded. Reads the in-Rust graph (override edges are
/// direct index lookups); a relative whose override EDGE is
/// edge-level-hidden, or whose own source is inactive, is skipped.
pub fn insert_override_ancestries_into_search_view (
  viewforest     : &mut Tree<ViewNode>,
  search_results : &[ID],
  active         : &ActiveSourceSet,
) {
  let Some (graph) = snapshot_global () else { return; };
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
    for dir in [ OverrideDir::Overriddenward,
                 OverrideDir::Overriderward ] {
      let mut path : HashSet<ID> =
        HashSet::from ([ node_id . clone () ]);
      graft_override_chain (
        node_id, *node_nid, dir, &graph,
        viewforest, active, &mut path ); }} }

/// Every id that 'insert_override_ancestries_into_search_view' would
/// graft under the given results -- the override-relative closure in
/// both directions, gated identically to the graft. The enrichment
/// thread unions these into the graphStats pre-fetch so the grafted
/// nodes get their relationship heralds; without this they would render
/// herald-less (their graphStats would never be fetched, since the
/// grafts do not exist yet when the pre-fetch runs). MUST stay in sync
/// with 'graft_override_chain' (same directions, same gated accessors,
/// same node-source gate). Returns empty without a graph handle.
pub fn collect_override_relative_ids (
  search_results : &[ID],
  active         : &ActiveSourceSet,
) -> HashSet<ID> {
  let mut out : HashSet<ID> = HashSet::new ();
  let Some (graph) = snapshot_global () else { return out; };
  for root in search_results {
    for dir in [ OverrideDir::Overriddenward,
                 OverrideDir::Overriderward ] {
      let mut seen  : HashSet<ID> = HashSet::from ([ root . clone () ]);
      let mut stack : Vec<ID> = vec![ root . clone () ];
      while let Some (cur) = stack . pop () {
        let relatives : Vec<ID> = match dir {
          OverrideDir::Overriddenward =>
            graph . outbound_pids_for_relation_gated (
              &cur, NodeRelation::OverridesViewOf, Some (active) ),
          OverrideDir::Overriderward =>
            graph . inbound_pids_for_relation_gated (
              &cur, NodeRelation::OverridesViewOf, Some (active) ), };
        for rel in relatives {
          let visible : bool = graph . nodes . get (&rel)
            . map_or ( false,
                       |n| active . contains_source (&n . source) );
          if ! visible { continue; }
          out . insert ( rel . clone () );
          if seen . insert ( rel . clone () ) {
            stack . push ( rel ); }} }} }
  out }

/// Append, under 'parent_nid', one indefinitive Independent child per
/// override relative of 'pid' in direction 'dir', recursing into each
/// relative not already on the path (cycle guard: a repeated id is
/// still drawn, so the stats pass marks it 'cycle', but its branch
/// stops).
fn graft_override_chain (
  pid        : &ID,
  parent_nid : NodeId,
  dir        : OverrideDir,
  graph      : &InRustGraph,
  viewforest : &mut Tree<ViewNode>,
  active     : &ActiveSourceSet,
  path       : &mut HashSet<ID>,
) {
  let relatives : Vec<ID> = match dir {
    OverrideDir::Overriddenward =>
      graph . outbound_pids_for_relation_gated (
        pid, NodeRelation::OverridesViewOf, Some (active) ),
    OverrideDir::Overriderward =>
      graph . inbound_pids_for_relation_gated (
        pid, NodeRelation::OverridesViewOf, Some (active) ), };
  let birth_role : RelationRole = match dir {
    OverrideDir::Overriddenward => RelationRole::OVERRIDDEN,
    OverrideDir::Overriderward  => RelationRole::OVERRIDER, };
  for rel in relatives {
    let Some (node) = graph . nodes . get (&rel) else { continue; };
    if ! active . contains_source (&node . source) { continue; }
    let child : ViewNode = mk_indefinitive_viewnode_with_birth (
      rel . clone (), node . source . clone (), node . title . clone (),
      ParentIs::Independent, Birth::Backpath (birth_role) );
    let child_nid : NodeId = {
      let mut parent_mut : NodeMut<ViewNode> =
        viewforest . get_mut (parent_nid) . unwrap ();
      parent_mut . append (child) . id () };
    if path . insert (rel . clone ()) {
      graft_override_chain (
        &rel, child_nid, dir, graph, viewforest, active, path );
      path . remove (&rel); }} }

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
