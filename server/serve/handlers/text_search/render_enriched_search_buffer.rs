use crate::dbs::tantivy::title_and_skgrepo_by_skgid;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::in_rust_graph::containerward_role_tree::ContainerwardRoleTree;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::viewnode::{Birth, Viewnode, ViewnodeKind, AffectsParent, mk_writeProtected_viewnode_with_birth};
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashMap, HashSet};

/// Insert full containerward role tree trees into the search viewforest,
/// under each level-1 result UnrestrictedVognode.
/// Role tree children are prepended (inserted first among siblings).
pub(crate) fn insert_full_containerward_role_trees_into_search_view (
  viewforest                        : &mut Tree<Viewnode>,
  graph                             : &InRustGraph,
  search_results                    : &[ID],
  containerward_role_trees_by_skgid : &HashMap<ID, ContainerwardRoleTree>,
  tantivy_index                     : &TantivyIndex,
  config                            : &SkgConfig,
  restriction                       : &SkgrepoRestriction,
) {
  // Search results ("hits") are forest roots.
  // Match them by ID from search_results.
  let level1_skgids : Vec<(NodeId, ID)> = {
    let root_ref : NodeRef<Viewnode> = viewforest . root ();
    root_ref . children ()
    . filter_map ( |c| match &c . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        => Some (( c . id (), t . skgid . clone () )),
      _ => None } )
    . collect () };
  for (node_treeid, skgid) in &level1_skgids {
    if ! search_results . contains (skgid) { continue; }
    if let Some (role_tree) = containerward_role_trees_by_skgid . get (skgid) {
      // The role tree root is the result node itself;
      // its children (containers) go under the level-1 node.
      if let ContainerwardRoleTree::Inner ( _, children ) = role_tree {
        for child in children . iter () . rev () {
          // Insert in reverse so the first child in
          // the role tree ends up first among siblings.
          insert_full_containerward_role_tree (
            child, skgid, *node_treeid,
            viewforest, graph, tantivy_index, config, restriction ); } } } } }

/// Recursively insert an ContainerwardRoleTree and its children
/// as write-protected non-content UnrestrictedVognode children
/// under the given parent. Role tree nodes are prepended.
fn insert_full_containerward_role_tree(
  node          : &ContainerwardRoleTree,
  contained_skgid  : &ID, // the node this role-tree step CONTAINS
  parent_treeid : NodeId,
  viewforest        : &mut Tree<Viewnode>,
  graph          : &InRustGraph,
  tantivy_index : &TantivyIndex,
  config        : &SkgConfig,
  restriction   : &SkgrepoRestriction,
) {
  if ! restriction . is_all () {
    // relRepo gating (render-and-gating, 5_plan.org): a private
    // MEMBERSHIP must not surface through enrichment role tree even
    // when both nodes are public. The relationship's recorder is the
    // container (this role-tree step).
    let rel_is_visible : bool =
      graph . relRepo (
        node . skgid (), NodeRelation::Contains, contained_skgid )
      . map ( |skgrepo| restriction . contains_skgrepo (&skgrepo) )
      . unwrap_or (true); // unknown relationship: fall through to the
                          // node-repo gate below, as before
    if ! rel_is_visible { return; }}
  let child_treeid : NodeId = match
    prepend_containing_child_from_tantivy (
      node . skgid (), parent_treeid,
      viewforest, tantivy_index, config, restriction ) {
        Some (child_treeid) => child_treeid,
        None => return, };
  if let ContainerwardRoleTree::Inner ( _, children ) = node {
    for child in children {
      insert_full_containerward_role_tree (
        child, node . skgid (), child_treeid,
        viewforest, graph, tantivy_index, config, restriction ); } } }

/// Which way an override role graft walks from a node, and the
/// role-graft birth it stamps on each grafted relative.
#[derive(Clone, Copy)]
enum OverrideDir {
  /// The nodes this node OVERRIDES (outbound `overrides`).
  /// Birth OVERRIDDEN; renders herald "aO" (parent overrides child).
  Overriddenward,
  /// The nodes that OVERRIDE this node (inbound). Birth OVERRIDER;
  /// renders herald "Oa" (child overrides parent).
  Overriderward,
}

/// Graft each result's override relatives -- BOTH directions -- as
/// inverted write-protected viewdescendants, so an overridden
/// node and the node(s) overriding it are navigable straight from the
/// search results
/// (TODO/DONE/override-ancestry-in-search-results.org). Each direction is
/// its own one-directional chain hanging under the result, recursive
/// and cycle-guarded. Reads the in-Rust graph (override relationships are
/// direct graph-index lookups); a relative whose override RELATIONSHIP is
/// relRepo-hidden, or whose own skgrepo is restricted, is skipped.
pub fn insert_overrideward_view_subtrees (
  viewforest     : &mut Tree<Viewnode>,
  graph          : &InRustGraph,
  search_results : &[ID],
  restriction    : &SkgrepoRestriction,
) {
  let level1_skgids : Vec<(NodeId, ID)> = {
    let root_ref : NodeRef<Viewnode> = viewforest . root ();
    root_ref . children ()
    . filter_map ( |c| match &c . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        => Some (( c . id (), t . skgid . clone () )),
      _ => None } )
    . collect () };
  for (node_treeid, skgid) in &level1_skgids {
    if ! search_results . contains (skgid) { continue; }
    for dir in [ OverrideDir::Overriddenward,
                 OverrideDir::Overriderward ] {
      let mut path : HashSet<ID> =
        HashSet::from ([ skgid . clone () ]);
      graft_override_chain (
        skgid, *node_treeid, dir, graph,
        viewforest, restriction, &mut path ); }} }

/// Every id that 'insert_overrideward_view_subtrees' would
/// graft under the given results -- the override-relative closure in
/// both directions, gated identically to the role graft. The enrichment
/// thread unions these into the graphStats pre-fetch so the grafted
/// nodes get their relationship heralds; without this they would render
/// herald-less (their graphStats would never be fetched, since the
/// role grafts do not exist yet when the pre-fetch runs). MUST stay in sync
/// with 'graft_override_chain' (same directions, same gated accessors,
/// same node-repo gate). Returns empty without a graph handle.
pub fn collect_overrideward_view_subtree_skgids (
  graph          : &InRustGraph,
  search_results : &[ID],
  restriction    : &SkgrepoRestriction,
) -> HashSet<ID> {
  let mut out : HashSet<ID> = HashSet::new ();
  for root in search_results {
    for dir in [ OverrideDir::Overriddenward,
                 OverrideDir::Overriderward ] {
      let mut seen  : HashSet<ID> = HashSet::from ([ root . clone () ]);
      let mut stack : Vec<ID> = vec![ root . clone () ];
      while let Some (cur) = stack . pop () {
        let relatives : Vec<ID> = match dir {
          OverrideDir::Overriddenward =>
            graph . outbound_pids_for_relation_gated (
              &cur, NodeRelation::Overrides, Some (restriction) ),
          OverrideDir::Overriderward =>
            graph . inbound_pids_for_relation_gated (
              &cur, NodeRelation::Overrides, Some (restriction) ), };
        for rel in relatives {
          let visible : bool = graph . nodes . get (&rel)
            . map_or ( false,
                       |n| restriction . contains_skgrepo (&n . home_skgrepo) );
          if ! visible { continue; }
          out . insert ( rel . clone () );
          if seen . insert ( rel . clone () ) {
            stack . push ( rel ); }} }} }
  out }

/// Append, under 'parent_treeid', one write-protected non-member child per
/// override relative of 'pid' in direction 'dir', recursing into each
/// relative not already on the path (cycle guard: a repeated id is
/// still drawn, so the stats pass marks it 'cycle', but its branch
/// stops).
fn graft_override_chain (
  pid           : &ID,
  parent_treeid : NodeId,
  dir           : OverrideDir,
  graph         : &InRustGraph,
  viewforest    : &mut Tree<Viewnode>,
  restriction   : &SkgrepoRestriction,
  path          : &mut HashSet<ID>,
) {
  let relatives : Vec<ID> = match dir {
    OverrideDir::Overriddenward =>
      graph . outbound_pids_for_relation_gated (
        pid, NodeRelation::Overrides, Some (restriction) ),
    OverrideDir::Overriderward =>
      graph . inbound_pids_for_relation_gated (
        pid, NodeRelation::Overrides, Some (restriction) ), };
  let birth_role : RelationRole = match dir {
    OverrideDir::Overriddenward => RelationRole::OVERRIDDEN,
    OverrideDir::Overriderward  => RelationRole::OVERRIDER, };
  for rel in relatives {
    let Some (node) = graph . nodes . get (&rel) else { continue; };
    if ! restriction . contains_skgrepo (&node . home_skgrepo) { continue; }
    let child : Viewnode = mk_writeProtected_viewnode_with_birth (
      rel . clone (), node . home_skgrepo . clone (), node . title . clone (),
      AffectsParent::False, Birth::RoleGraft (birth_role) );
    let child_treeid : NodeId = {
      let mut parent_mut : NodeMut<Viewnode> =
    viewforest . get_mut (parent_treeid) . unwrap ();
      parent_mut . append (child) . id () };
    if path . insert (rel . clone ()) {
      graft_override_chain (
        &rel, child_treeid, dir, graph, viewforest, restriction, path );
      path . remove (&rel); }} }

/// Looks up a node's title and skgrepo from Tantivy,
/// prepends a write-protected independent UnrestrictedVognode child
/// under the given parent.
/// Returns the new child's NodeId.
fn prepend_containing_child_from_tantivy (
  skgid         : &ID, // what to prepend
  parent_treeid : NodeId, // where to prepend
  viewforest        : &mut Tree<Viewnode>,
  tantivy_index : &TantivyIndex,
  _config       : &SkgConfig,
  restriction   : &SkgrepoRestriction,
) -> Option<NodeId> {
  let viewnode : Viewnode =
    match title_and_skgrepo_by_skgid ( tantivy_index, skgid ) {
      Some ((title, skgrepo)) => {
        if ! restriction . contains_skgrepo (&skgrepo) {
          return None;
        } else {
          mk_writeProtected_viewnode_with_birth (
            skgid . clone (), skgrepo, title,
            AffectsParent::False, Birth::RoleGraft (RelationRole::CONTAINER) ) }},
      None =>
        mk_writeProtected_viewnode_with_birth (
          skgid . clone (), SkgrepoName::from ("search"),
          skgid . as_str () . to_string (),
          AffectsParent::False, Birth::RoleGraft (RelationRole::CONTAINER) ) };
  let mut parent_mut : NodeMut<Viewnode> =
    viewforest . get_mut (parent_treeid) . unwrap ();
  Some (parent_mut . prepend (viewnode) . id ()) }
