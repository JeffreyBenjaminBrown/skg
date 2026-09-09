use crate::dbs::graph_queries::relations::visible_related_pids;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;

use std::collections::{HashMap, HashSet};

/// A node in the full containerward ancestry tree.
///
/// Root: a genuine root (no containers).
/// Repeated: already visited via another branch (cycle or diamond).
/// DepthTruncated: max_ancestry_depth reached; may have containers we didn't explore.
/// Inner: has at least one container; children are its containers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AncestryTree {
  Root           ( ID ),
  Repeated       ( ID ),
  DepthTruncated ( ID ),
  Inner          ( ID, Vec<AncestryTree> ),
}

impl AncestryTree {
  pub fn id ( &self ) -> &ID {
    match self {
      AncestryTree::Root           (id)      => id,
      AncestryTree::Repeated       (id)      => id,
      AncestryTree::DepthTruncated (id)      => id,
      AncestryTree::Inner          ( id, _ ) => id, }} }

/// Internal: tracks what will happen to a child node
/// during the BFS in 'full_containerward_ancestry'.
enum NodeFate {
  Open (usize), // will be expanded; usize is its position key
  Repeated,
}

/// Compute each requested node's ancestry using one supplied snapshot.
pub fn ancestry_by_id_from_ids (
  graph : &InRustGraph,
  ids : &[ID],
  max_depth : usize,
  active : Option<&ActiveSourceSet>,
) -> HashMap<ID, AncestryTree> {
  ids . iter () . map ( |id| (id . clone (),
    full_containerward_ancestry (graph, id, max_depth, active))) . collect () }

/// Breadth-first traversal, counting the origin at depth one. Cycles
/// and diamonds retain repeated leaves; source-hidden edges cannot
/// affect traversal, repetition markers, or depth truncation.
pub fn full_containerward_ancestry (
  graph     : &InRustGraph,
  origin    : &ID,
  max_depth : usize,
  active    : Option<&ActiveSourceSet>,
) -> AncestryTree {
  let origin : ID = graph . pid_of (origin) . unwrap_or_else ( || origin . clone ());
  let mut children_of : HashMap<usize, Vec<(ID, NodeFate)>> =
    HashMap::new ();
  let mut id_of : HashMap<usize, ID> =
    HashMap::new ();
  let mut depth_truncated_keys : HashSet<usize> =
    HashSet::new ();
  let mut next_key : usize = 0;
  let origin_key : usize = next_key;
  id_of . insert (origin_key, origin . clone ());
  next_key += 1;
  let mut frontier : Vec<(usize, ID)> =
    vec![(origin_key, origin . clone ())];
  let mut visited : HashSet<ID> =
    HashSet::from ([origin . clone ()]);
  let mut depth : usize = 1;
  while ! frontier . is_empty () {
    if depth >= max_depth {
      for (parent_key, _) in & frontier {
        depth_truncated_keys . insert ( *parent_key ); }
      break; }
    let mut next_frontier : Vec<(usize, ID)> =
      Vec::new ();
    for (parent_key, current_id) in & frontier {
      let containers : Vec<ID> = visible_related_pids (
        graph, current_id, RelationRole::CONTAINER . opposite_role (), active);
      if containers . is_empty () {
        children_of . insert ( *parent_key, vec![] );
      } else {
        let mut node_children : Vec<(ID, NodeFate)> =
          Vec::new ();
        for container_id in &containers {
          if visited . contains (container_id) {
            node_children . push ((
              container_id . clone (), NodeFate::Repeated ));
          } else {
            let child_key : usize = next_key;
            next_key += 1;
            id_of . insert (child_key, container_id . clone ());
            visited . insert (container_id . clone ());
            node_children . push ((
              container_id . clone (),
              NodeFate::Open ( child_key ) ));
            next_frontier . push ((
              child_key, container_id . clone () )); } }
        children_of . insert ( *parent_key, node_children ); } }
    frontier = next_frontier;
    depth += 1; }
  assemble (
    origin_key, & id_of, & children_of,
    & depth_truncated_keys ) }

fn assemble(
  key                  : usize,
  id_of                : &HashMap<usize, ID>,
  children_of          : &HashMap<usize, Vec<(ID, NodeFate)>>,
  depth_truncated_keys : &HashSet<usize>,
) -> AncestryTree {
  let id : ID =
    id_of . get (& key)
    . expect ("id_of should have every key")
    . clone ();
  if depth_truncated_keys . contains (& key) {
    return AncestryTree::DepthTruncated (id); }
  match children_of . get (& key) {
    None =>
      AncestryTree::Root (id),
    Some ( child_entries ) if child_entries . is_empty () =>
      AncestryTree::Root (id),
    Some ( child_entries ) => {
      let children : Vec<AncestryTree> =
        child_entries . iter ()
        . map ( |(child_id, fate)| match fate {
          NodeFate::Repeated =>
            AncestryTree::Repeated ( child_id . clone () ),
          NodeFate::Open ( child_key ) =>
            assemble (
              *child_key, id_of, children_of,
              depth_truncated_keys ), } )
        . collect ();
      AncestryTree::Inner ( id, children ) }, } }
