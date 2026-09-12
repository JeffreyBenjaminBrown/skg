use std::collections::{HashMap, HashSet};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::ID;

/// A node in the full containerward ancestry tree.
///
/// Root: a genuine root (no containers).
/// Repeated: already visited via another branch (cycle or diamond).
/// DepthTruncated: max_ancestry_depth reached; may have containers we didn't explore.
/// Inner: plays content to at least one container; children are its containers.
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

/// Recursively build an AncestryTree from the BFS maps.
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

/// In-Rust-graph containerward ancestry, walking the 'contained_by'
/// inverse index. Uses breadth-first traversal with no
/// async / no parallel queries / no frontier-batching.
pub fn full_containerward_ancestry_from_in_rust_graph (
  graph     : &InRustGraph,
  origin    : &ID,
  max_depth : usize,
) -> AncestryTree {
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
      let containers : HashSet<ID> = {
        let empty : im::HashSet<ID> = im::HashSet::new ();
        graph . contained_by . get (current_id)
          . unwrap_or (&empty)
          . iter () . cloned () . collect () };
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

/// Compute full containerward ancestry for each ID.
pub fn ancestry_by_id_from_ids (
  graph     : &InRustGraph,
  ids       : &[ID],
  max_depth : usize,
) -> HashMap<ID, AncestryTree> {
  let mut map : HashMap<ID, AncestryTree> =
    HashMap::new ();
  for id in ids {
    map . insert (
      id . clone (),
      full_containerward_ancestry_from_in_rust_graph (
        graph, id, max_depth ) ); }
  map }
