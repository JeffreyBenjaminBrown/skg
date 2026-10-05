use std::collections::{HashMap, HashSet};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::ID;

/// A node in the full containerward role tree tree.
///
/// Root: a genuine root (no containers).
/// Repeated: already visited via another branch (cycle or diamond).
/// DepthTruncated: max_role_tree_depth reached; may have containers we didn't explore.
/// Inner: plays content to at least one container; children are its containers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContainerwardRoleTree {
  Root           ( ID ),
  Repeated       ( ID ),
  DepthTruncated ( ID ),
  Inner          ( ID, Vec<ContainerwardRoleTree> ),
}

impl ContainerwardRoleTree {
  pub fn skgid ( &self ) -> &ID {
    match self {
      ContainerwardRoleTree::Root           (skgid)      => skgid,
      ContainerwardRoleTree::Repeated       (skgid)      => skgid,
      ContainerwardRoleTree::DepthTruncated (skgid)      => skgid,
      ContainerwardRoleTree::Inner          ( skgid, _ ) => skgid, }} }

/// Internal: tracks what will happen to a child node
/// during the BFS in 'full_containerward_role_tree'.
enum NodeFate {
  Open (usize), // will be expanded; usize is its position key
  Repeated,
}

/// Recursively build an ContainerwardRoleTree from the BFS maps.
fn assemble(
  key                  : usize,
  id_of                : &HashMap<usize, ID>,
  children_of          : &HashMap<usize, Vec<(ID, NodeFate)>>,
  depth_truncated_keys : &HashSet<usize>,
) -> ContainerwardRoleTree {
  let skgid : ID =
    id_of . get (& key)
    . expect ("id_of should have every key")
    . clone ();
  if depth_truncated_keys . contains (& key) {
    return ContainerwardRoleTree::DepthTruncated (skgid); }
  match children_of . get (& key) {
    None =>
      ContainerwardRoleTree::Root (skgid),
    Some ( child_entries ) if child_entries . is_empty () =>
      ContainerwardRoleTree::Root (skgid),
    Some ( child_entries ) => {
      let children : Vec<ContainerwardRoleTree> =
        child_entries . iter ()
        . map ( |(child_skgid, fate)| match fate {
          NodeFate::Repeated =>
            ContainerwardRoleTree::Repeated ( child_skgid . clone () ),
          NodeFate::Open ( child_key ) =>
            assemble (
              *child_key, id_of, children_of,
              depth_truncated_keys ), } )
        . collect ();
      ContainerwardRoleTree::Inner ( skgid, children ) }, } }

/// In-Rust-graph containerward role tree, walking the 'contained_by'
/// inverse index. Uses breadth-first traversal with no
/// async / no parallel queries / no frontier-batching.
pub fn full_containerward_role_tree_from_in_rust_graph (
  graph     : &InRustGraph,
  origin    : &ID,
  max_depth : usize,
) -> ContainerwardRoleTree {
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
    for (parent_key, current_skgid) in & frontier {
      let containers : HashSet<ID> = {
        let empty : im::HashSet<ID> = im::HashSet::new ();
        graph . contained_by . get (current_skgid)
          . unwrap_or (&empty)
          . iter () . cloned () . collect () };
      if containers . is_empty () {
        children_of . insert ( *parent_key, vec![] );
      } else {
        let mut node_children : Vec<(ID, NodeFate)> =
          Vec::new ();
        for container_skgid in &containers {
          if visited . contains (container_skgid) {
            node_children . push ((
              container_skgid . clone (), NodeFate::Repeated ));
          } else {
            let child_key : usize = next_key;
            next_key += 1;
            id_of . insert (child_key, container_skgid . clone ());
            visited . insert (container_skgid . clone ());
            node_children . push ((
              container_skgid . clone (),
              NodeFate::Open ( child_key ) ));
            next_frontier . push ((
              child_key, container_skgid . clone () )); } }
        children_of . insert ( *parent_key, node_children ); } }
    frontier = next_frontier;
    depth += 1; }
  assemble (
    origin_key, & id_of, & children_of,
    & depth_truncated_keys ) }

/// Compute full containerward role tree for each ID.
pub fn containerward_role_trees_by_skgid_from_skgids (
  graph     : &InRustGraph,
  skgids    : &[ID],
  max_depth : usize,
) -> HashMap<ID, ContainerwardRoleTree> {
  let mut map : HashMap<ID, ContainerwardRoleTree> =
    HashMap::new ();
  for skgid in skgids {
    map . insert (
      skgid . clone (),
      full_containerward_role_tree_from_in_rust_graph (
        graph, skgid, max_depth ) ); }
  map }
