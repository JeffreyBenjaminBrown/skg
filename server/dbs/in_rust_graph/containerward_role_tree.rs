use std::collections::{HashMap, HashSet};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::misc::ID;

/// The partners playing 'role' toward a node, as a function, so that
/// the containerward role tree can walk the live graph
/// ('containers_in_inRustGraph') or the union of a graph straddle
/// ('GraphStraddle::partners').
pub type PartnersOf<'a> = &'a dyn Fn (&ID, RelationRole) -> HashSet<ID>;

/// A node in the full containerward role tree.
///
/// Root: a genuine root (no containers).
/// Repeated: already visited via another branch (cycle or diamond).
/// DepthTruncated: max_roleTree_depth reached; may have containers we didn't explore.
/// Inner: plays content to at least one container; children are its containers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContainerwardRoleTree {
  Root           ( ID ),
  Repeated       ( ID ),
  DepthTruncated ( ID ),
  Inner          ( ID, Vec<ContainerwardRoleTree> ),
}

impl ContainerwardRoleTree {
  pub fn gnid ( &self ) -> &ID {
    match self {
      ContainerwardRoleTree::Root           (gnid)      => gnid,
      ContainerwardRoleTree::Repeated       (gnid)      => gnid,
      ContainerwardRoleTree::DepthTruncated (gnid)      => gnid,
      ContainerwardRoleTree::Inner          ( gnid, _ ) => gnid, }} }

/// Internal: tracks what will happen to a child node
/// during the BFS in 'full_containerward_role_tree'.
enum NodeFate {
  Open (usize), // will be extended; usize is its position key
  Repeated,
}

/// Recursively build an ContainerwardRoleTree from the BFS maps.
fn assemble(
  key                  : usize,
  id_of                : &HashMap<usize, ID>,
  children_of          : &HashMap<usize, Vec<(ID, NodeFate)>>,
  depth_truncated_keys : &HashSet<usize>,
) -> ContainerwardRoleTree {
  let gnid : ID =
    id_of . get (& key)
    . expect ("id_of should have every key")
    . clone ();
  if depth_truncated_keys . contains (& key) {
    return ContainerwardRoleTree::DepthTruncated (gnid); }
  match children_of . get (& key) {
    None =>
      ContainerwardRoleTree::Root (gnid),
    Some ( child_entries ) if child_entries . is_empty () =>
      ContainerwardRoleTree::Root (gnid),
    Some ( child_entries ) => {
      let children : Vec<ContainerwardRoleTree> =
        child_entries . iter ()
        . map ( |(child_gnid, fate)| match fate {
          NodeFate::Repeated =>
            ContainerwardRoleTree::Repeated ( child_gnid . clone () ),
          NodeFate::Open ( child_key ) =>
            assemble (
              *child_key, id_of, children_of,
              depth_truncated_keys ), } )
        . collect ();
      ContainerwardRoleTree::Inner ( gnid, children ) }, } }

/// In-Rust-graph containerward role tree, walking the 'contained_by'
/// recorderward relmap.
pub fn full_containerward_role_tree_from_inRustGraph (
  graph     : &InRustGraph,
  origin    : &ID,
  max_depth : usize,
) -> ContainerwardRoleTree {
  full_containerward_role_tree (
    &containers_in_inRustGraph (graph), origin, max_depth ) }

/// The containers of a node, through the graph's 'contained_by'
/// recorderward relmap, as the partners function of the containerward
/// role tree. Ungated: the caller gates what it shows.
pub fn containers_in_inRustGraph (
  graph : &InRustGraph,
) -> impl Fn (&ID, RelationRole) -> HashSet<ID> + '_ {
  move |gnid : &ID, _role : RelationRole| -> HashSet<ID> {
    let empty : im::HashSet<ID> = im::HashSet::new ();
    graph . contained_by . get (gnid)
      . unwrap_or (&empty)
      . iter () . cloned () . collect () } }

/// The containerward role tree of 'origin', walking the containers
/// that 'partners_of' gives (asked for the CONTAINER role), so it can
/// walk the live graph or a graph straddle's union. Uses breadth-first
/// traversal with no async / no parallel queries / no
/// frontier-batching.
pub fn full_containerward_role_tree (
  partners_of : PartnersOf,
  origin      : &ID,
  max_depth   : usize,
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
    for (parent_key, current_gnid) in & frontier {
      let containers : Vec<ID> = { // sorted, so that a walk is deterministic
        let mut containers : Vec<ID> =
          partners_of (current_gnid, RelationRole::CONTAINER)
          . into_iter () . collect ();
        containers . sort ();
        containers };
      if containers . is_empty () {
        children_of . insert ( *parent_key, vec![] );
      } else {
        let mut node_children : Vec<(ID, NodeFate)> =
          Vec::new ();
        for container_gnid in &containers {
          if visited . contains (container_gnid) {
            node_children . push ((
              container_gnid . clone (), NodeFate::Repeated ));
          } else {
            let child_key : usize = next_key;
            next_key += 1;
            id_of . insert (child_key, container_gnid . clone ());
            visited . insert (container_gnid . clone ());
            node_children . push ((
              container_gnid . clone (),
              NodeFate::Open ( child_key ) ));
            next_frontier . push ((
              child_key, container_gnid . clone () )); } }
        children_of . insert ( *parent_key, node_children ); } }
    frontier = next_frontier;
    depth += 1; }
  assemble (
    origin_key, & id_of, & children_of,
    & depth_truncated_keys ) }

/// Compute full containerward role tree for each ID, walking the
/// containers that 'partners_of' gives.
pub fn containerward_role_trees_by_gnid_from_gnids (
  partners_of : PartnersOf,
  gnids      : &[ID],
  max_depth   : usize,
) -> HashMap<ID, ContainerwardRoleTree> {
  let mut map : HashMap<ID, ContainerwardRoleTree> =
    HashMap::new ();
  for gnid in gnids {
    map . insert (
      gnid . clone (),
      full_containerward_role_tree (
        partners_of, gnid, max_depth ) ); }
  map }
