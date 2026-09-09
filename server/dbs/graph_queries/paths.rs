use crate::dbs::graph_queries::relations::visible_related_pids;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;
use crate::types::viewnode::ContainerwardPathStats;

use std::collections::{HashMap, HashSet};

/// A linear path followed by the first fork, cycle, or both.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PathToFirstNonlinearity {
  pub path        : Vec<ID>,
  pub cycle_nodes : HashSet<ID>,
  pub branches    : HashSet<ID>,
}

/// Expand an immediate fork into one path per branch, in ID order.
pub fn paths_to_first_nonlinearities (
  graph  : &InRustGraph,
  node   : &ID,
  role   : RelationRole,
  active : Option<&ActiveSourceSet>,
) -> Vec<PathToFirstNonlinearity> {
  let result : PathToFirstNonlinearity =
    path_to_first_nonlinearity (graph, node, role, active);
  if !result . path . is_empty () || result . branches . is_empty () {
    return vec![result]; }
  let mut branches : Vec<ID> = result . branches . into_iter () . collect ();
  branches . sort ();
  branches . into_iter () . map ( |branch| {
    let mut path : PathToFirstNonlinearity =
      path_to_first_nonlinearity (graph, &branch, role, active);
    path . path . insert (0, branch);
    path } ) . collect () }

/// Traverse the selected graph, applying privacy before it shapes a
/// fork or cycle. The role names the partner being drawn.
pub fn path_to_first_nonlinearity (
  graph  : &InRustGraph,
  node   : &ID,
  role   : RelationRole,
  active : Option<&ActiveSourceSet>,
) -> PathToFirstNonlinearity {
  let origin : ID = graph . pid_of (node) . unwrap_or_else ( || node . clone () );
  let mut current : ID = origin . clone ();
  let mut visited : HashSet<ID> = HashSet::from ([origin]);
  let mut path : Vec<ID> = Vec::new ();
  loop {
    let related : HashSet<ID> = visible_related_pids (
      graph, &current, role . opposite_role (), active)
      . into_iter () . collect ();
    let cycle_nodes : HashSet<ID> = related . intersection (&visited)
      . cloned () . collect ();
    if related . len () != 1 || !cycle_nodes . is_empty () {
      return PathToFirstNonlinearity {
        path, cycle_nodes,
        branches : if related . len () > 1 { related }
                   else { HashSet::new () }, }; }
    current = related . into_iter () . next () . unwrap ();
    visited . insert (current . clone ());
    path . push (current . clone ()); } }

pub fn path_containerward_to_first_nonlinearity (
  graph : &InRustGraph,
  node : &ID,
  active : Option<&ActiveSourceSet>,
) -> PathToFirstNonlinearity {
  path_to_first_nonlinearity (graph, node, RelationRole::CONTAINER, active) }

/// Compute path lengths and first fork/cycle counts. Frontier queries
/// are shared when several paths converge on the same selected node.
pub fn containerward_path_stats_bulk (
  graph : &InRustGraph,
  nodes : &[ID],
  active : Option<&ActiveSourceSet>,
) -> HashMap<ID, ContainerwardPathStats> {
  let mut result : HashMap<ID, ContainerwardPathStats> = HashMap::new ();
  let mut frontier : Vec<(ID, TrackedPath)> = nodes . iter () . map ( |id| {
    let pid : ID = graph . pid_of (id) . unwrap_or_else ( || id . clone ());
    (pid . clone (), TrackedPath {
      original : id . clone (), depth : 0,
      visited : HashSet::from ([pid]), }) }) . collect ();
  while !frontier . is_empty () {
    let unique_ids : HashSet<ID> = frontier . iter () . map ( |(id, _)| id . clone ())
      . collect ();
    let containers : HashMap<ID, Vec<ID>> = unique_ids . into_iter () . map ( |id| {
      let members : Vec<ID> = visible_related_pids (
        graph, &id, RelationRole::CONTAINER . opposite_role (), active);
      (id, members) }) . collect ();
    let mut next : Vec<(ID, TrackedPath)> = Vec::new ();
    for (current, mut path) in frontier {
      let members : &[ID] = &containers [&current];
      let cycles : bool = members . iter () . any ( |id| path . visited . contains (id));
      if members . len () != 1 || cycles {
        result . insert (path . original, ContainerwardPathStats {
          length : path . depth, forks : members . len () . max (1), cycles });
      } else {
        let member : ID = members [0] . clone ();
        path . visited . insert (member . clone ());
        path . depth += 1;
        next . push ((member, path)); } }
    frontier = next; }
  result }

struct TrackedPath {
  original : ID,
  depth : usize,
  visited : HashSet<ID>,
}
