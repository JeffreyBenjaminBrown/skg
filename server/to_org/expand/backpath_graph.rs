use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;

use std::collections::HashSet;

/// A linear path followed by the first fork, cycle, or both.
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
fn path_to_first_nonlinearity (
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
    let related : HashSet<ID> = graph . other_member_pids_gated (
      &current, role . opposite_role (), active )
      . into_iter ()
      . filter ( |id| active . is_none_or ( |active|
          graph . pid_and_source (id)
            . is_none_or ( |(_, source)| active . contains_source (&source) )))
      . collect ();
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
