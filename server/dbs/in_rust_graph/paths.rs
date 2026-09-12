use std::collections::HashSet;
use std::error::Error;

use crate::dbs::in_rust_graph::query::find_related_nodes;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;

/// Most paths probably end without a fork or a cycle, but the same path can actually end in both: If it ends in a fork, any of the nodes in that fork might be cycles.
pub struct PathToFirstNonlinearity {
  pub path        : Vec<ID>,     // Does not include the origin.
  pub cycle_nodes : HashSet<ID>, // Nodes already in the path, if any. Unless there's a fork, there can be at most one of these.
  pub branches    : HashSet<ID>, // If the path ends in a fork, these are its's branches.
}

/// Graph-native path traversal. Visibility is applied before topology is
/// classified, so private edges and private targets cannot create apparent
/// forks, cycles, or extra path length.
pub fn paths_to_first_nonlinearities_in_graph (
  graph       : &InRustGraph,
  active      : Option<&ActiveSourceSet>,
  node        : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Result<Vec<PathToFirstNonlinearity>, Box<dyn Error>> {
  let result = path_to_first_nonlinearity_in_graph (
    graph, active, node, relation, input_role, output_role)?;
  if result . path . is_empty () && ! result . branches . is_empty () {
    let mut branches : Vec<ID> = result . branches . into_iter () . collect ();
    branches . sort ();
    let mut paths = Vec::with_capacity (branches . len ());
    for branch in branches {
      let mut sub = path_to_first_nonlinearity_in_graph (
        graph, active, &branch, relation, input_role, output_role)?;
      sub . path . insert (0, branch);
      paths . push (sub); }
    Ok (paths)
  } else { Ok (vec! [result]) }
}

/// Graph-native containerward path traversal used by tests and callers
/// that need one path rather than the branch-expanded representation.
pub fn path_containerward_to_first_nonlinearity_in_graph (
  graph : &InRustGraph,
  node  : &ID,
) -> Result<PathToFirstNonlinearity, Box<dyn Error>> {
  path_to_first_nonlinearity_in_graph (
    graph, None, node, "contains", "contained", "container") }

fn path_to_first_nonlinearity_in_graph (
  graph       : &InRustGraph,
  active      : Option<&ActiveSourceSet>,
  node        : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Result<PathToFirstNonlinearity, Box<dyn Error>> {
  let relation_kind = node_relation_from_name (relation)
    . ok_or_else (|| format! ("Unknown graph relation: {}", relation))?;
  let (first_role, second_role) = relation_kind . roles ();
  if ! ((input_role == first_role && output_role == second_role)
        || (input_role == second_role && output_role == first_role)) {
    return Err (format! (
      "Invalid roles {} -> {} for relation {}",
      input_role, output_role, relation) . into ()); }
  let mut path = vec! [node . clone ()];
  let mut path_set = HashSet::from ([node . clone ()]);
  let mut current = node . clone ();
  loop {
    let related = related_nodes_from_graph_gated (
      graph, active, &current, relation_kind,
      input_role == first_role, relation, input_role, output_role);
    if related . is_empty () {
      path . remove (0);
      return Ok (PathToFirstNonlinearity {
        path, cycle_nodes : HashSet::new (), branches : HashSet::new () }); }
    let cycle_nodes : HashSet<ID> = related . iter ()
      . filter (|id| path_set . contains (*id)) . cloned () . collect ();
    if related . len () == 1 && cycle_nodes . is_empty () {
      let next = related . into_iter () . next () . unwrap ();
      path . push (next . clone ());
      path_set . insert (next . clone ());
      current = next;
    } else {
      path . remove (0);
      return Ok (PathToFirstNonlinearity {
        path, cycle_nodes,
        branches : if related . len () == 1 {
          HashSet::new () } else { related }, }); }
  }
}

fn related_nodes_from_graph_gated (
  graph       : &InRustGraph,
  active      : Option<&ActiveSourceSet>,
  origin      : &ID,
  relation    : NodeRelation,
  origin_is_first_role : bool,
  relation_name : &str,
  input_role  : &str,
  output_role : &str,
) -> HashSet<ID> {
  find_related_nodes (
    graph, &[origin . clone ()], relation_name, input_role, output_role)
    . into_iter ()
    . filter (|partner| match active {
      None => true,
      Some (set) if set . is_all () => true,
      Some (set) => {
        let target_is_active = graph . pid_and_source (partner)
          . map (|(_, source)| set . contains_source (&source))
          . unwrap_or (false);
        let edge_source = if origin_is_first_role {
          graph . edge_source (origin, relation, partner)
        } else {
          graph . edge_source (partner, relation, origin) };
        target_is_active && edge_source
          . map (|source| set . contains_source (&source))
          . unwrap_or (false) } })
    . collect ()
}

fn node_relation_from_name (name : &str) -> Option<NodeRelation> {
  [ NodeRelation::Contains, NodeRelation::TextlinksTo,
    NodeRelation::Subscribes, NodeRelation::HidesFromItsSubscriptions,
    NodeRelation::OverridesViewOf ]
    . into_iter () . find (|relation| relation . relation_name () == name)
}

/// Graph-native containerward traversal for one origin.
pub fn path_containerward_to_first_nonlinearity (
  graph : &InRustGraph,
  node  : &ID,
) -> Result<PathToFirstNonlinearity, Box<dyn Error>> {
  path_to_first_nonlinearity_in_graph (
    graph, None, node, "contains", "contained", "container") }
