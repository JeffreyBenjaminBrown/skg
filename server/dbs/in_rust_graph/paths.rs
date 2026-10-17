use std::collections::HashSet;
use std::error::Error;

use crate::dbs::in_rust_graph::query::find_related_nodes;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationRole};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::ID;

/// A *branching graphnode* has more than one partner in the direction
/// walked: walking containerward, more than one container -- it is
/// "containerward-branching". (Once drawn, it becomes a branching
/// viewnode.) Most paths probably end without a branching graphnode or
/// a cycle, but the same path can actually end in both: if it ends in a
/// branching graphnode, any of its branches might be cycles.
pub struct PathToFirstNonlinearity {
  pub path        : Vec<ID>,     // Does not include the origin.
  pub cycle_nodes : HashSet<ID>, // Nodes already in the path, if any. Unless there is a branching graphnode, there can be at most one of these.
  pub branches    : HashSet<ID>, // If the path ends in a branching graphnode, these are its branches.
}

/// The partners one step from a node in a role tree: those playing
/// 'role' toward it (the role the PARTNER plays, as in
/// 'PARTNER_ROLE_VOCAB'). The role-tree engine walks any such
/// function: the live graph's ('role_partners_in_graph_gated'), or the
/// union of a graph pair's ('GraphPair::partners').
pub type PartnersOf<'a> = &'a dyn Fn (&ID, RelationRole) -> HashSet<ID>;

/// The partners playing 'role' toward 'node' in 'graph' whose
/// relationship and node the skgrepo restriction lets one see.
pub fn role_partners_in_graph_gated (
  graph       : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
  node        : &ID,
  role        : RelationRole,
) -> HashSet<ID> {
  let (relation, input_role, output_role)
    : (&'static str, &'static str, &'static str) =
    role . role_tree_triple ();
  related_nodes_from_graph_gated (
    graph, restriction, node, role . relation,
    ! role . is_first_role (), // the origin plays the other role
    relation, input_role, output_role ) }

/// Graph-native path traversal. Visibility is applied before topology is
/// classified, so private relationships and private targets cannot create apparent
/// branching graphnodes, cycles, or extra path length.
pub fn paths_to_first_nonlinearities_in_graph (
  graph       : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
  node        : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Result<Vec<PathToFirstNonlinearity>, Box<dyn Error>> {
  let partners = partners_in_graph_for_roles (
    graph, restriction, relation, input_role, output_role ) ?;
  Ok ( paths_to_first_nonlinearities_via (&partners, node) ) }

/// 'paths_to_first_nonlinearities_in_graph' over any function giving
/// the partners one step from a node.
pub fn paths_to_first_nonlinearities_via (
  partners_of : &dyn Fn (&ID) -> HashSet<ID>,
  node        : &ID,
) -> Vec<PathToFirstNonlinearity> {
  let result = path_to_first_nonlinearity_via (partners_of, node);
  if result . path . is_empty () && ! result . branches . is_empty () {
    let mut branches : Vec<ID> = result . branches . into_iter () . collect ();
    branches . sort ();
    let mut paths = Vec::with_capacity (branches . len ());
    for branch in branches {
      let mut sub = path_to_first_nonlinearity_via (partners_of, &branch);
      sub . path . insert (0, branch);
      paths . push (sub); }
    paths
  } else { vec! [result] }
}

/// Graph-native containerward path traversal used by tests and callers
/// that need one path rather than the branch-expanded representation.
pub fn path_containerward_to_first_nonlinearity_in_graph (
  graph : &InRustGraph,
  node  : &ID,
) -> Result<PathToFirstNonlinearity, Box<dyn Error>> {
  path_to_first_nonlinearity_in_graph (
    graph, None, node, "contains", "content", "container") }

fn path_to_first_nonlinearity_in_graph (
  graph       : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
  node        : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Result<PathToFirstNonlinearity, Box<dyn Error>> {
  let partners = partners_in_graph_for_roles (
    graph, restriction, relation, input_role, output_role ) ?;
  Ok ( path_to_first_nonlinearity_via (&partners, node) ) }

/// The partners of a node in 'graph' at 'output_role' of 'relation',
/// gated by 'restriction'. Refuses roles that are not the relation's.
fn partners_in_graph_for_roles<'a> (
  graph       : &'a InRustGraph,
  restriction : Option<&'a SkgrepoRestriction>,
  relation    : &'a str,
  input_role  : &'a str,
  output_role : &'a str,
) -> Result<impl Fn (&ID) -> HashSet<ID> + 'a, Box<dyn Error>> {
  let relation_kind = NodeRelation::from_relation_name (relation)
    . ok_or_else (|| format! ("Unknown graph relation: {}", relation))?;
  let (first_role, second_role) = relation_kind . roles ();
  if ! ((input_role == first_role && output_role == second_role)
        || (input_role == second_role && output_role == first_role)) {
    return Err (format! (
      "Invalid roles {} -> {} for relation {}",
      input_role, output_role, relation) . into ()); }
  Ok ( move |current : &ID| related_nodes_from_graph_gated (
    graph, restriction, current, relation_kind,
    input_role == first_role, relation, input_role, output_role ) ) }

fn path_to_first_nonlinearity_via (
  partners_of : &dyn Fn (&ID) -> HashSet<ID>,
  node        : &ID,
) -> PathToFirstNonlinearity {
  let mut path = vec! [node . clone ()];
  let mut path_set = HashSet::from ([node . clone ()]);
  let mut current = node . clone ();
  loop {
    let related = partners_of (&current);
    if related . is_empty () {
      path . remove (0);
      return PathToFirstNonlinearity {
        path, cycle_nodes : HashSet::new (), branches : HashSet::new () }; }
    let cycle_nodes : HashSet<ID> = related . iter ()
      . filter (|skgid| path_set . contains (*skgid)) . cloned () . collect ();
    if related . len () == 1 && cycle_nodes . is_empty () {
      let next = related . into_iter () . next () . unwrap ();
      path . push (next . clone ());
      path_set . insert (next . clone ());
      current = next;
    } else {
      path . remove (0);
      return PathToFirstNonlinearity {
        path, cycle_nodes,
        branches : if related . len () == 1 {
          HashSet::new () } else { related }, }; }
  }
}

fn related_nodes_from_graph_gated (
  graph       : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
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
    . filter (|partner| match restriction {
      None => true,
      Some (set) if set . is_all () => true,
      Some (set) => {
        let target_is_unrestricted = graph . pid_and_skgrepo (partner)
          . map (|(_, skgrepo)| set . contains_skgrepo (&skgrepo))
          . unwrap_or (false);
        let relRepo = if origin_is_first_role {
          graph . relRepo (origin, relation, partner)
        } else {
          graph . relRepo (partner, relation, origin) };
        target_is_unrestricted && relRepo
          . map (|skgrepo| set . contains_skgrepo (&skgrepo))
          . unwrap_or (false) } })
    . collect ()
}

/// Graph-native containerward traversal for one origin.
pub fn path_containerward_to_first_nonlinearity (
  graph : &InRustGraph,
  node  : &ID,
) -> Result<PathToFirstNonlinearity, Box<dyn Error>> {
  path_to_first_nonlinearity_in_graph (
    graph, None, node, "contains", "content", "container") }
