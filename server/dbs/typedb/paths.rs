use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

use crate::dbs::typedb::search::{
  find_container_ids_of_pid,
  find_related_nodes};
use crate::types::misc::ID;
use crate::types::viewnode::ContainerwardPathStats;

/// Most paths probably end without a fork or a cycle, but the same path can actually end in both: If it ends in a fork, any of the nodes in that fork might be cycles.
pub struct PathToFirstNonlinearity {
  pub path        : Vec<ID>,     // Does not include the origin.
  pub cycle_nodes : HashSet<ID>, // Nodes already in the path, if any. Unless there's a fork, there can be at most one of these.
  pub branches    : HashSet<ID>, // If the path ends in a fork, these are its's branches.
}

/// Compute containerward path stats for multiple nodes at once.
/// Level-by-level traversal: each round issues one TypeDB query
/// per frontier node (in parallel via join_all), then advances.
/// Convergent paths merge (shared containers queried only once).
///
/// PITFALL: Uses per-node queries rather than a single bulk query
/// with N-way disjunction, because TypeDB's disjunction scaling
/// is non-linear (see all_graphnodestats.rs).
pub async fn containerward_path_stats_bulk (
  db_name : &str,
  driver  : &TypeDBDriver,
  nodes   : &[ID],
) -> Result < HashMap<ID, ContainerwardPathStats>,
              Box<dyn Error> > {
  struct TrackedPath {
    original : ID,
    depth    : usize,
    visited  : HashSet<ID>,
  }
  let mut result : HashMap<ID, ContainerwardPathStats> =
    HashMap::new ();
  let mut frontier : Vec<(ID, TrackedPath)> =
    nodes . iter ()
    . map ( |id| {
      let visited : HashSet<ID> =
        HashSet::from ( [ id . clone () ] );
      ( id . clone (),
        TrackedPath {
          original : id . clone (),
          depth    : 0,
          visited } ) } )
    . collect ();
  while ! frontier . is_empty () {
    // Deduplicate frontier IDs so each distinct node
    // is queried only once (multiple tracked paths may
    // have converged onto the same frontier node).
    let unique_frontier_ids : Vec<ID> = {
      let mut seen : HashSet<ID> = HashSet::new ();
      frontier . iter ()
        . filter_map ( |(id, _)| {
          if seen . insert ( id . clone () ) {
            Some ( id . clone () )
          } else { None } } )
        . collect () };
    let futures : Vec<_> =
      unique_frontier_ids . iter ()
      . map ( |pid|
        find_container_ids_of_pid ( db_name, driver, pid ) )
      . collect ();
    let results : Vec < Result < HashSet<ID>,
                                 Box<dyn Error> > > =
      futures::future::join_all (futures) . await;
    let mut containers_map : HashMap<ID, HashSet<ID>> =
      HashMap::new ();
    for ( id, r ) in unique_frontier_ids . into_iter ()
                     . zip ( results )
    { containers_map . insert (
        id, r . unwrap_or_default () ); }
    let mut next_frontier : Vec<(ID, TrackedPath)> =
      Vec::new ();
    for (current_id, tracked) in frontier {
      let containers : HashSet<ID> =
        containers_map . get (& current_id)
        . cloned ()
        . unwrap_or_default ();
      if containers . is_empty () {
        // Root: no containers.
        result . insert (
          tracked . original,
          ContainerwardPathStats {
            length : tracked . depth,
            forks  : 1,
            cycles : false } );
      } else if containers . len () > 1 {
        // Fork: multiple containers.
        let cycles : bool =
          containers . iter ()
          . any ( |c| tracked . visited . contains (c) );
        result . insert (
          tracked . original,
          ContainerwardPathStats {
            length : tracked . depth,
            forks  : containers . len (),
            cycles } );
      } else {
        // Exactly one container.
        let container : ID =
          containers . into_iter () . next () . unwrap ();
        if tracked . visited . contains (& container) {
          // Cycle detected.
          result . insert (
            tracked . original,
            ContainerwardPathStats {
              length : tracked . depth,
              forks  : 1,
              cycles : true } );
        } else {
          // Advance: continue climbing.
          let mut next_visited : HashSet<ID> =
            tracked . visited;
          next_visited . insert ( container . clone () );
          next_frontier . push ((
            container,
            TrackedPath {
              original : tracked . original,
              depth    : tracked . depth + 1,
              visited  : next_visited } )); } } }
    frontier = next_frontier; }
  Ok (result) }

/// See path_to_first_nonlinearity.
/// This is the case that searches sourceward.
pub async fn path_containerward_to_first_nonlinearity (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < PathToFirstNonlinearity, Box<dyn Error> > {
  path_to_first_nonlinearity (
    db_name,
    driver,
    node,
    "contains",
    "contained",
    "container"
  ) . await }

/// See path_to_first_nonlinearity.
/// This is the case that searches containerward.
pub async fn path_sourceward_to_first_nonlinearity (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < PathToFirstNonlinearity, Box<dyn Error> > {
  path_to_first_nonlinearity (
    db_name,
    driver,
    node,
    "textlinks_to",
    "dest",
    "source"
  ) . await }

pub async fn paths_to_first_nonlinearities (
  db_name     : &str,
  driver      : &TypeDBDriver,
  node        : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str
) -> Result < Vec<PathToFirstNonlinearity>, Box<dyn Error> > {
  let result : PathToFirstNonlinearity =
    path_to_first_nonlinearity (
      db_name, driver, node, relation, input_role, output_role
    ) . await ?;
  if result.path.is_empty() && ! result.branches.is_empty() {
    let mut paths : Vec<PathToFirstNonlinearity> = Vec::new();
    let mut sorted_branches : Vec<ID> =
      result.branches.into_iter().collect();
    sorted_branches.sort();
    for branch_id in sorted_branches {
      let mut sub : PathToFirstNonlinearity =
        path_to_first_nonlinearity (
          db_name, driver, &branch_id,
          relation, input_role, output_role
        ) . await ?;
      sub.path.insert(0, branch_id);
      paths.push(sub); }
    Ok(paths)
  } else {
    Ok(vec![result]) } }

/// Follows a path via a single directed binary relation
///   until reaching a cycle and/or branches.
/// The path does NOT begin with the origin node,
///   but during computation the origin is retained
///   to detect cycles, so it might appear later in the path.
/// Following the specified relationship,
///   each time we find exactly one 'related node'
///   (meaning a node related in the specified manner),
///   we append it to the path.
/// The process can end in three ways:
/// 1 - If no related node is found, we return the path and exit.
///     The option and the set are both null.
/// 2 - If at any point multiple related nodes are found,
///     the choice of 'next in path' has no answer,
///     so they are added to the set,
///     nothing is added to the path, and the function returns.
/// 3 - If at any point the related node is
///     equal to one already in the path, we have found a cycle.
///     That ID becomes the option, the path is not extended,
///     and the function returns.
/// Note that 2 and 3 can coincide. That is the only case
///   in which are all three outputs non-null.
pub async fn path_to_first_nonlinearity (
  db_name     : &str,
  driver      : &TypeDBDriver,
  node        : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str
) -> Result < PathToFirstNonlinearity, Box<dyn Error> > {
  let mut path : Vec<ID> = vec![ node . clone () ];
  let mut path_set : HashSet<ID> =
    // path and path_set have the same nodes.
    HashSet::from ( [ node . clone() ] );
  let mut current_node : ID = node . clone ();
  loop {
    let related_nodes : HashSet<ID> =
      find_related_nodes (
        db_name, driver, & [ current_node . clone () ],
        relation, input_role, output_role ) . await ?;
    if related_nodes . is_empty () {
      path . remove (0); // Strip origin.
      return Ok ( PathToFirstNonlinearity {
        path, cycle_nodes: HashSet::new (), branches: HashSet::new () } );
    } else {
      let cycle_nodes : HashSet<ID> =
        related_nodes . iter ()
          . filter ( |c| path_set . contains (c) )
          . cloned ()
          . collect ();
      if ( related_nodes . len () == 1
           && cycle_nodes . is_empty () ) {
        // Add the related node to the path and continue.
        let next_node : ID =
          related_nodes . into_iter() . next() . unwrap();
        path . push ( next_node . clone () );
        path_set . insert ( next_node . clone () );
        current_node = next_node;
      } else { // We are at a fork, or a cycle, or both.
        path . remove (0); // Strip origin.
        return Ok ( PathToFirstNonlinearity {
          path,
          cycle_nodes,
          branches: if related_nodes . len () == 1 {
            HashSet::new () }
            else {related_nodes} } );
        }} }}
