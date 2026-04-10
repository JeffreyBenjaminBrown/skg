use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

use crate::dbs::typedb::search::find_container_ids_of_pid;
use crate::types::misc::ID;

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

/// Compute the full containerward ancestry tree for a given origin node.
///
/// BFS level-by-level: each round queries containers for every frontier node
/// (in parallel via join_all), then classifies each result as Root, Repeated,
/// DepthTruncated, or Inner.
///
/// Returns an AncestryTree rooted at the origin.
pub async fn full_containerward_ancestry(
  db_name   : &str,
  driver    : &TypeDBDriver,
  origin    : &ID,
  max_depth : usize,
) -> Result<AncestryTree, Box<dyn Error>> {
  // BFS state: for each node we've decided to expand,
  // record its containers (once queried). Leaves (Root,
  // Repeated, DepthTruncated) get no entry here.
  //
  // Because the same ID can appear at multiple positions
  // in the tree (e.g. a Repeated leaf and an Inner node),
  // we track tree positions, not just IDs. Each position
  // is identified by a unique usize key.
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
  // Frontier: (position key, ID) pairs to expand next round.
  let mut frontier : Vec<(usize, ID)> =
    vec![(origin_key, origin . clone ())];
  let mut visited : HashSet<ID> =
    HashSet::from ([origin . clone ()]);
  let mut depth : usize = 1;
  while ! frontier . is_empty () {
    // Depth limit: frontier nodes are at this depth.
    // If at max_depth, don't expand; leave them as leaves.
    // (They have no entry in children_of, so assemble
    // will check depth_truncated_keys to distinguish them
    // from nodes that were never reached.)
    if depth >= max_depth {
      for (parent_key, _) in & frontier {
        depth_truncated_keys . insert ( *parent_key ); }
      break; }
    let containers_map : HashMap<ID, HashSet<ID>> =
      query_containers_for_frontier (
        db_name, driver, & frontier
      ) . await ?;
    frontier =
      classify_frontier_containers (
        & frontier, & containers_map,
        & mut children_of, & mut id_of,
        & mut visited, & mut next_key );
    depth += 1; }
  // Assemble the tree from the collected structure.
  Ok ( assemble (
    origin_key, & id_of, & children_of,
    & depth_truncated_keys )) }

/// Deduplicate frontier IDs and query each node's containers
/// in parallel. Returns a map from ID to its set of container IDs.
async fn query_containers_for_frontier(
  db_name  : &str,
  driver   : &TypeDBDriver,
  frontier : &[(usize, ID)],
) -> Result<HashMap<ID, HashSet<ID>>, Box<dyn Error>> {
  let unique_ids : Vec<ID> = {
    let mut seen : HashSet<ID> = HashSet::new ();
    frontier . iter ()
      . filter_map ( |(_, id)| {
        if seen . insert ( id . clone () ) {
          Some ( id . clone () )
        } else { None } } )
      . collect () };
  let futures : Vec<_> =
    unique_ids . iter ()
    . map ( |pid|
      find_container_ids_of_pid ( db_name, driver, pid ) )
    . collect ();
  let results : Vec<Result<HashSet<ID>, Box<dyn Error>>> =
    futures::future::join_all (futures) . await;
  let mut containers_map : HashMap<ID, HashSet<ID>> =
    HashMap::new ();
  for ( id, r ) in unique_ids . into_iter ()
                   . zip ( results )
  { containers_map . insert ( id, r? ); }
  Ok ( containers_map ) }

/// Walk each frontier node's containers and classify them:
/// - No containers → Root (empty children_of entry)
/// - Already visited → Repeated leaf
/// - New → Open (added to next frontier and visited set)
///
/// Returns the next frontier.
fn classify_frontier_containers(
  frontier       : &[(usize, ID)],
  containers_map : &HashMap<ID, HashSet<ID>>,
  children_of    : &mut HashMap<usize, Vec<(ID, NodeFate)>>,
  id_of          : &mut HashMap<usize, ID>,
  visited        : &mut HashSet<ID>,
  next_key       : &mut usize,
) -> Vec<(usize, ID)> {
  let mut next_frontier : Vec<(usize, ID)> =
    Vec::new ();
  for (parent_key, current_id) in frontier {
    let containers : &HashSet<ID> =
      containers_map . get (current_id)
      . expect ("containers_map should have every frontier ID");
    if containers . is_empty () {
      // No containers: this node is a Root leaf.
      children_of . insert (
        *parent_key, vec![] );
    } else {
      let mut node_children : Vec<(ID, NodeFate)> =
        Vec::new ();
      for container_id in containers {
        if visited . contains (container_id) {
          // Already seen: Repeated leaf.
          node_children . push ((
            container_id . clone (),
            NodeFate::Repeated ));
        } else {
          // Open: will be expanded next round.
          let child_key : usize = *next_key;
          *next_key += 1;
          id_of . insert (child_key, container_id . clone ());
          visited . insert (container_id . clone ());
          node_children . push ((
            container_id . clone (),
            NodeFate::Open ( child_key ) ));
          next_frontier . push ((
            child_key, container_id . clone () )); } }
      children_of . insert (
        *parent_key, node_children ); } }
  next_frontier }

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

/// Compute full containerward ancestry for each ID in parallel.
/// IDs whose ancestry lookup fails are logged and omitted.
pub async fn ancestry_by_id_from_ids_async (
  ids       : &[ID],
  db_name   : &str,
  driver    : &TypeDBDriver,
  max_depth : usize,
) -> HashMap<ID, AncestryTree> {
  let futures : Vec<_> =
    ids . iter ()
    . map ( |id|
      full_containerward_ancestry (
        db_name, driver, id, max_depth ) )
    . collect ();
  let results : Vec<Result<AncestryTree,
                           Box<dyn Error>>> =
    futures::future::join_all (futures) . await;
  let mut map : HashMap<ID, AncestryTree> =
    HashMap::new ();
  for ( id, result ) in ids . iter ()
                        . zip ( results )
  { match result {
      Ok (tree) => { map . insert ( id . clone (), tree ); },
      Err (e) => {
        tracing::warn! (
          "ancestry_by_id_from_ids_async: {} failed: {}",
          id, e ); }} }
  map }
