/// TODO ? Maybe this module could be simpler,
/// by reusing the existing code for executing a
/// containerward view request.

use crate::dbs::tantivy::title_and_source_by_id;
use crate::dbs::typedb::paths::PathToFirstNonlinearity;
use crate::dbs::typedb::search::find_container_ids_of_pid;
use crate::types::memory::skgnode_from_map_or_disk;
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::{
  ViewNode, ViewNodeKind, Scaffold, GraphNodeStats};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use typedb_driver::TypeDBDriver;

struct TrackedPath {
  origin   : ID,          // the path starts here
  path     : Vec<ID>,     // does not include the origin
  visited  : HashSet<ID>, // *does* include the origin
}

/// Insert containerward path nodes into the search forest,
/// under each level-2 result node.
/// Also populates the pool with SkgNodes for each path node.
pub(crate) fn insert_containerward_paths_into_search_view (
  forest        : &mut Tree<ViewNode>,
  result_ids    : &[ID],
  paths_by_id   : &HashMap < ID, Vec < PathToFirstNonlinearity > >,
  tantivy_index : &TantivyIndex,
  pool          : &mut HashMap<ID, SkgNode>,
  config        : &SkgConfig,
) {
  let level1_id : NodeId = { // the unique level-1 headline
    let root_ref : NodeRef<ViewNode> =
      forest . root ();
    match root_ref . children () . next () {
      Some (child) => child . id (),
      None => return } };
  let level2_ids : Vec < NodeId > = // the matches
    forest . get (level1_id) . unwrap ()
    . children ()
    . map ( |c| c . id () )
    . collect ();
  for (block_idx, &level2_nid)
    in level2_ids . iter () . enumerate ()
    // insert paths under each
  { if block_idx >= result_ids . len () { break; }
    let id : &ID = &result_ids [ block_idx ];
    if let Some (paths) = paths_by_id . get (id) {
      for path in paths {
        // Each path step becomes a deeper child.
        let mut parent_nid : NodeId = level2_nid;
        for path_node_id in &path . path {
          parent_nid = append_search_result_child_from_tantivy (
            path_node_id, parent_nid,
            forest, tantivy_index, pool, config ); }
        if ! path . branches . is_empty () // It ends at a fork.
        { let mut sorted_branches : Vec < &ID > =
            path . branches . iter () . collect ();
          sorted_branches . sort ();
          for branch_id in sorted_branches {
            append_search_result_child_from_tantivy (
              branch_id, parent_nid,
              forest, tantivy_index, pool, config ); }}
        if ! path . cycle_nodes . is_empty ()
           && path . branches . is_empty ()
           // it ends at a non-fork cycle
        { let mut sorted_cycles : Vec < &ID > =
            path . cycle_nodes . iter () . collect ();
          sorted_cycles . sort ();
          for cycle_id in sorted_cycles {
            append_search_result_child_from_tantivy (
              cycle_id, parent_nid,
              forest, tantivy_index, pool, config ); }} }} }}

/// Looks up a node's title and source from Tantivy,
/// populates the SkgNode pool, and
/// appends a SearchResult scaffold child under the given parent.
/// Returns the new child's NodeId.
fn append_search_result_child_from_tantivy (
  node_id       : &ID,
  parent_nid    : NodeId,
  forest        : &mut Tree<ViewNode>,
  tantivy_index : &TantivyIndex,
  pool          : &mut HashMap<ID, SkgNode>,
  config        : &SkgConfig,
) -> NodeId {
  let (title, source) : (String, SourceName) =
    title_and_source_by_id ( tantivy_index, node_id )
    . unwrap_or_else ( ||
      ( node_id . as_str () . to_string (),
        SourceName::from ("search") ) );
  let _ = skgnode_from_map_or_disk (
    node_id, &source, pool, config );
  let mut parent_mut : NodeMut<ViewNode> =
    forest . get_mut (parent_nid) . unwrap ();
  parent_mut . append ( ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::Scaff (
      Scaffold::SearchResult {
        id         : node_id . clone (),
        source,
        title,
        graphStats : GraphNodeStats::default () } ) } )
  . id () }

/// Compute containerward paths for a list of search result IDs.
/// Uses level-by-level frontier expansion: at each depth level,
/// all active paths share a single round of parallel TypeDB queries
/// (via find_container_ids_of_pid + join_all). Convergent paths
/// merge, so each distinct container is queried only once per level.
pub(super) async fn compute_containerward_paths_using_parallel_frontier
  ( ids     : &[ID],
    db_name : &str,
    driver  : &TypeDBDriver,
  ) -> HashMap < ID, Vec < PathToFirstNonlinearity > > {
  let mut finished : HashMap < ID, Vec<PathToFirstNonlinearity> > =
    HashMap::new ();
  let mut frontier : Vec<(ID, TrackedPath)> =
    seed_frontier_from_origins (
      ids, db_name, driver, &mut finished ) . await;
  while ! frontier . is_empty () {
    let containers_map : HashMap<ID, HashSet<ID>> =
      query_containers_for_unique_frontier_ids (
        &frontier, db_name, driver ) . await;
    let mut next_frontier : Vec<(ID, TrackedPath)> = Vec::new ();
    for (current_id, tracked) in frontier {
      let containers : HashSet<ID> =
        containers_map . get (&current_id)
        . cloned ()
        . unwrap_or_default ();
      advance_tracked_path (
        tracked, containers,
        &mut finished, &mut next_frontier ); }
    frontier = next_frontier; }
  finished }

/// Query containers of each origin ID in parallel.
/// Roots and immediate forks are finished directly;
/// single-container origins seed the frontier for climbing.
async fn seed_frontier_from_origins (
  ids      : &[ID],
  db_name  : &str,
  driver   : &TypeDBDriver,
  finished : &mut HashMap < ID, Vec<PathToFirstNonlinearity> >,
) -> Vec<(ID, TrackedPath)> {
  let init_futures : Vec<_> =
    ids . iter ()
    . map ( |id| find_container_ids_of_pid ( db_name, driver, id ) )
    . collect ();
  let init_results =
    futures::future::join_all (init_futures) . await;
  let mut frontier : Vec<(ID, TrackedPath)> = Vec::new ();
  for (id, containers_result) in ids . iter () . zip (init_results) {
    let containers : HashSet<ID> =
      containers_result . unwrap_or_default ();
    let visited : HashSet<ID> =
      HashSet::from ( [ id . clone () ] );
    if containers . is_empty () {
      // Root: no path.
      finished . entry ( id . clone () )
        . or_insert_with (Vec::new)
        . push ( PathToFirstNonlinearity {
          path        : vec![],
          cycle_nodes : HashSet::new (),
          branches    : HashSet::new () } );
    } else if containers . len () > 1 {
      // Immediate fork: expand each branch separately.
      let mut sorted : Vec<ID> =
        containers . into_iter () . collect ();
      sorted . sort ();
      for branch_id in sorted {
        let mut branch_set : HashSet<ID> = visited . clone ();
        if ! branch_set . insert ( branch_id . clone () ) {
          // Cycle back to origin.
          finished . entry ( id . clone () )
            . or_insert_with (Vec::new)
            . push ( PathToFirstNonlinearity {
              path        : vec![ branch_id . clone () ],
              cycle_nodes : HashSet::from ( [ branch_id ] ),
              branches    : HashSet::new () } );
        } else {
          frontier . push ((
            branch_id . clone (),
            TrackedPath {
              origin   : id . clone (),
              path     : vec![ branch_id . clone () ],
              visited  : branch_set } )); } }
    } else {
      // Single container: start climbing.
      let container : ID =
        containers . into_iter () . next () . unwrap ();
      let mut new_set : HashSet<ID> = visited;
      if ! new_set . insert ( container . clone () ) {
        finished . entry ( id . clone () )
          . or_insert_with (Vec::new)
          . push ( PathToFirstNonlinearity {
            path        : vec![ container . clone () ],
            cycle_nodes : HashSet::from ( [ container ] ),
            branches    : HashSet::new () } );
      } else {
        frontier . push ((
          container . clone (),
          TrackedPath {
            origin   : id . clone (),
            path     : vec![ container . clone () ],
            visited  : new_set } )); } } }
  frontier }

/// Deduplicate frontier IDs and query each distinct one's
/// containers in parallel via join_all.
async fn query_containers_for_unique_frontier_ids (
  frontier : &[(ID, TrackedPath)],
  db_name  : &str,
  driver   : &TypeDBDriver,
) -> HashMap<ID, HashSet<ID>> {
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
  let results =
    futures::future::join_all (futures) . await;
  let mut containers_map : HashMap<ID, HashSet<ID>> =
    HashMap::new ();
  for (id, r) in unique_frontier_ids . into_iter ()
                 . zip (results)
  { containers_map . insert (
      id, r . unwrap_or_default () ); }
  containers_map }

/// Given a tracked path and its current node's containers,
/// either finish the path (root, fork, or cycle)
/// or extend it and push onto next_frontier.
fn advance_tracked_path (
  tracked        : TrackedPath,
  containers     : HashSet<ID>,
  finished       : &mut HashMap < ID, Vec<PathToFirstNonlinearity> >,
  next_frontier  : &mut Vec<(ID, TrackedPath)>,
) {
  if containers . is_empty () {
    // Reached root.
    finished . entry ( tracked . origin )
      . or_insert_with (Vec::new)
      . push ( PathToFirstNonlinearity {
        path        : tracked . path,
        cycle_nodes : HashSet::new (),
        branches    : HashSet::new () } );
  } else if containers . len () > 1 {
    // Fork mid-path.
    let cycle_nodes : HashSet<ID> =
      containers . iter ()
      . filter ( |c| tracked . visited . contains (c) )
      . cloned ()
      . collect ();
    finished . entry ( tracked . origin )
      . or_insert_with (Vec::new)
      . push ( PathToFirstNonlinearity {
        path        : tracked . path,
        cycle_nodes,
        branches    : containers } );
  } else {
    // Single container: keep climbing.
    let container : ID =
      containers . into_iter () . next () . unwrap ();
    if tracked . visited . contains (&container) {
      // Cycle.
      finished . entry ( tracked . origin )
        . or_insert_with (Vec::new)
        . push ( PathToFirstNonlinearity {
          path        : tracked . path,
          cycle_nodes : HashSet::from ( [ container ] ),
          branches    : HashSet::new () } );
    } else {
      let mut next_path : Vec<ID> = tracked . path;
      next_path . push ( container . clone () );
      let mut next_set : HashSet<ID> = tracked . visited;
      next_set . insert ( container . clone () );
      next_frontier . push ((
        container,
        TrackedPath {
          origin   : tracked . origin,
          path     : next_path,
          visited  : next_set } )); } } }
