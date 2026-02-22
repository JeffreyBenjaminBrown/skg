use std::error::Error;
use std::collections::{HashMap, HashSet};
use neo4rs::Graph;

use crate::types::misc::ID;

/// PURPOSE: Run one Neo4j query to find all 'contains' relations
/// among a collection of nodes.
/// Takes a vector of IDs (PIDs) and returns two maps:
/// 1. container_to_contents: Map from each container to the set of nodes it contains
/// 2. content_to_containers: Map from each contained node to the set of containers
/// If a value (set) is empty, the corresponding key is omitted from the map.
pub async fn contains_from_pids (
  graph : &Graph,
  pids  : &[ID]
) -> Result <
    ( HashMap < ID, HashSet < ID > >,
      HashMap < ID, HashSet < ID > > ),
  Box < dyn Error > > {
  if pids . is_empty () {
    return Ok (( HashMap::new (), HashMap::new () )); }
  let mut container_to_contents : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  let mut content_to_containers : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  let pid_set : HashSet<ID> =
    pids . iter () . cloned () . collect ();
  let id_strings : Vec<String> =
    pids . iter () . map ( |id| id.0.clone () ) . collect ();
  // Resolve input IDs (which despite the name may be extra IDs),
  // then find contains relationships.
  let mut result_stream =
    graph . execute (
      neo4rs::query ( "\
        UNWIND $pids AS input_id \
        OPTIONAL MATCH (direct:Node {id: input_id}) \
        OPTIONAL MATCH (alias:IdAlias {id: input_id}) \
        WITH input_id, coalesce(direct.id, alias.primary_id) AS resolved_pid \
        WHERE resolved_pid IS NOT NULL \
        MATCH (n:Node {id: resolved_pid}) \
        OPTIONAL MATCH (n)-[:contains]->(child:Node) \
        OPTIONAL MATCH (parent:Node)-[:contains]->(n) \
        RETURN n.id AS node_id, \
               COLLECT(DISTINCT child.id) AS contains, \
               COLLECT(DISTINCT parent.id) AS contained_by" )
      . param ( "pids", id_strings )
    ) . await ?;
  while let Some ( row ) = result_stream . next () . await ? {
    let node_id : String = row . get ( "node_id" ) ?;
    let node_id : ID = ID ( node_id );
    let contains : Vec<String> = row . get ( "contains" ) ?;
    for cid in contains {
      let cid : ID = ID ( cid );
      if pid_set . contains ( &cid ) {
        container_to_contents
          . entry ( node_id . clone () )
          . or_insert_with ( HashSet::new )
          . insert ( cid ); }}
    let contained_by : Vec<String> = row . get ( "contained_by" ) ?;
    for cid in contained_by {
      let cid : ID = ID ( cid );
      if pid_set . contains ( &cid ) {
        content_to_containers
          . entry ( node_id . clone () )
          . or_insert_with ( HashSet::new )
          . insert ( cid ); }} }
  Ok (( container_to_contents,
        content_to_containers )) }
