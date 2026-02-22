use std::error::Error;
use std::collections::{HashMap, HashSet};
use neo4rs::Graph;

use crate::types::misc::ID;
use crate::dbs::neo4j::search::cypher_direction;

/// PURPOSE: Count how many times each node appears as 'contained'
/// in a 'contains' relationship.
pub async fn count_containers (
  graph : &Graph,
  ids   : &[ID]
) -> Result < HashMap < ID, usize >,
              Box < dyn Error > > {
  count_relations ( graph, ids,
                    "contains", "contained"
  ). await }

/// PURPOSE: Count how many nodes each node contains.
pub async fn count_contents (
  graph : &Graph,
  ids   : &[ID]
) -> Result < HashMap < ID, usize >,
              Box < dyn Error > > {
  count_relations ( graph, ids,
                    "contains", "container"
  ). await }

/// PURPOSE: Count how many nodes link to each node
/// in a 'textlinks_to' relationship.
pub async fn count_link_sources (
  graph : &Graph,
  ids   : &[ID]
) -> Result < HashMap < ID, usize >,
              Box < dyn Error > > {
  count_relations ( graph, ids,
                    "textlinks_to", "dest"
  ). await }

/// Generic function to count relationships for a collection of nodes.
/// Counts how many relationships exist where each input node
/// plays a specified role.
/// Input IDs are resolved via both Node.id and IdAlias.id.
async fn count_relations (
  graph      : &Graph,
  ids        : &[ID],
  relation   : &str,
  input_role : &str
) -> Result < HashMap < ID, usize >,
              Box < dyn Error > > {
  if ids . is_empty () {
    return Ok ( HashMap::new () ); }
  let mut result : HashMap < ID, usize > =
    HashMap::new ();
  for id in ids {
    result . insert ( id . clone (), 0 ); }
  let input_is_tail : bool =
    cypher_direction ( relation, input_role ) ?;
  let cypher : String = format! ( "\
    UNWIND $ids AS input_id \
    OPTIONAL MATCH (direct:Node {{id: input_id}}) \
    OPTIONAL MATCH (alias:IdAlias {{id: input_id}}) \
    WITH input_id, coalesce(direct.id, alias.primary_id) AS resolved_pid \
    WHERE resolved_pid IS NOT NULL \
    MATCH (input:Node {{id: resolved_pid}}) \
    OPTIONAL MATCH {}-[:{}]->{} \
    RETURN input_id AS queried_id, COUNT(DISTINCT other) AS count",
    if input_is_tail { "(input)" }     else { "(other:Node)" },
    relation,
    if input_is_tail { "(other:Node)" } else { "(input)" } );
  let id_strings : Vec<String> =
    ids . iter () . map ( |id| id.0.clone () ) . collect ();
  let mut result_stream =
    graph . execute (
      neo4rs::query ( &cypher )
      . param ( "ids", id_strings )
    ) . await ?;
  while let Some ( row ) = result_stream . next () . await ? {
    let queried_id : String = row . get ( "queried_id" ) ?;
    let count : i64 = row . get ( "count" ) ?;
    result . insert ( ID ( queried_id ), count as usize ); }
  Ok (result) }

/// Check whether each input ID participates in a given relation
/// in either role. Returns HashSet<ID> — presence means participation.
async fn participates_in_relation (
  graph    : &Graph,
  ids      : &[ID],
  relation : &str,
) -> Result < HashSet < ID >,
              Box < dyn Error > > {
  if ids . is_empty () {
    return Ok ( HashSet::new () ); }
  let mut result : HashSet < ID > =
    HashSet::new ();
  let cypher : String = format! ( "\
    UNWIND $ids AS input_id \
    OPTIONAL MATCH (direct:Node {{id: input_id}}) \
    OPTIONAL MATCH (alias:IdAlias {{id: input_id}}) \
    WITH input_id, coalesce(direct.id, alias.primary_id) AS resolved_pid \
    WHERE resolved_pid IS NOT NULL \
    MATCH (input:Node {{id: resolved_pid}}) \
    OPTIONAL MATCH (input)-[r:{}]-() \
    RETURN input_id AS queried_id, COUNT(r) > 0 AS participates",
    relation );
  let id_strings : Vec<String> =
    ids . iter () . map ( |id| id.0.clone () ) . collect ();
  let mut result_stream =
    graph . execute (
      neo4rs::query ( &cypher )
      . param ( "ids", id_strings )
    ) . await ?;
  while let Some ( row ) = result_stream . next () . await ? {
    let queried_id : String = row . get ( "queried_id" ) ?;
    let participates : bool = row . get ( "participates" ) ?;
    if participates {
      result . insert ( ID ( queried_id ) ); }}
  Ok (result) }

pub async fn has_subscribes (
  graph : &Graph, ids : &[ID]
) -> Result<HashSet<ID>, Box<dyn Error>> {
  participates_in_relation (
    graph, ids, "subscribes" ) . await }

pub async fn has_overrides (
  graph : &Graph, ids : &[ID]
) -> Result<HashSet<ID>, Box<dyn Error>> {
  participates_in_relation (
    graph, ids, "overrides" ) . await }
