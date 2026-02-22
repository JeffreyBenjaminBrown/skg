use std::error::Error;

use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::textlinks::textlinks_from_node;

use neo4rs::Graph;

pub async fn create_all_relationships (
  // Maps `create_relationships_from_node` over `nodes`,
  // then commits.
  graph : &Graph,
  nodes : &[SkgNode]
) -> Result < (), Box<dyn Error> > {
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  println! ( "Creating relationships ..." );
  for node in nodes {
    let primary_id : &ID = node.primary_id()?;
    create_relationships_from_node_in_txn (node, &mut txn)
      . await . map_err(
        |e| format!(
          "Failed to create relationships for node '{}': {}",
          primary_id . as_str (),
          e ))? ; }
  txn . commit () . await
    . map_err(|e| format!("Failed to commit relationships transaction: {}", e))?;
  Ok (()) }

pub async fn create_relationships_from_node (
  node  : &SkgNode,
  graph : &Graph,
) -> Result < (), Box<dyn Error> > {
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  create_relationships_from_node_in_txn ( node, &mut txn )
    . await ?;
  txn . commit () . await ?;
  Ok (()) }

async fn create_relationships_from_node_in_txn (
  node : &SkgNode,
  txn  : &mut neo4rs::Txn,
) -> Result < (), Box<dyn Error> > {
  let primary_id : &ID = node.primary_id()?;
  insert_relationship_from_list_in_txn (
    primary_id . as_str (),
    node.contains.as_ref().unwrap_or(&vec![]),
    "contains",
    true,
    txn ) . await
    . map_err(|e| format!("Failed to create 'contains' relationships: {}", e))?;
  insert_relationship_from_list_in_txn (
    primary_id . as_str (),
    & ( textlinks_from_node ( &node )
        . iter ()
        . map ( |textlink|
                 ID::from ( textlink.id.clone() ) )
        . collect::<Vec<ID>>() ),
    "textlinks_to",
    true,
    txn ). await
    . map_err(|e| format!("Failed to create 'textlinks_to' relationships: {}", e))?;
  insert_relationship_from_list_in_txn (
    primary_id . as_str (),
    node.subscribes_to.as_ref().unwrap_or(&vec![]),
    "subscribes",
    true,
    txn ). await
    . map_err(|e| format!("Failed to create 'subscribes' relationships: {}", e))?;
  insert_relationship_from_list_in_txn (
    primary_id . as_str (),
    node.hides_from_its_subscriptions.as_ref().unwrap_or(&vec![]),
    "hides",
    true,
    txn ). await
    . map_err(|e| format!("Failed to create 'hides' relationships: {}", e))?;
  insert_relationship_from_list_in_txn (
    primary_id . as_str (),
    node.overrides_view_of.as_ref().unwrap_or(&vec![]),
    "overrides",
    true,
    txn ). await
    . map_err(|e| format!("Failed to create 'overrides' relationships: {}", e))?;
  Ok (()) }

/// Instantiates relationships in Neo4j. Does not commit.
/// `from_is_tail`: if true, (from)-[r]->(to); if false, (to)-[r]->(from).
/// The target side resolves via both Node.id and IdAlias.id.
pub async fn insert_relationship_from_list (
  primary_id    : &str,
  id_list       : &Vec<ID>,
  relation_name : &str,
  from_is_tail  : bool,
  graph         : &Graph,
) -> Result<(), Box<dyn Error>> {
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  insert_relationship_from_list_in_txn (
    primary_id, id_list, relation_name,
    from_is_tail, &mut txn ) . await ?;
  txn . commit () . await ?;
  Ok (( )) }

async fn insert_relationship_from_list_in_txn (
  primary_id    : &str,
  id_list       : &Vec<ID>,
  relation_name : &str,
  from_is_tail  : bool,
  txn           : &mut neo4rs::Txn,
) -> Result<(), Box<dyn Error>> {
  for target_id in id_list {
    // Resolve target via Node.id or IdAlias.id
    let cypher : String = format! (
      "MATCH (from:Node {{id: $from_id}}) \
       OPTIONAL MATCH (direct:Node {{id: $to_id}}) \
       OPTIONAL MATCH (alias:IdAlias {{id: $to_id}}) \
       WITH from, coalesce(direct.id, alias.primary_id) AS resolved_pid \
       WHERE resolved_pid IS NOT NULL \
       MATCH (to_node:Node {{id: resolved_pid}}) \
       CREATE ({})-[:{}]->({})",
      if from_is_tail { "from" } else { "to_node" },
      relation_name,
      if from_is_tail { "to_node" } else { "from" } );
    txn . run (
      neo4rs::query ( &cypher )
      . param ( "from_id", primary_id )
      . param ( "to_id",   target_id . as_str () )
    ) . await
      . map_err(|e| format!(
        "Cypher query failed for relationship '{}' from '{}' to '{}': {}",
        relation_name, primary_id, target_id.as_str(), e))?; }
  Ok (( )) }

/// Delete every instance of `relation`
/// where the node plays the outgoing (tail) role.
/// Returns the number of IDs processed.
pub async fn delete_out_links (
  graph    : &Graph,
  ids      : &Vec<ID>,
  relation : &str,
) -> Result < usize, Box<dyn Error> > {
  let cypher : String = format! (
    "MATCH (n:Node {{id: $id}})-[r:{}]->() DELETE r",
    relation );
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  for id in ids {
    txn . run (
      neo4rs::query ( &cypher )
      . param ( "id", id . as_str () )
    ). await ?; }
  txn . commit () . await ?;
  Ok ( ids.len () ) }

/// Delete every instance of `relation`
/// where the node plays the incoming (head) role.
/// Returns the number of IDs processed.
pub async fn delete_in_links (
  graph    : &Graph,
  ids      : &Vec<ID>,
  relation : &str,
) -> Result < usize, Box<dyn Error> > {
  let cypher : String = format! (
    "MATCH ()-[r:{}]->(n:Node {{id: $id}}) DELETE r",
    relation );
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  for id in ids {
    txn . run (
      neo4rs::query ( &cypher )
      . param ( "id", id . as_str () )
    ). await ?; }
  txn . commit () . await ?;
  Ok ( ids.len () ) }
