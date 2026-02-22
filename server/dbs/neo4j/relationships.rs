use std::error::Error;

use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::textlinks::textlinks_from_node;

use neo4rs::Graph;

/// Maps `create_relationships_from_node` over `nodes`,
/// then commits.
pub async fn create_all_relationships (
  graph : &Graph,
  nodes : &[SkgNode]
) -> Result < (), Box<dyn Error> > {
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  println! ( "Creating relationships ..." );
  for node in nodes {
    let primary_id : &ID = node . primary_id ()?;
    create_relationships_from_node_in_txn (node, &mut txn)
      . await . map_err(
        |e| format!(
          "Failed to create relationships for node '{}': {}",
          primary_id . as_str (),
          e ))? ; }
  txn . commit () . await . map_err( |e| format!(
    "Failed to commit relationships transaction: {}", e))?;
  Ok (( )) }

async fn create_relationships_from_node_in_txn (
  node : &SkgNode,
  txn  : &mut neo4rs::Txn,
) -> Result < (), Box<dyn Error> > {
  let primary_id : &ID =
    node . primary_id()?;
  insert_relationship_from_list_in_txn (
      primary_id . as_str (),
      node . contains . as_ref() . unwrap_or(&vec![]),
      "contains", true, txn
    ). await . map_err( |e| format!(
      "Failed to create 'contains' relationships: {}", e))?;
  insert_relationship_from_list_in_txn (
      primary_id . as_str (),
      & ( textlinks_from_node ( &node )
          . iter ()
          . map ( |textlink|
                   ID::from ( textlink . id . clone() ) )
          . collect::<Vec<ID>>() ),
      "textlinks_to", true, txn
    ). await . map_err( |e| format!(
      "Failed to create 'textlinks_to' relationships: {}", e))?;
  insert_relationship_from_list_in_txn (
      primary_id . as_str (),
      node . subscribes_to . as_ref() . unwrap_or(&vec![]),
      "subscribes", true, txn
    ). await . map_err( |e| format!(
      "Failed to create 'subscribes' relationships: {}", e))?;
  insert_relationship_from_list_in_txn (
      primary_id . as_str (),
      node . hides_from_its_subscriptions . as_ref()
        . unwrap_or(&vec![]),
      "hides", true, txn
    ). await . map_err( |e| format!(
      "Failed to create 'hides' relationships: {}", e))?;
  insert_relationship_from_list_in_txn (
      primary_id . as_str (),
      node . overrides_view_of . as_ref() . unwrap_or(&vec![]),
      "overrides", true, txn
    ). await . map_err( |e| format!(
      "Failed to create 'overrides' relationships: {}", e))?;
  Ok (( )) }

/// Instantiates relationships in Neo4j. Does not commit.
/// Regarding the `from_is_tail` argument:
///   - if true, (from)-[r]->(to)
///   - if false, (to)-[r]->(from)
/// The target side resolves via both Node.id and IdAlias.id.
async fn insert_relationship_from_list_in_txn (
  primary_id    : &str,
  id_list       : &Vec<ID>,
  relation_name : &str,
  from_is_tail  : bool,
  txn           : &mut neo4rs::Txn,
) -> Result<(), Box<dyn Error>> {
  if id_list . is_empty () { return Ok (( )); }
  let target_strings : Vec<String> =
    id_list . iter () . map ( |id| id . 0 . clone ()
                            ). collect ();
  let cypher : String = format! (
    "UNWIND $targets AS target_id                                          \
     MATCH (from_node:Node {{id: $from_id}})                               \
     OPTIONAL MATCH (direct:Node {{id: target_id}})                        \
     OPTIONAL MATCH (alias:IdAlias {{id: target_id}})                      \
     WITH from_node, coalesce(direct.id, alias.primary_id) AS resolved_pid \
     WHERE resolved_pid IS NOT NULL                                        \
     MATCH (to_node:Node {{id: resolved_pid}})                             \
     CREATE ({})-[:{}]->({})",
    if from_is_tail { "from_node" } else { "to_node" },
    relation_name,
    if from_is_tail { "to_node" } else { "from_node" } );
  txn . run (
    neo4rs::query ( &cypher )
    . param ( "from_id", primary_id )
    . param ( "targets", target_strings )
  ) . await
    . map_err(|e| format!(
      "Cypher query failed for batch relationship '{}' from '{}': {}",
      relation_name, primary_id, e))?;
  Ok (( )) }

/// Delete every instance of `relation`
/// where the node plays the outgoing (tail) role.
/// Returns the number of IDs processed.
pub async fn delete_out_links (
  graph    : &Graph,
  ids      : &Vec<ID>,
  relation : &str,
) -> Result < usize, Box<dyn Error> > {
  if ids . is_empty () { return Ok ( 0 ); }
  let cypher : String = format! (
    "UNWIND $ids AS nid                      \
     MATCH (n:Node {{id: nid}})-[r:{}]->() \
     DELETE r",
    relation );
  let id_strings : Vec<String> =
    ids . iter () . map ( |id| id . 0 . clone ()
                        ). collect ();
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  txn . run (
    neo4rs::query ( &cypher )
    . param ( "ids", id_strings )
  ). await ?;
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
  if ids . is_empty () { return Ok ( 0 ); }
  let cypher : String = format! (
    "UNWIND $ids AS nid                      \
     MATCH ()-[r:{}]->(n:Node {{id: nid}}) \
     DELETE r",
    relation );
  let id_strings : Vec<String> =
    ids . iter () . map ( |id| id . 0 . clone ()
                        ). collect ();
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  txn . run ( neo4rs::query ( &cypher )
              . param ( "ids", id_strings )
            ). await ?;
  txn . commit () . await ?;
  Ok ( ids . len () ) }
