use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;

use std::collections::{ HashSet, BTreeSet };
use std::error::Error;
use neo4rs::Graph;

pub async fn create_all_nodes (
  // Creates all Node and IdAlias entities, then commits.
  graph : &Graph,
  nodes : &[SkgNode]
) -> Result < (), Box<dyn Error> > {
  println!("Creating nodes ...");
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  for node in nodes {
    create_node_in_txn ( node, &mut txn )
      . await ?; }
  txn . commit () . await ?;
  Ok (()) }

/// Creates (in Neo4j) nodes whose primary IDs are not in the DB.
/// Returns the number of nodes created.
pub async fn create_only_nodes_with_no_ids_present (
  graph : &Graph,
  nodes : &Vec <SkgNode>
) -> Result < usize, Box<dyn Error> > {
  let mut all_pids : BTreeSet < String > =
    BTreeSet::new ();
  for node in nodes {
    all_pids.insert (
      node . primary_id()? . to_string() ); }
  let known_ids : HashSet < String > =
    which_ids_exist (
      graph,
      &all_pids
    ) . await ?;
  let mut to_create : Vec < &SkgNode > =
    Vec::new ();
  for node in nodes {
    if ! known_ids.contains (
      node.primary_id()? . as_str () )
    { to_create.push ( node ); }}
  { let mut txn : neo4rs::Txn =
      graph . start_txn () . await ?;
    println! ( "Creating {} new nodes ...",
                to_create.len () );
    for node in &to_create {
      create_node_in_txn ( node, &mut txn ) . await ?; }
    txn . commit () . await ?; }
  Ok ( to_create.len () ) }

/// Batch existence check:
/// Given a set of candidate ID *strings*,
/// return the subset that already exist in the DB.
pub async fn which_ids_exist (
  graph : &Graph,
  ids   : &BTreeSet < String >
) -> Result < HashSet < String >, Box<dyn Error> > {
  if ids.is_empty () {
    return Ok ( HashSet::new () ); }
  let ids_vec : Vec<String> =
    ids . iter () . cloned () . collect ();
  let mut result =
    graph . execute (
      neo4rs::query (
        "MATCH (n:Node) WHERE n.id IN $ids RETURN n.id AS id" )
      . param ( "ids", ids_vec )
    ) . await ?;
  let mut found : HashSet < String > =
    HashSet::new ();
  while let Some ( row ) = result . next () . await ? {
    let id : String = row . get ( "id" ) ?;
    found . insert ( id ); }
  Ok ( found ) }

pub async fn create_node (
  // Creates the Node and any IdAlias entities.
  // Uses an auto-commit transaction.
  node  : &SkgNode,
  graph : &Graph,
) -> Result < (), Box<dyn Error> > {
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  create_node_in_txn ( node, &mut txn ) . await ?;
  txn . commit () . await ?;
  Ok (()) }

async fn create_node_in_txn (
  // Creates the Node and any IdAlias entities.
  // Does *not* commit.
  node : &SkgNode,
  txn  : &mut neo4rs::Txn,
) -> Result < (), Box<dyn Error> > {
  let primary_id : &ID = node.primary_id()?;
  txn . run (
    neo4rs::query (
      "CREATE (:Node {id: $id, source: $source})" )
    . param ( "id",     primary_id . as_str () )
    . param ( "source", node . source . 0 . as_str () )
  ) . await ?;
  insert_extra_ids_in_txn ( &node, txn ) . await ?;
  Ok (()) }

async fn insert_extra_ids_in_txn (
  node : &SkgNode,
  txn  : &mut neo4rs::Txn,
) -> Result < (), Box<dyn Error> > {
  if node.ids.len () > 1 {
    let primary_id : &ID = node.primary_id()?;
    for extra_id in { let extra_ids: Vec < &ID > =
                        node . ids . iter() . skip(1) . collect();
                      extra_ids } {
      txn . run (
        neo4rs::query (
          "CREATE (:IdAlias {id: $eid, primary_id: $pid})" )
        . param ( "eid", extra_id . as_str () )
        . param ( "pid", primary_id . as_str () )
      ) . await ?; }}
  Ok (()) }

/// PURPOSE: Add extra IDs for a node that already exists.
pub async fn insert_extra_ids (
  graph      : &Graph,
  primary_id : &ID,
  extra_ids  : &[ID],
) -> Result < (), Box<dyn Error> > {
  for extra_id in extra_ids {
    graph . run (
      neo4rs::query (
        "CREATE (:IdAlias {id: $eid, primary_id: $pid})" )
      . param ( "eid", extra_id . as_str () )
      . param ( "pid", primary_id . as_str () )
    ) . await ?; }
  Ok (()) }

/// ASSUMES: All input IDs are PIDs.
/// PURPOSE: Delete the node corresponding to every ID it receives,
/// including its IdAlias entities.
pub async fn delete_nodes_from_pids (
  graph : &Graph,
  ids   : &[ID]
) -> Result < (), Box<dyn Error> > {
  if ids.is_empty() { return Ok ( () ); }
  let id_strings : Vec<String> =
    ids . iter () . map ( |id| id.0.clone () ) . collect ();
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  txn . run (
    neo4rs::query (
      "MATCH (a:IdAlias) WHERE a.primary_id IN $pids DELETE a" )
    . param ( "pids", id_strings . clone () )
  ) . await ?;
  txn . run (
    neo4rs::query (
      "MATCH (n:Node) WHERE n.id IN $pids DETACH DELETE n" )
    . param ( "pids", id_strings )
  ) . await ?;
  txn . commit () . await ?;
  Ok ( () ) }
