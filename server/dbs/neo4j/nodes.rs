use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;

use std::collections::{ HashSet, BTreeSet };
use std::error::Error;
use neo4rs::Graph;

/// Creates all Node and IdAlias entities, then commits.
pub async fn create_all_nodes (
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
  Ok (( )) }

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
/// Given a set of candidate ID strings,
/// return the subset that resolve to an existing node.
/// If an extra ID is in the input and is found,
/// it is not converted to the corresponding PID in the output.
pub async fn which_ids_exist (
  graph : &Graph,
  ids   : &BTreeSet < String >
) -> Result < HashSet < String >, Box<dyn Error> > {
  if ids . is_empty () {
    return Ok ( HashSet::new () ); }
  let ids_vec : Vec<String> =
    ids . iter () . cloned () . collect ();
  let mut result =
    graph . execute (
      neo4rs::query ( "                                   \
        UNWIND $ids AS input_id                           \
        OPTIONAL MATCH (direct:Node {id: input_id})       \
        OPTIONAL MATCH (alias:IdAlias {id: input_id})     \
        WITH input_id,                                    \
             coalesce(direct.id, alias.primary_id) AS pid \
        WHERE pid IS NOT NULL                             \
        RETURN input_id" )
      . param ( "ids", ids_vec )
    ). await ?;
  let mut found : HashSet < String > =
    HashSet::new ();
  while let Some ( row ) = result . next () . await ? {
    let input_id : String = row . get ( "input_id" ) ?;
    found . insert ( input_id ); }
  Ok (found) }

/// Creates the Node and any IdAlias entities.
/// Does *not* commit.
async fn create_node_in_txn (
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
  Ok (( )) }

/// PURPOSE: Add extra IDs for a node that already exists.
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
  Ok (( )) }

/// ASSUMES: All input IDs are PIDs.
/// PURPOSE: Delete the node corresponding to each input ID,
///          *and* any corresponding IdAlias entities.
pub async fn delete_nodes_from_pids (
  graph : &Graph,
  ids   : &[ID]
) -> Result < (), Box<dyn Error> > {
  if ids . is_empty() { return Ok (( )); }
  let id_strings : Vec<String> =
    ids . iter () . map ( |id| id . 0 . clone ()
                        ) . collect ();
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
  Ok (( )) }
