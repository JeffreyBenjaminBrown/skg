use crate::types::misc::ID;
use crate::types::save::Merge;
use crate::types::skgnode::SkgNode;

use std::collections::HashSet;
use std::error::Error;
use neo4rs::Graph;

/// Merges nodes in Neo4j by applying Merge.
/// All merges are batched in a single transaction.
pub(super) async fn merge_nodes_in_neo4j (
  graph              : &Graph,
  merge_instructions : &[Merge],
) -> Result < (), Box<dyn Error> > {
  if merge_instructions.is_empty() {
    return Ok (( )); }
  let mut txn : neo4rs::Txn =
    graph . start_txn () . await ?;
  for merge in merge_instructions {
    let ( acquiree_text_preserver,
          updated_acquirer,
          (acquiree_id, _acquiree_source) )
      : (&SkgNode, &SkgNode, (&ID, _))
      = merge.targets_from_merge();
    merge_one_node_in_neo4j (
      graph,
      &mut txn,
      acquiree_text_preserver,
      updated_acquirer,
      acquiree_id,
    ).await?; }
  txn . commit () . await ?;
  Ok (( )) }

/// Adds the processing for a single merge to the transaction.
async fn merge_one_node_in_neo4j(
  graph                   : &Graph,
  txn                     : &mut neo4rs::Txn,
  acquiree_text_preserver : &SkgNode,
  updated_acquirer        : &SkgNode,
  acquiree_id             : &ID,
) -> Result<(), Box<dyn Error>> {
  let acquirer_id  : &ID = updated_acquirer        . primary_id()?;
  let preserver_id : &ID = acquiree_text_preserver . primary_id()?;
  // Create the text preserver node before using it.
  create_text_preserver_in_txn (
    acquiree_text_preserver, txn ) . await ?;

  { // Reroute relationships.
    reroute_relationships_for_merge (
      txn, acquiree_id, acquirer_id,
      "contains", true ). await ?;
    reroute_relationships_for_merge (
      txn, acquiree_id, acquirer_id,
      "contains", false ). await ?;
    reroute_relationships_for_merge (
      txn, acquiree_id, preserver_id,
      "textlinks_to", true ). await ?;
    reroute_relationships_for_merge (
      txn, acquiree_id, acquirer_id,
      "textlinks_to", false ). await ?;
    reroute_relationships_for_merge (
      txn, acquiree_id, acquirer_id,
      "subscribes", true ). await ?;
    reroute_relationships_for_merge (
      txn, acquiree_id, acquirer_id,
      "subscribes", false ). await ?;
    drop_relationships_for_merge (
      // Whether the acquiree was hidden from N says nothing about
      // whether the acquirer should be hidden from N.
      txn, acquiree_id,
      "hides", false ). await ?;
    reroute_what_acquiree_hides (
      // PITFALL: Must happen after rerouting 'contains',
      // to know which hide relationships to filter out.
      graph, txn, acquirer_id, acquiree_id,
      ( &updated_acquirer.contains.as_ref().map(
        |v| v.iter().cloned().collect()).unwrap_or_default()
      )) . await ?;
    reroute_relationships_for_merge (
      txn, acquiree_id, acquirer_id,
      "overrides", true ). await ?;
    drop_relationships_for_merge (
      // If the acquiree was replaced by something else,
      // we don't know whether the acquirer should be.
      txn, acquiree_id,
      "overrides", false ). await ?; }

  { // Give the acquirer more IDs.
    // Reroute existing aliases from acquiree to acquirer
    txn . run (
      neo4rs::query ( "\
        MATCH (a:IdAlias {primary_id: $old_id}) \
        SET a.primary_id = $new_id" )
      . param ( "old_id", acquiree_id . as_str () )
      . param ( "new_id", acquirer_id . as_str () )
    ) . await ?;
    // Record acquiree's PID as a new alias
    txn . run (
      neo4rs::query ( "\
        CREATE (:IdAlias {id: $eid, primary_id: $pid})" )
      . param ( "eid", acquiree_id . as_str () )
      . param ( "pid", acquirer_id . as_str () )
    ) . await ?; }

  // Delete the acquiree.
  txn . run (
    neo4rs::query ( "\
      MATCH (n:Node {id: $id}) DETACH DELETE n" )
    . param ( "id", acquiree_id . as_str () )
  ) . await . map_err( |e|
    format!( "Failed to delete acquiree node '{}': {}",
             acquiree_id.as_str(), e))?;

  // Add preserver to acquirer's content
  txn . run (
    neo4rs::query ( "\
      MATCH (from:Node {id: $from_id}) \
      MATCH (to:Node {id: $to_id}) \
      CREATE (from)-[:contains]->(to)" )
    . param ( "from_id", acquirer_id . as_str () )
    . param ( "to_id",   preserver_id . as_str () )
  ) . await ?;
  Ok (( )) }

/// Creates a text preserver node within an existing transaction.
async fn create_text_preserver_in_txn (
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
  if node.ids.len () > 1 {
    for extra_id in node . ids . iter() . skip(1) {
      txn . run (
        neo4rs::query (
          "CREATE (:IdAlias {id: $eid, primary_id: $pid})" )
        . param ( "eid", extra_id . as_str () )
        . param ( "pid", primary_id . as_str () )
      ) . await ?; }}
  Ok (()) }

/// Drops relationships where old_id plays the specified direction.
/// `outgoing`: if true, deletes (old)-[r:rel]->(); if false, ()-[r:rel]->(old)
async fn drop_relationships_for_merge (
  txn      : &mut neo4rs::Txn,
  old_id   : &ID,
  relation : &str,
  outgoing : bool,
) -> Result < (), Box<dyn Error> > {
  let cypher : String = if outgoing {
    format! ( "MATCH (old:Node {{id: $old_id}})-[r:{}]->() DELETE r",
              relation )
  } else {
    format! ( "MATCH ()-[r:{}]->(old:Node {{id: $old_id}}) DELETE r",
              relation ) };
  txn . run (
    neo4rs::query ( &cypher )
    . param ( "old_id", old_id . as_str () )
  ) . await ?;
  Ok (( )) }

/// Reroute relationships from old_id to new_id.
/// `outgoing`: if true, reroutes (old)-[r:rel]->(other) to (new)-[r:rel]->(other)
///             if false, reroutes (other)-[r:rel]->(old) to (other)-[r:rel]->(new)
async fn reroute_relationships_for_merge (
  txn      : &mut neo4rs::Txn,
  old_id   : &ID,
  new_id   : &ID,
  relation : &str,
  outgoing : bool,
) -> Result < (), Box<dyn Error> > {
  let cypher : String = if outgoing {
    format! ( "\
      MATCH (old:Node {{id: $old_id}})-[r:{}]->(other) \
      MATCH (new:Node {{id: $new_id}}) \
      DELETE r \
      CREATE (new)-[:{}]->(other)",
      relation, relation )
  } else {
    format! ( "\
      MATCH (other)-[r:{}]->(old:Node {{id: $old_id}}) \
      MATCH (new:Node {{id: $new_id}}) \
      DELETE r \
      CREATE (other)-[:{}]->(new)",
      relation, relation ) };
  txn . run (
    neo4rs::query ( &cypher )
    . param ( "old_id", old_id . as_str () )
    . param ( "new_id", new_id . as_str () )
  ) . await ?;
  Ok (( )) }

/// Reroute what the acquiree hides.
/// Transfer if hidden node not in acquirer's contents.
/// Otherwise just drop the relationship.
async fn reroute_what_acquiree_hides (
  graph                    : &Graph,
  txn                      : &mut neo4rs::Txn,
  acquirer_id              : &ID,
  acquiree_id              : &ID,
  acquirer_final_contains  : &HashSet<ID>,
) -> Result < (), Box<dyn Error> > {
  // First find what acquiree hides
  let mut result_stream =
    graph . execute (
      neo4rs::query ( "\
        MATCH (acquiree:Node {id: $acquiree_id})-[r:hides]->(hidden:Node) \
        RETURN hidden.id AS hidden_id" )
      . param ( "acquiree_id", acquiree_id . as_str () )
    ) . await ?;
  let mut hidden_ids : Vec<ID> = Vec::new();
  while let Some ( row ) = result_stream . next () . await ? {
    let hidden_id : String = row . get ( "hidden_id" ) ?;
    hidden_ids . push ( ID ( hidden_id ) ); }
  // Delete all hides relationships from acquiree
  txn . run (
    neo4rs::query ( "\
      MATCH (acquiree:Node {id: $acquiree_id})-[r:hides]->() \
      DELETE r" )
    . param ( "acquiree_id", acquiree_id . as_str () )
  ) . await ?;
  // Re-create the ones we want to keep on the acquirer
  for hidden_id in &hidden_ids {
    if !acquirer_final_contains.contains(hidden_id) {
      txn . run (
        neo4rs::query ( "\
          MATCH (acquirer:Node {id: $acquirer_id}) \
          MATCH (hidden:Node {id: $hidden_id}) \
          CREATE (acquirer)-[:hides]->(hidden)" )
        . param ( "acquirer_id", acquirer_id . as_str () )
        . param ( "hidden_id",   hidden_id . as_str () )
      ) . await ?; } }
  Ok (( )) }
