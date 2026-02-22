pub mod pids_from_ids;

use std::error::Error;
use neo4rs::Graph;

/// Clear all data from the Neo4j database.
/// Neo4j Community Edition has one database,
/// so this clears all nodes and relationships.
pub async fn delete_database (
  graph : &Graph,
) -> Result<(), Box<dyn std::error::Error>> {
  graph . run (
    neo4rs::query ( "MATCH (n) DETACH DELETE n" )
  ) . await ?;
  println! ( "Neo4j database cleared successfully" );
  Ok (( )) }

/// Returns true if the from_role is the arrow's tail (outgoing).
pub fn cypher_direction (
  relation   : &str,
  input_role : &str
) -> Result < bool, Box<dyn Error> > {
  match ( relation, input_role ) {
    ( "contains",     "container" )   => Ok ( true ),
    ( "contains",     "contained" )   => Ok ( false ),
    ( "textlinks_to", "source" )      => Ok ( true ),
    ( "textlinks_to", "dest" )        => Ok ( false ),
    ( "subscribes",   "subscriber" )  => Ok ( true ),
    ( "subscribes",   "subscribee" )  => Ok ( false ),
    ( "hides",        "hider" )       => Ok ( true ),
    ( "hides",        "hidden" )      => Ok ( false ),
    ( "overrides",    "replacement" ) => Ok ( true ),
    ( "overrides",    "replaced" )    => Ok ( false ),
    _ => Err ( format! (
      "Unknown relation/role pair: '{}/{}'",
      relation, input_role ) . into () ) } }
