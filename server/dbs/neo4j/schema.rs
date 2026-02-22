use std::error::Error;
use neo4rs::Graph;

pub const NODE_ID_CONSTRAINT: &str =
  "CREATE CONSTRAINT node_id IF NOT EXISTS \
   FOR (n:Node) REQUIRE n.id IS UNIQUE";
pub const NODE_SOURCE_INDEX: &str =
  "CREATE INDEX node_source IF NOT EXISTS \
   FOR (n:Node) ON (n.source)";
pub const IDALIAS_ID_CONSTRAINT: &str =
  "CREATE CONSTRAINT idalias_id IF NOT EXISTS \
   FOR (a:IdAlias) REQUIRE a.id IS UNIQUE";
pub const IDALIAS_PRIMARY_ID_INDEX: &str =
  "CREATE INDEX idalias_primary_id IF NOT EXISTS \
   FOR (a:IdAlias) ON (a.primary_id)";

pub async fn apply_schema (
  graph : &Graph,
) -> Result < (), Box<dyn Error> > {
  graph . run ( neo4rs::query ( NODE_ID_CONSTRAINT ) )
    . await ?;
  graph . run ( neo4rs::query ( NODE_SOURCE_INDEX ) )
    . await ?;
  graph . run ( neo4rs::query ( IDALIAS_ID_CONSTRAINT ) )
    . await ?;
  graph . run ( neo4rs::query ( IDALIAS_PRIMARY_ID_INDEX ) )
    . await ?;
  Ok (( )) }
