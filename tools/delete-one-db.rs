/* delete-one-db
 * Quick utility to clear all data from Neo4j.
 * Neo4j Community Edition has a single database,
 * so there is nothing to select by name.
 *
 * Usage: cargo run --bin delete-one-db
 */

use neo4rs::Graph;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  let graph : Graph = Graph::new(
    "bolt://localhost:7687",
    "neo4j",
    "password",
  ).await?;

  graph.run(
    neo4rs::query("MATCH (n) DETACH DELETE n")
  ).await?;

  println!("✓ All data cleared.");
  Ok(())
}
