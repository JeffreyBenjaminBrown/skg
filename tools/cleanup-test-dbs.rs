/* cleanup-test-dbs
 *
 * Utility to clear all data from the Neo4j database.
 * Neo4j Community Edition has a single database,
 * so this simply deletes all nodes and relationships.
 *
 * USAGE:
 *   cargo run --bin cleanup-test-dbs
 *   OR
 *   ./target/debug/cleanup-test-dbs
 *
 * Safe to run even if the database is already empty.
 * Safe to run while Neo4j is running.
 */

use neo4rs::Graph;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  println!("Connecting to Neo4j...");

  let graph : Graph = Graph::new(
    "bolt://localhost:7687",
    "neo4j",
    "password",
  ).await?;

  println!("Clearing all data...");
  graph.run(
    neo4rs::query("MATCH (n) DETACH DELETE n")
  ).await?;

  println!("✓ All data cleared.");
  Ok(())
}
