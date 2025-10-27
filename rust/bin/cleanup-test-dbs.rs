/* cleanup-test-dbs
 *
 * Utility to safely delete all TypeDB test databases (those starting with "skg-test").
 * Uses TypeDB's API to delete databases, avoiding filesystem-level deletion which
 * can cause TypeDB to crash when it tries to checkpoint deleted databases.
 *
 * USAGE:
 *   cargo run --bin cleanup-test-dbs
 *   OR
 *   ./target/debug/cleanup-test-dbs
 *
 * Safe to run even if no test databases exist.
 * Safe to run while TypeDB is running (uses proper API).
 */

use typedb_driver::{
  Credentials,
  DriverOptions,
  TypeDBDriver,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  println!("Connecting to TypeDB...");

  let driver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?
  ).await?;

  println!("Fetching database list...");
  let databases = driver.databases();
  let all_dbs = databases.all().await?;

  // Filter for test databases (those starting with "skg-test")
  let test_dbs: Vec<String> = all_dbs
    .iter()
    .filter(|db| db.name().starts_with("skg-test"))
    .map(|db| db.name().to_string())
    .collect();

  if test_dbs.is_empty() {
    println!("✓ No test databases found. Nothing to clean up.");
    return Ok(());
  }

  println!("Found {} test database(s) to delete:", test_dbs.len());
  for db_name in &test_dbs {
    println!("  - {}", db_name);
  }
  println!();

  // Delete each test database
  let mut deleted = 0;
  let mut failed = 0;

  for db_name in &test_dbs {
    print!("Deleting '{}'... ", db_name);
    match databases.get(db_name).await {
      Ok(db) => {
        match db.delete().await {
          Ok(_) => {
            println!("✓ deleted");
            deleted += 1;
          }
          Err(e) => {
            println!("✗ failed: {}", e);
            failed += 1;
          }
        }
      }
      Err(e) => {
        println!("✗ failed to get database: {}", e);
        failed += 1;
      }
    }
  }

  println!();
  println!("=== Cleanup Summary ===");
  println!("  Deleted: {}", deleted);
  if failed > 0 {
    println!("  Failed:  {}", failed);
  }
  println!();

  Ok(())
}
