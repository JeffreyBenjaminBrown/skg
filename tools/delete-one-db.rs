/* delete-one-db
 * Quick utility to delete a single database by name
 * Usage: cargo run --bin delete-one-db test-manual
 */

use typedb_driver::{Credentials, DriverOptions, TypeDBDriver};
use std::env;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <database-name>", args[0]);
        std::process::exit(1);
    }

    let db_name = &args[1];

    let driver = TypeDBDriver::new(
        "127.0.0.1:1729",
        Credentials::new("admin", "password"),
        DriverOptions::new(false, None)?
    ).await?;

    let databases = driver.databases();
    if databases.contains(db_name).await? {
        let db = databases.get(db_name).await?;
        db.delete().await?;
        println!("✓ Deleted '{}'", db_name);
    } else {
        println!("✗ Database '{}' not found", db_name);
    }
    Ok(())
}
