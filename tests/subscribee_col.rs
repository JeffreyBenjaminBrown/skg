// cargo test --test subscribee_col -- --nocapture

use indoc::indoc;
use skg::dbs::init::{overwrite_new_empty_db, define_schema};
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::typedb::nodes::create_all_nodes;
use skg::dbs::typedb::relationships::create_all_relationships;
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::{SkgConfig, ID};
use skg::types::skgnode::SkgNode;
use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};

const CONFIG_PATH: &str = "tests/subscribee_col/fixtures/skgconfig.toml";

/// Helper to set up multi-source test environment
async fn setup_multi_source_test(
  db_name: &str,
) -> Result<(SkgConfig, TypeDBDriver), Box<dyn Error>> {
  let config: SkgConfig =
    load_config_with_overrides(CONFIG_PATH, Some(db_name), &[])?;
  let driver: TypeDBDriver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?,
  ).await?;
  let nodes: Vec<SkgNode> =
    read_all_skg_files_from_sources(&config)?;
  overwrite_new_empty_db(db_name, &driver).await?;
  define_schema(db_name, &driver).await?;
  create_all_nodes(db_name, &driver, &nodes).await?;
  create_all_relationships(db_name, &driver, &nodes).await?;
  Ok((config, driver)) }

async fn cleanup_test(
  db_name: &str,
  driver: &TypeDBDriver,
  tantivy_folder: &std::path::Path,
) -> Result<(), Box<dyn Error>> {
  // Delete the database
  if driver.databases().contains(db_name).await? {
    driver.databases().get(db_name).await?.delete().await?;
  }
  // Clean up tantivy folder
  if tantivy_folder.exists() {
    std::fs::remove_dir_all(tantivy_folder)?;
  }
  Ok(())
}

#[test]
fn test_subscribee_col_appears_for_subscribers(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-subscribee-col";
    let (config, driver) =
      setup_multi_source_test(db_name).await?;
    let result: String = single_root_view(
      &driver,
      &config,
      &ID("1".to_string()),
    ).await?;
    println!("SubscribeeCol test result:\n{}", result);

    // Nodes 11 and 12 subscribe to something, so they get SubscribeeCol children.
    // Each SubscribeeCol has Subscribee children showing what the node subscribes to.
    // Nodes 13 and 14 do not subscribe to anything, so no SubscribeeCol.
    let expected = indoc! {
      "* (skg (id 1) (source home) (view (rels (containers 0) (contents 4)))) 1
      ** (skg (id 11) (source home) (view (rels (contents 1)))) 11
      *** (skg (code (interp subscribeeCol))) it subscribes to these
      **** (skg (id 11-sees) (source away) (view (rels (containers 0))) (code indefinitive)) 11-sees
      *** (skg (id 111) (source home)) 111
      ** (skg (id 12) (source home)) 12
      *** (skg (code (interp subscribeeCol))) it subscribes to these
      **** (skg (id 12-sees) (source away) (view (rels (containers 0))) (code indefinitive)) 12-sees
      ** (skg (id 13) (source home)) 13
      ** (skg (id 14) (source home) (view (rels (contents 1)))) 14
      *** (skg (id 141) (source home)) 141
"};
    assert_eq!(result, expected,
      "Nodes with subscriptions should have SubscribeeCol children");
    cleanup_test(
      db_name,
      &driver,
      &config.tantivy_folder,
    ).await?;
    Ok (( )) } ) }
