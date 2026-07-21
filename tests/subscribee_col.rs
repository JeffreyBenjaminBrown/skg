// cargo nextest run --test grouped_unit -E 'test(subscribee_col::)'

use indoc::indoc;
use skg::assert_metadata_eq;
use skg::dbs::init::{overwrite_new_empty_typedb_db, read_and_use_schema};
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::typedb::nodes::create_all_nodes;
use skg::dbs::typedb::relationships::create_all_relationships;
use skg::dbs::typedb::sources::create_all_sources;
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::{SkgConfig, ID};

use skg::types::nodes::typedb::NodeTypedb;
use skg::types::nodes::complete::NodeComplete;
use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{TypeDBDriver, Addresses, Credentials, DriverOptions, DriverTlsConfig};

const CONFIG_PATH: &str = "tests/subscribee_col/fixtures/skgconfig.toml";

/// Helper to set up multi-source test environment
async fn setup_multi_source_test(
  db_name: &str,
) -> Result<(SkgConfig, TypeDBDriver), Box<dyn Error>> {
  let config: SkgConfig =
    load_config_with_overrides(CONFIG_PATH, Some (db_name), &[])?;
  let driver: TypeDBDriver = TypeDBDriver::new(
    Addresses::try_from_address_str("127.0.0.1:1729")?,
    Credentials::new("admin", "password"),
    DriverOptions::new(DriverTlsConfig::disabled()),
  ) . await?;
  let nodes: Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config)?;
  let typedb_nodes : Vec<NodeTypedb> =
    nodes . iter ()
    . map (NodeTypedb::from_complete_parsing_textlinks)
    . collect ();
  overwrite_new_empty_typedb_db(db_name, &driver) . await?;
  read_and_use_schema(db_name, &driver) . await?;
  create_all_sources(db_name, &driver, &config) . await?;
  create_all_nodes(db_name, &driver, &typedb_nodes) . await?;
  create_all_relationships(db_name, &driver, &typedb_nodes) . await?;
  Ok((config, driver)) }

async fn cleanup_test(
  db_name: &str,
  driver: &TypeDBDriver,
  tantivy_folder: &std::path::Path,
) -> Result<(), Box<dyn Error>> {
  // Delete the database
  if driver . databases() . contains (db_name) . await? {
    driver . databases() . get (db_name) . await?. delete() . await?;
  }
  // Clean up tantivy folder
  if tantivy_folder . exists() {
    std::fs::remove_dir_all (tantivy_folder)?;
  }
  Ok(())
}

#[test]
fn test_subscribee_col_appears_for_subscribers(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-subscribee-col";
    let (config, driver) =
      setup_multi_source_test (db_name) . await?;
    skg::dbs::in_rust_graph::install_or_swap_global_handle (
      skg::test_utils::graph_handle_from_config (&config) ? );
    let driver : std::sync::Arc<TypeDBDriver> =
      std::sync::Arc::new (driver);
    let (result, _pids, _) : (String, Vec<ID>, _) =
      single_root_view( &driver, &config, None, &ID("1" . to_string()), false
                      ) . await?;
    println!("SubscribeeCol test result:\n{}", result);

    // Nodes 11 and 12 subscribe to something, so they get SubscribeeCol children.
    // Each SubscribeeCol has Subscribee children showing what the node subscribes to.
    // Nodes 13 and 14 do not subscribe to anything, so no SubscribeeCol.
    // The 11/12 -> *-sees edges are recorded in the (public) "home"
    // section even though their target lives in the (private) "away"
    // section -- older fixture data predating the privacy-telescope
    // levels, which the telescope-validators leak warning already
    // flags but tolerates. Since that recorded level ("home") differs
    // from the edge's DEFAULT (more_private_of(home, away) = "away"),
    // render-and-gating's herald surfaces it: (relSource home).
    let expected = indoc! {
      "* (skg (node (id 1) (source home) (parentIs absent) (rels (contains (out 4))) (viewStats (sourceHerald ⌂:home)))) 1
      ** (skg (node (id 11) (source home) (rels (contains (in 1 (ancestors 1)) (out 1)) (subscribes (out 1)) (birth contains)))) 11
      *** (skg subscribeeCol)
      **** (skg (node (id 11-sees) (source away) indef (rels (subscribes (in 1 (ancestors 2))) (birth subscribes)) (viewStats (relSource home) (sourceHerald ⌂:away)))) 11-sees
      *** (skg (node (id 111) (source home) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 111
      ** (skg (node (id 12) (source home) (rels (contains (in 1 (ancestors 1))) (subscribes (out 1)) (birth contains)))) 12
      *** (skg subscribeeCol)
      **** (skg (node (id 12-sees) (source away) indef (rels (subscribes (in 1 (ancestors 2))) (birth subscribes)) (viewStats (relSource home) (sourceHerald ⌂:away)))) 12-sees
      ** (skg (node (id 13) (source home) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 13
      ** (skg (node (id 14) (source home) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth contains)))) 14
      *** (skg (node (id 141) (source home) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 141
"};
    assert_metadata_eq!(result, expected,
      "Nodes with subscriptions should have SubscribeeCol children");
    cleanup_test(
      db_name,
      &driver,
      &config . tantivy_folder,
    ) . await?;
    Ok (( )) } ) }
