// cargo test --test subscribee_col -- --nocapture

use indoc::indoc;
use neo4rs::Graph;
use skg::dbs::neo4j::util::delete_database;
use skg::dbs::neo4j::schema::apply_schema;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::neo4j::nodes::create_all_nodes;
use skg::dbs::neo4j::relationships::create_all_relationships;
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::{SkgConfig, ID};
use skg::types::skgnode::SkgNode;
use tokio::runtime::Runtime;
use std::error::Error;

const CONFIG_PATH: &str = "tests/subscribee_col/fixtures/skgconfig.toml";

/// Helper to set up multi-source test environment
async fn setup_multi_source_test(
  db_name: &str,
) -> Result<(SkgConfig, Graph), Box<dyn Error>> {
  let config: SkgConfig =
    load_config_with_overrides(CONFIG_PATH, Some(db_name), &[])?;
  let graph: Graph = Graph::new(
    "bolt://localhost:7687", "neo4j", "password"
  ).await?;
  let nodes: Vec<SkgNode> =
    read_all_skg_files_from_sources(&config)?;
  delete_database(&graph).await?;
  apply_schema(&graph).await?;
  create_all_nodes(&graph, &nodes).await?;
  create_all_relationships(&graph, &nodes).await?;
  Ok((config, graph)) }

async fn cleanup_test(
  graph: &Graph,
  tantivy_folder: &std::path::Path,
) -> Result<(), Box<dyn Error>> {
  // Delete the database
  delete_database(graph).await?;
  // Clean up tantivy folder
  if tantivy_folder.exists() {
    std::fs::remove_dir_all(tantivy_folder)?;
  }
  Ok(())
}

#[test]
fn test_subscribee_col_appears_for_subscribers(
) -> Result<(), Box<dyn Error>> {
  let rt : Runtime = Runtime::new().unwrap();
  rt.block_on(async {
    let db_name = "skg-test-subscribee-col";
    let (config, graph) =
      setup_multi_source_test(db_name).await?;
    let result: String = single_root_view(
      &graph,
      &config,
      &ID("1".to_string()),
      false,
    ).await?;
    println!("SubscribeeCol test result:\n{}", result);

    // Nodes 11 and 12 subscribe to something, so they get SubscribeeCol children.
    // Each SubscribeeCol has Subscribee children showing what the node subscribes to.
    // Nodes 13 and 14 do not subscribe to anything, so no SubscribeeCol.
    let expected = indoc! {
      "* (skg (node (id 1) (source home) (graphStats (containers 0) (contents 4)))) 1
      ** (skg (node (id 11) (source home) (graphStats (contents 1) subscribing))) 11
      *** (skg subscribeeCol) it subscribes to these
      **** (skg (node (id 11-sees) (source away) indefinitive (graphStats (containers 0) subscribing))) 11-sees
      *** (skg (node (id 111) (source home))) 111
      ** (skg (node (id 12) (source home) (graphStats subscribing))) 12
      *** (skg subscribeeCol) it subscribes to these
      **** (skg (node (id 12-sees) (source away) indefinitive (graphStats (containers 0) subscribing))) 12-sees
      ** (skg (node (id 13) (source home))) 13
      ** (skg (node (id 14) (source home) (graphStats (contents 1)))) 14
      *** (skg (node (id 141) (source home))) 141
"};
    assert_eq!(result, expected,
      "Nodes with subscriptions should have SubscribeeCol children");
    cleanup_test(
      &graph,
      &config.tantivy_folder,
    ).await?;
    Ok (( )) } ) }
