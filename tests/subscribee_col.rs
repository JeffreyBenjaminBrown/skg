// cargo nextest run --test grouped_unit -E 'test(subscribee_col::)'

use indoc::indoc;
use skg::assert_metadata_eq;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::{SkgConfig, ID};

use futures::executor::block_on;
use std::error::Error;

const CONFIG_PATH: &str = "tests/subscribee_col/fixtures/skgconfig.toml";

/// Helper to set up multi-source test environment
async fn setup_multi_source_test(
  test_name: &str,
) -> Result<SkgConfig, Box<dyn Error>> {
  let config: SkgConfig =
    load_config_with_overrides(CONFIG_PATH, Some (test_name), &[])?;
  Ok(config) }

async fn cleanup_test(
  _test_name: &str,
  tantivy_folder: &std::path::Path,
) -> Result<(), Box<dyn Error>> {
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
    let test_name = "skg-test-subscribee-col";
    let config =
      setup_multi_source_test (test_name) . await?;
    let (result, _pids, _) : (String, Vec<ID>, _) =
      single_root_view( &config, None, &ID("1" . to_string()), false
                      )?;
    println!("SubscribeeCol test result:\n{}", result);

    // Nodes 11 and 12 subscribe to something, so they get SubscribeeCol children.
    // Each SubscribeeCol has Subscribee children showing what the node subscribes to.
    // Nodes 13 and 14 do not subscribe to anything, so no SubscribeeCol.
    // The 11/12 -> *-sees edges run from owned "home" nodes to foreign
    // "away" nodes. Their default is the owner's home: that is writable
    // and deliberately exposes the foreign ID/relationship there. Since
    // the fixture records both edges at exactly that default, no
    // (relSource ...) override appears.
    let expected = indoc! {
      "* (skg (node (id 1) (source home) (parentIs absent) (rels (contains (out 4))) (viewStats (sourceHerald ⌂:home)))) 1
      ** (skg (node (id 11) (source home) (rels (contains (in 1 (ancestors 1)) (out 1)) (subscribes (out 1)) (birth contains)))) 11
      *** (skg subscribeeCol)
      **** (skg (node (id 11-sees) (source away) indef (rels (subscribes (in 1 (ancestors 2))) (birth subscribes)) (viewStats (sourceHerald ⌂:away)))) 11-sees
      *** (skg (node (id 111) (source home) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 111
      ** (skg (node (id 12) (source home) (rels (contains (in 1 (ancestors 1))) (subscribes (out 1)) (birth contains)))) 12
      *** (skg subscribeeCol)
      **** (skg (node (id 12-sees) (source away) indef (rels (subscribes (in 1 (ancestors 2))) (birth subscribes)) (viewStats (sourceHerald ⌂:away)))) 12-sees
      ** (skg (node (id 13) (source home) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 13
      ** (skg (node (id 14) (source home) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth contains)))) 14
      *** (skg (node (id 141) (source home) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 141
"};
    assert_metadata_eq!(result, expected,
      "Nodes with subscriptions should have SubscribeeCol children");
    cleanup_test(
      test_name,
      &config . tantivy_folder,
    ) . await?;
    Ok (( )) } ) }
