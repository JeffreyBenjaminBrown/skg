// cargo test --test file_io

#[allow(unused_imports)]
use indoc::indoc; // A macro, which acts like an unused import.
use std::fs;
use std::path::PathBuf;

use skg::dbs::filesystem::one_node::{
  fetch_aliases_from_file,
  skgnode_from_pid_and_source, write_skgnode_to_source};
use skg::dbs::filesystem::misc::load_config_with_overrides;
use skg::types::skgnode::{SkgNode, skgnode_example, empty_skgnode};
use skg::types::misc::{ID, SkgConfig};
use skg::test_utils::run_with_test_db;

const CONFIG_PATH: &str = "tests/file_io/fixtures/skgconfig.toml";

#[test]
fn test_node_io() {
  // Create temporary directory for test output
  let test_dir : PathBuf =
    PathBuf::from("/tmp/file_io_test");
  fs::create_dir_all ( &test_dir )
    . unwrap ();

  // Load config, overriding "output" to point to temp dir
  let config: SkgConfig = load_config_with_overrides(
    CONFIG_PATH,
    None,
    &[("output", test_dir.clone())],
  ).unwrap();

  // Write the example node to a file
  let mut example : SkgNode = skgnode_example();
  example.source = "output".to_string();
  write_skgnode_to_source ( &example, &config )
    . unwrap ();

  // Read that file, reverse its lists, write to another file
  let read_node : SkgNode = skgnode_from_pid_and_source (
    &config, example.ids[0].clone(), "output" ). unwrap ();
  let mut reversed = reverse_some_of_node(&read_node);
  reversed.source = "output".to_string();
  reversed . ids = // match pid to filename
    vec![ ID::new("reversed") ];

  write_skgnode_to_source(&reversed, &config).unwrap();
  let out_filename: PathBuf = test_dir.join(&example.ids[0].0);
  let reversed_filename : &str =
    "/tmp/file_io_test/reversed.skg";

  let expected_example_path = "tests/file_io/fixtures/example.skg";
  let expected_reversed_path = "tests/file_io/fixtures/reversed.skg";

  let generated_example =
    fs::read_to_string(out_filename).unwrap();
  let expected_example =
    fs::read_to_string(expected_example_path).unwrap();
  let generated_reversed =
    fs::read_to_string(reversed_filename).unwrap();
  let expected_reversed =
    fs::read_to_string(expected_reversed_path).unwrap();

  let parsed_generated_example: serde_yaml::Value =
    serde_yaml::from_str(&generated_example).unwrap();
  let parsed_expected_example: serde_yaml::Value =
    serde_yaml::from_str(&expected_example).unwrap();
  let parsed_generated_reversed: serde_yaml::Value =
    serde_yaml::from_str(&generated_reversed).unwrap();
  let parsed_expected_reversed: serde_yaml::Value =
    serde_yaml::from_str(&expected_reversed).unwrap();

  assert_eq!(parsed_generated_example, parsed_expected_example,
             "Generated example file doesn't match expected");
  assert_eq!(parsed_generated_reversed, parsed_expected_reversed,
             "Generated reversed file doesn't match expected");

  verify_body_not_needed();
}

fn verify_body_not_needed() {
  // If a SkgNode's `body` is the empty string,
  // then that field need not be written to disk.

  // Load config, overriding "output" to point to temp dir
  let config: SkgConfig = load_config_with_overrides(
    CONFIG_PATH,
    None,
    &[("output", PathBuf::from("/tmp/file_io_test"))],
  ).unwrap();

  let mut node = skgnode_from_pid_and_source (
    &config, ID::new("example"), "fixtures" ) . unwrap();
  node.source = "output".to_string();
  node.body = None; // mutate it
  node.ids = vec![ID::new("no_unindexed")]; // match pid to filename
  write_skgnode_to_source(
    &node, &config ) . unwrap();
  // Parse both files as YAML for semantic comparison
  let generated_yaml: serde_yaml::Value =
    serde_yaml::from_str (
      &fs::read_to_string (
        "/tmp/file_io_test/no_unindexed.skg"
      ) . unwrap () ). unwrap ();
  let expected_yaml: serde_yaml::Value =
    serde_yaml::from_str (
      &fs::read_to_string (
        "tests/file_io/fixtures/no_unindexed.skg"
      ) . unwrap () ). unwrap ();
  assert_eq!(
    generated_yaml,
    expected_yaml,
    "Deleting body did not have the intended effect."
  );
}

pub fn reverse_some_of_node(node: &SkgNode) -> SkgNode {
  // Create a new SkgNode reversing two of its lists,
  // `contains` and `subscribes_to`.
  // This is only for testing purposes,
  // to show reading from and writing to disk work;
  // there's no other reason anyone would want to do this.
  let reversed_contains =
    node.contains.clone().map(|mut v| {
      v.reverse();
      v });
  let reversed_subscribes_to =
    node.subscribes_to.clone().map(|mut v| {
      v.reverse();
      v });
  SkgNode {
    contains          : reversed_contains,
    subscribes_to     : reversed_subscribes_to,

    title             : node.title             .clone(),
    aliases           : node.aliases           .clone(),
    source            : node.source            .clone(),
    ids               : node.ids               .clone(),
    body              : node.body              .clone(),
    hides_from_its_subscriptions :
      node.hides_from_its_subscriptions        .clone(),
    overrides_view_of : node.overrides_view_of .clone(),
  }}

#[test]
fn test_textlinks_extracted_during_read() -> std::io::Result<()> {
  use std::collections::HashMap;
  use skg::types::misc::SkgfileSource;
  use tempfile::tempdir;

  // Create a temporary directory
  let dir = tempdir()?;

  // Create a config with the temp directory as a source
  let config: SkgConfig = {
    let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
    sources.insert("temp".to_string(), SkgfileSource {
      nickname: "temp".to_string(),
      path: dir.path().to_path_buf(),
      user_owns_it: true, });
    SkgConfig::dummyFromSources(sources) };

  let mut test_node : SkgNode = empty_skgnode ();
  { test_node.title = "Title with two textlinks: [[(id textlink1][First) TextLink]] and [[(id textlink2][Second) TextLink]]"
      .to_string();
    test_node.aliases = Some(vec![ "alias 1" . to_string(),
                                    "alias 2" . to_string() ]);
    test_node.ids = vec![ID::new("test123")];
    test_node.source = "temp".to_string();
    test_node.body = Some("Some text with a link [[(id textlink3][Third) TextLink]] and another [[(id textlink4][Fourth) TextLink]]".to_string()); }

  { // Write to a file and read it back.
    write_skgnode_to_source(&test_node, &config)?;
    let read_node = skgnode_from_pid_and_source(
      &config, ID::new("test123"), "temp")?;
    assert_eq!( test_node, read_node,
                "Nodes should have matched." ); }
  Ok (( ))
}

#[test]
fn test_fetch_aliases_from_file(
) -> Result<(), Box<dyn std::error::Error>> {
  run_with_test_db(
    "skg-test-fetch-aliases",
    "tests/file_io/fixtures",
    "/tmp/tantivy-test-fetch-aliases",
    |config, driver, _tantivy| Box::pin(async move {
      test_fetch_aliases_from_file_impl(config, driver).await
    } )) }

async fn test_fetch_aliases_from_file_impl(
  config: &SkgConfig,
  driver: &typedb_driver::TypeDBDriver,
) -> Result<(), Box<dyn std::error::Error>> {
  let aliases_result : Vec<String> =
    fetch_aliases_from_file (
      &config, driver, ID::new ("node_with_aliases") ).await;
  assert_eq! ( aliases_result,
               vec![ "first alias".to_string (),
                     "second alias".to_string () ],
               "Should return aliases when present" );
  let no_aliases_result =
    fetch_aliases_from_file (
      &config, driver, ID::new ("node_without_aliases") ).await;
  assert_eq! ( no_aliases_result, Vec::<String>::new(),
               "Should return empty Vec when no aliases" );
  Ok (( )) }
