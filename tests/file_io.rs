// cargo test --test file_io

#[allow(unused_imports)]
use indoc::indoc; // A macro, which acts like an unused import.
use std::fs;
use std::path::PathBuf;

use skg::file_io::{
  read_node, write_node, fetch_aliases_from_file};
use skg::mk_org_text::aliases_to_org;
use skg::types::{SkgNode, ID, SkgConfig, skgnode_example, empty_skgnode};

#[test]
fn test_node_io() {
  // Create temporary directory for test output
  let test_dir : PathBuf =
    PathBuf::from("/tmp/file_io_test");
  fs::create_dir_all ( &test_dir )
    . unwrap ();

  // Write the example node to a file
  let example : SkgNode = skgnode_example();
  let out_filename: PathBuf =
    test_dir . join ( example
                      .ids[0] // the primary id
                      .0      // the string part of it
                      .clone() );
  write_node ( &example, &out_filename )
    . unwrap ();

  // Read that file, reverse its lists, write to another file
  let read_node : SkgNode = read_node (
    & out_filename ). unwrap ();
  let mut reversed = reverse_some_of_node(&read_node);
  reversed . ids = // match pid to filename
    vec![ ID::new("reversed") ];

  let reversed_filename : &str =
    "/tmp/file_io_test/reversed.skg";
  write_node(&reversed, reversed_filename).unwrap();

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

  let mut node = read_node (
    "tests/file_io/fixtures/example.skg" ) . unwrap();
  node.body = None; // mutate it
  node.ids = vec![ID::new("no_unindexed")]; // match pid to filename
  write_node(
    &node, "/tmp/file_io_test/no_unindexed.skg" ) . unwrap();
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
    ids               : node.ids               .clone(),
    body              : node.body              .clone(),
    hides_from_its_subscriptions :
      node.hides_from_its_subscriptions        .clone(),
    overrides_view_of : node.overrides_view_of .clone(),
  } }

#[test]
fn test_hyperlinks_extracted_during_read() -> std::io::Result<()> {
  use std::fs::File;
  use std::io::Write;
  use tempfile::tempdir;

  // Create a temporary directory
  let dir = tempdir()?;
  let file_path = dir.path().join("test_node.skg");

  let mut test_node : SkgNode = empty_skgnode ();
  { test_node.title = "Title with two hyperlinks: [[(id hyperlink1][First) Hyperlink]] and [[(id hyperlink2][Second) Hyperlink]]"
      .to_string();
    test_node.aliases = Some(vec![ "alias 1" . to_string(),
                                    "alias 2" . to_string() ]);
    test_node.ids = vec![ID::new("test123")];
    test_node.body = Some("Some text with a link [[(id hyperlink3][Third) Hyperlink]] and another [[(id hyperlink4][Fourth) Hyperlink]]".to_string()); }

  { // Write the node to a file
    let yaml = serde_yaml::to_string(&test_node)
      .map_err (
        |e| std::io::Error::new(
          std::io::ErrorKind::InvalidData,
          e.to_string()))?;
    let mut file = File::create( &file_path )?;
    file.write_all(yaml.as_bytes())?; }
  let read_node = // Read it back from the file.
    read_node(&file_path)?;
  assert_eq!( test_node, read_node,
              "Nodes should have matched." );
  Ok (( ))
}

#[test]
fn test_fetch_aliases_from_file() -> std::io::Result<()> {
  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : PathBuf::from ("tests/file_io/fixtures"),
    tantivy_folder : PathBuf::from ("/tmp/tantivy"),
    port           : 1730,
    delete_on_quit : false,
  };

  let aliases_result : Vec<String> =
    fetch_aliases_from_file (
      &config, ID::new ("node_with_aliases") );
  assert_eq! ( aliases_result,
               vec![ "first alias".to_string (),
                     "second alias".to_string () ],
               "Should return aliases when present" );

  let no_aliases_result =
    fetch_aliases_from_file (
      &config, ID::new ("node_without_aliases") );
  assert_eq! ( no_aliases_result, Vec::<String>::new(),
               "Should return empty Vec when no aliases" );

  Ok (())
}

#[test]
fn test_aliases_to_org() -> std::io::Result<()> {
  assert_eq! ( // a node without aliases
    aliases_to_org ( vec![], 3 ),
    "**** (skg (code (relToParent aliasCol)))\n",
    "Should return just header for node without aliases" );

  assert_eq! ( // a node with aliases
    aliases_to_org (
      vec![ "first alias".to_string(),
             "second alias".to_string() ], 1 ),
    indoc! { r#"
      ** (skg (code (relToParent aliasCol)))
      *** (skg (code (relToParent alias))) first alias
      *** (skg (code (relToParent alias))) second alias
      "# }, // trailing newline matters
    "Should return header + alias lines for node with aliases" );

  Ok (( )) }
