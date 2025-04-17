use std::fs;
use tempfile::NamedTempFile;

use skg::file_io::{
  read_skgnode_from_path, write_skgnode_to_path};
use skg::types::{SkgNode, skgnode_example};

#[test]
fn test_skgnode_io() {
    // Write the example node to a file
    let example = skgnode_example();
    let out_filename = example.path.clone();
    write_skgnode_to_path( &example,
                            out_filename.to_str().expect(
                                "Invalid UTF-8 in path")
    ) . unwrap();

    // Read that file, reverse its lists, write to another file
    let read_node = read_skgnode_from_path(
        out_filename.to_str().expect("Invalid UTF-8 in path")
    ).unwrap();
    let reversed = reverse_some_of_skgnode(&read_node);
    let reversed_filename = "tests/file_io/generated/reversed.skg";
    write_skgnode_to_path(&reversed, reversed_filename).unwrap();

    // Verify that the generated files match expected files
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

  verify_unindexed_text_not_needed();
}

fn verify_unindexed_text_not_needed() {
  // If a SkgNode's `unindexed_text` is the empty string,
  // then that field need not be written to disk.

  let mut node = read_skgnode_from_path (
    "tests/file_io/fixtures/example.skg" ) . unwrap();
  node.unindexed_text = String::new(); // mutate it
  let temp_file = NamedTempFile::new().unwrap();
  let temp_path = temp_file.path();
  write_skgnode_to_path(
    &node, temp_path ) . unwrap();
  let file_content =
    fs::read_to_string(temp_path).unwrap();
  assert!( !file_content.contains("unindexed"),
            "Expected 'unindexed' to be absent from the serialized YAML, but it was found.\nFile content:\n{}",
            file_content);
  println!("Verified: Empty unindexed_text was omitted from YAML output"); }

pub fn reverse_some_of_skgnode(node: &SkgNode) -> SkgNode {
    // Create a new SkgNode with some reversed lists --
    // specifically, `titles`, `nodes_contained`
    // and `nodes_subscribed`.
    // This is only for testing purposes,
    // to show reading from and writing to disk work;
    // there's no other reason anyone would want to do this.
    let mut reversed_titles = node.titles.clone();
    reversed_titles.reverse();

    let mut reversed_nodes_contained =
        node.nodes_contained.clone();
    reversed_nodes_contained.reverse();

    let mut reversed_nodes_subscribed =
        node.nodes_subscribed.clone();
    reversed_nodes_subscribed.reverse();

  SkgNode {
    titles             : reversed_titles,
    nodes_contained    : reversed_nodes_contained,
    nodes_subscribed   : reversed_nodes_subscribed,

    ids                : node.ids                .clone(),
    unindexed_text     : node.unindexed_text     .clone(),
    path               : node.path               .clone(),
    properties         : node.properties         .clone(),
    nodes_unsubscribed : node.nodes_unsubscribed .clone(),
    links              : node.links              .clone(),
  } }
