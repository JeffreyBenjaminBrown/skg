use std::fs;

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

  verify_body_not_needed();
}

fn verify_body_not_needed() {
  // If a SkgNode's `body` is the empty string,
  // then that field need not be written to disk.

  let mut node = read_skgnode_from_path (
    "tests/file_io/fixtures/example.skg" ) . unwrap();
  node.body = None; // mutate it
  write_skgnode_to_path(
    &node, "tests/file_io/generated/no_unindexed.skg" ) . unwrap();
  assert_eq!(
    fs::read_to_string(
      "tests/file_io/generated/no_unindexed.skg").unwrap(),
    fs::read_to_string(
      "tests/file_io/fixtures/no_unindexed.skg").unwrap(),
    "Deleting body did not have the intended effect."); }

pub fn reverse_some_of_skgnode(node: &SkgNode) -> SkgNode {
    // Create a new SkgNode with some reversed lists --
    // specifically, `titles`, `contains`
    // and `subscribes_to`.
    // This is only for testing purposes,
    // to show reading from and writing to disk work;
    // there's no other reason anyone would want to do this.
    let mut reversed_titles = node.titles.clone();
    reversed_titles.reverse();

    let mut reversed_contains =
        node.contains.clone();
    reversed_contains.reverse();

    let mut reversed_subscribes_to =
        node.subscribes_to.clone();
    reversed_subscribes_to.reverse();

  SkgNode {
    titles             : reversed_titles,
    contains           : reversed_contains,
    subscribes_to      : reversed_subscribes_to,

    ids                : node.ids                .clone(),
    body               : node.body               .clone(),
    path               : node.path               .clone(),
    comments_on        : node.comments_on        .clone(),
    no_tantivy_index   : node.no_tantivy_index,
    ignores            : node.ignores            .clone(),
    replaces_view_of   : node.replaces_view_of   .clone(),
    links              : node.links              .clone(),
  } }
