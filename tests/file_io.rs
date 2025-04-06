use std::fs;

use skg::types::{SkgNode, skgnode_example};
use skg::file_io::{read_skgnode_from_path, write_skgnode_to_path};

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
    let reversed = reverse_skgnode(&read_node);
    let reversed_filename = "tests/file_io/generated/reversed.skg";
    write_skgnode_to_path(&reversed, reversed_filename).unwrap();

    // Verify that the generated files match expected files
    let expected_example_path = "tests/file_io/fixtures/example.skg";
    let expected_reversed_path = "tests/file_io/fixtures/reversed.skg";
    let generated_example = fs::read(out_filename).unwrap();
    let expected_example = fs::read(expected_example_path).unwrap();
    let generated_reversed = fs::read(reversed_filename).unwrap();
    let expected_reversed = fs::read(expected_reversed_path).unwrap();
    assert_eq!(generated_example, expected_example,
	       "Generated example file doesn't match expected");
    assert_eq!(generated_reversed, expected_reversed,
	       "Generated reversed file doesn't match expected");
}

pub fn reverse_skgnode(node: &SkgNode) -> SkgNode {
    // Create a new SkgNode with reversed lists.
    // This is only for testing purposes,
    // to show reading from and writing to disk work.
    let mut reversed_titles = node.titles.clone();
    reversed_titles.reverse();

    let mut reversed_nodes_contained =
        node.nodes_contained.clone();
    reversed_nodes_contained.reverse();

    let mut reversed_nodes_subscribed =
        node.nodes_subscribed.clone();
    reversed_nodes_subscribed.reverse();

    SkgNode {
        path: node.path.clone(),
        format: node.format.clone(),
        id: node.id.clone(),
        context: node.context.clone(),
        is_comment: node.is_comment,
        titles: reversed_titles,
        unindexed_text: node.unindexed_text.clone(),
        nodes_contained: reversed_nodes_contained,
        nodes_subscribed: reversed_nodes_subscribed,
    } }
