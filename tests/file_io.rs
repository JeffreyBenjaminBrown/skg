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
    assert_eq!(generated_example.trim_end(),
	       expected_example.trim_end(),
	       "Generated example file doesn't match expected");
    assert_eq!(generated_reversed.trim_end(),
	       expected_reversed.trim_end(),
	       "Generated reversed file doesn't match expected");
}

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
    } }
