use std::fs;

use skg::types::{skgnode_example, reverse_skgnode};
use skg::file_io::{read_skgnode_from_path, write_skgnode_to_path};

#[test]
fn test_skgnode_io() {
    // Write the example node to a file
    let example = skgnode_example();
    let out_filename = "generated-data/example.skg";
    write_skgnode_to_path(&example, out_filename).unwrap();

    // Read that file, reverse its lists, write to another file
    let read_node = read_skgnode_from_path(out_filename).unwrap();
    let reversed = reverse_skgnode(&read_node);
    let reversed_filename = "generated-data/reversed.skg";
    write_skgnode_to_path(&reversed, reversed_filename).unwrap();

    // Verify that the generated files match expected files
    let expected_example_path = "tests/fixtures/example.skg";
    let expected_reversed_path = "tests/fixtures/reversed.skg";
    let generated_example = fs::read(out_filename).unwrap();
    let expected_example = fs::read(expected_example_path).unwrap();
    let generated_reversed = fs::read(reversed_filename).unwrap();
    let expected_reversed = fs::read(expected_reversed_path).unwrap();
    assert_eq!(generated_example, expected_example,
	       "Generated example file doesn't match expected");
    assert_eq!(generated_reversed, expected_reversed,
	       "Generated reversed file doesn't match expected");
}
