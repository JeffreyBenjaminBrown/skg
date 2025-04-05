use std::io;

mod types;
mod file_io;

use types::{skgnode_example, reverse_skgnode};
use file_io::{read_skgnode_from_path, write_skgnode_to_path};

fn main() -> io::Result<()> {
    // Write the example node to a file.
    let example = skgnode_example();
    let out_filename = "generated-data/example.skg";
    write_skgnode_to_path(&example, out_filename)?;

    // Read that file, reverse its lists, write to another file.
    let read_node = read_skgnode_from_path(out_filename)?;
    let reversed = reverse_skgnode(&read_node);
    let reversed_filename = "generated-data/reversed.skg";
    write_skgnode_to_path(&reversed, reversed_filename)?;
    Ok(()) }
