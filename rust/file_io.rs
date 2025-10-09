// PURPOSE & SUMMARY:
// Reads and writes the SkgNode type on disk.
// As noted in types.rs, there is a 1-to-1 correspondence
// between the SkgNode type and each (path, file) pair,
// which makes this pretty simple.

pub mod one_node;
pub use one_node::{
    read_node_from_id,
    fetch_aliases_from_file,
    read_node,
    write_node,
};

pub mod more;
pub use more::{
    read_skg_files,
    load_config,
};

pub mod update_fs;
pub use update_fs::{
    update_fs_from_saveinstructions,
    write_all_nodes_to_fs,
    delete_all_nodes_from_fs,
};
