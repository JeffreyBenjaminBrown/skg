// PURPOSE & SUMMARY:
// Reads and writes the SkgNode type on disk.
// As noted in types.rs, there is a 1-to-1 correspondence
// between the SkgNode type and each (path, file) pair,
// which makes this pretty simple.
// .
// DON'T IMPORT these re-exports. They are for documentation.
// INSTEAD, imports in the codebase should use the original,
// longer definition path. That makes it easier to find definitions.

pub mod one_node;
pub use one_node::{
    skgnode_from_id,
    optskgnode_from_id,
    skgnode_from_pid_and_source,
    fetch_aliases_from_file,
    read_skgnode,
    write_skgnode, };
pub mod multiple_nodes;
pub use multiple_nodes::{
    read_all_skg_files_from_sources,
    write_all_nodes_to_fs,
    delete_all_nodes_from_fs, };
pub mod misc;
pub use misc::{
    load_config, };
