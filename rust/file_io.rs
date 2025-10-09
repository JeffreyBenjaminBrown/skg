// PURPOSE & SUMMARY:
// Reads and writes the SkgNode type on disk.
// As noted in types.rs, there is a 1-to-1 correspondence
// between the SkgNode type and each (path, file) pair,
// which makes this pretty simple.

pub mod one_node;
pub use one_node::*;

pub mod more;
pub use more::*;

pub mod update_fs;
pub use update_fs::*;
