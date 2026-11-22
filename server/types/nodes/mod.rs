//! A node's shapes. 'complete::Graphnode' is the whole node; the
//! others are what each place holds of it:
//! - 'complete': 'Graphnode', and its 'Flag's.
//! - 'fs': 'GraphnodeOnDisk', one telescope section as a '.skg' file
//!   holds it.
//! - 'rust': 'GraphnodeInRust', the in-Rust graph's node.
//! - 'tantivy': 'GraphnodeInTantivy', what the search index holds.
//! - 'attribution': what a fork records of the note it forked.

pub mod attribution;
pub mod complete;
pub mod fs;
pub mod rust;
pub mod tantivy;
