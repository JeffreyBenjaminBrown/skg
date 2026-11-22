//! The '.skg' files on disk, skg's canonical data, and the config
//! file. The session-lifetime stores built from these files are
//! 'dbs::in_rust_graph' and 'dbs::tantivy'.
//! - 'one_node': one node's telescope (its same-pid section files in
//!   each configured skgrepo), read by gnid or pid and written back.
//! - 'multiple_nodes': every configured skgrepo read at once (startup,
//!   rebuild), with its load reports; every node written at once.
//! - 'not_nodes': the config file ('load_config') and skgrepo paths.
//! - 'retired_keys': the retired '.skg' keys a read refuses.

pub mod one_node;
pub mod multiple_nodes;
pub mod not_nodes;
pub mod retired_keys;
