//! Where skg's data lives. The '.skg' files are canonical; the
//! in-Rust graph and the Tantivy index are session-lifetime stores,
//! rebuilt from them at startup.
//! - 'filesystem': reading and writing '.skg' files, and the config file.
//! - 'in_rust_graph': every node in memory, its relation indexes, and
//!   the rules a graph must obey (fork rules, mirrors, validation).
//! - 'tantivy': the search index of titles, aliases and bodies.
//! - 'init': building both stores at startup, and remaking the index.
//! - 'node_lookup': a 'Graphnode' from the in-Rust graph by gnid.

pub mod filesystem;
pub mod init;
pub mod in_rust_graph;
pub mod node_lookup;
pub mod tantivy;
