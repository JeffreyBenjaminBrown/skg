//! The types the rest of the server shares, and their small helpers.
//! Code that works on these types lives with its subsystem.
//! - 'viewnode': what a view's tree is made of ('Viewnode', 'Vognode',
//!   'Folder', edit and view requests, view stats, diff-view facts).
//! - 'nodes': a node's four shapes (whole, on disk, in the in-Rust
//!   graph, in Tantivy) and its attribution.
//! - 'tree': trees and forests of viewnodes ('ViewForest') and
//!   'ego_tree' helpers.
//! - 'views_state': a connection's open views ('OpenViews', 'ViewState').
//! - 'save': what a save plans and does ('SavePlan', 'NodeInstruction').
//! - 'errors': save and buffer-validation errors, and how messages name
//!   a headline.
//! - 'config': the config file's contents ('SkgConfig'), and the
//!   privacy ladder and fold order of its skgrepos.
//! - 'env': the runtime generation every request reads ('SkgEnv').
//! - 'misc': 'ID', 'SkgrepoName', 'MSV', 'RelPartner', 'TantivyIndex'.
//! - 'links': Org links in a node's text ('Link').
//! - 'disk_home': a node's home skgrepo, found on disk.
//! - 'git': a skgrepo's git diff.
//! - 'list': list diffs.
//! - 'sexp': s-expression helpers.
//! - 'many_to_many', 'maps': small containers.

pub mod sexp;
pub mod links;
pub mod misc;
pub mod config;
pub mod env;
pub mod errors;
pub mod git;
pub mod list;
pub mod disk_home;
pub mod viewnode;
pub mod nodes;
pub mod many_to_many;
pub mod views_state;
pub mod save;
pub mod tree;
pub mod maps;
