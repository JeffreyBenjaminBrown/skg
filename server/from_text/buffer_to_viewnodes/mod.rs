//! From a saved buffer's text to a viewforest, and the checks the tree
//! must pass before anything is extracted from it.
//! - 'uninterpreted': the parse ('org_to_viewforest'), headline by
//!   headline.
//! - 'add_missing_info': what the parse supplies that a headline may
//!   leave out (pids, skgrepos, aliases).
//! - 'validate_tree': the whole buffer's structure
//!   ('find_buffer_errors_for_saving_in_graph').
//! - 'local': checks of one viewnode at a time.

pub mod uninterpreted;
pub mod validate_tree;
pub mod add_missing_info;
pub mod local;
