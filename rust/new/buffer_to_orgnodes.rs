pub mod uninterpreted2;
pub mod validate_tree;
pub mod add_missing_info;

// Converts an org buffer (a String)
// to a Vec<Tree<OrgNode2>>.
pub use uninterpreted2::{org_to_uninterpreted_nodes2, headline_to_triple, HeadlineInfo};

// Finds inconsistent instructions across nodes with the same ID.
pub use validate_tree::find_inconsistent_instructions;

// Finds user errors in the forest that would make it invalid for saving.
pub use validate_tree::{find_buffer_errors_for_saving, Buffer_Cannot_Be_Saved};

// Adds missing information to trees (Alias relations and UUIDs).
pub use add_missing_info::add_missing_info_to_trees;
