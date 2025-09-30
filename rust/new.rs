pub mod uninterpreted2;
pub mod validate_tree;

// Converts an org buffer (a String)
// to a Vec<Tree<OrgNode2>>.
pub use uninterpreted2::org_to_uninterpreted_nodes2;

// Finds inconsistent toDelete instructions across nodes with the same ID.
pub use validate_tree::find_inconsistent_toDelete_instructions;

// Finds user errors in the forest that would make it invalid for saving.
pub use validate_tree::{find_buffer_errors_for_saving, BufferInvalidForSaving};
