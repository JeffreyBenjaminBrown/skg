pub mod from_tree;
pub mod reduce_dups;

pub use from_tree::interpret_forest;
pub use reduce_dups::reduce_instructions_for_one_id;
