pub mod from_tree;

pub use from_tree::interpret_forest;
pub use reduce_dups::{collect_dup_instructions, reduce_dup_instruction};
