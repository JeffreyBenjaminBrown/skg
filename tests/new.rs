// cargo test --test new

#[path = "new/buffer_to_viewnodes/uninterpreted2.rs"]
mod uninterpreted2;

#[path = "new/buffer_to_viewnodes/validate_tree/contradictory_instructions.rs"]
mod validate_tree_contradictory_instructions;

#[path = "new/buffer_to_viewnodes/validate_tree.rs"]
mod validate_tree;

#[path = "new/buffer_to_viewnodes/add_missing_info.rs"]
mod add_missing_info;

#[path = "new/local_instruction_collection/predicates.rs"]
mod local_instruction_collection_predicates;

#[path = "new/local_instruction_collection/types.rs"]
mod local_instruction_collection_types;

#[path = "new/local_instruction_collection/traverse.rs"]
mod local_instruction_collection_traverse;

#[path = "new/local_instruction_collection/lower.rs"]
mod local_instruction_collection_lower;

#[path = "new/local_instruction_collection/pipeline.rs"]
mod local_instruction_collection_pipeline;

#[path = "new/local_instruction_collection/extraction.rs"]
mod local_instruction_collection_extraction;
