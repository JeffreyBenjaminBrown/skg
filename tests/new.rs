// cargo nextest run --test grouped_saves -E 'test(new::)'

#[path = "new/buffer_to_viewnodes/uninterpreted2.rs"]
mod uninterpreted2;

#[path = "new/buffer_to_viewnodes/validate_tree/contradictory_instructions.rs"]
mod validate_tree_contradictory_instructions;

#[path = "new/buffer_to_viewnodes/validate_tree.rs"]
mod validate_tree;

#[path = "new/buffer_to_viewnodes/add_missing_info.rs"]
mod add_missing_info;

#[path = "new/local_fieldintent_collection/predicates.rs"]
mod local_fieldintent_collection_predicates;

#[path = "new/local_fieldintent_collection/types.rs"]
mod local_fieldintent_collection_types;

#[path = "new/local_fieldintent_collection/traverse.rs"]
mod local_fieldintent_collection_traverse;

#[path = "new/local_fieldintent_collection/lower.rs"]
mod local_fieldintent_collection_lower;

#[path = "new/local_fieldintent_collection/pipeline.rs"]
mod local_fieldintent_collection_pipeline;

#[path = "new/local_fieldintent_collection/extraction.rs"]
mod local_fieldintent_collection_extraction;
