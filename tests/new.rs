// cargo test --test new

#[path = "new/buffer_to_viewnodes/uninterpreted2.rs"]
mod uninterpreted2;

#[path = "new/buffer_to_viewnodes/validate_tree/contradictory_instructions.rs"]
mod validate_tree_contradictory_instructions;

#[path = "new/buffer_to_viewnodes/validate_tree.rs"]
mod validate_tree;

#[path = "new/buffer_to_viewnodes/add_missing_info.rs"]
mod add_missing_info;

#[path = "new/viewnodes_to_instructions.rs"]
mod viewnodes_to_instructions;

#[path = "new/viewnodes_to_instructions/reconcile_same_id_instructions.rs"]
mod reconcile_same_id_instructions;

#[path = "new/viewnodes_to_instructions/reconcile_same_id_instructions/by_claude.rs"]
mod reconcile_same_id_instructions_by_claude;
