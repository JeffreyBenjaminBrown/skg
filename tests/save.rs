// cargo nextest run --test grouped_saves -E 'test(save::)'

#[path = "save/none_node_fields_are_noops.rs"]
mod none_node_fields_are_noops;

#[path = "save/birth_and_write_protected.rs"]
mod birth_and_write_protected;

#[path = "save/write_protected_edits.rs"]
mod write_protected_edits;

#[path = "save/validate_foreign_nodes.rs"]
mod validate;

#[path = "save/extra_id_revocation.rs"]
mod extra_id_revocation;
