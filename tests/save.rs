// cargo nextest run --test grouped_saves -E 'test(save::)'

#[path = "save/none_node_fields_are_noops.rs"]
mod none_node_fields_are_noops;

#[path = "save/birth_and_indefinitive.rs"]
mod birth_and_indefinitive;

#[path = "save/indefinitive_edits.rs"]
mod indefinitive_edits;

#[path = "save/validate_foreign_nodes.rs"]
mod validate;

#[path = "save/extra_id_revocation.rs"]
mod extra_id_revocation;
