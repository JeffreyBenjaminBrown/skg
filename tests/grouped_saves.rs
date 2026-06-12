// Binary grouping (TODO/faster-tests.org): save-pipeline, merge,
// and parsing/extraction tests. See tests/grouped_unit.rs for why
// test files are grouped into a few [[test]] targets.

#[path = "dangling_reference_renders_unknown_node.rs"]
mod dangling_reference_renders_unknown_node;

#[path = "delete_strips_references_from_neighbors.rs"]
mod delete_strips_references_from_neighbors;

#[path = "file_io.rs"]
mod file_io;

#[path = "indef_should_not_count_as_donotdelete.rs"]
mod indef_should_not_count_as_donotdelete;

#[path = "merge.rs"]
mod merge;

#[path = "new.rs"]
mod new;

#[path = "rebuild.rs"]
mod rebuild;

#[path = "save.rs"]
mod save;
