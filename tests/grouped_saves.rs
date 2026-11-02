// Binary grouping: save-pipeline, merge,
// and parsing/extraction tests. See tests/grouped_unit.rs for why
// test files are grouped into a few [[test]] targets.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture_helpers;
#[path = "shared/tcp_stream_pair.rs"]
mod tcp_stream_pair;

#[path = "shared/parse_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod parse_helpers;

#[path = "dangling_reference_renders_unknown_node.rs"]
mod dangling_reference_renders_unknown_node;

#[path = "delete_strips_references_from_neighbors.rs"]
mod delete_strips_references_from_neighbors;

#[path = "file_io.rs"]
mod file_io;

#[path = "write_protected_should_not_count_as_donotdelete.rs"]
mod write_protected_should_not_count_as_donotdelete;

#[path = "merge.rs"]
mod merge;

#[path = "orphan_warning.rs"]
mod orphan_warning;

#[path = "mutation_gate.rs"]
mod mutation_gate;

#[path = "new.rs"]
mod new;

#[path = "rebuild.rs"]
mod rebuild;

#[path = "reload_config.rs"]
mod reload_config;

#[path = "repo_watch.rs"]
mod repo_watch;

#[path = "save.rs"]
mod save;

#[path = "save_request_envelope.rs"]
mod save_request_envelope;

#[path = "save_dirty_conflicts.rs"]
mod save_dirty_conflicts;

#[path = "save_stale_buffers.rs"]
mod save_stale_buffers;

#[path = "save_rerender_release.rs"]
mod save_rerender_release;

#[path = "strandeds.rs"]
mod strandeds;
