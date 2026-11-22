//! Test binary (see 'grouped_unit.rs' for why tests are grouped):
//! saving, and what a save leaves on disk and in the stores.
//! - parsing a saved buffer and extracting its instructions: new
//!   (its modules follow 'server/from_text/');
//! - the save request and its pipeline: save, save_request_envelope,
//!   save_rerender_release, mutation_gate, file_io;
//! - stale and conflicting buffers: save_stale_buffers,
//!   members_as_such_and_stale_buffers, save_dirty_conflicts;
//! - deletes and dangling IDs: delete_strips_references_from_neighbors,
//!   dangling_reference_renders_unknown_node,
//!   write_protected_should_not_count_as_donotdelete, orphan_warning;
//! - merges: merge;
//! - moving between skgrepos: strandeds;
//! - reloading: rebuild, reload_config, repo_watch.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture_helpers;

#[path = "shared/save_handler_helpers.rs"]
#[allow(dead_code)] // Each test file uses only some of the helpers.
mod save_handler_helpers;

#[path = "shared/view_and_save_helpers.rs"]
#[allow(dead_code)] // Each test file uses only some of the helpers.
mod view_and_save_helpers;

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

#[path = "members_as_such_and_stale_buffers.rs"]
mod members_as_such_and_stale_buffers;

#[path = "save_rerender_release.rs"]
mod save_rerender_release;

#[path = "strandeds.rs"]
mod strandeds;
