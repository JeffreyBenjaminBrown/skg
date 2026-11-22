//! Test binary (see 'grouped_unit.rs' for why tests are grouped):
//! several skgrepos, the skgrepo restriction, and the diff view.
//! - the skgrepo restriction: skgrepo_sets, leak_battery, hidden_nodes,
//!   foreign_hidden, illegal_edits, restricted_graphnode_in_the_buffer,
//!   requests_refused_under_a_narrowing,
//!   forker_hides_under_a_narrowing, hide_inference_under_a_narrowing,
//!   fork_merge_delete_under_a_narrowing;
//! - moving a node between skgrepos: move_skgrepo;
//! - the diff view and git: graph_straddle, graph_straddle_engines,
//!   pruned_change_view, diff_view, processed_requests;
//! - views: search_buffer, redraw_delta, viewnode_id_stability,
//!   editability_carrier.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture_helpers;

#[path = "shared/parse_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod parse_helpers;

#[path = "shared/git_fixture.rs"]
mod git_fixture;

#[path = "graph_straddle.rs"]
mod graph_straddle;

#[path = "graph_straddle_engines.rs"]
mod graph_straddle_engines;

#[path = "leak_battery.rs"]
mod leak_battery;

#[path = "move_repo.rs"]
mod move_skgrepo;

#[path = "search_buffer.rs"]
mod search_buffer;

#[path = "processed_requests.rs"]
mod processed_requests;

#[path = "redraw_delta.rs"]
mod redraw_delta;

#[path = "repo_sets.rs"]
mod skgrepo_sets;

#[path = "pruned_change_view.rs"]
mod pruned_change_view;

#[path = "diff_view.rs"]
mod diff_view;

#[path = "viewnode_id_stability.rs"]
mod viewnode_id_stability;

#[path = "editability_carrier.rs"]
mod editability_carrier;

#[path = "hidden_nodes.rs"]
mod hidden_nodes;

#[path = "foreign_hidden.rs"]
mod foreign_hidden;

#[path = "illegal_edits.rs"]
mod illegal_edits;

#[path = "forker_hides_under_a_narrowing.rs"]
mod forker_hides_under_a_narrowing;

#[path = "hide_inference_under_a_narrowing.rs"]
mod hide_inference_under_a_narrowing;

#[path = "fork_merge_delete_under_a_narrowing.rs"]
mod fork_merge_delete_under_a_narrowing;

#[path = "restricted_graphnode_in_the_buffer.rs"]
mod restricted_graphnode_in_the_buffer;

#[path = "requests_refused_under_a_narrowing.rs"]
mod requests_refused_under_a_narrowing;
