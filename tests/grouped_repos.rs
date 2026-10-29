// Binary grouping: multi-repo, skgrepo-set,
// and skgrepo/storage-layer tests. See tests/grouped_unit.rs for why test
// files are grouped into a few [[test]] targets.

#[path = "shared/git_fixture_helpers.rs"]
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

#[path = "readiness_requests.rs"]
mod readiness_requests;

#[path = "repo_sets.rs"]
mod skgrepo_sets;

#[path = "pruned_change_view.rs"]
mod pruned_change_view;

#[path = "diff_view.rs"]
mod diff_view;

#[path = "viewnode_id_stability.rs"]
mod viewnode_id_stability;

#[path = "hidden_nodes.rs"]
mod hidden_nodes;
