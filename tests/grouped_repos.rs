// Binary grouping (TODO/DONE/faster-tests.org): multi-repo, skgrepo-set,
// and skgrepo/storage-layer tests. See tests/grouped_unit.rs for why test
// files are grouped into a few [[test]] targets.

#[path = "shared/git_fixture_helpers.rs"]
mod git_fixture_helpers;

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

#[path = "search_enrichment_terminal.rs"]
mod search_enrichment_terminal;

#[path = "readiness_requests.rs"]
mod readiness_requests;

#[path = "repo_sets.rs"]
mod skgrepo_sets;

#[path = "pcsvf.rs"]
mod pcsvf;

#[path = "diff_view.rs"]
mod diff_view;
