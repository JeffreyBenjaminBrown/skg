// Binary grouping (TODO/faster-tests.org): multi-repo, repo-set,
// and repo/storage-layer tests. See tests/grouped_unit.rs for why test
// files are grouped into a few [[test]] targets.

#[path = "diff_mode_refusals.rs"]
mod diff_mode_refusals;

#[path = "leak_battery.rs"]
mod leak_battery;

#[path = "move_repo.rs"]
mod move_repo;

#[path = "search_enrichment_terminal.rs"]
mod search_enrichment_terminal;

#[path = "repo_sets.rs"]
mod repo_sets;
