// Binary grouping (TODO/DONE/faster-tests.org): multi-repo, skgrepo-set,
// and skgrepo/storage-layer tests. See tests/grouped_unit.rs for why test
// files are grouped into a few [[test]] targets.

#[path = "diff_mode_refusals.rs"]
mod diff_mode_refusals;

#[path = "leak_battery.rs"]
mod leak_battery;

#[path = "move_repo.rs"]
mod move_skgrepo;

#[path = "search_enrichment_terminal.rs"]
mod search_enrichment_terminal;

#[path = "repo_sets.rs"]
mod skgrepo_sets;
