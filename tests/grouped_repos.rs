// Binary grouping (TODO/DONE/faster-tests.org): multi-repo, skgrepo-set,
// and skgrepo/storage-layer tests. See tests/grouped_unit.rs for why test
// files are grouped into a few [[test]] targets.

#[path = "graph_pair.rs"]
mod graph_pair;

#[path = "graph_pair_engines.rs"]
mod graph_pair_engines;

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

#[path = "tdv.rs"]
mod tdv;
