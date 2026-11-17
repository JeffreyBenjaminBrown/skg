// Binary grouping: view rendering and
// forkee-visibility tests. See tests/grouped_unit.rs for why
// test files are grouped into a few [[test]] targets.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture_helpers;
#[path = "shared/tcp_stream_pair.rs"]
mod tcp_stream_pair;
#[path = "shared/view_and_save_helpers.rs"]
#[allow(dead_code)] // Each test file uses only some of the helpers.
mod view_and_save_helpers;

#[path = "content_view.rs"]
mod content_view;

#[path = "editable_view_cascade_and_budget.rs"]
mod editable_view_cascade_and_budget;

#[path = "hidden_from_forks.rs"]
mod hidden_from_forks;

#[path = "initial_view_bfs.rs"]
mod initial_view_bfs;

#[path = "forks_folder_empty_persists.rs"]
mod forks_folder_empty_persists;

#[path = "forkee_heralds.rs"]
mod forkee_heralds;

#[path = "viewnode_ids.rs"]
mod viewnode_ids;

#[path = "containerward_chain.rs"]
mod containerward_chain;
