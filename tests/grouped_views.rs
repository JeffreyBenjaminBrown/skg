//! Test binary (see 'grouped_unit.rs' for why tests are grouped):
//! drawing views.
//! - content views and their budget: content_view, initial_view_bfs,
//!   editable_view_cascade_and_budget;
//! - viewnode IDs: viewnode_ids;
//! - the containerward chain: containerward_chain;
//! - forks in a view: hidden_from_forks, forks_folder_empty_persists,
//!   forkee_heralds.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture_helpers;
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
