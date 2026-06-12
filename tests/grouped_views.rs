// Binary grouping (TODO/faster-tests.org): view rendering and
// subscription-visibility tests. See tests/grouped_unit.rs for why
// test files are grouped into a few [[test]] targets.

#[path = "content_view.rs"]
mod content_view;

#[path = "definitive_view_cascade_and_budget.rs"]
mod definitive_view_cascade_and_budget;

#[path = "hidden_from_subscriptions.rs"]
mod hidden_from_subscriptions;

#[path = "initial_view_bfs.rs"]
mod initial_view_bfs;

#[path = "subscribee_col_empty_persists.rs"]
mod subscribee_col_empty_persists;
