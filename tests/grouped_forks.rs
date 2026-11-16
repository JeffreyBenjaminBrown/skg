// Binary grouping: forks / partner folder
// tests. See tests/grouped_unit.rs for why test files are grouped
// into a few [[test]] targets.
// Store-using tests across this binary are serialized by the fixture mutex in
// server/test_utils.rs when plain `cargo test` runs them in one process.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture_helpers;
#[path = "shared/tcp_stream_pair.rs"]
mod tcp_stream_pair;

#[path = "shared/git_fixture.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture;

#[path = "collateral_partner_folder.rs"]
mod collateral_partner_folder;

#[path = "collateral_forkee_staleness.rs"]
mod collateral_forkee_staleness;

#[path = "collateral_delete.rs"]
mod collateral_delete;

#[path = "collateral_new_link.rs"]
mod collateral_new_link;

#[path = "fork.rs"]
mod fork;

#[path = "what_the_refresh_keeps.rs"]
mod what_the_refresh_keeps;

#[path = "collateral_repo_move.rs"]
mod collateral_skgrepo_move;

#[path = "extend_partner_folder_member.rs"]
mod extend_partner_folder_member;

#[path = "forks_folder.rs"]
mod forks_folder;

#[path = "forkees_drawn_editable.rs"]
mod forkees_drawn_editable;
#[path = "forkers_drawn_editable.rs"]
mod forkers_drawn_editable;

#[path = "hides_apply_to_every_fork_partner.rs"]
mod hides_apply_to_every_fork_partner;

#[path = "children_of_a_raw_root_are_raw.rs"]
mod children_of_a_raw_root_are_raw;

#[path = "idempotence.rs"]
mod idempotence;

#[path = "restricted_suppression.rs"]
mod restricted_suppression;

#[path = "forks_under_search_hits.rs"]
mod forks_under_search_hits;

#[path = "mirror_substitution.rs"]
mod mirror_substitution;

#[path = "partner_folder_matrix.rs"]
mod partner_folder_matrix;

#[path = "forkee_as_such_extension.rs"]
mod forkee_as_such_extension;

#[path = "partner_folder_order.rs"]
mod partner_folder_order;

#[path = "partner_folder_warnings.rs"]
mod partner_folder_warnings;

#[path = "view_stats_sharing.rs"]
mod view_stats_sharing;
