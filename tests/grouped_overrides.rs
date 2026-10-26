// Binary grouping: override / PartnerFolder
// tests. See tests/grouped_unit.rs for why test files are grouped
// into a few [[test]] targets.
// Store-using tests across this binary are serialized by the fixture mutex in
// server/test_utils.rs when plain `cargo test` runs them in one process.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture_helpers;

#[path = "shared/git_fixture.rs"]
#[allow(dead_code)] // This target uses only some of the helpers.
mod git_fixture;

#[path = "collateral_partner_folder.rs"]
mod collateral_partner_folder;

#[path = "corrections_channel.rs"]
mod corrections_channel;

#[path = "collateral_subscribee_staleness.rs"]
mod collateral_subscribee_staleness;

#[path = "collateral_delete.rs"]
mod collateral_delete;

#[path = "fork.rs"]
mod fork;

#[path = "retain_or_entombstone_then_prune.rs"]
mod retain_or_entombstone_then_prune;

#[path = "collateral_repo_move.rs"]
mod collateral_skgrepo_move;

#[path = "extend_partner_folder_member.rs"]
mod extend_partner_folder_member;

#[path = "extend_raw_overridden_children.rs"]
mod extend_raw_overridden_children;

#[path = "idempotence.rs"]
mod idempotence;

#[path = "restricted_suppression.rs"]
mod restricted_suppression;

#[path = "overridden_as_such.rs"]
mod overridden_as_such;

#[path = "overrideward_view_subtree.rs"]
mod overrideward_view_subtree;

#[path = "override_substitution.rs"]
mod override_substitution;

#[path = "partner_folder_matrix.rs"]
mod partner_folder_matrix;

#[path = "partner_folder_order.rs"]
mod partner_folder_order;

#[path = "partner_folder_warnings.rs"]
mod partner_folder_warnings;

#[path = "view_stats_sharing.rs"]
mod view_stats_sharing;
