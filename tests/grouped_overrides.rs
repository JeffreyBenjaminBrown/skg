// Binary grouping (TODO/faster-tests.org): override / PartnerCol
// tests. See tests/grouped_unit.rs for why test files are grouped
// into a few [[test]] targets.
// Store-using tests across this binary are serialized by the fixture mutex in
// server/test_utils.rs when plain `cargo test` runs them in one process.

#[path = "collateral_partner_col.rs"]
mod collateral_partner_col;

#[path = "collateral_subscribee_staleness.rs"]
mod collateral_subscribee_staleness;

#[path = "collateral_delete.rs"]
mod collateral_delete;

#[path = "fork.rs"]
mod fork;

#[path = "collateral_source_move.rs"]
mod collateral_source_move;

#[path = "expand_partner_col_member.rs"]
mod expand_partner_col_member;

#[path = "expand_raw_overridden_children.rs"]
mod expand_raw_overridden_children;

#[path = "idempotence.rs"]
mod idempotence;

#[path = "inactive_suppression.rs"]
mod inactive_suppression;

#[path = "overridden_as_such.rs"]
mod overridden_as_such;

#[path = "override_menu.rs"]
mod override_menu;

#[path = "override_search_ancestry.rs"]
mod override_search_ancestry;

#[path = "override_substitution.rs"]
mod override_substitution;

#[path = "partner_col_matrix.rs"]
mod partner_col_matrix;

#[path = "partner_col_order.rs"]
mod partner_col_order;

#[path = "partner_col_warnings.rs"]
mod partner_col_warnings;

#[path = "view_stats_sharing.rs"]
mod view_stats_sharing;
