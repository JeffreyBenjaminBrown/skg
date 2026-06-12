// cargo nextest run --test git_diff_view_partner_cols
//
// PartnerCol diff completeness
// (TODO/full-schema/12-2_diff-mode-policy_discussion.org): the
// modules here exercise de novo renders, which install the
// PROCESS-GLOBAL graph handle (PartnerCol creation reads it).
// Historically that handle could not be re-pointed, so these modules
// needed a dedicated target; they now refresh it per sub-test via
// install_or_swap_global_handle, but the separate target is kept --
// it documents the dependency and costs little.

#[path = "git_diff_view/common.rs"]
#[allow(unused_imports, dead_code)] // shared with tests/git_diff_view.rs, which uses more of it
mod common;

#[path = "git_diff_view/overrides/mod.rs"]
mod overrides;
