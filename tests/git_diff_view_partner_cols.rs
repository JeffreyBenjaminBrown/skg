// cargo nextest run --test git_diff_view_partner_cols
//
// PartnerCol diff completeness
// (TODO/full-schema/12-2_diff-mode-policy_discussion.org): the
// modules here exercise the larger de novo partner-column renders.
// The separate target keeps those scenarios quick to run in isolation.

#[path = "git_diff_view/common.rs"]
#[allow(unused_imports, dead_code)] // shared with tests/git_diff_view.rs, which uses more of it
mod common;

#[path = "git_diff_view/overrides/mod.rs"]
mod overrides;
