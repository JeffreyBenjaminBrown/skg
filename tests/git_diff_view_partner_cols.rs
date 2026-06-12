// cargo nextest run --test git_diff_view_partner_cols
//
// PartnerCol diff completeness
// (TODO/full-schema/12-2_diff-mode-policy_discussion.org): the
// modules here exercise de novo renders, which install the
// PROCESS-GLOBAL graph handle (PartnerCol creation reads it).  That
// handle must not leak into other targets' save tests under plain
// 'cargo test', where one process hosts a whole target -- hence this
// dedicated target.  All modules here must share fixture content
// compatible with one global graph per process.

#[path = "git_diff_view/common.rs"]
#[allow(unused_imports)] // shared with tests/git_diff_view.rs, which uses more of it
mod common;

#[path = "git_diff_view/overrides/mod.rs"]
mod overrides;
