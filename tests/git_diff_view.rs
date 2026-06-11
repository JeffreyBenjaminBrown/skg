/// Git diff view tests.

#[path = "git_diff_view/common.rs"]
mod common;

#[path = "git_diff_view/content/mod.rs"]
mod content;

#[path = "git_diff_view/text/mod.rs"]
mod text;

#[path = "git_diff_view/ids/mod.rs"]
mod ids;

#[path = "git_diff_view/aliases/mod.rs"]
mod aliases;

#[path = "git_diff_view/newhere_cycle/mod.rs"]
mod newhere_cycle;

#[path = "git_diff_view/collateral/mod.rs"]
mod collateral;

#[path = "git_diff_view/inbound/mod.rs"]
mod inbound;

// The outbound-col diff tests (git_diff_view/overrides/) live in
// their own target, tests/git_diff_view_partner_cols.rs: their de
// novo renders install the process-global graph handle, which must
// not leak into this target's save tests (or the coherence
// debug_assert in save_buffer.rs) under plain 'cargo test', where
// one process hosts a whole target. The inbound tests save without
// the global handle, so they live here.
