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

#[path = "git_diff_view/filter_folders/mod.rs"]
mod filter_folders;

#[path = "git_diff_view/roundtrip/mod.rs"]
mod roundtrip;

// The outbound-folder diff tests live in their own focused target,
// tests/git_diff_view_partner_folders.rs. The inbound tests remain here.
