// The pruned change view and the "all changes" buffer of the diff view
// ('docs/diff-view.org', "C, R, U and D"),
// over git fixtures in 'tests/pruned_change_view/fixtures/'.
//
// cargo nextest run --test grouped_repos -E 'test(pruned_change_view::)'

#[path = "pruned_change_view/common.rs"]
mod common;
#[path = "pruned_change_view/sets.rs"]
mod sets;
#[path = "pruned_change_view/buffer.rs"]
mod buffer;
#[path = "pruned_change_view/request.rs"]
mod request;
#[path = "pruned_change_view/readiness.rs"]
mod readiness;
#[path = "pruned_change_view/other_buffers.rs"]
mod other_buffers;
#[path = "pruned_change_view/stage_and_commit.rs"]
mod stage_and_commit;
#[path = "pruned_change_view/editing.rs"]
mod editing;
#[path = "pruned_change_view/folding.rs"]
mod folding;
#[path = "pruned_change_view/recomputed_sets.rs"]
mod recomputed_sets;
#[path = "pruned_change_view/write_protecting.rs"]
mod write_protecting;
#[path = "pruned_change_view/partner_folder_check.rs"]
mod partner_folder_check;
#[path = "pruned_change_view/users_notes.rs"]
mod users_notes;
#[path = "pruned_change_view/viewnode_ids.rs"]
mod viewnode_ids;
#[path = "pruned_change_view/order.rs"]
mod order;
#[path = "pruned_change_view/after_a_commit.rs"]
mod after_a_commit;
#[path = "pruned_change_view/restore.rs"]
mod restore;
#[path = "pruned_change_view/adoption_by_key.rs"]
mod adoption_by_key;
