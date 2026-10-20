// The PCSVF and the "all changes" buffer of the total diff view
// ('TODO/total-diff-view/02_plan.org', "The PCSVF"), over git fixtures
// in 'tests/pcsvf/fixtures/'.
//
// cargo nextest run --test grouped_repos -E 'test(pcsvf::)'

#[path = "pcsvf/common.rs"]
mod common;
#[path = "pcsvf/sets.rs"]
mod sets;
#[path = "pcsvf/buffer.rs"]
mod buffer;
#[path = "pcsvf/request.rs"]
mod request;
#[path = "pcsvf/readiness.rs"]
mod readiness;
#[path = "pcsvf/other_buffers.rs"]
mod other_buffers;
#[path = "pcsvf/stage_and_commit.rs"]
mod stage_and_commit;
#[path = "pcsvf/editing.rs"]
mod editing;
#[path = "pcsvf/folding.rs"]
mod folding;
#[path = "pcsvf/recomputed_sets.rs"]
mod recomputed_sets;
#[path = "pcsvf/write_protecting.rs"]
mod write_protecting;
