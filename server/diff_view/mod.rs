//! The diff view (C-c v d): up to four buffers, each rendered over the
//! graph straddle ('server/graph_straddle/'). 'docs/diff-view.org'
//! describes it; this module implements it:
//! - 'pruned_change_view.rs': C, R, U and D, each buffer's sets, and
//!   its stub roots;
//! - 'buffer_rerender.rs': rendering and rerendering a whole buffer,
//!   each a refresh in diff-view mode ('server/refresh/diff_view.rs');
//! - 'changes.rs': what changed about a node, and the change viewnodes
//!   that say it;
//! - 'change_viewnodes.rs': the diff view's own viewnodes;
//! - 'change_leaves.rs': the change-forest leaves and their readiness;
//! - 'readiness_store.rs': the marks the user set;
//! - 'readiness_requests.rs': where the store meets a viewforest;
//! - 'restore_requests.rs': what a save writes for the two restore
//!   requests.

pub mod buffer_rerender;
pub mod change_leaves;
pub mod change_viewnodes;
pub mod changes;
pub mod pruned_change_view;
pub mod readiness_requests;
pub mod readiness_store;
pub mod restore_requests;
