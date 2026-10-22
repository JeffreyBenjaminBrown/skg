//! The diff view (C-c v d): up to four buffers, each drawn over the
//! graph straddle ('server/graph_straddle/'). 'docs/diff-view.org'
//! describes it; this module implements it:
//! - 'pruned_change_view.rs': C, R, U and D, each buffer's sets, and
//!   its stub roots;
//! - 'completion.rs': drawing and rerendering a whole buffer;
//! - 'visit.rs': where view completion reads the diff-view switch,
//!   before a node's visit extends and after;
//! - 'changes.rs': the per-visit step that view completion runs at
//!   each unrestricted vognode and historical phantom;
//! - 'change_viewnodes.rs': the diff view's own viewnodes;
//! - 'shallow_model_subtree.rs': reconciling what a viewnode holds with a
//!   shallow model subtree of it;
//! - 'partner_folders.rs': the partner folders drawn over the straddle;
//! - 'change_leaves.rs': the change-forest leaves and their readiness;
//! - 'readiness_store.rs': the marks the user set;
//! - 'readiness_requests.rs': where the store meets a viewforest.

pub mod change_leaves;
pub mod change_viewnodes;
pub mod changes;
pub mod completion;
pub mod shallow_model_subtree;
pub mod partner_folders;
pub mod pruned_change_view;
pub mod readiness_requests;
pub mod readiness_store;
pub mod visit;
