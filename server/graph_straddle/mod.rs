//! The graph straddle, the graph layer's comparison of two graphs:
//! the before-graph, read from each owned skgrepo's HEAD, and the
//! after-graph, the live graph. The diff view ('server/diff_view/') is
//! built on it ('docs/diff-view.org', "The graph straddle").
//! - 'types': 'GraphStraddle' and the sets it computes once.
//! - 'build': building it from HEAD and the live graph, and keeping a
//!   connection's copy up to date ('current_graph_straddle_in_slot').
//! - 'union_walk': questions about the union of the two graphs.

pub mod build;
pub mod types;
pub mod union_walk;
