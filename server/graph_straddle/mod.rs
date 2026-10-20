//! The graph straddle: the before-graph, read from each owned skgrepo's
//! HEAD, and the after-graph, the live graph. The union walk answers
//! questions about the union of the two. The diff view is built
//! on it ('docs/diff-view.org', "The graph straddle").

pub mod build;
pub mod types;
pub mod union_walk;
