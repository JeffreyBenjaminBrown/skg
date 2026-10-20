//! The graph straddle: the before-graph, read from each owned skgrepo's
//! HEAD, and the after-graph, the live graph. The union walk answers
//! questions about the union of the two. The total diff view is built
//! on it ('TODO/total-diff-view/02_plan.org', "graph pair").

pub mod build;
pub mod types;
pub mod union_walk;
