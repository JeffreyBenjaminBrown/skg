// DON'T IMPORT these re-exports. They are for documentation.
// INSTEAD, imports in the codebase should use the original,
// longer definition path. That makes it easier to find definitions.

// OUTSIDE ENTRY POINTS:
// The only (so far) functions defined here used outside of it are:
// - single_root_view
// - completeAndRestoreForest_collectingDefinitiveRequests
// - execute_definitive_view_requests

pub mod complete;
pub mod expand;
pub mod render;
pub mod util;

pub use complete::{
  completeDefinitiveOrgnode,
  clobberIndefinitiveOrgnode,
  ensure_skgnode,
  completeAndRestoreForest_collectingDefinitiveRequests,
};

pub use expand::{
  build_and_integrate_aliases,
  wrapped_build_and_integrate_aliases_view,
  build_and_integrate_containerward_path,
  build_and_integrate_sourceward_path,
  integrate_path_that_might_fork_or_cycle,
};

pub use render::{
  multi_root_view,
  single_root_view,
  render_initial_forest_bfs,
};

pub use util::{
  skgnode_and_orgnode_from_id,
  skgnode_and_orgnode_from_pid_and_source,
};
