// DON'T IMPORT these re-exports. They are for documentation.
// INSTEAD, imports in the codebase should use the original,
// longer definition path. That makes it easier to find definitions.

pub mod complete;
pub mod expand;
pub mod render;
pub mod text;
pub mod util;
pub mod viewdata;

pub use complete::{
  completeDefinitiveOrgnode,
  clobberIndefinitiveOrgnode,
  make_indefinitive_if_repeated,
  completeOrgnodeForest_collectingDefinitiveRequests,
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

pub use text::{
  orgnode_to_text,
};

pub use util::{
  newline_to_space,
  org_bullet,
  skgnode_and_orgnode_from_id,
  skgnode_and_orgnode_from_pid_and_source,
  content_ids_from_skgnode,
};
