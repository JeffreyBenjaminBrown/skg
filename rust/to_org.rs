// DON'T IMPORT these re-exports. They are for documentation.
// INSTEAD, imports in the codebase should use the original,
// longer definition path. That makes it easier to find definitions.

pub mod aliases;
pub mod complete_aliascol;
pub mod complete_contents;
pub mod content_view;
pub mod initial_view_bfs;
pub mod integrate_backpath;
pub mod orgnode;
pub mod util;

pub use aliases::{
  build_and_integrate_aliases,
  wrapped_build_and_integrate_aliases_view, };

pub use complete_aliascol::completeAliasCol;

pub use complete_contents::{
  completeDefinitiveOrgnode,
  clobberIndefinitiveOrgnode,
  make_indefinitive_if_repeated,
  completeOrgnodeForest, };

pub use content_view::{
  multi_root_view,
  single_root_view,
};

pub use initial_view_bfs::{
  render_initial_forest_bfs,
};

pub use integrate_backpath::{
  build_and_integrate_containerward_path,
  build_and_integrate_sourceward_path,
  integrate_path_that_might_fork_or_cycle };

pub use orgnode::{
  orgnode_to_text,
};

pub use util::{
  newline_to_space,
  org_bullet,
  skgnode_and_orgnode_from_id,
  skgnode_and_orgnode_from_pid_and_source,
};
