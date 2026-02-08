/// PURPOSE: Build a string intended as buffer-text,
/// to represent an 'inital view' (content relationships only).
///
/// METHOD (very simple):
/// - render_initial_forest_bfs
/// - set_graphnodestats_in_forest
/// - if diff_mode_enabled, apply diff markers
/// - render to string

use crate::serve::handlers::save_buffer::compute_diff_for_every_source;
use crate::types::git::SourceDiff;
use crate::org_to_text::viewnode_forest_to_string;
use crate::to_org::render::diff::apply_diff_to_forest;
use crate::to_org::render::initial_bfs::render_initial_forest_bfs;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::ViewNode;
use crate::types::skgnodemap::SkgNodeMap;
use crate::viewdata::set_graphnodestats_in_forest;

use ego_tree::Tree;
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;


/// See file header comment.
pub async fn single_root_view (
  driver            : &TypeDBDriver,
  config            : &SkgConfig,
  root_id           : &ID,
  diff_mode_enabled : bool,
) -> Result < String, Box<dyn Error> > {
  multi_root_view (
    driver,
    config,
    & [ root_id . clone () ],
    diff_mode_enabled ) . await }

/// See file header comment.
pub async fn multi_root_view (
  driver            : &TypeDBDriver,
  config            : &SkgConfig,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
) -> Result < String, Box<dyn Error> > {
  let (mut forest, _map) : (Tree<ViewNode>, SkgNodeMap) =
    render_initial_forest_bfs (
      root_ids, config, driver ) . await ?;
  set_graphnodestats_in_forest (
    &mut forest, config, driver ) . await ?;
  if diff_mode_enabled {
    let source_diffs : HashMap<SourceName, SourceDiff> =
      compute_diff_for_every_source ( config );
    apply_diff_to_forest ( &mut forest, &source_diffs, config ) ?; }
  let buffer_content : String =
    viewnode_forest_to_string ( & forest ) ?;
  Ok ( buffer_content ) }
