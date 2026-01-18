/// PURPOSE: Build a string intended as buffer-text,
/// to represent an 'inital view' (content relationships only).
///
/// METHOD (very simple):
/// - render_initial_forest_bfs
/// - set_metadata_relationship_viewdata_in_forest
/// - render to string

use crate::viewdata::set_metadata_relationship_viewdata_in_forest;
use crate::org_to_text::orgnode_forest_to_string;
use crate::to_org::render::initial_bfs::render_initial_forest_bfs;
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::OrgNode;
use crate::types::skgnode::SkgNodeMap;

use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;


/// See file header comment.
pub async fn single_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  root_id : &ID,
) -> Result < String, Box<dyn Error> > {
  multi_root_view (
    driver,
    config,
    & [ root_id . clone () ] ) . await }

/// See file header comment.
pub async fn multi_root_view (
  driver   : &TypeDBDriver,
  config   : &SkgConfig,
  root_ids : &[ID],
) -> Result < String, Box<dyn Error> > {
  let (mut forest, _map) : (Tree<OrgNode>, SkgNodeMap) =
    render_initial_forest_bfs (
      root_ids, config, driver ) . await ?;
  set_metadata_relationship_viewdata_in_forest (
    &mut forest, config, driver ) . await ?;
  let buffer_content : String =
    orgnode_forest_to_string ( & forest ) ?;
  Ok ( buffer_content ) }
