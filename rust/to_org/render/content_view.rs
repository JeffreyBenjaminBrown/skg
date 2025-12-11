use crate::compute_viewdata::set_metadata_relationship_viewdata_in_forest;
use crate::media::tree::map_snd_over_forest;
use crate::org_to_text::orgnode_forest_to_string;
use crate::to_org::render::initial_bfs::render_initial_forest_bfs;
use crate::types::{ID, SkgConfig, OrgNode};
use crate::types::trees::PairTree;

use std::error::Error;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;

/// Build a tree from a root ID and render it to org text.
pub async fn single_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  root_id : &ID,
) -> Result < String, Box<dyn Error> > {
  multi_root_view (
    driver,
    config,
    & [ root_id . clone () ] ) . await }

/// Build a forest from multiple root IDs and render it to org text.
pub async fn multi_root_view (
  driver   : &TypeDBDriver,
  config   : &SkgConfig,
  root_ids : &[ID],
) -> Result < String, Box<dyn Error> > {
  let paired_forest : Vec < PairTree > =
    render_initial_forest_bfs (
      root_ids, config, driver ) . await ?;
  let mut forest : Vec < Tree < OrgNode > > =
    map_snd_over_forest ( paired_forest );
  set_metadata_relationship_viewdata_in_forest (
    &mut forest, config, driver ) . await ?;
  let buffer_content : String =
    orgnode_forest_to_string ( & forest );
  Ok ( buffer_content ) }
