use crate::media::tree::map_snd_over_forest;
use crate::to_org::render::initial_bfs::render_initial_forest_bfs;
use crate::to_org::viewdata::set_metadata_relationship_viewdata_in_forest;
use crate::types::{ID, SkgConfig, OrgNode};
use crate::types::trees::PairTree;
use crate::to_org::text::orgnode_to_text;

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
    render_forest_to_org ( & forest );
  Ok ( buffer_content ) }

/// Render a forest of OrgNode trees to org-mode text.
/// Each tree's root starts at level 1.
/// Assumes metadata has already been enriched with relationship data.
pub fn render_forest_to_org (
  forest : &[Tree < OrgNode >],
) -> String {
  fn render_node_subtree_to_org (
    node_ref : ego_tree::NodeRef < OrgNode >,
    level    : usize,
  ) -> String {
    let node : &OrgNode = node_ref . value ();
    let mut out : String =
      orgnode_to_text ( level, node );
    for child in node_ref . children () {
      out . push_str (
        & render_node_subtree_to_org (
          child,
          level + 1 )); }
    out }
  let mut result : String =
    String::new ();
  for tree in forest {
    result . push_str (
      & render_node_subtree_to_org (
        tree . root (),
        1 )); }
  result }
