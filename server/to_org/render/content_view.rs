/// PURPOSE: Build a string intended as buffer-text,
/// to represent an 'inital view' (content relationships only).
///
/// METHOD (very simple):
/// - render_initial_forest_bfs
/// - set_graphnodestats_in_forest
/// - if diff_mode_enabled, apply diff markers
/// - render to string

use crate::serve::handlers::save_buffer::{compute_diff_for_every_source, deleted_ids_to_source};
use crate::serve::timing_log::{timed, timed_async};
use crate::types::git::SourceDiff;
use crate::org_to_text::viewnode_forest_to_string;
use crate::to_org::render::diff::apply_diff_to_forest;
use crate::to_org::render::initial_bfs::render_initial_forest_bfs;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::types::skgnodemap::SkgNodeMap;
use crate::update_buffer::graphnodestats::set_graphnodestats_in_forest;
use crate::update_buffer::viewnodestats::set_viewnodestats_in_forest;

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
) -> Result < (String, SkgNodeMap, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
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
) -> Result < (String, SkgNodeMap, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  let (mut forest, mut map) : (Tree<ViewNode>, SkgNodeMap) =
    timed_async ( config, "render_initial_forest_bfs",
                  render_initial_forest_bfs (
                    root_ids, config, driver )) . await ?;
  let ( container_to_contents, content_to_containers ) =
    timed_async ( config, "set_graphnodestats_in_forest",
                  set_graphnodestats_in_forest (
                    &mut forest, &mut map,
                    config, driver )) . await ?;
  set_viewnodestats_in_forest (
    &mut forest, &container_to_contents, &content_to_containers );
  if diff_mode_enabled {
    let source_diffs : HashMap<SourceName, SourceDiff> =
      compute_diff_for_every_source ( config );
    let deleted_id_src_map : HashMap<ID, SourceName> =
      deleted_ids_to_source ( &source_diffs );
    apply_diff_to_forest (
      &mut forest, &source_diffs,
      &deleted_id_src_map, config ) ?; }
  let pids : Vec<ID> = {
    // Collect PIDs from forest before rendering to string.
    let mut ids : Vec<ID> = Vec::new ();
    for node_ref in forest . root () . descendants () {
      if let ViewNodeKind::True ( t ) = &node_ref . value () . kind
        { ids . push ( t . id . clone () ); }}
    ids };
  let buffer_content : String =
    timed ( config, "viewnode_forest_to_string",
            || viewnode_forest_to_string ( & forest )) ?;
  Ok ((buffer_content, map, pids, forest)) }
