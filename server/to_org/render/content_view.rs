/// PURPOSE: Build a string intended as buffer-text,
/// to represent an 'inital view' (content relationships only).
///
/// METHOD (very simple):
/// - render_initial_viewforest_bfs
/// - set_graphnodestats_in_viewforest
/// - if diff_mode_enabled, apply diff markers
/// - render to string

use crate::serve::handlers::save_buffer::{compute_diff_for_every_source, deleted_ids_to_source};
use crate::types::git::SourceDiff;
use crate::org_to_text::viewforest_to_string;
use crate::to_org::render::diff::apply_diff_to_viewforest;
use crate::to_org::render::initial_bfs::render_initial_viewforest_bfs;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::update_buffer::graphnodestats::set_graphnodestats_in_viewforest;
use crate::update_buffer::viewnodestats::set_viewnodestats_in_viewforest;

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
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
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
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  let mut viewforest : Tree<ViewNode> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "render_initial_viewforest_bfs" ). entered();
      render_initial_viewforest_bfs (
        root_ids, config, driver ) . await } ?;
  if diff_mode_enabled {
    let source_diffs : HashMap<SourceName, SourceDiff> =
      compute_diff_for_every_source (config);
    let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
      deleted_ids_to_source (&source_diffs);
    apply_diff_to_viewforest (
      &mut viewforest, &source_diffs,
      &deleted_since_head_pid_src_map, config ) ?; }
  let ( container_to_contents, content_to_containers ) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "set_graphnodestats_in_viewforest" ). entered();
      set_graphnodestats_in_viewforest (
        &mut viewforest,
        config, driver ) . await } ?;
  set_viewnodestats_in_viewforest (
    &mut viewforest, &container_to_contents, &content_to_containers,
    config );
  let pids : Vec<ID> = {
    // Collect PIDs from viewforest before rendering to string.
    let mut ids : Vec<ID> = Vec::new ();
    for node_ref in viewforest . root () . descendants () {
      if let ViewNodeKind::True (t) = &node_ref . value () . kind
        { ids . push ( t . id . clone () ); }}
    ids };
  let buffer_content : String =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "viewforest_to_string" ). entered();
      viewforest_to_string (& viewforest, config) } ?;
  Ok ((buffer_content, pids, viewforest)) }
