/// PURPOSE:
/// Build a string intended as buffer-text,
/// to represent an 'inital view' (content relationships only).
///
/// METHOD:
/// - render_initial_viewforest_bfs
/// - if diff_mode_enabled, apply diff markers
/// - prepend each view-root's containerward ancestry (if any)
/// - set_graphnodestats_in_viewforest
/// - set_viewnodestats_in_viewforest
/// - render to string

use crate::serve::handlers::save_buffer::{compute_diff_for_every_source, deleted_ids_to_source};
use crate::to_org::expand::backpath::attach_containerward_ancestries_at_nodeids;
use crate::types::git::SourceDiff;
use crate::org_to_text::viewforest_to_string;
use crate::to_org::render::diff::apply_diff_to_viewforest;
use crate::to_org::render::initial_bfs::render_initial_viewforest_bfs;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::update_buffer::graphnodestats::set_graphnodestats_in_viewforest;
use crate::update_buffer::viewnodestats::set_viewnodestats_in_viewforest;

use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// See file header comment.
pub async fn single_root_view (
  driver            : &TypeDBDriver,
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_id           : &ID,
  diff_mode_enabled : bool,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  multi_root_view (
    driver,
    config,
    tantivy_index,
    & [ root_id . clone () ],
    diff_mode_enabled ) . await }

/// See file header comment.
pub async fn multi_root_view (
  driver            : &TypeDBDriver,
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
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
      &deleted_since_head_pid_src_map,
      tantivy_index, config ) ?; }
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "attach_containerward_ancestries_to_view_roots" ). entered();
    attach_containerward_ancestries_to_view_roots (
      &mut viewforest, config, driver ) . await ?; }
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

/// For each view-root in the forest (= each TrueNode child of
/// BufferRoot), if the graph says it has containers, prepend its
/// full containerward ancestry as that root's first children.
async fn attach_containerward_ancestries_to_view_roots (
  viewforest : &mut Tree<ViewNode>,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let view_root_nodeids : Vec<NodeId> =
    viewforest . root () . children () . map ( |c| c . id () )
      . collect ();
  attach_containerward_ancestries_at_nodeids (
    viewforest, &view_root_nodeids, config, driver ) . await }
