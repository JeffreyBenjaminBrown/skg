/// PURPOSE:
/// Build a string intended as buffer-text,
/// to represent an 'inital view' (content relationships only).
///
/// METHOD (phase 8 §13): the de-novo render now runs the ONE post-save driver
/// (multi_root_view_via_env -> render_initial_view_via_driver) over a stub
/// forest, then:
/// - if diff_mode_enabled, apply diff markers
/// - prepend each view-root's containerward ancestry (if any)
/// - set_graphnodestats_in_viewforest
/// - set_viewnodestats_in_viewforest
/// - render to string

use crate::to_org::expand::backpath::attach_containerward_ancestries_at_nodeids_with_source_set;
use crate::org_to_text::viewforest_to_string;
use crate::to_org::util::mark_view_roots_parent_absent;
use crate::types::tree::forest::ViewForest;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::types::viewnode::Vognode;
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::dbs::in_rust_graph::{InRustGraph, new_handle};
use crate::dbs::init::empty_in_ram_tantivy_index;
use std::sync::Arc;
use crate::update_buffer::graphnodestats::{
  set_graphnodestats_in_viewforest,
  set_graphnodestats_in_viewforest_with_source_set};
use crate::update_buffer::viewnodestats::set_viewnodestats_in_viewforest;

use ego_tree::{NodeId, Tree};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// See file header comment.
pub async fn single_root_view (
  driver            : &Arc<TypeDBDriver>,
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
  driver            : &Arc<TypeDBDriver>,
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  multi_root_view_inner (
    driver, config, tantivy_index, root_ids,
    diff_mode_enabled, None ) . await
}

/// Phase 8 (§13): the legacy parts-based de-novo entry is now a thin shim that
/// assembles a SkgEnv and routes through the ONE driver (multi_root_view_via_env).
/// Used only by tests now (production calls multi_root_view_via_env directly).
/// The env's in-Rust graph is fresh+empty -- de-novo callers/tests don't merge,
/// so there are no extra_ids to resolve, and content is fetched from the
/// in-Rust-graph *global* / disk -- and tantivy, when not supplied, is an empty
/// in-RAM index (find_source falls back past it to the graph/disk).
async fn multi_root_view_inner (
  driver            : &Arc<TypeDBDriver>,
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  let tantivy_owned : TantivyIndex = match tantivy_index {
    Some (t) => t . clone (),
    None     => empty_in_ram_tantivy_index () ?, };
  // Build the in-Rust graph from the source .skg files so the driver's content
  // reconcile can resolve extra_ids (graph_snap.pid_of) -- a node's contains may
  // reference another node by an extra_id, and the old de-novo BFS resolved
  // those via TypeDB. Production's env carries the real (global) graph already;
  // this shim is test-only, so a per-call file read is fine.
  let nodes : Vec<crate::types::nodes::complete::NodeComplete> =
    crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources (config)
    . unwrap_or_default ();
  let env : SkgEnv = SkgEnv {
    config        : config . clone (),
    in_rust_graph : new_handle ( InRustGraph::from_nodecompletes (&nodes) ),
    tantivy_index : tantivy_owned,
    driver        : Arc::clone (driver), };
  multi_root_view_via_env (
    &env, root_ids, diff_mode_enabled, active_source_set ) . await }

/// Phase 8 (§13): the de-novo view built through the ONE driver
/// (render_initial_view_via_driver) instead of render_initial_viewforest_bfs.
/// Same post-steps as multi_root_view_inner (diff overlay, containerward, stats,
/// render). Takes a SkgEnv (carries config/driver/tantivy/graph). The legacy
/// multi_root_view* shims still use the old BFS until the unit tests migrate.
pub async fn multi_root_view_via_env (
  env               : &SkgEnv,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  // §9 reversal (#3): the diff (when diff_mode_enabled) is now computed INLINE
  // by the driver, per Normal node at its BFS visit -- no post-BFS overlay.
  let mut viewforest : ViewForest =
    crate::update_buffer::render_initial_view_via_driver (
      env, root_ids, active_source_set, diff_mode_enabled ) . await ?;
  attach_containerward_ancestries_to_view_roots (
    &mut viewforest, &env . config, &env . driver, active_source_set ) . await ?;
  let ( container_to_contents, content_to_containers ) =
    match active_source_set {
      Some (active) =>
        set_graphnodestats_in_viewforest_with_source_set (
          &mut viewforest, &env . config, &env . driver, active ) . await,
      None =>
        set_graphnodestats_in_viewforest (
          &mut viewforest, &env . config, &env . driver ) . await,
    } ?;
  mark_view_roots_parent_absent ( &mut viewforest );
  set_viewnodestats_in_viewforest (
    &mut viewforest, &container_to_contents, &content_to_containers,
    &env . config );
  let pids : Vec<ID> = {
    let mut ids : Vec<ID> = Vec::new ();
    for node_ref in viewforest . nodes () {
      match &node_ref . value () . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t))
          => ids . push ( t . id . clone () ),
        ViewNodeKind::Vognode (Vognode::DiffPhantom (p))
          => ids . push ( p . id . clone () ),
        _ => {} }}
    ids };
  // Match the old multi_root_view_with_source_set, which finished with
  // render_viewforest_with_source_set (= apply_source_set_to_viewforest + render).
  // A no-op when every source is active; under a restricted set it annotates/
  // filters, so skipping it would drop source-set rendering for the handler.
  if let Some (active) = active_source_set {
    crate::source_sets::apply_source_set_to_viewforest (&mut viewforest, active); }
  let buffer_content : String =
    viewforest_to_string (& viewforest, &env . config) ?;
  Ok ((buffer_content, pids, viewforest . into_internal_tree ())) }

pub async fn multi_root_view_with_source_set (
  driver            : &Arc<TypeDBDriver>,
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
  active_source_set : &ActiveSourceSet,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  // multi_root_view_inner now applies the source set during rendering (via
  // multi_root_view_via_env), so the former extra render_viewforest_with_source_set
  // pass is redundant.
  multi_root_view_inner (
    driver, config, tantivy_index, root_ids,
    diff_mode_enabled, Some (active_source_set) ) . await }

/// For each view root in the forest, if the graph says it has
/// containers, prepend its full containerward ancestry as that root's
/// first children.
async fn attach_containerward_ancestries_to_view_roots (
  viewforest : &mut ViewForest,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
  active     : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let view_root_nodeids : Vec<NodeId> =
    viewforest . root_ids ();
  attach_containerward_ancestries_at_nodeids_with_source_set (
    viewforest, &view_root_nodeids, config, driver, active ) . await }
