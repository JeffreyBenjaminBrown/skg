/// PURPOSE:
/// Build a string intended as buffer-text,
/// to represent an 'inital view' (content relationships only).
///
/// METHOD (phase 8 §13): the de-novo render runs the ONE post-save driver
/// (multi_root_view_via_env -> render_initial_view_via_driver) over a stub
/// forest. The driver completes each node at its BFS visit -- including, when
/// diff_mode_enabled, that node's git diff inline (§9 reversal / #3). After the
/// driver, the shared finish_viewforest tail (server/update_buffer.rs, §20.3)
/// attaches containerward ancestry (roots + removed-here phantoms),
/// marks/validates parentIs, sets graph/view stats, applies the source set,
/// and renders to string.

use crate::types::tree::forest::ViewForest;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::viewnode::ViewNode;
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::dbs::in_rust_graph::{InRustGraph, new_handle};
use crate::dbs::init::empty_in_ram_tantivy_index;
use std::sync::Arc;

use ego_tree::Tree;
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

/// Phase 8 (§13): the de-novo view, built through the ONE driver
/// (render_initial_view_via_driver). Takes a SkgEnv (carries
/// config/driver/tantivy/graph), runs the driver over a stub forest of the
/// requested roots, then attaches containerward ancestry and stats and renders.
pub async fn multi_root_view_via_env (
  env               : &SkgEnv,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  // §9 reversal (#3): the diff (when diff_mode_enabled) is computed inline by
  // the driver, per Normal node at its BFS visit.
  let mut viewforest : ViewForest =
    crate::update_buffer::render_initial_view_via_driver (
      env, root_ids, active_source_set, diff_mode_enabled ) . await ?;
  // §20.3: the de-novo and post-save render tails are one shared helper now --
  // attach containerward ancestry (roots + removed-here phantoms), mark/validate
  // parentIs, graph/view stats, apply source set, render to string.
  let buffer_content : String =
    crate::update_buffer::finish_viewforest (
      &mut viewforest, &env . config, &env . driver,
      active_source_set ) . await ?;
  // §20.5: the pids the caller registers for this view -- the {Normal, Inactive}
  // set, via the one shared source of which-kinds-count
  // (OpenViews::pids_from_viewforest), the same helper update_view uses post-save.
  let pids : Vec<ID> =
    crate::types::views_state::pids_from_viewforest (&viewforest)
      . into_iter () . collect ();
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

