/// PURPOSE:
/// Build a string intended as buffer-text,
/// to represent an 'inital view' (content relationships only).
///
/// METHOD (phase 8 TODO/DONE/local-view-update/plan_v2.org §13): the de-novo render runs the ONE post-save view
/// completion path (multi_root_view_via_env -> render_initial_view)
/// over a stub forest. View completion (complete_viewforest) completes each node
/// at its BFS visit -- including, when diff_mode_enabled, that node's git diff
/// inline (TODO/DONE/local-view-update/plan_v2.org §9 reversal / #3). After view completion, the shared finish_viewforest
/// tail (server/update_buffer.rs, TODO/DONE/local-view-update/plan_v2.org §20.3)
/// attaches containerward ancestry (roots + removed-here phantoms),
/// marks/validates parentIs, sets graph/view stats, applies the source set,
/// and renders to string.

use crate::types::tree::forest::ViewForest;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::ViewNode;
use crate::types::views_state::pids_from_viewforest;
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::init::empty_in_ram_tantivy_index;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::update_buffer::{finish_viewforest, render_initial_view};
use std::sync::Arc;

use ego_tree::Tree;
use std::error::Error;

/// See file header comment.
pub fn single_root_view (
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_id           : &ID,
  diff_mode_enabled : bool,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  multi_root_view (
    config,
    tantivy_index,
    & [ root_id . clone () ],
    diff_mode_enabled ) }

/// See file header comment.
pub fn multi_root_view (
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  multi_root_view_inner (
    config, tantivy_index, root_ids,
    diff_mode_enabled, None )
}

/// Phase 8 (TODO/DONE/local-view-update/plan_v2.org §13): the parts-based de-novo entry -- a thin shim that assembles a
/// SkgEnv and routes through the ONE view completion path (multi_root_view_via_env). Used only
/// by tests (production calls multi_root_view_via_env directly).
/// The shim builds a fixture-local graph from disk and, when Tantivy is not
/// supplied, uses an empty in-RAM index.
fn multi_root_view_inner (
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
  // Build the in-Rust graph from the source .skg files so view completion's content
  // reconcile can resolve extra_ids (graph_snap.pid_of) -- a node's contains may
  // reference another node by an extra_id. Production's env carries the real
  // graph already; this shim is test-only, so a per-call file read is
  // fine.
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (config) ?;
  let env : SkgEnv = SkgEnv::new (
    config . clone (),
    Arc::new (InRustGraph::from_nodecompletes (&nodes)),
    tantivy_owned);
  multi_root_view_via_env (
    &env, root_ids, diff_mode_enabled, active_source_set,
    // The test shims discard render warnings; the production
    // caller (the single-root handler) surfaces them.
    &mut Vec::new () ) }

/// Phase 8 (TODO/DONE/local-view-update/plan_v2.org §13): the de-novo view, built through the ONE view completion path
/// (render_initial_view). Takes a SkgEnv (carries
/// config/driver/tantivy/graph), runs view completion over a stub forest of the
/// requested roots, then attaches containerward ancestry and stats and renders.
/// Warning strings the render produces (today only the
/// compound-override-chain notice) are appended to 'warnings_out'.
pub fn multi_root_view_via_env (
  env               : &SkgEnv,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
  active_source_set : Option<&ActiveSourceSet>,
  warnings_out      : &mut Vec<String>,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  let runtime = env . runtime_snapshot ();
  multi_root_view_via_runtime (
    &runtime, root_ids, diff_mode_enabled, active_source_set, warnings_out)

}

pub(crate) fn multi_root_view_via_runtime (
  runtime           : &crate::types::env::RuntimeGeneration,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
  active_source_set : Option<&ActiveSourceSet>,
  warnings_out      : &mut Vec<String>,
) -> Result < (String, Vec<ID>, Tree<ViewNode>), Box<dyn Error> > {
  // TODO/DONE/local-view-update/plan_v2.org §9 reversal (#3): the diff (when diff_mode_enabled) is computed inline by
  // view completion, per Active node at its BFS visit.
  let mut viewforest : ViewForest =
    { let (viewforest, render_warnings)
        : (ViewForest, Vec<String>) =
        render_initial_view (
          runtime, root_ids, active_source_set,
          diff_mode_enabled ) ?;
      warnings_out . extend (render_warnings);
      viewforest };
  // TODO/DONE/local-view-update/plan_v2.org §20.3: the de-novo and post-save render tails are one shared helper now.
  // Root containerward is requested per-root in render_initial_view
  // (de-novo only) and fulfilled + dropped inside finish_viewforest, so the tail
  // itself is caller-agnostic -- no mode flag.
  let buffer_content : String =
    finish_viewforest (
      &mut viewforest, &runtime . graph, &runtime . config,
      active_source_set ) ?;
  // TODO/DONE/local-view-update/plan_v2.org §20.5: the pids the caller registers for this view -- the {Normal, Inactive}
  // set, via the one shared source of which-kinds-count
  // (views_state::pids_from_viewforest), the same helper update_view uses post-save.
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest)
      . into_iter () . collect ();
  Ok ((buffer_content, pids, viewforest . into_internal_tree ())) }

pub fn multi_root_view_with_source_set (
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_ids          : &[ID],
  diff_mode_enabled : bool,
  active_source_set : &ActiveSourceSet,
) -> Result < (String, Vec<ID>, Tree<ViewNode>),
              Box<dyn Error> > {
  // multi_root_view_inner applies the source set during rendering (inside
  // multi_root_view_via_env), so this wrapper just forwards it.
  multi_root_view_inner (
    config, tantivy_index, root_ids,
    diff_mode_enabled, Some (active_source_set) ) }
