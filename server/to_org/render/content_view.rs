/// PURPOSE:
/// Build a string intended as buffer-text,
/// to represent an initial view: the roots' content, their
/// containers folders and their default folders.
///
/// METHOD: the de-novo render (multi_root_view_via_env ->
/// render_initial_view) is the refresh of a stub forest of the
/// requested roots ('refresh_viewforest'), each asking for its
/// containers folder, which is drawn with the containerward chain.

use crate::types::tree::forest::ViewForest;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::Viewnode;
use crate::types::views_state::pids_from_viewforest;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::SkgEnv;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::init::empty_in_ram_tantivy_index;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use crate::update_buffer::render_initial_view;
use std::sync::Arc;

use ego_tree::Tree;
use std::error::Error;

/// See file header comment.
pub fn single_root_view (
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_gnid        : &ID,
) -> Result < (String, Vec<ID>, Tree<Viewnode>),
              Box<dyn Error> > {
  multi_root_view (
    config,
    tantivy_index,
    & [ root_gnid . clone () ] ) }

/// See file header comment.
pub fn multi_root_view (
  config            : &SkgConfig,
  tantivy_index     : Option<&TantivyIndex>,
  root_gnids       : &[ID],
) -> Result < (String, Vec<ID>, Tree<Viewnode>),
              Box<dyn Error> > {
  multi_root_view_inner (
    config, tantivy_index, root_gnids, None )
}

/// The parts-based de-novo entry -- a thin shim that assembles a
/// SkgEnv and routes through the de-novo render (multi_root_view_via_env). Used only
/// by tests (production calls multi_root_view_via_env or
/// multi_root_view_via_runtime directly).
/// The shim builds a fixture-local graph from disk and, when Tantivy is not
/// supplied, uses an empty in-RAM search index.
fn multi_root_view_inner (
  config             : &SkgConfig,
  tantivy_index      : Option<&TantivyIndex>,
  root_gnids        : &[ID],
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < (String, Vec<ID>, Tree<Viewnode>),
              Box<dyn Error> > {
  let tantivy_owned : TantivyIndex = match tantivy_index {
    Some (t) => t . clone (),
    None     => empty_in_ram_tantivy_index () ?, };
  // Build the in-Rust graph from the skgrepo .skg files so the refresh's
  // content goal can resolve extra_ids (graph_snap.pid_of) -- a node's contains may
  // reference another node by an extra_id. Production's env carries the real
  // graph already; this shim is test-only, so a per-call file read is
  // fine.
  let nodes : Vec<Graphnode> =
    read_all_skg_files_from_skgrepos (config) ?;
  let env : SkgEnv = SkgEnv::new (
    config . clone (),
    Arc::new (InRustGraph::from_graphnodes (&nodes)),
    tantivy_owned);
  multi_root_view_via_env (
    &env, root_gnids, skgrepo_restriction ) }

/// The de-novo view (render_initial_view). Takes a SkgEnv (carries
/// the shared runtime: config, graph and Tantivy index) and refreshes a stub forest of the
/// requested roots.
pub fn multi_root_view_via_env (
  env                : &SkgEnv,
  root_gnids        : &[ID],
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < (String, Vec<ID>, Tree<Viewnode>),
              Box<dyn Error> > {
  let runtime = env . runtime_snapshot ();
  multi_root_view_via_runtime (
    &runtime, root_gnids, skgrepo_restriction ) }

pub(crate) fn multi_root_view_via_runtime (
  runtime            : &crate::types::env::RuntimeGeneration,
  root_gnids        : &[ID],
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < (String, Vec<ID>, Tree<Viewnode>), Box<dyn Error> > {
  let (viewforest, buffer_content) : (ViewForest, String) =
    render_initial_view ( runtime, root_gnids, skgrepo_restriction ) ?;
  // The pids the caller registers for this view -- its occurrences,
  // via the one shared skgrepo of which-kinds-count
  // (views_state::pids_from_viewforest), the same helper update_view uses post-save.
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest)
      . into_iter () . collect ();
  Ok ((buffer_content, pids, viewforest . into_internal_tree ())) }

pub fn multi_root_view_with_skgrepo_set (
  config             : &SkgConfig,
  tantivy_index      : Option<&TantivyIndex>,
  root_gnids        : &[ID],
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result < (String, Vec<ID>, Tree<Viewnode>),
              Box<dyn Error> > {
  // multi_root_view_inner applies the skgrepo set during rendering (inside
  // multi_root_view_via_env), so this wrapper just forwards it.
  multi_root_view_inner (
    config, tantivy_index, root_gnids,
    Some (skgrepo_restriction) ) }
