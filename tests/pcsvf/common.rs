// Fixtures for the PCSVF tests: one gitrepo per owned skgrepo of a
// case, at HEAD '<case>/<skgrepo>/head' with
// '<case>/<skgrepo>/worktree' in the worktree.

#[path = "../shared/git_fixture_helpers.rs"]
#[allow(unused_imports, dead_code)] // shared with the git diff view tests, which use more of it
pub mod git_fixture_helpers;

use git_fixture_helpers::{mk_test_tcp_stream_pair, setup_gitrepo_with_fixtures};

use skg::change_forest::LeafKey;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::init::create_empty_tantivy_index;
use skg::graph_pair::build::current_graph_pair_in_slot;
use skg::graph_pair::types::GraphPair;
use skg::readiness_store::ReadinessStore;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::serve::handlers::total_diff_view::{
  TotalDiffViewBuffer, total_diff_view_buffer};
use skg::test_utils::{
  cleanup_test_tantivy, graph_handle_from_config, skg_env_from_parts,
  update_from_and_rerender_buffer_test};
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::misc::{ID, SkgConfig, Skgrepo, SkgrepoName, TantivyIndex};
use skg::types::views_state::{OpenViews, TdvBuffer, ViewId};

use futures::executor::block_on;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::error::Error;
use std::fs;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::sync::{Arc, MutexGuard};
use tempfile::TempDir;

const FIXTURES : &str = "tests/pcsvf/fixtures";

pub struct Fixture {
  pub root        : TempDir,
  pub skgrepos    : Vec<String>,
  pub config      : SkgConfig,
  pub tantivy     : TantivyIndex,
  pub graph       : InRustGraphHandle,
  pub views_state : ViewsState,
}

/// The case's gitrepos, one owned skgrepo each, in privacy order.
pub fn fixture (
  case     : &str,
  skgrepos : &[&str],
) -> Result<Fixture, Box<dyn Error>> {
  let root : TempDir = TempDir::new () ?;
  let mut skgrepo_map : HashMap<SkgrepoName, Skgrepo> = HashMap::new ();
  for name in skgrepos {
    let fixtures : PathBuf = Path::new (FIXTURES) . join (case) . join (name);
    setup_gitrepo_with_fixtures (
      &root . path () . join (name),
      fixtures . join ("head") . to_str () . unwrap (),
      fixtures . join ("worktree") . to_str () . unwrap () ) ?;
    skgrepo_map . insert ( SkgrepoName::from (*name), Skgrepo {
      name : SkgrepoName::from (*name), abbreviation : None,
      path : root . path () . join (name), owned : true } ); }
  let mut config : SkgConfig = SkgConfig::fromSkgreposAndTantivyFolder (
    skgrepo_map, root . path () . join ("tantivy") . to_str () . unwrap () );
  config . skgrepo_order =
    skgrepos . iter () . map ( |name| SkgrepoName::from (*name) ) . collect ();
  config . data_root = root . path () . to_path_buf ();
  let tantivy : TantivyIndex = create_empty_tantivy_index (&config . tantivy_folder) ?;
  let graph : InRustGraphHandle = graph_handle_from_config (&config) ?;
  Ok ( Fixture {
    root,
    skgrepos : skgrepos . iter () . map ( |name| name . to_string () ) . collect (),
    config, tantivy, graph,
    views_state : ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),
      graph_pair        : None, } } ) }

/// Run 'body' on the fixture, then remove its search index.
pub fn with_fixture (
  case     : &str,
  skgrepos : &[&str],
  body     : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture (case, skgrepos) ?;
  let result : Result<(), Box<dyn Error>> = body (&mut f);
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }

pub fn graph_pair (
  f : &Fixture,
) -> Result<GraphPair, Box<dyn Error>> {
  Ok ( GraphPair::from_head_and_live (&f . config, f . graph . load_full ()) ? ) }

/// The text of the "all changes" buffer, with no marks; None when it
/// would be empty.
pub fn all_changes_buffer (
  f : &Fixture,
) -> Result<Option<String>, Box<dyn Error>> {
  all_changes_buffer_with_marks (f, &[]) }

/// The same, with the leaves 'marked' marked ready at their current
/// fingerprints.
pub fn all_changes_buffer_with_marks (
  f      : &Fixture,
  marked : &[LeafKey],
) -> Result<Option<String>, Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let pair : GraphPair = graph_pair (f) ?;
  let mut store : ReadinessStore = ReadinessStore::default ();
  for key in marked {
    store . mark ( key . clone (),
                   pair . change_forest () . leaf_fingerprint (key)
                   . expect ("a marked key is a leaf") ); }
  Ok ( match total_diff_view_buffer (
    &runtime, &pair, &store, TdvBuffer::AllChanges ) ? {
    TotalDiffViewBuffer::Empty => None,
    TotalDiffViewBuffer::Content { text, .. } => Some (text), } ) }

/// The view id under which 'open_all_changes_buffer' registers the
/// buffer, and 'save' saves it.
pub const ALL_CHANGES_VIEW_ID : &str = "diff-view: all changes";

/// Open the "all changes" buffer as the "total diff view" request
/// does: drawn from the connection's graph pair with the marks on
/// disk, and registered as a view of the total diff view.
pub fn open_all_changes_buffer (
  f : &mut Fixture,
) -> Result<String, Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let pair : Arc<GraphPair> = current_graph_pair_in_slot (
    &mut f . views_state . graph_pair, &f . config, runtime . graph . clone () ) ?;
  let buffer : TotalDiffViewBuffer = {
    let store : MutexGuard<ReadinessStore> =
      env . runtime . readiness_store . lock () . unwrap ();
    total_diff_view_buffer (&runtime, &pair, &store, TdvBuffer::AllChanges) ? };
  match buffer {
    TotalDiffViewBuffer::Empty =>
      Err ( "the all-changes buffer is empty" . into () ),
    TotalDiffViewBuffer::Content { text, pids, viewforest } => {
      f . views_state . open_views . register_tdv_view (
        &runtime . graph,
        ViewId::ContentView ( ALL_CHANGES_VIEW_ID . to_string () ),
        viewforest, &pids, TdvBuffer::AllChanges );
      Ok (text) }, } }

pub fn save (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  Ok ( block_on ( update_from_and_rerender_buffer_test (
    &mut stream, buffer, &f . config, &f . tantivy, &f . graph, false,
    &Ok ( ViewId::ContentView ( ALL_CHANGES_VIEW_ID . to_string () )),
    &mut f . views_state )) ? ) }

/// Every '.skg' file of the fixture's skgrepos, by path, with its
/// contents.
pub fn skg_files (
  f : &Fixture,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for skgrepo in &f . skgrepos {
    for entry in fs::read_dir ( f . root . path () . join (skgrepo) ) ? {
      let path : PathBuf = entry ? . path ();
      if path . extension () . is_some_and ( |ext| ext == "skg" ) {
        files . insert (
          format! ( "{}/{}", skgrepo,
                    path . file_name () . unwrap () . to_string_lossy () ),
          fs::read_to_string (&path) ? ); }}}
  Ok (files) }

pub fn ids (
  list : &[&str],
) -> BTreeSet<ID> {
  list . iter () . map ( |s| ID::from (*s) ) . collect () }
