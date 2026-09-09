mod guard;
pub use guard::TestFixtureGuard;

use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_with_manifest;
use crate::dbs::filesystem::not_nodes::load_config_with_overrides;
use crate::dbs::init::create_empty_tantivy_index;
use crate::dbs::in_rust_graph::{
  InRustGraph, InRustGraphHandle, new_handle, new_handle_with_manifest};
use crate::dbs::tantivy::search::{SearchOptions, search_index};
use crate::types::env::SkgEnv;
use crate::from_text::buffer_to_viewnodes::uninterpreted::{headline_to_triple, HeadlineInfo};
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::{SaveResponse, update_from_and_rerender_buffer};
use crate::serve::parse_metadata_sexp::ViewnodeMetadata;
use crate::types::views_state::ViewUri;
use crate::types::misc::{MSV, SkgConfig, SkgfileSource, ID, TantivyIndex, SourceName, members_at_source, members_at_source_msv, MemberAtSource};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::nodes::complete::NodeComplete;
use crate::types::maybe_placed_viewnode::{ MpViewnode, MpViewnodeKind };
use crate::types::maybe_placed_viewnode::{MpVognode, MpPhantom};

use ego_tree::{Tree, NodeRef};
use futures::FutureExt;
use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::future::Future;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;
use tantivy::{DocAddress, Searcher, TantivyDocument};
use tantivy::schema::document::Value;


/// Copy a fixture corpus to a disposable directory, load its explicit graph,
/// create a disposable Tantivy index, and run one test future.
pub fn run_with_test_graph<F>(
  fixture_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a InRustGraphHandle, &'a mut TantivyIndex)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  let _groups_lock : std::sync::MutexGuard<()> =
    TEST_FIXTURE_MUTEX . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () );
  let temp_fixtures : PathBuf =
    // Copy fixtures to temp so saves don't corrupt originals
    PathBuf::from(format!("/tmp/{}-fixtures", fixture_name));
  if temp_fixtures . exists() {
    fs::remove_dir_all (&temp_fixtures)?;
  }
  copy_dir_all(
    &PathBuf::from (fixtures_folder),
    &temp_fixtures)?;
  let result : Result<(), Box<dyn Error>> = block_on(async {
    let (config, graph, mut tantivy) = setup_test_graph_and_tantivy(
      temp_fixtures . to_str() . unwrap(), tantivy_folder)?;
    guarded_test_then_cleanup(
      Some(config . tantivy_folder . clone()),
      test_fn(&config, &graph, &mut tantivy),
    ). await
  } );
  if temp_fixtures . exists() { // more cleanup
    fs::remove_dir_all (&temp_fixtures)?; }
  result
}

/// Load a multi-source fixture config and its explicit graph. This form skips
/// Tantivy because its callers exercise graph and filesystem behavior only.
pub fn run_with_test_graph_from_config<F>(
  _fixture_name: &str,
  config_path: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a InRustGraphHandle)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  let _groups_lock : std::sync::MutexGuard<()> =
    TEST_FIXTURE_MUTEX . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () );
  block_on(async {
    let config: SkgConfig = load_config_with_overrides(config_path, &[])?;
    let graph: InRustGraphHandle = graph_handle_from_config (&config)?;
    guarded_test_then_cleanup(None, test_fn(&config, &graph)) . await
  } )
}

/// Tantivy's background writer is process-wide, so fixture tests in the same
/// libtest binary serialize index teardown.
static TEST_FIXTURE_MUTEX : std::sync::Mutex<()> =
  std::sync::Mutex::new (( ));

/// Filesystem, graph, and search state reused by sequential sub-tests.
pub struct SharedGraphSession {
  pub fixture_name : String,
  temp_fixtures  : PathBuf, // where reset() copies each sub-test's fixtures, so saves don't corrupt originals
  tantivy_folder : PathBuf,
  pub config     : SkgConfig,
  pub graph      : InRustGraphHandle,
  pub tantivy    : TantivyIndex,
}

impl SharedGraphSession {
  /// Restore a pristine state for the next sub-test: wipe all data,
  /// copy `fixtures_folder` to the temp dir, point a config at it,
  /// repopulate, fresh Tantivy index. If the fixtures contain a
  /// 'skgconfig.toml' (multi-source setups), that config is loaded
  /// from the temp copy; otherwise a single-source ("main") config
  /// is built around the copy.
  /// `subtest_name` is printed so a failing group identifies which
  /// sub-test died (libtest replays captured stdout on failure).
  pub async fn reset (
    &mut self,
    subtest_name    : &str,
    fixtures_folder : &str,
  ) -> Result<(), Box<dyn Error>> {
    self . reset_with_fixture_prep (
      subtest_name, fixtures_folder, |_| Ok (( )) ) . await }

  /// Like 'reset', but runs `prep` on the temp fixture copy BEFORE
  /// the config and graph are loaded -- for sub-tests whose fixtures
  /// need mutation that the graph must reflect (e.g. git-initializing
  /// a source and leaving a
  /// worktree-vs-HEAD diff).
  pub async fn reset_with_fixture_prep<P> (
    &mut self,
    subtest_name    : &str,
    fixtures_folder : &str,
    prep            : P,
  ) -> Result<(), Box<dyn Error>>
  where
    P : FnOnce (&Path) -> Result<(), Box<dyn Error>>,
  {
    println! ("-- sub-test: {}", subtest_name);
    if self . temp_fixtures . exists () {
      fs::remove_dir_all ( &self . temp_fixtures ) ?; }
    copy_dir_all (
      Path::new (fixtures_folder),
      &self . temp_fixtures ) ?;
    prep ( &self . temp_fixtures ) ?;
    self . config = {
      let copied_config : PathBuf =
        self . temp_fixtures . join ("skgconfig.toml");
      if copied_config . exists () {
        let mut config : SkgConfig = load_config_with_overrides (
          copied_config . to_str () . unwrap (), &[] ) ?;
        config . tantivy_folder = self . tantivy_folder . clone ();
        config
      } else {
        let mut sources : HashMap<SourceName, SkgfileSource> =
          HashMap::new ();
        sources . insert (
          SourceName::from ("main"),
          SkgfileSource {
            name         : SourceName::from ("main"),
            abbreviation : None,
            path         : self . temp_fixtures . clone (),
            user_owns_it : true, });
        SkgConfig::from_sources (
          sources, self . tantivy_folder . to_str () . unwrap () ) }};
    self . reload_graph_and_search () }

  /// Like 'reset', but for sub-tests that prepare their own source
  /// directory (e.g. a git repo in a TempDir): no fixture copy; the
  /// single-source ("main") config points at `source_path` directly.
  pub async fn reset_with_source_path (
    &mut self,
    subtest_name : &str,
    source_path  : &Path,
  ) -> Result<(), Box<dyn Error>> {
    println! ("-- sub-test: {}", subtest_name);
    self . config = {
      let mut sources : HashMap<SourceName, SkgfileSource> =
        HashMap::new ();
      sources . insert (
        SourceName::from ("main"),
        SkgfileSource {
          name         : SourceName::from ("main"),
          abbreviation : None,
          path         : source_path . to_path_buf (),
          user_owns_it : true, });
      SkgConfig::from_sources (
        sources, self . tantivy_folder . to_str () . unwrap () ) };
    self . reload_graph_and_search () }

  /// Like 'reset', but loads a (possibly multi-source) config from a
  /// TOML file, reading fixtures in place -- the same convention as
  /// 'run_with_test_db_from_config'.
  pub async fn reset_from_config (
    &mut self,
    subtest_name : &str,
    config_path  : &str,
  ) -> Result<(), Box<dyn Error>> {
    println! ("-- sub-test: {}", subtest_name);
    self . config = load_config_with_overrides (config_path, &[] ) ?;
    self . config . tantivy_folder =
      self . tantivy_folder . clone ();
    self . reload_graph_and_search () }

  fn reload_graph_and_search (
    &mut self,
  ) -> Result<(), Box<dyn Error>> {
    crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
    self . graph = graph_handle_from_config (&self . config)?;
    self . tantivy = create_empty_tantivy_index (
      &self . tantivy_folder ) ?;
    Ok (( )) }
}

/// Run a group of sub-tests against one explicitly owned graph fixture.
pub fn run_with_shared_test_graph<F> (
  fixture_name : &str,
  test_fn : F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a mut SharedGraphSession)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  let _groups_lock : std::sync::MutexGuard<()> =
    TEST_FIXTURE_MUTEX . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () );
  let temp_fixtures : PathBuf =
    PathBuf::from ( format! ("/tmp/{}-fixtures", fixture_name) );
  let tantivy_folder : PathBuf =
    PathBuf::from ( format! ("/tmp/tantivy-test-{}", fixture_name) );
  let result : Result<(), Box<dyn Error>> = block_on ( async {
    let mut session : SharedGraphSession = SharedGraphSession {
      fixture_name   : fixture_name . to_string (),
      temp_fixtures  : temp_fixtures . clone (),
      tantivy_folder : tantivy_folder . clone (),
      config         : // placeholder; every sub-test runs after a reset, which overwrites it
        SkgConfig::from_sources (
          HashMap::new (), tantivy_folder . to_str () . unwrap () ),
      graph          : new_handle (InRustGraph::new ()),
      tantivy        : create_empty_tantivy_index (&tantivy_folder) ?, };
    let mut guard : TestFixtureGuard = TestFixtureGuard::new (
      Some ( tantivy_folder . clone () ));
    let test_result : Result<Result<(), Box<dyn Error>>, _> =
      AssertUnwindSafe ( test_fn (&mut session) )
      . catch_unwind () . await;
    let cleanup_result : Result<(), Box<dyn Error>> =
      cleanup_test_tantivy (Some (&tantivy_folder));
    guard . disarm ();
    match test_result {
      Ok (inner) => { cleanup_result ?; inner },
      Err (panic_payload) => {
        if let Err (e) = cleanup_result {
          tracing::error! ("Cleanup error after test panic: {}", e); }
        std::panic::resume_unwind (panic_payload) }, }} );
  if temp_fixtures . exists () { // more cleanup
    fs::remove_dir_all (&temp_fixtures) ?; }
  result }

/// Run a test future with a cleanup guard, including panic cleanup.
/// Catches panics so cleanup runs even if the test fails.
async fn guarded_test_then_cleanup(
  tantivy_folder: Option<PathBuf>,
  test_future: Pin<Box<dyn Future<Output = Result
                                  <(), Box<dyn Error>>> + '_>>,
) -> Result<(), Box<dyn Error>> {
  let mut guard: TestFixtureGuard = TestFixtureGuard::new(
    tantivy_folder.clone());
  let test_result: Result<Result<(), Box<dyn Error>>, _> =
    AssertUnwindSafe(test_future)
    . catch_unwind() . await;
  // A save's Tantivy write now commits in the background and outlives
  // update_from_and_rerender_buffer; drain it before deleting the test
  // index out from under the worker.
  crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
  let cleanup_result: Result<(), Box<dyn Error>> =
    cleanup_test_tantivy(tantivy_folder . as_deref());
  guard . disarm();
  match test_result {
    Ok (inner) => { cleanup_result?; inner },
    Err (panic_payload) => {
      if let Err (e) = cleanup_result {
        tracing::error!("Cleanup error after test panic: {}", e); }
      std::panic::resume_unwind(panic_payload) }, }
}

/// Recursively copy a directory and its contents.
fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), Box<dyn Error>> {
  fs::create_dir_all (dst)?;
  for entry in fs::read_dir (src)? {
    let entry : fs::DirEntry = entry?;
    let ty : fs::FileType = entry . file_type()?;
    let src_path : PathBuf = entry . path();
    let dst_path : PathBuf = dst . join(entry . file_name());
    if ty . is_dir() {
      copy_dir_all(&src_path, &dst_path)?;
    } else {
      fs::copy(&src_path, &dst_path)?;
    }
  }
  Ok(())
}

/// Build a graph handle preloaded from the test config's fixture files.
pub fn graph_handle_from_config (
  config : &SkgConfig,
) -> Result<InRustGraphHandle, Box<dyn Error>> {
  let loaded = read_all_skg_files_with_manifest (config) ?;
  Ok ( new_handle_with_manifest (
    InRustGraph::from_nodecompletes (&loaded . nodes),
    loaded . manifest )) }

/// Bundle a test's explicit graph and search handles into an `SkgEnv`.
pub fn skg_env_from_parts (
  config        : &SkgConfig,
  tantivy_index : &TantivyIndex,
  graph         : &InRustGraphHandle,
) -> SkgEnv {
  SkgEnv {
    config        : config . clone (),
    in_rust_graph : graph . clone (),
    searcher: tantivy_index . reader . searcher (),
    tantivy_index : tantivy_index . clone (),
    startup_warnings : Arc::new (Vec::new ()), } }

/// Test shim around `update_from_and_rerender_buffer` that builds an `SkgEnv`.
///
/// PITFALL: a SkgEnv field swap inside the call (e.g. a save's
/// rebuild path replacing 'tantivy_index') will not propagate back
/// out to the test's '&mut TantivyIndex' since the env clones the
/// index. Production paths that swap the index are not exercised
/// by these tests, so this is fine in practice.
pub async fn update_from_and_rerender_buffer_test (
  stream                      : &mut std::net::TcpStream,
  org_buffer_text             : &str,
  config                      : &SkgConfig,
  tantivy_index               : &TantivyIndex,
  graph                       : &InRustGraphHandle,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                 : &mut ViewsState,
) -> Result<SaveResponse, Box<dyn Error>> {
  // Auto-approve forks: a test driving the save directly is exercising
  // the COMMIT path. The fork-confirmation (commit-nothing) path has its
  // own shim below.
  update_from_and_rerender_buffer_with_fork_approval_test (
    stream, org_buffer_text, config, tantivy_index, graph,
    diff_mode_enabled, viewuri_from_request_result, views_state,
    /* fork_approved = */ true ) . await }

/// As 'update_from_and_rerender_buffer_test', but lets the test choose
/// whether forks are approved -- pass false to exercise the
/// fork-confirmation (commit-nothing) path.
pub async fn update_from_and_rerender_buffer_with_fork_approval_test (
  stream                      : &mut std::net::TcpStream,
  org_buffer_text             : &str,
  config                      : &SkgConfig,
  tantivy_index               : &TantivyIndex,
  graph                       : &InRustGraphHandle,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                 : &mut ViewsState,
  fork_approved               : bool,
) -> Result<SaveResponse, Box<dyn Error>> {
  // No user-set clone sources: every fork's source resolves by
  // inference-else-default.
  update_from_and_rerender_buffer_with_fork_sources_test (
    stream, org_buffer_text, config, tantivy_index, graph,
    diff_mode_enabled, viewuri_from_request_result, views_state,
    fork_approved, &HashMap::new () ) . await }

/// As 'update_from_and_rerender_buffer_with_fork_approval_test', but also
/// lets the test supply the per-fork clone sources (keyed by N's pid)
/// the user would have chosen in the confirmation buffer -- exercising
/// the 'fork-sources' transport without an Emacs client.
pub async fn update_from_and_rerender_buffer_with_fork_sources_test (
  stream                      : &mut std::net::TcpStream,
  org_buffer_text             : &str,
  config                      : &SkgConfig,
  tantivy_index               : &TantivyIndex,
  graph                       : &InRustGraphHandle,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                 : &mut ViewsState,
  fork_approved               : bool,
  fork_sources                : &HashMap<ID, SourceName>,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut env : SkgEnv =
    skg_env_from_parts (config, tantivy_index, graph);
  update_from_and_rerender_buffer (
    stream,
    org_buffer_text,
    &mut env,
    diff_mode_enabled,
    viewuri_from_request_result,
    views_state,
    None,
    fork_approved,
    fork_sources ) . await }

/// Move NODE to SOURCE: set its home AND retag every relationship
/// member and alias to that source. Under the historical
/// 'leveled-lists' work item, the invariant was member source == home, so any
/// test that reassigns a node's source must go through this, or the
/// telescope write would emit sections at the old source. The real
/// source-move rule (which member sources follow a home move) is owned by
/// work item save-leveling.
pub fn set_source_retagging_member_sources (
  node  : &mut NodeComplete,
  source : &SourceName,
) {
  node . source = source . clone ();
  for m in node . contains . iter_mut () {
    m . source = source . clone (); }
  let retag_msv = |msv : &mut MSV<MemberAtSource<ID>>| {
    if let MSV::Specified (v) = msv {
      for m in v . iter_mut () {
        m . source = source . clone (); }} };
  retag_msv ( &mut node . subscribes_to );
  retag_msv ( &mut node . hides_from_its_subscriptions );
  retag_msv ( &mut node . overrides_view_of );
  if let MSV::Specified (v) = &mut node . aliases {
    for m in v . iter_mut () {
      m . source = source . clone (); }} }

/// Assert that a mutated graph publication agrees with its durable fixture
/// files. This catches graph/disk divergence without a second data store.
pub fn assert_graph_matches_config (
  handle : &InRustGraphHandle,
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let actual = handle . load_full ();
  let expected = graph_handle_from_config (config)? . load_full ();
  let a : &InRustGraph = &actual . graph;
  let e : &InRustGraph = &expected . graph;
  assert_eq! (a . nodes, e . nodes, "graph nodes differ from disk");
  assert_eq! (a . contained_by, e . contained_by,
              "contains inverse index differs from disk");
  assert_eq! (a . subscribers_of, e . subscribers_of,
              "subscribes inverse index differs from disk");
  assert_eq! (a . hiders_of, e . hiders_of,
              "hides inverse index differs from disk");
  assert_eq! (a . overriders_of, e . overriders_of,
              "overrides inverse index differs from disk");
  assert_eq! (a . textlinks_in, e . textlinks_in,
              "text-link inverse index differs from disk");
  assert_eq! (a . extra_id_to_pid, e . extra_id_to_pid,
              "extra-id index differs from disk");
  Ok (( )) }

/// A converted fixture (author-folder layout) keeps its .skg files
/// under owned/; a flat fixture keeps them at the root. The
/// test-config synthesizers point their single "main" source at
/// whichever the fixture uses.
pub fn prefer_owned_subdir (
  root : &Path,
) -> PathBuf {
  let owned : PathBuf = root . join ("owned");
  if owned . is_dir () { owned }
  else { root . to_path_buf () }}

/// Build a single-source config, explicit graph, and empty search index from a
/// disposable fixture directory.
pub fn setup_test_graph_and_tantivy (
  fixtures_folder: &str,
  tantivy_folder: &str,
) -> Result<(SkgConfig, InRustGraphHandle, TantivyIndex), Box<dyn Error>> {
  let config: SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> = HashMap::new();
    sources . insert (
      SourceName::from ("main"),
      SkgfileSource {
        name         : SourceName::from ("main"),
        abbreviation : None,
        path         : prefer_owned_subdir (
          Path::new (fixtures_folder) ), // see populate_test_db_from_fixtures
        user_owns_it : true, });
    SkgConfig::from_sources (sources, tantivy_folder) };
  let graph : InRustGraphHandle = graph_handle_from_config (&config)?;
  let tantivy_index: TantivyIndex =
    create_empty_tantivy_index(&config . tantivy_folder)?;
  Ok ((config, graph, tantivy_index)) }

/// Drain pending search writes and remove a disposable Tantivy index.
pub fn cleanup_test_tantivy(
  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  // The Tantivy index is written by a background worker
  // (server/dbs/tantivy/background_writer.rs). Drain it before touching
  // the index directory, so no commit or merge thread is still creating
  // files when we delete it -- otherwise remove_dir_all races them and
  // fails with DirectoryNotEmpty. (guarded_test_then_cleanup drains too,
  // but many tests call this helper directly.)
  crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();

  // Delete Tantivy index if path provided and exists. Belt-and-suspenders:
  // even after draining the worker, retry on DirectoryNotEmpty in case the
  // OS or Tantivy is still finishing background file cleanup.
  if let Some (tantivy_path) = tantivy_folder {
    if tantivy_path . exists() {
      let max_attempts : usize = 20; // 20 * 50ms = 1s total
      for attempt in 0 .. max_attempts {
        match fs::remove_dir_all (tantivy_path) {
          Ok (()) => break,
          Err (e)
            if e . raw_os_error() == Some (39) // ENOTEMPTY
               && attempt + 1 < max_attempts => {
            std::thread::sleep (
              std::time::Duration::from_millis (50) ); }
          Err (e) => return Err ( Box::new (e) ), } } } }

  Ok (( )) }

/// Compare two org-mode headlines ignoring ID differences.
/// Converts each headline to HeadlineInfo and strips ID from metadata.
pub fn compare_headlines_modulo_id(
  headline1: &str,
  headline2: &str
) -> bool {
  let info1: Result<HeadlineInfo, String> =
    headline_to_triple (headline1);
  let info2: Result<HeadlineInfo, String> =
    headline_to_triple (headline2);

  match (info1, info2) {
    (Ok((level1, metadata1, title1)),
     Ok((level2, metadata2, title2))) => {
      let has_id1: bool =
        metadata1 . as_ref() . map_or(false, |m| m . id . is_some());
      let has_id2: bool =
        metadata2 . as_ref() . map_or(false, |m| m . id . is_some());
      if has_id1 != has_id2 {
        // One has an ID and the other doesn't, so they are unequal.
        return false; }
      // Strip IDs from both (no-op if no ID present) and compare
      let stripped_metadata1: Option<ViewnodeMetadata> =
        strip_id_from_metadata_struct (metadata1);
      let stripped_metadata2: Option<ViewnodeMetadata> =
        strip_id_from_metadata_struct (metadata2);
      (level1, stripped_metadata1, title1) ==
        (level2, stripped_metadata2, title2) },
    (Err (e1), Err (e2)) if (e1 == "__NOT_A_HEADLINE__" &&
                           e2 == "__NOT_A_HEADLINE__")
      => { // Both are not headlines, so compare directly
        headline1 == headline2 },
    _ => false,  // One is headline, other is not, or they have different errors
  }}

/// Compare two MpViewnode trees by DFS.
/// (PITFALL: Naive comparison of trees just compares NodeIds,
/// which are nearly meaningless.)
pub fn compare_viewnode_trees (
  node1 : NodeRef < MpViewnode >,
  node2 : NodeRef < MpViewnode >
) -> bool {
  let n1 : & MpViewnode =
    node1 . value ();
  let n2 : & MpViewnode =
    node2 . value ();
  if n1 != n2 { return false; }
  { // recurse
    let children1 : Vec < NodeRef < '_, MpViewnode >> =
      node1 . children () . collect ();
    let children2 : Vec < NodeRef < '_, MpViewnode >> =
      node2 . children () . collect ();
    children1 . len () == children2 . len () &&
      children1 . iter () . zip ( children2 . iter () )
      . all ( | ( c1, c2 ) |
              compare_viewnode_trees ( *c1, *c2 )) }}

/// Compares ignoring ID value but not ID presence/absence.
pub fn compare_viewnode_trees_modulo_id(
  viewforest1: &Tree<MpViewnode>,
  viewforest2: &Tree<MpViewnode>
) -> bool {
  let root1 : Vec < NodeRef < '_, MpViewnode >> =
    viewforest1 . root() . children() . collect();
  let root2 : Vec < NodeRef < '_, MpViewnode >> =
    viewforest2 . root() . children() . collect();
  if root1 . len() != root2 . len() {
    return false; }
  for (tree1, tree2) in root1 . iter() . zip(root2 . iter()) {
    if !compare_two_viewnode_branches_recursively_modulo_id(
      *tree1, *tree2 )
    { return false; }}
  true }

/// Compare two MpViewnode subtrees, ignoring ID values.
fn compare_two_viewnode_branches_recursively_modulo_id (
  node1: NodeRef<MpViewnode>,
  node2: NodeRef<MpViewnode>
) -> bool {
  let n1 : &MpViewnode = node1 . value();
  let n2 : &MpViewnode = node2 . value();
  match (&n1 . kind, &n2 . kind) {
    ( MpViewnodeKind::Vognode (MpVognode::Active (_))
        | MpViewnodeKind::Phantom (MpPhantom::Diff (_)),
      MpViewnodeKind::Vognode (MpVognode::Active (_))
        | MpViewnodeKind::Phantom (MpPhantom::Diff (_))) =>
    { // Copy the ID from one to the other, then compare. TODO/DONE/local-view-update/plan_v2.org §11: Normal and
      // Diff phantom payloads are now different types, so read n2's id via the
      // shared accessor and write n1_copy's per variant.
      let id2 : Option<ID> = n2 . id_opt () . cloned ();
      let mut n1_copy : MpViewnode =
        n1 . clone();
      match &mut n1_copy . kind {
        MpViewnodeKind::Vognode (MpVognode::Active (t)) => t . id = id2,
        MpViewnodeKind::Phantom (MpPhantom::Diff (p)) => p . id = id2,
        _ => {} }
      if n1_copy != *n2 { return false; }}
    ( MpViewnodeKind::QualCol (_)
      | MpViewnodeKind::Qual (_)
      | MpViewnodeKind::PartnerCol (_)
      | MpViewnodeKind::BufferRoot,
      MpViewnodeKind::QualCol (_)
      | MpViewnodeKind::Qual (_)
      | MpViewnodeKind::PartnerCol (_)
      | MpViewnodeKind::BufferRoot) =>
    { if n1 != n2 { return false; }}
    ( MpViewnodeKind::Phantom (MpPhantom::Deleted (_)),
      MpViewnodeKind::Phantom (MpPhantom::Deleted (_))) =>
    { if n1 != n2 { return false; }}
    ( MpViewnodeKind::DeadScaffold,
      MpViewnodeKind::DeadScaffold) =>
    { if n1 != n2 { return false; }}
    _ => return false, // mismatched kinds
  }
  { // Recurse on children
    let children1 : Vec < NodeRef < '_, MpViewnode >> =
      node1 . children() . collect();
    let children2 : Vec < NodeRef < '_, MpViewnode >> =
      node2 . children() . collect();
    ( children1 . len() == children2 . len() &&
      children1 . iter() . zip(children2 . iter())
      . all (|(c1, c2)|
             compare_two_viewnode_branches_recursively_modulo_id(
               *c1, *c2)) ) }}

/// Remove ID from metadata struct while preserving other metadata
fn strip_id_from_metadata_struct(
  metadata: Option<ViewnodeMetadata>
) -> Option<ViewnodeMetadata> {
  metadata . map(|mut meta| {
    meta . id = None;
    meta
  } ) }

/// Return every primary ID in an explicit graph snapshot.
pub fn all_pids_from_graph (
  graph : &InRustGraph,
) -> HashSet<ID> {
  graph . nodes . keys () . cloned () . collect () }

/// Return the aliases carried by one primary node in an explicit graph.
pub fn extra_ids_from_pid (
  graph : &InRustGraph,
  pid   : &ID,
) -> Vec<ID> {
  graph . get (pid)
    . map (|node| node . extra_ids . clone ())
    . unwrap_or_default () }

/// Check if a specific ID exists in Tantivy search results.
/// Searches for the given query and checks if any result has the exact ID.
pub fn tantivy_contains_id(
  tantivy_index: &TantivyIndex,
  query: &str,
  expected_id: &str,
) -> Result<bool, Box<dyn Error>> {
  let (matches, searcher)
    : (Vec<(f32, DocAddress)>, Searcher)
    = search_index ( tantivy_index, &tantivy_index . reader . searcher (), query,
                     & SearchOptions::default () )?;
  for (_score, doc_address) in matches {
    let doc: TantivyDocument = searcher . doc (doc_address)?;
    let id_value: Option<String> =
      doc . get_first(tantivy_index . id_field)
      . and_then(|v| v . as_str() . map (String::from));
    if id_value == Some(expected_id . to_string()) {
      return Ok (true); }}
  Ok (false) }

/// Strips comments from an org-buffer string.
///
/// Comments are marked by '#' - everything after the first '#' on each line
/// is removed, along with any trailing whitespace before the '#'.
///
/// # Example
/// ```
/// use skg::test_utils::strip_org_comments;
/// let input = "  * (skg (id 1)) title 1 # here's a comment\n  ** (skg (id 2)) title 2 # here's another";
/// let result = strip_org_comments(input);
/// assert_eq!(result, "  * (skg (id 1)) title 1\n  ** (skg (id 2)) title 2");
/// ```
pub fn strip_org_comments(s: &str) -> String {
  s . lines()
    . map(|line| {
      if let Some (hash_pos) = line . find ('#') {
        // Remove everything from '#' onwards, then trim trailing whitespace
        line[..hash_pos] . trim_end() . to_string()
      } else { line . to_string() }} )
    . collect::<Vec<String>>()
    . join ("\n") }

/// Example NodeComplete for use in tests.
pub fn nodecomplete_example () -> NodeComplete {
  let source : SourceName = SourceName::from ("main");
  NodeComplete {
    title: "This text gets indexed." . to_string(),
    ugly_telescope: false,
    aliases: MSV::Unspecified,
    source: source . clone (),
    pid: ID::new ("example"),
    extra_ids: vec![],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    contains: members_at_source ( &source,
                    vec![ ID::new ("1"),
                          ID::new ("2"),
                          ID::new ("3")] ),
    subscribes_to: members_at_source_msv ( &source,
                    MSV::Specified(vec![ID::new ("11"),
                             ID::new ("12"),
                             ID::new ("13")])),
    hides_from_its_subscriptions: MSV::Unspecified,
    overrides_view_of: MSV::Unspecified,
    misc: Vec::new (), }}

/// Extract NodeComplete from Save variant; panics on Delete.
pub fn extract_nodecomplete_if_save_else_error(
  instr: &DefineNode
) -> &NodeComplete {
  match instr {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") }}

/// Read one length-prefixed message from a TCP stream.
/// Returns the body as a String.
/// Format: "Content-Length: N\r\n\r\n<N bytes>"
pub fn read_lp_message (
  reader : &mut std::io::BufReader<std::net::TcpStream>,
) -> Result<String, Box<dyn std::error::Error>> {
  use std::io::{BufRead, Read};
  let mut header_line : String = String::new ();
  reader . read_line (&mut header_line) ?;
  let mut blank : String = String::new ();
  reader . read_line (&mut blank) ?;
  let content_length : usize =
    header_line . trim ()
    . strip_prefix ("Content-Length: ")
    . ok_or ("missing Content-Length header") ?
    . parse () ?;
  let mut body_bytes : Vec<u8> = vec![0u8; content_length];
  reader . read_exact (&mut body_bytes) ?;
  Ok ( String::from_utf8 (body_bytes) ? ) }

/// Read all LP messages from a stream until one contains the
/// given terminal response type (e.g. "save-result").
/// Returns (non_terminal_messages, terminal_message).
pub fn read_lp_messages_until (
  reader        : &mut std::io::BufReader<std::net::TcpStream>,
  terminal_type : &str,
) -> Result<(Vec<String>, String),
            Box<dyn std::error::Error>> {
  let mut collected : Vec<String> = Vec::new ();
  loop {
    let msg : String = read_lp_message (reader) ?;
    if msg . contains (terminal_type) {
      return Ok (( collected, msg )); }
    collected . push (msg); } }

/// Read all LP messages from a stream until EOF.
/// Useful for tests that call pipeline functions directly
/// (without the full handler that sends a terminal message).
pub fn read_all_lp_messages (
  reader : &mut std::io::BufReader<std::net::TcpStream>,
) -> Vec<String> {
  let mut messages : Vec<String> = Vec::new ();
  while let Ok (msg) = read_lp_message (reader) {
    messages . push (msg); }
  messages }

/// Drive the retained collateral scheduler until its next exact text offer,
/// acknowledge that offer as applied, and return the offered content.  Direct
/// handler tests use this instead of the real editor request loop.
pub fn apply_next_scheduled_view (
  scheduler : &mut crate::serve::handlers::collateral_scheduler::CollateralScheduler,
  views     : &mut ViewsState,
  server    : &mut std::net::TcpStream,
  reader    : &mut std::io::BufReader<std::net::TcpStream>,
) -> Result<String, Box<dyn std::error::Error>> {
  use std::time::{Duration, Instant};
  reader . get_mut () . set_read_timeout (Some (Duration::from_millis (20)))?;
  let deadline = Instant::now () + Duration::from_secs (10);
  loop {
    scheduler . pump (server, views);
    match read_lp_message (reader) {
      Ok (message) if message . contains ("(response-type collateral-view)") => {
        let field = |key : &str| -> Result<String, Box<dyn std::error::Error>> {
          response_atom_field (&message, key)
            . ok_or_else (|| format! ("offer has no {}: {}", key, message) . into ())
        };
        if response_atom_field (&message, "needs-authorization")
           . as_deref () == Some ("true")
        {
          return Err ("test rerender unexpectedly needs scalar authorization"
            . into ()); }
        if let Some (error) = response_atom_field (&message, "render-error") {
          return Err (format! ("scheduled view failed: {}", error) . into ()); }
        let quoted = |value : String| format! ("{:?}", value);
        let mut acknowledgement = format! (concat! (
          "((request . \"apply collateral\") ",
          "(operation-id . {}) (view-uri . {}) (applied . \"true\") ",
          "(authorized . \"nil\") (graph-generation . {}) ",
          "(presentation-generation . {}) ",
          "(viewforest-base-revision . {}) ",
          "(resulting-server-revision . {}) ",
          "(view-base-graph-generation . {}) ",
          "(view-base-presentation-generation . {}) ",
          "(expected-client-application-token . {}) ",
          "(resulting-client-application-token . {}) ",
          "(client-token . {}) (view-base-source-set . {}) ",
          "(resulting-source-set . {})"),
          quoted (field ("operation-id")?), quoted (field ("view-uri")?),
          quoted (field ("graph-generation")?),
          quoted (field ("presentation-generation")?),
          quoted (field ("viewforest-base-revision")?),
          quoted (field ("resulting-server-revision")?),
          quoted (field ("view-base-graph-generation")?),
          quoted (field ("view-base-presentation-generation")?),
          quoted (field ("expected-client-application-token")?),
          quoted (field ("resulting-client-application-token")?),
          quoted (field ("resulting-client-application-token")?),
          quoted (field ("view-base-source-set")?),
          quoted (field ("resulting-source-set")?));
        if let Some (buffer_id) = response_atom_field (
            &message, "client-buffer-id")
        {
          acknowledgement . push_str (&format! (
            " (client-buffer-id . {:?})", buffer_id)); }
        acknowledgement . push (')');
        scheduler . handle_apply_ack (server, &acknowledgement, views);
        return field ("content");
      }
      Ok (_) => {}
      Err (_) if Instant::now () < deadline => {
        std::thread::sleep (Duration::from_millis (5)); }
      Err (error) => return Err (error),
    }
    if Instant::now () >= deadline {
      return Err ("timed out waiting for scheduled view offer" . into ()); }
  }
}

fn response_atom_field (response : &str, key : &str) -> Option<String> {
  let sexp::Sexp::List (fields) = sexp::parse (response) . ok ()? else {
    return None; };
  fields . iter () . find_map (|field| {
    let sexp::Sexp::List (parts) = field else { return None; };
    if parts . len () != 2
       || crate::types::sexp::atom_to_string (&parts [0]) . ok ()? != key
    { return None; }
    crate::types::sexp::atom_to_string (&parts [1]) . ok ()
  })
}

/// Extract a string field from a tagged sexp like
/// ((response-type X) (view-uri "URI") (content "...")).
/// Returns None if the key is not found.
pub fn extract_string_field_from_sexp (
  sexp_str : &str,
  key      : &str,
) -> Option<String> {
  let parsed : sexp::Sexp = sexp::parse (sexp_str) . ok () ?;
  match &parsed {
    sexp::Sexp::List (items) =>
      items . iter () . find_map ( |item| match item {
        sexp::Sexp::List (pair) if pair . len () == 2 =>
          match (&pair[0], &pair[1]) {
            ( sexp::Sexp::Atom (sexp::Atom::S (k)),
              sexp::Sexp::Atom (sexp::Atom::S (v)) )
              if k == key => Some (v . clone ()),
            _ => None },
        _ => None } ),
    _ => None } }
