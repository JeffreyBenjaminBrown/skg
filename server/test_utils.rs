mod guard;
pub use guard::TestStoreGuard;

use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::filesystem::not_nodes::load_config_with_overrides;
use crate::dbs::init::create_empty_tantivy_index;
use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle, new_handle};
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
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::future::Future;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;
use tantivy::{DocAddress, Searcher, TantivyDocument};
use tantivy::schema::document::Value;


/// Run tests with automatic database setup and cleanup.
///
/// This helper function encapsulates the common pattern of:
/// 1. Copying fixtures to a temp directory (so saves don't corrupt originals)
/// 2. Setting up a Tantivy index
/// 3. Running test functions
/// 4. Cleaning up the database, index, and temp fixtures
///
/// The test_fn closure receives references to SkgConfig and TantivyIndex
/// and can run multiple test functions sequentially.
///
/// Example:
/// ```
/// #[test]
/// fn my_test() -> Result<(), Box<dyn Error>> {
///   run_with_test_stores(
///     "skg-test-my-test",
///     "tests/my_test/fixtures",
///     "/tmp/tantivy-test-my-test",
///     |config, tantivy| Box::pin(async move {
///       test_function_1(config, tantivy).await?;
///       test_function_2(config, tantivy).await?;
///       Ok(())
///     } )) }
/// ```
pub fn run_with_test_stores<F>(
  test_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a mut TantivyIndex)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  let _groups_lock : std::sync::MutexGuard<()> =
    // Serializes against other store tests in this process (see
    // TEST_STORE_GROUPS_MUTEX): under plain 'cargo test' a binary's
    // tests share one process, and tests can race on shared temporary paths
    // and the Tantivy background writer -- the historical flake family in
    // TODO/problems.org. Uncontended under nextest.
    TEST_STORE_GROUPS_MUTEX . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () );
  let temp_fixtures : PathBuf =
    // Copy fixtures to temp so saves don't corrupt originals
    PathBuf::from(format!("/tmp/{}-fixtures", test_name));
  if temp_fixtures . exists() {
    fs::remove_dir_all (&temp_fixtures)?;
  }
  copy_dir_all(
    &PathBuf::from (fixtures_folder),
    &temp_fixtures)?;
  let result : Result<(), Box<dyn Error>> = block_on(async {
    let (config, mut tantivy) : (SkgConfig, TantivyIndex)
      = setup_test_tantivy(
          test_name,
          temp_fixtures . to_str() . unwrap(),
          tantivy_folder )?;
    guarded_test_then_cleanup(
      test_name,
      Some(config . tantivy_folder . clone()),
      test_fn(&config, &mut tantivy),
    ). await
  } );
  if temp_fixtures . exists() { // more cleanup
    fs::remove_dir_all (&temp_fixtures)?; }
  result
}

/// Like run_with_test_stores, but loads config from a TOML file
/// (supporting multi-source setups) and skips Tantivy.
pub fn run_with_test_stores_from_config<F>(
  test_name: &str,
  config_path: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  let _groups_lock : std::sync::MutexGuard<()> =
    // See the twin lock in run_with_test_stores.
    TEST_STORE_GROUPS_MUTEX . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () );
  block_on(async {
    let config: SkgConfig =
      load_config_with_overrides(config_path, Some (test_name), &[])?;
    guarded_test_then_cleanup(
      test_name, None,
      test_fn(&config),
    ) . await
  } )
}

/// Serializes store-using tests (shared groups AND the per-test
/// run_with_test_stores family) within one process. Under nextest every
/// test is its own process, so this is uncontended there; under
/// plain 'cargo test' (threads in one process) it keeps tests from racing on
/// shared temporary paths and the Tantivy background writer.
static TEST_STORE_GROUPS_MUTEX : std::sync::Mutex<()> =
  std::sync::Mutex::new (( ));

/// One fixture workspace, shared by a whole group of
/// sequential sub-tests. 'reset' restores a pristine state between
/// sub-tests by wiping the DATA in place (~10ms) instead of
/// deleting and recreating the fixture stores (~250ms) -- see
/// TODO/faster-tests.org for the measurements.
pub struct SharedStoreSession {
  pub test_name    : String,
  temp_fixtures  : PathBuf, // where reset() copies each sub-test's fixtures, so saves don't corrupt originals
  tantivy_folder : PathBuf,
  pub config     : SkgConfig,
  pub tantivy    : TantivyIndex,
}

impl SharedStoreSession {
  /// Restore a pristine state for the next sub-test: wipe all data,
  /// copy `fixtures_folder` to the temp dir, point a config at it,
  /// repopulate, fresh Tantivy index. If the fixtures contain a
  /// 'skgconfig.toml' (multi-source setups), that config is loaded
  /// from the temp copy; otherwise a single-source ("main") config
  /// is built around the copy. The shared session object survives;
  /// its graph snapshot and Tantivy index are replaced.
  /// `subtest_name` is printed so a failing group identifies which
  /// sub-test died (libtest replays captured stdout on failure).
  pub fn reset (
    &mut self,
    subtest_name    : &str,
    fixtures_folder : &str,
  ) -> Result<(), Box<dyn Error>> {
    self . reset_with_fixture_prep (
      subtest_name, fixtures_folder, |_| Ok (( )) ) }

  /// Like 'reset', but runs `prep` on the temp fixture copy BEFORE
  /// the config is loaded and the graph snapshot populated -- for
  /// sub-tests whose fixtures need mutation that the snapshot must
  /// reflect (e.g. git-initializing a source and leaving a
  /// worktree-vs-HEAD diff).
  pub fn reset_with_fixture_prep<P> (
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
          copied_config . to_str () . unwrap (),
          Some ( &self . test_name ), &[] ) ?;
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
        SkgConfig::fromSourcesAndTantivyFolder (
          sources,
          self . tantivy_folder . to_str () . unwrap () ) }};
    self . wipe_then_repopulate () }

  /// Like 'reset', but for sub-tests that prepare their own source
  /// directory (e.g. a git repo in a TempDir): no fixture copy; the
  /// single-source ("main") config points at `source_path` directly.
  pub fn reset_with_source_path (
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
      SkgConfig::fromSourcesAndTantivyFolder (
        sources,
        self . tantivy_folder . to_str () . unwrap () ) };
    self . wipe_then_repopulate () }

  /// Like 'reset', but loads a (possibly multi-source) config from a
  /// TOML file, reading fixtures in place -- the same convention as
  /// 'run_with_test_stores_from_config'.
  pub fn reset_from_config (
    &mut self,
    subtest_name : &str,
    config_path  : &str,
  ) -> Result<(), Box<dyn Error>> {
    println! ("-- sub-test: {}", subtest_name);
    self . config = load_config_with_overrides (
      config_path, Some ( &self . test_name ), &[] ) ?;
    self . config . tantivy_folder =
      self . tantivy_folder . clone ();
    self . wipe_then_repopulate () }

  fn wipe_then_repopulate (
    &mut self,
  ) -> Result<(), Box<dyn Error>> {
    crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
    self . tantivy = create_empty_tantivy_index (
      &self . tantivy_folder ) ?;
    Ok (( )) }
}

/// Run a group of sub-tests against one shared fixture workspace.
/// The test_fn
/// should call 'session.reset' (or 'session.reset_from_config')
/// before each sub-test. Cleanup mirrors 'run_with_test_stores':
/// panic-safe via TestStoreGuard + catch_unwind.
pub fn run_with_shared_test_stores<F> (
  test_name : &str,
  test_fn : F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a mut SharedStoreSession)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  let _groups_lock : std::sync::MutexGuard<()> =
    TEST_STORE_GROUPS_MUTEX . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () );
  let temp_fixtures : PathBuf =
    PathBuf::from ( format! ("/tmp/{}-fixtures", test_name) );
  let tantivy_folder : PathBuf =
    PathBuf::from ( format! ("/tmp/tantivy-test-{}", test_name) );
  let result : Result<(), Box<dyn Error>> = block_on ( async {
    let mut session : SharedStoreSession = SharedStoreSession {
      test_name        : test_name . to_string (),
      temp_fixtures  : temp_fixtures . clone (),
      tantivy_folder : tantivy_folder . clone (),
      config         : // placeholder; every sub-test runs after a reset, which overwrites it
        SkgConfig::fromSourcesAndTantivyFolder (
          HashMap::new (),
          tantivy_folder . to_str () . unwrap () ),
      tantivy        : create_empty_tantivy_index (&tantivy_folder) ?, };
    let mut guard : TestStoreGuard = TestStoreGuard::new (
      test_name, Some ( tantivy_folder . clone () ));
    let test_result : Result<Result<(), Box<dyn Error>>, _> =
      AssertUnwindSafe ( test_fn (&mut session) )
      . catch_unwind () . await;
    crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
    let cleanup_result : Result<(), Box<dyn Error>> =
      cleanup_test_tantivy (Some (&tantivy_folder)) ;
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

/// Run a test future with a TestStoreGuard safety net, then clean up.
/// Catches panics so cleanup runs even if the test fails.
async fn guarded_test_then_cleanup(
  test_name: &str,
  tantivy_folder: Option<PathBuf>,
  test_future: Pin<Box<dyn Future<Output = Result
                                  <(), Box<dyn Error>>> + '_>>,
) -> Result<(), Box<dyn Error>> {
  let mut guard: TestStoreGuard = TestStoreGuard::new(
    test_name, tantivy_folder.clone());
  let test_result: Result<Result<(), Box<dyn Error>>, _> =
    AssertUnwindSafe(test_future)
    . catch_unwind() . await;
  // A save's Tantivy write now commits in the background and outlives
  // update_from_and_rerender_buffer; drain it before deleting the test
  // index out from under the worker.
  crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
  let cleanup_result: Result<(), Box<dyn Error>> =
    cleanup_test_tantivy(tantivy_folder . as_deref()) ;
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

/// Build an in-Rust graph handle preloaded from the test config's
/// fixtures on disk.
pub fn graph_handle_from_config (
  config : &SkgConfig,
) -> Result<InRustGraphHandle, Box<dyn Error>> {
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (config) ?;
  Ok ( new_handle ( InRustGraph::from_nodecompletes (&nodes) )) }

/// Bundle a test's existing handles into a 'SkgEnv'.
pub fn skg_env_from_parts (
  config        : &SkgConfig,
  tantivy_index : &TantivyIndex,
  graph         : &InRustGraphHandle,
) -> SkgEnv {
  SkgEnv::new_with_graph_handle (
    config . clone (), graph . clone (), tantivy_index . clone () ) }

/// Test shim around 'update_from_and_rerender_buffer' that accepts
/// the four DB handles separately, builds a 'SkgEnv', and calls the
/// underlying function. Lets pre-SkgEnv test code continue to pass
/// '(driver, config, tantivy, graph)' tuples without change.
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

/// Verify the published graph's inverse indexes after a mutation.
pub fn audit_inrustgraph_or_panic (
  handle  : &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let snap : Arc<InRustGraph> = handle . load_full ();
  let errors = crate::dbs::in_rust_graph::internal_index_validation
    ::validate_internal_indexes (&snap);
  if ! errors . is_empty () {
    panic! ("graph index audit failed:\n{:#?}", errors); }
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

/// Set up a test config and empty Tantivy index from copied fixtures.
pub fn setup_test_tantivy (
  _test_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
) -> Result<(SkgConfig, TantivyIndex), Box<dyn Error>> {
  let mut sources : HashMap<SourceName, SkgfileSource> = HashMap::new();
  sources . insert (
    SourceName::from ("main"),
    SkgfileSource {
      name         : SourceName::from ("main"),
      abbreviation : None,
      path         : prefer_owned_subdir (Path::new (fixtures_folder)),
      user_owns_it : true, });
  let config : SkgConfig = SkgConfig::fromSourcesAndTantivyFolder (
    sources, tantivy_folder );
  let tantivy_index : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  Ok ((config, tantivy_index)) }

/// Drain Tantivy's writer and remove a test index.
pub fn cleanup_test_tantivy (
  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
  if let Some (tantivy_path) = tantivy_folder {
    if tantivy_path . exists() {
      let max_attempts : usize = 20;
      for attempt in 0 .. max_attempts {
        match fs::remove_dir_all (tantivy_path) {
          Ok (()) => break,
          Err (e)
            if e . raw_os_error() == Some (39)
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


/// Check if a specific ID exists in Tantivy search results.
/// Searches for the given query and checks if any result has the exact ID.
pub fn tantivy_contains_id(
  tantivy_index: &TantivyIndex,
  query: &str,
  expected_id: &str,
) -> Result<bool, Box<dyn Error>> {
  let (matches, searcher)
    : (Vec<(f32, DocAddress)>, Searcher)
    = search_index ( tantivy_index, query,
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
    overPrivateText_telescope: false,
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
