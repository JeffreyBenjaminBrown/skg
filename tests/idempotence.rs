// cargo nextest run --test grouped_overrides -E 'test(idempotence::)'
//
// The refresh is a fixpoint. Re-rendering an already-rendered buffer,
// with no edits and an unchanged graph, must yield byte-identical output. This is
// the property the whole local-update refactor rests on (order-robust, no
// accumulating drift across saves), so it deserves a regression guard.
//
// Method: render the view de-novo to get a complete buffer, then run it through
// the save+rerender pipeline twice. The two rerenders must be identical. (We do
// not assert de-novo == first-rerender -- those can legitimately differ in
// metadata; the fixpoint is specifically that a second rerender changes nothing.)
//
// Fixture exercises several Viewnode kinds in one view: occurrences as content (a -> b,
// c), an AliasFolder + Alias (b has alias "bee"), the IDFolders, a SubscribeeFolder +
// subscribee-as-such (c subscribes to d), the HiddenOutsideOfSubscribeeFolder, and
// a partner folder -- e subscribes to b, so b shows a SubscriberFolder listing e.
// (The death-cleanup / generalized-orphan path -- a folder whose owning node was
// deleted -- is covered separately by the save_collateral_delete_then_edit_under_it
// integration test, which only a delete can trigger, not an unchanged-graph rerender.)

use skg::test_utils::save_and_rerender;
use std::error::Error;

use skg::test_utils::run_with_test_stores;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::init::empty_in_ram_tantivy_index;
use skg::refresh::{Truth, refresh_content_viewforest};
use skg::refresh::buffer_corrections::BufferCorrectionPolicy;
use skg::test_utils::{graph_from_config, main_skgrepo_config, refresh_reaches_fixpoint};
use skg::types::env::RuntimeGeneration;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::types::misc::{SkgrepoName, SkgrepoSetName};
use skg::types::tree::forest::ViewForest;
use skg::update_buffer::render_initial_view;

use crate::git_fixture::{OwnedSkgrepos, git_fixture, graph_straddle};
use skg::diff_view::completion::complete_viewforest_with_graph_straddle;
use skg::diff_view::pruned_change_view::RenderedDiffViewBuffer;
use skg::diff_view::readiness_store::ReadinessStore;
use skg::graph_straddle::types::GraphStraddle;
use skg::serve::handlers::diff_view::diffView_buffer;
use skg::test_utils::cleanup_test_tantivy;
use skg::types::views_state::DiffViewBufferKind;

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[test]
fn rerender_is_a_fixpoint
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-idempotence",
    "tests/idempotence/fixtures",
    "/tmp/tantivy-test-idempotence",
    |config, tantivy| Box::pin ( async move {
      rerender_is_a_fixpoint_impl (
        config, tantivy ) . await
    } )) }

async fn rerender_is_a_fixpoint_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // The complete de-novo view of root `a`, which we then feed back through the
  // pipeline as if the user saved it unchanged.
  let (complete_buffer, _pids, _tree)
    : (String, Vec<ID>, _) =
    multi_root_view (
      config, Some (tantivy),
      &[ ID ("a" . to_string ()) ] ) ?;

  let first  : String =
    save_and_rerender (&complete_buffer, config, tantivy) . await ?;
  let second : String =
    save_and_rerender (&first, config, tantivy) . await ?;

  assert_eq! ( first, second,
    "the refresh is not a fixpoint: a second rerender of an unchanged buffer \
     changed it.\n--- first rerender ---\n{}\n--- second rerender ---\n{}",
    first, second );
  Ok (( )) }

// The refresh is a fixpoint over every content-view fixture
// ('docs/data-model_technical.org', "The refresh: how a view follows
// the graph"): for each fixture folder under 'tests/' and each node in
// it, the fresh content view of the node, parsed and refreshed again
// over the same graph, comes back as it was, and the second refresh
// corrects nothing. Where the fixture has several skgrepos, that
// holds under each restriction too, for each node it shows. A fixture
// folder whose graph does not load (one built to be refused, or
// holding no '.skg' files) is passed over.

#[test]
fn every_content_view_fixture_is_a_fixpoint_of_the_refresh (
) -> Result<(), Box<dyn Error>> {
  let mut failures : Vec<String> = Vec::new ();
  let mut views : usize = 0;
  let mut folders : usize = 0;
  let mut restricted_folders : HashSet<PathBuf> = HashSet::new ();
  for folder in fixture_folders (Path::new ("tests")) {
    let Some (config) = fixture_config (&folder) else { continue; };
    let Ok (graph) = graph_from_config (&config) else { continue; };
    if graph . nodes . is_empty () { continue; }
    folders += 1;
    let runtime : RuntimeGeneration = RuntimeGeneration {
      config        : Arc::new (config),
      graph         : Arc::new (graph),
      tantivy_index : empty_in_ram_tantivy_index () ?,
      generation    : 0, };
    let mut pids : Vec<ID> = runtime . graph . nodes . keys () . cloned () . collect ();
    pids . sort ();
    for restriction in restrictions_of (&runtime . config) {
      if restriction . is_some () { restricted_folders . insert (folder . clone ()); }
      for pid in &pids {
        if runtime . graph . restriction_keeps_out (pid, restriction . as_ref ()) {
          continue; }
        views += 1;
        if let Err (failure) = refreshes_unchanged (&runtime, pid, restriction . as_ref ()) {
          failures . push ( format! (
            "{} {} under {}: {}", folder . display (), pid . 0,
            restriction . as_ref () . map_or ("no restriction" . to_string (),
                                              |r| format! ("'{}'", r . name . 0) ),
            failure )); }}}}
  println! ( "{} views over {} fixture folders, {} of them restricted",
             views, folders, restricted_folders . len () );
  assert! ( folders > 40, "too few fixture folders were read: {}", folders );
  assert! ( restricted_folders . len () > 5,
            "too few fixture folders were read under a restriction: {}",
            restricted_folders . len () );
  assert! ( failures . is_empty (),
            "{} of {} views over {} fixture folders are not fixpoints:\n\n{}",
            failures . len (), views, folders, failures . join ("\n\n") );
  Ok (( )) }

/// Whether the fresh view of 'pid' under 'restriction' refreshes to
/// itself: Err says how not.
fn refreshes_unchanged (
  runtime     : &RuntimeGeneration,
  pid         : &ID,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<(), String> {
  let (_, text) : (ViewForest, String) =
    render_initial_view (runtime, &[ pid . clone () ], restriction)
    . map_err ( |e| format! ("the fresh view failed: {}", e) ) ?;
  refresh_reaches_fixpoint (
    &text, &runtime . graph,
    |forest, record| refresh_content_viewforest (
      forest, record, &Truth::of_runtime (runtime, restriction),
      &BufferCorrectionPolicy::default () )) }

/// No restriction, then the restriction through each skgrepo but the
/// most private (through which it would be no restriction).
fn restrictions_of (
  config : &SkgConfig,
) -> Vec<Option<SkgrepoRestriction>> {
  let ordered : Vec<SkgrepoName> = config . ordered_skgrepos ();
  let narrower : usize = ordered . len () . saturating_sub (1);
  std::iter::once (None)
    . chain ( ordered . into_iter () . take (narrower)
              . filter_map ( |skgrepo| SkgrepoRestriction::named (
                  config, SkgrepoSetName ( skgrepo . 0 . clone () ))
                . ok () . map (Some) ))
    . collect () }

/// Every folder named 'fixtures' under 'root', but the test results.
fn fixture_folders (
  root : &Path,
) -> Vec<PathBuf> {
  let mut found : Vec<PathBuf> = Vec::new ();
  let Ok (entries) = std::fs::read_dir (root) else { return found; };
  let mut paths : Vec<PathBuf> =
    entries . filter_map ( |entry| entry . ok () . map ( |e| e . path () ))
    . filter ( |path| path . is_dir () ) . collect ();
  paths . sort ();
  for path in paths {
    if path . ends_with ("results") { continue; }
    if path . file_name () . is_some_and ( |name| name == "fixtures" ) {
      found . push (path . clone ()); }
    found . extend ( fixture_folders (&path) ); }
  found }

/// The config a test reads 'folder' with: its 'skgconfig.toml', or
/// else the one skgrepo 'main' there ('main_skgrepo_config').
fn fixture_config (
  folder : &Path,
) -> Option<SkgConfig> {
  let toml : PathBuf = folder . join ("skgconfig.toml");
  if toml . is_file () {
    return load_config_with_overrides (
      toml . to_str () ?, Some ("fixpoint"), &[] ) . ok (); }
  Some ( main_skgrepo_config (
    folder, Path::new ("/tmp/tantivy-test-fixpoint-unused") )) }

// The refresh is a fixpoint over every diff-view fixture too: for each
// git fixture case (a folder whose owned skgrepos each hold a 'head'
// and a 'worktree'), each buffer of the diff view that is not empty,
// as first rendered, parsed and refreshed again over the same graph
// straddle and readiness store, comes back as it was, and the second
// refresh corrects nothing. A case whose straddle cannot be built (one
// made to be refused) is passed over. A skgrepo folder named
// 'foreign' is foreign; the others are owned, 'public' first and
// 'private' last.

const GIT_FIXTURE_CASES : [&str; 4] = [
  "tests/pruned_change_view/fixtures",
  "tests/graph_straddle/fixtures",
  "tests/diff_view/roundtrip",
  "tests/readiness_requests", ];

#[test]
fn every_diff_view_fixture_is_a_fixpoint_of_the_refresh (
) -> Result<(), Box<dyn Error>> {
  let mut failures : Vec<String> = Vec::new ();
  let mut buffers : usize = 0;
  let mut cases : usize = 0;
  for case in git_fixture_cases () {
    let (owned, foreign) : (Vec<String>, Vec<String>) = skgrepos_of_case (&case);
    let owned_names : Vec<&str> = owned . iter () . map ( |name| name . as_str () ) . collect ();
    let foreign_names : Vec<&str> = foreign . iter () . map ( |name| name . as_str () ) . collect ();
    let owned_skgrepos : OwnedSkgrepos =
      if owned . is_empty () { OwnedSkgrepos::OnlyMain }
      else { OwnedSkgrepos::Named (&owned_names) };
    let Ok (fixture) = git_fixture (&case, owned_skgrepos, &foreign_names)
      else { continue; };
    let result : Result<(), Box<dyn Error>> = (|| {
      let Ok (straddle) = graph_straddle (&fixture) else { return Ok (( )); };
      cases += 1;
      let runtime : RuntimeGeneration = RuntimeGeneration {
        config        : Arc::new ( fixture . config . clone () ),
        graph         : fixture . graph . load_full (),
        tantivy_index : empty_in_ram_tantivy_index () ?,
        generation    : 0, };
      let store : ReadinessStore = ReadinessStore::default ();
      for buffer_kind in DiffViewBufferKind::ALL {
        let rendered : Option<RenderedDiffViewBuffer> =
          match diffView_buffer (&runtime, &straddle, &store, buffer_kind) {
            Ok (rendered) => rendered,
            Err (e) => {
              failures . push ( format! ( "{} {:?}: the first rendering failed: {}",
                                          case . display (), buffer_kind, e ));
              continue; }, };
        let Some (rendered) = rendered else { continue; };
        buffers += 1;
        if let Err (failure) = diff_view_buffer_refreshes_unchanged (
             &runtime, &straddle, &store, buffer_kind, &rendered . text ) {
          failures . push ( format! ( "{} {:?}: {}", case . display (), buffer_kind, failure )); }}
      Ok (( )) }) ();
    cleanup_test_tantivy ( Some (&fixture . config . tantivy_folder) ) ?;
    result ?; }
  println! ("{} buffers over {} cases", buffers, cases);
  assert! ( cases > 20, "too few git fixture cases were read: {}", cases );
  assert! ( failures . is_empty (),
            "{} of {} diff-view buffers over {} cases are not fixpoints:\n\n{}",
            failures . len (), buffers, cases, failures . join ("\n\n") );
  Ok (( )) }

/// Whether the buffer 'text' of kind 'buffer_kind', parsed and
/// refreshed over 'straddle', comes back as it was, correcting
/// nothing: Err says how not.
fn diff_view_buffer_refreshes_unchanged (
  runtime     : &RuntimeGeneration,
  straddle    : &GraphStraddle,
  store       : &ReadinessStore,
  buffer_kind : DiffViewBufferKind,
  text        : &str,
) -> Result<(), String> {
  refresh_reaches_fixpoint (
    text, &runtime . graph,
    |forest, record| complete_viewforest_with_graph_straddle (
      runtime, straddle, buffer_kind, store, None,
      &BufferCorrectionPolicy::default (), forest, record )) }

/// The git fixture cases under 'GIT_FIXTURE_CASES': each folder there
/// that holds a 'head' folder, or whose subfolders do.
fn git_fixture_cases (
) -> Vec<PathBuf> {
  let mut cases : Vec<PathBuf> = Vec::new ();
  for parent in GIT_FIXTURE_CASES {
    let parent : &Path = Path::new (parent);
    if parent . join ("fixtures") . join ("head") . is_dir () {
      cases . push ( parent . join ("fixtures") );
      continue; }
    let Ok (entries) = std::fs::read_dir (parent) else { continue; };
    let mut found : Vec<PathBuf> = entries
      . filter_map ( |entry| entry . ok () . map ( |e| e . path () ))
      . filter ( |path| path . is_dir ()
                        && ( path . join ("head") . is_dir ()
                             || skgrepos_of_case (path) . 0 . len () > 0 ))
      . collect ();
    found . sort ();
    cases . extend (found); }
  cases }

/// A case's owned skgrepos, in privacy order ('public' first, 'private'
/// last), and its foreign ones: its subfolders holding a 'head' folder.
/// Both empty for a case of the one skgrepo 'main' ('<case>/head').
fn skgrepos_of_case (
  case : &Path,
) -> (Vec<String>, Vec<String>) {
  if case . join ("head") . is_dir () { return ( Vec::new (), Vec::new () ); }
  let Ok (entries) = std::fs::read_dir (case) else { return ( Vec::new (), Vec::new () ); };
  let mut names : Vec<String> = entries
    . filter_map ( |entry| entry . ok () . map ( |e| e . path () ))
    . filter ( |path| path . join ("head") . is_dir () )
    . filter_map ( |path| path . file_name () . map ( |name| name . to_string_lossy () . to_string () ))
    . collect ();
  let privacy = |name : &String| -> (u8, String) {
    ( match name . as_str () { "public" => 0, "private" => 2, _ => 1 }, name . clone () ) };
  names . sort_by_key (privacy);
  let (foreign, owned) : (Vec<String>, Vec<String>) =
    names . into_iter () . partition ( |name| name == "foreign" );
  (owned, foreign) }
