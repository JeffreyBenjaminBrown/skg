// cargo nextest run --test grouped_overrides -E 'test(idempotence::)'
//
// Completion is a fixpoint. Re-rendering an already-rendered buffer,
// with no edits and an unchanged graph, must yield byte-identical output. This is
// the property the whole local-update refactor rests on (order-robust, no
// accumulating drift across saves), so it deserves a regression guard.
//
// Method: render the view de-novo to get a complete buffer, then run it through
// the save+rerender pipeline twice. The two rerenders must be identical. (We do
// not assert de-novo == first-rerender -- those can legitimately differ in
// metadata; idempotence is specifically that a second rerender changes nothing.)
//
// Fixture exercises several Viewnode kinds in one view: Unrestricted content (a -> b,
// c), an AliasFolder + Alias (b has alias "bee"), the IDFolders, a SubscribeeFolder +
// subscribee-as-such (c subscribes to d), the HiddenOutsideOfSubscribeeFolder, and
// a PartnerFolder -- e subscribes to b, so b shows a SubscriberFolder listing e.
// (The death-cleanup / generalized-orphan path -- a folder whose owning node was
// deleted -- is covered separately by the save_collateral_delete_then_edit_under_it
// integration test, which only a delete can trigger, not an unchanged-graph rerender.)

use std::error::Error;
use std::net::TcpStream;

use skg::test_utils::{run_with_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::init::empty_in_ram_tantivy_index;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use skg::refresh::{Refreshed, Truth, refresh_viewforest};
use skg::test_utils::{graph_from_config, prefer_owned_subdir};
use skg::types::env::RuntimeGeneration;
use skg::types::misc::{Skgrepo, SkgrepoName};
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::ViewnodeIdRecord;
use skg::update_buffer::completion_mode::CompletionMode;
use skg::update_buffer::render_initial_view;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[test]
fn rerender_is_idempotent
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-idempotence",
    "tests/idempotence/fixtures",
    "/tmp/tantivy-test-idempotence",
    |config, tantivy| Box::pin ( async move {
      rerender_is_idempotent_impl (
        config, tantivy ) . await
    } )) }

/// Run one save+rerender of `buf` through the full pipeline, returning the
/// rerendered saved view.
async fn rerender_once (
  buf     : &str,
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None,
  };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    buf, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "rerender must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

async fn rerender_is_idempotent_impl (
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
    rerender_once (&complete_buffer, config, tantivy) . await ?;
  let second : String =
    rerender_once (&first, config, tantivy) . await ?;

  assert_eq! ( first, second,
    "completion is not a fixpoint: a second rerender of an unchanged buffer \
     changed it.\n--- first rerender ---\n{}\n--- second rerender ---\n{}",
    first, second );
  Ok (( )) }

// The refresh is a fixpoint over every content-view fixture
// ('TODO/simplify-refresh/04_1_plan_rewrite-refresh.org', "the
// fixpoint"): for each fixture folder under 'tests/' and each node in
// it, the fresh content view of the node, parsed and refreshed again
// over the same graph, comes back as it was, and the second refresh
// corrects nothing. A fixture folder whose graph does not load (one
// built to be refused, or holding no '.skg' files) is passed over.

#[test]
fn every_content_view_fixture_is_a_fixpoint_of_the_refresh (
) -> Result<(), Box<dyn Error>> {
  let mut failures : Vec<String> = Vec::new ();
  let mut views : usize = 0;
  let mut folders : usize = 0;
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
    for pid in pids {
      views += 1;
      if let Err (failure) = refreshes_unchanged (&runtime, &pid) {
        failures . push ( format! ( "{} {}: {}", folder . display (), pid . 0, failure )); }}}
  println! ("{} views over {} fixture folders", views, folders);
  assert! ( folders > 40, "too few fixture folders were read: {}", folders );
  assert! ( failures . is_empty (),
            "{} of {} views over {} fixture folders are not fixpoints:\n\n{}",
            failures . len (), views, folders, failures . join ("\n\n") );
  Ok (( )) }

/// Whether the fresh view of 'pid' refreshes to itself: Err says how not.
fn refreshes_unchanged (
  runtime : &RuntimeGeneration,
  pid     : &ID,
) -> Result<(), String> {
  let (_, text) : (ViewForest, String) =
    render_initial_view (runtime, &[ pid . clone () ], None)
    . map_err ( |e| format! ("the fresh view failed: {}", e) ) ?;
  let (mut forest, errors, _warnings, _provenance) =
    org_to_viewforest (&text, &runtime . graph)
    . map_err ( |e| format! ("the parse failed: {}", e) ) ?;
  if ! errors . is_empty () {
    return Err ( format! ( "the parse refused it: {:?}\n{}", errors, text )); }
  let truth : Truth = Truth { graph       : &runtime . graph,
                              config      : &runtime . config,
                              restriction : None, };
  let mut record : ViewnodeIdRecord = ViewnodeIdRecord::of_viewforest (&forest);
  let again : Refreshed = refresh_viewforest (
    &mut forest, &mut record, &truth,
    CompletionMode::ContentView, runtime . config . initial_node_limit, None )
    . map_err ( |e| format! ("the second refresh failed: {}", e) ) ?;
  if again . text != text {
    return Err ( format! ( "changed:\n--- first ---\n{}--- second ---\n{}",
                           text, again . text )); }
  if ! again . corrections . is_empty () {
    return Err ( format! ( "corrected: {:?}", again . corrections )); }
  Ok (( )) }

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
/// else the one skgrepo 'main' there ('setup_test_tantivy').
fn fixture_config (
  folder : &Path,
) -> Option<SkgConfig> {
  let toml : PathBuf = folder . join ("skgconfig.toml");
  if toml . is_file () {
    return load_config_with_overrides (
      toml . to_str () ?, Some ("fixpoint"), &[] ) . ok (); }
  let skgrepos : HashMap<SkgrepoName, Skgrepo> = HashMap::from ([ (
    SkgrepoName::from ("main"),
    Skgrepo { name         : SkgrepoName::from ("main"),
              abbreviation : None,
              path         : prefer_owned_subdir (folder),
              owned        : true, } ) ]);
  Some ( SkgConfig::fromSkgreposAndTantivyFolder (
    skgrepos, "/tmp/tantivy-test-fixpoint-unused" )) }
