// cargo nextest run --test grouped_repos -E 'test(readiness_requests::)'
//
// A save consumes the readiness requests of the buffer it saves, at
// the fingerprints of the fresh graph pair, over a git fixture whose
// HEAD and worktree differ: r is retitled and gains the member a, and
// a gains the alias "ay".

#[path = "shared/git_fixture_helpers.rs"]
#[allow(unused_imports, dead_code)] // shared by several test targets, each using part of it
mod git_fixture_helpers;

use git_fixture_helpers::{mk_test_tcp_stream_pair, setup_gitrepo_with_fixtures};

use skg::change_forest::{LeafKey, NodeAspect, RelationshipAspect};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::in_rust_graph::relation_accessors::NodeRelation;
use skg::readiness_store::ReadinessStore;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  cleanup_test_tantivy, graph_handle_from_config, setup_test_tantivy,
  update_from_and_rerender_buffer_test};
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::views_state::{OpenViews, ViewId};

use futures::executor::block_on;
use std::collections::HashSet;
use std::error::Error;
use std::net::TcpStream;
use tempfile::TempDir;

const FIXTURES : &str = "tests/readiness_requests/fixtures";

struct Fixture {
  _root       : TempDir,
  config      : SkgConfig,
  tantivy     : TantivyIndex,
  graph       : InRustGraphHandle,
  views_state : ViewsState,
}

fn fixture (
  test_name : &str,
) -> Result<Fixture, Box<dyn Error>> {
  let root : TempDir = TempDir::new () ?;
  setup_gitrepo_with_fixtures (
    &root . path () . join ("main"),
    &format! ("{}/head", FIXTURES),
    &format! ("{}/worktree", FIXTURES) ) ?;
  let (mut config, tantivy) : (SkgConfig, TantivyIndex) =
    setup_test_tantivy (
      test_name,
      root . path () . join ("main") . to_str () . unwrap (),
      root . path () . join ("tantivy") . to_str () . unwrap () ) ?;
  config . data_root = root . path () . to_path_buf ();
  let graph : InRustGraphHandle = graph_handle_from_config (&config) ?;
  Ok ( Fixture {
    _root : root, config, tantivy, graph,
    views_state : ViewsState {
      open_views        : OpenViews::new (),
      graph_pair        : None, } } ) }

fn save (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = block_on (
    update_from_and_rerender_buffer_test (
      &mut stream, buffer, &f . config, &f . tantivy, &f . graph,
      &Ok ( ViewId::ContentView ("readiness" . to_string ()) ),
      &mut f . views_state )) ?;
  Ok (response) }

fn marked_keys (
  f : &Fixture,
) -> HashSet<LeafKey> {
  ReadinessStore::load (&f . config . data_root) . marks
    . into_keys () . collect () }

fn r_text () -> LeafKey {
  LeafKey::Node { pid : ID::from ("r"), aspect : NodeAspect::Text } }

fn r_contains_a () -> LeafKey {
  LeafKey::Relationship {
    relation : NodeRelation::Contains,
    recorder : ID::from ("r"),
    member   : ID::from ("a"),
    aspect   : RelationshipAspect::Existence } }

fn a_alias () -> LeafKey {
  LeafKey::Node { pid : ID::from ("a"),
                  aspect : NodeAspect::Alias ("ay" . to_string ()) } }

#[test]
fn a_save_marks_and_unmarks_the_leaves_its_requests_name (
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture ("skg-test-readiness-requests") ?;
  let result : Result<(), Box<dyn Error>> = (|| {
    { // The relationship axis of a member, and an alias leaf.
      let response : SaveResponse = save (&mut f, "\
* (skg (node (id r) (repo main))) r, retitled
** (skg (node (id a) (repo main) (readyRequest rel))) a
*** (skg aliasFolder)
**** (skg alias (readyRequest)) ay
** (skg (node (id b) (repo main))) b
") ?;
      assert_eq! ( marked_keys (&f),
                   [ r_contains_a (), a_alias () ] . into_iter () . collect () );
      assert! ( ! response . saved_view . contains ("Request"),
                "the save consumes its requests:\n{}", response . saved_view );
      assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
      { // Each mark holds the fingerprint of the change it approved.
        let pair = f . views_state . graph_pair . clone ()
          . expect ("consuming requests built the graph pair");
        let store : ReadinessStore = ReadinessStore::load (&f . config . data_root);
        for key in [ r_contains_a (), a_alias () ] {
          assert! ( store . is_ready (
            &key, pair . change_forest () . leaf_fingerprint (&key) . unwrap () )); }} }
    { // 'all' on a node names every leaf it owns.
      save (&mut f, "\
* (skg (node (id r) (repo main) (readyRequest all))) r, retitled
** (skg (node (id a) (repo main))) a
** (skg (node (id b) (repo main))) b
") ?;
      assert_eq! ( marked_keys (&f),
                   [ r_text (), r_contains_a (), a_alias () ]
                   . into_iter () . collect () ); }
    { // Unmarking.
      save (&mut f, "\
* (skg (node (id r) (repo main) (unreadyRequest all))) r, retitled
** (skg (node (id a) (repo main))) a
** (skg (node (id b) (repo main))) b
") ?;
      assert_eq! ( marked_keys (&f),
                   [ a_alias () ] . into_iter () . collect () ); }
    { // A request that names no change is dropped, with a warning.
      let response : SaveResponse = save (&mut f, "\
* (skg (node (id r) (repo main))) r, retitled
** (skg (node (id a) (repo main))) a
** (skg (node (id b) (repo main) (readyRequest rel))) b
") ?;
      assert_eq! ( marked_keys (&f),
                   [ a_alias () ] . into_iter () . collect () );
      assert! ( response . warnings . iter ()
                . any ( |w| w . contains ("names no current change") ),
                "{:?}", response . warnings ); }
    Ok (( )) }) ();
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }
