// cargo nextest run --test grouped_saves -E 'test(strandeds::)'
//
// What a home skgrepo move strands, decided by the save
// (server/from_text/strandeds.rs). In the fixture, the public node p
// contains the public node m, and the public node q contains the
// private node n, recorded at private (q's private section). Each
// test views only the moved node, so the recorder is outside the view.

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle, new_handle};
use skg::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use skg::dbs::init::create_empty_tantivy_index;
use skg::from_text::strandeds::StrandedsReview;
use skg::diff_view::readiness_store::readiness_marks_path;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::{
  SaveResponse, update_from_and_rerender_buffer_with_approvals};
use skg::test_utils::{
  cleanup_test_tantivy, copy_dir_all, extract_string_field_from_sexp,
  read_all_lp_messages, skg_env_from_parts};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
use skg::types::nodes::complete::Graphnode;
use skg::types::views_state::{OpenViews, ViewforestId};
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;
use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use tempfile::TempDir;

struct Fixture {
  _dir        : TempDir,
  config      : SkgConfig,
  tantivy     : TantivyIndex,
  graph       : InRustGraphHandle,
  views_state : ViewsState,
}

fn fixture (
  test_name : &str,
) -> Result<Fixture, Box<dyn Error>> {
  let dir : TempDir = TempDir::new () ?;
  copy_dir_all ( Path::new ("tests/strandeds/fixtures"), dir . path () ) ?;
  let root : &Path = dir . path ();
  let config : SkgConfig = load_config_with_overrides (
    root . join ("skgconfig.toml") . to_str () . unwrap (),
    Some (test_name),
    &[ ("public",  root . join ("owned/public")),
       ("private", root . join ("owned/private")),
       ("foreign", root . join ("foreign")) ] ) ?;
  let config : SkgConfig = SkgConfig {
    tantivy_folder : root . join ("tantivy"),
    data_root      : root . to_path_buf (),
    .. config };
  let nodes : Vec<Graphnode> = read_all_skg_files_from_skgrepos (&config) ?;
  let tantivy : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  Ok ( Fixture {
    _dir : dir, config, tantivy,
    graph : new_handle ( InRustGraph::from_graphnodes (&nodes) ),
    views_state : ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None, } } ) }

/// Render a view of 'roots', registered under 'viewforest_id'.
fn open_view (
  f       : &mut Fixture,
  roots   : &[&str],
  viewforest_id : &ViewforestId,
) -> Result<String, Box<dyn Error>> {
  let root_ids : Vec<ID> = roots . iter () . map ( |r| ID::from (*r) ) . collect ();
  let (buffer, pids, viewforest) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view (&f . config, None, &root_ids) ?;
  f . views_state . open_views . register_view (
    &f . graph . load_full (), viewforest_id . clone (), viewforest, &pids );
  Ok (buffer) }

/// Save 'buffer' as the view 'viewforest_id', with 'review'. Returns the
/// response and the collateral views streamed meanwhile, by viewforest id.
fn save (
  f       : &mut Fixture,
  buffer  : &str,
  viewforest_id : &ViewforestId,
  review  : &StrandedsReview,
) -> Result<(SaveResponse, HashMap<String, String>), Box<dyn Error>> {
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") ?;
  let mut stream : TcpStream = TcpStream::connect (listener . local_addr () ?) ?;
  let (read_end, _) = listener . accept () ?;
  let mut env : SkgEnv =
    skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let response : SaveResponse = block_on (
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, buffer, &mut env,  &Ok (viewforest_id . clone ()),
      &mut f . views_state, None, true, &HashMap::new (),
      &HashSet::new (), &HashSet::new (), review, &[] )) ?;
  // The test helper's env holds its own graph handle: carry the
  // swapped-in graph back to the fixture.
  f . graph . store ( env . runtime_snapshot () . graph . clone () );
  drop (stream);
  let collateral : HashMap<String, String> =
    read_all_lp_messages ( &mut BufReader::new (read_end) ) . into_iter ()
    . filter ( |message| message . contains ("collateral-view") )
    . filter_map ( |message| Some ((
      extract_string_field_from_sexp (&message, "view-id") ?,
      extract_string_field_from_sexp (&message, "content") ? )) )
    . collect ();
  Ok ((response, collateral)) }

fn relRepo_of (
  f        : &Fixture,
  recorder : &str,
  member   : &str,
) -> Option<SkgrepoName> {
  f . graph . load_full () . relRepo (
    &ID::from (recorder), NodeRelation::Contains, &ID::from (member) ) }

fn home_of (
  f    : &Fixture,
  node : &str,
) -> Option<SkgrepoName> {
  f . graph . load_full () . pid_and_skgrepo (&ID::from (node))
    . map ( |(_pid, home)| home ) }

fn q_contains_n () -> RelationshipKey {
  RelationshipKey { relation : NodeRelation::Contains,
                    recorder : ID::from ("q"), member : ID::from ("n") } }

#[test]
fn a_privatizing_move_raises_an_inbound_relationship (
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture ("skg-test-strandeds-privatizing") ?;
  let result : Result<(), Box<dyn Error>> = (|| {
    let view_m : ViewforestId = ViewforestId::ContentView ("m" . to_string ());
    let view_p : ViewforestId = ViewforestId::ContentView ("p" . to_string ());
    let buffer : String = open_view (&mut f, &["m"], &view_m) ?;
    open_view (&mut f, &["p"], &view_p) ?;
    let (response, collateral) : (SaveResponse, HashMap<String, String>) =
      save ( &mut f,
             &buffer . replace ("(gnid m) (skgRepos (nodeRepo public))", "(gnid m) (skgRepos (nodeRepo private))"),
             &view_m, &StrandedsReview::NotReviewed ) ?;
    assert_eq! ( response . strandeds_confirmation, None,
                 "a raise asks nothing" );
    assert_eq! ( home_of (&f, "m"), Some (SkgrepoName::from ("private")) );
    assert_eq! ( relRepo_of (&f, "p", "m"), Some (SkgrepoName::from ("private")),
                 "p's relationship to m rises with m" );
    assert! ( response . warnings . iter () . any ( |w|
                w . contains ("Raised the relationship by which p contains m") ),
              "{:?}", response . warnings );
    assert! ( collateral . get (&view_p . repr_in_client ()) . is_some_and (
                |content| content . contains ("(gnid m) (skgRepos (nodeRepo private))") ),
              "the view of p rerenders: {:?}", collateral );
    Ok (( )) }) ();
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }

#[test]
fn a_publicizing_move_asks_about_a_relationship_recorded_outside_the_view (
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture ("skg-test-strandeds-publicizing") ?;
  let result : Result<(), Box<dyn Error>> = (|| {
    let view_n : ViewforestId = ViewforestId::ContentView ("n" . to_string ());
    let buffer : String = open_view (&mut f, &["n"], &view_n) ?;
    let moved : String =
      buffer . replace ("(gnid n) (skgRepos (nodeRepo private))", "(gnid n) (skgRepos (nodeRepo public))");
    { // The first save asks, and changes nothing, consuming no request.
      let (response, _) : (SaveResponse, HashMap<String, String>) =
        save ( &mut f,
               &moved . replace ("(skgRepos (nodeRepo public))", "(skgRepos (nodeRepo public)) (diff (readyRequest all))"),
               &view_n, &StrandedsReview::NotReviewed ) ?;
      let stranded = response . strandeds_confirmation
        . expect ("the move strands q's relationship to n");
      assert_eq! ( stranded . len (), 1 );
      assert_eq! ( stranded [0] . relationship, q_contains_n () );
      assert_eq! ( stranded [0] . relRepo,     SkgrepoName::from ("private") );
      assert_eq! ( stranded [0] . new_default, SkgrepoName::from ("public") );
      assert_eq! ( home_of (&f, "n"), Some (SkgrepoName::from ("private")) );
      assert! ( ! readiness_marks_path (&f . config . data_root) . exists (),
                "a save that stops for confirmation consumes nothing" ); }
    { // The approving re-save resets the chosen relationship to default.
      let (response, _) : (SaveResponse, HashMap<String, String>) =
        save ( &mut f, &moved, &view_n,
               &StrandedsReview::Reviewed ( [q_contains_n ()] . into () )) ?;
      assert_eq! ( response . strandeds_confirmation, None );
      assert! ( response . errors . is_empty (), "{:?}", response . errors );
      assert_eq! ( home_of (&f, "n"), Some (SkgrepoName::from ("public")) );
      assert_eq! ( relRepo_of (&f, "q", "n"), Some (SkgrepoName::from ("public")) );
      assert! ( ! f . config . skgrepos [&SkgrepoName::from ("private")] . path
                  . join ("q.skg") . exists (),
                "q's telescope is rewritten without its private section" ); }
    Ok (( )) }) ();
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }

#[test]
fn a_reviewed_save_that_chooses_nothing_leaves_the_relationship_where_it_was (
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture ("skg-test-strandeds-declined") ?;
  let result : Result<(), Box<dyn Error>> = (|| {
    let view_n : ViewforestId = ViewforestId::ContentView ("n" . to_string ());
    let buffer : String = open_view (&mut f, &["n"], &view_n) ?;
    let (response, _) : (SaveResponse, HashMap<String, String>) =
      save ( &mut f,
             &buffer . replace ("(gnid n) (skgRepos (nodeRepo private))", "(gnid n) (skgRepos (nodeRepo public))"),
             &view_n, &StrandedsReview::Reviewed (HashSet::new ()) ) ?;
    assert_eq! ( response . strandeds_confirmation, None );
    assert_eq! ( home_of (&f, "n"), Some (SkgrepoName::from ("public")) );
    assert_eq! ( relRepo_of (&f, "q", "n"), Some (SkgrepoName::from ("private")) );
    Ok (( )) }) ();
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }
