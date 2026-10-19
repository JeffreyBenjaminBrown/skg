// A buffer holding every one of the total diff view's viewnodes,
// drawn from the graph straddle of a git fixture, saves back unchanged:
// no graph change, no validation error, and every one of them kept.
// The buffer the save returns is a fixed point: saved again, it comes
// back the same. (The first save's view completion adds the display
// facts the builders leave out, and moves a historical phantom ahead
// of the live content it stood among; positioning removed members is
// the total diff view's reconciler's job.)
// The fixture's HEAD and worktree differ so: a is retitled and its
// body edited; gone, which held gc, kept and the dangling ghost, is
// deleted with gc, and p no longer contains it; acq acquired
// 'acquired' by a nodeMerge. (One skgrepo cannot show a skgrepo move,
// so the two move properties are drawn from made-up skgrepos.)

#[path = "../../shared/git_fixture_helpers.rs"]
#[allow(unused_imports, dead_code)] // shared by several test targets, each using part of it
mod git_fixture_helpers;

use git_fixture_helpers::{mk_test_tcp_stream_pair, setup_gitrepo_with_fixtures};

use skg::change_forest::{LeafKey, NodeAspect, RelationshipAspect};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::in_rust_graph::relation_accessors::NodeRelation;
use skg::graph_straddle::types::{GraphStraddle, Partner, Presence};
use skg::org_to_text::viewforest_tree_to_string;
use skg::readiness_store::ReadinessStore;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  cleanup_test_tantivy, graph_handle_from_config, setup_test_tantivy,
  update_from_and_rerender_buffer_test};
use skg::to_org::render::change_viewnodes::{
  append_acquired_folder, append_historical_phantom_content, append_old_text,
  mk_changeset_marker_viewnode, mk_historical_viewnode, mk_relRepo_move_viewnode};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::git::Sign;
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
use skg::types::tree::moves::{move_before, move_to_first_child};
use skg::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, Phantom, Property, Viewnode, ViewnodeKind,
  Vognode};
use skg::types::views_state::{OpenViews, ViewId};

use ego_tree::{NodeId, Tree};
use futures::executor::block_on;
use std::collections::{BTreeMap, HashSet};
use std::error::Error;
use std::fs;
use std::net::TcpStream;
use std::path::PathBuf;
use tempfile::TempDir;

const FIXTURES : &str = "tests/tdv/roundtrip/fixtures";

struct Fixture {
  _root       : TempDir,
  skgrepo     : PathBuf,
  config      : SkgConfig,
  tantivy     : TantivyIndex,
  graph       : InRustGraphHandle,
  views_state : ViewsState,
}

fn fixture (
  test_name : &str,
) -> Result<Fixture, Box<dyn Error>> {
  let root : TempDir = TempDir::new () ?;
  let skgrepo : PathBuf = root . path () . join ("main");
  setup_gitrepo_with_fixtures (
    &skgrepo,
    &format! ("{}/head", FIXTURES),
    &format! ("{}/worktree", FIXTURES) ) ?;
  let (mut config, tantivy) : (SkgConfig, TantivyIndex) =
    setup_test_tantivy (
      test_name,
      skgrepo . to_str () . unwrap (),
      root . path () . join ("tantivy") . to_str () . unwrap () ) ?;
  config . data_root = root . path () . to_path_buf ();
  let graph : InRustGraphHandle = graph_handle_from_config (&config) ?;
  Ok ( Fixture {
    _root : root, skgrepo, config, tantivy, graph,
    views_state : ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None, } } ) }

fn save (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = block_on (
    update_from_and_rerender_buffer_test (
      &mut stream, buffer, &f . config, &f . tantivy, &f . graph,
      &Ok ( ViewId::ContentView ("tdv-roundtrip" . to_string ()) ),
      &mut f . views_state )) ?;
  Ok (response) }

/// Each '.skg' file of the skgrepo, by name, with its contents.
fn skg_files (
  f : &Fixture,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for entry in fs::read_dir (&f . skgrepo) ? {
    let path : PathBuf = entry ? . path ();
    if path . extension () . is_some_and ( |ext| ext == "skg" ) {
      files . insert (
        path . file_name () . unwrap () . to_string_lossy () . to_string (),
        fs::read_to_string (&path) ? ); }}
  Ok (files) }

/// The first vognode of 'tree' that represents 'skgid'.
fn treeid_of (
  tree  : &Tree<Viewnode>,
  skgid : &str,
) -> NodeId {
  tree . root () . descendants ()
    . find ( |node| matches! ( &node . value () . kind,
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) if t . skgid . 0 == skgid ))
    . unwrap_or_else ( || panic! ("no vognode {} in the view", skgid) )
    . id () }

/// Under 'parent', the historical phantom of 'pid' with its old
/// content, a deleted member of which with its own.
fn append_historical_phantom (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
  rel      : Option<Sign>,
) -> Option<NodeId> {
  let phantom : NodeId = tree . get_mut (parent) ? . append (
    mk_historical_viewnode (straddle, pid, rel) ? ) . id ();
  append_historical_phantom_content (tree, phantom, straddle, pid) ?;
  let deleted_members : Vec<(NodeId, ID)> =
    tree . get (phantom) ? . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
        Some (( child . id (), h . skgid . clone () )),
      _ => None, } )
    . collect ();
  for (member, skgid) in deleted_members {
    append_historical_phantom_content (tree, member, straddle, &skgid) ?; }
  Some (phantom) }

/// The view of p, with every one of the total diff view's viewnodes
/// added where the total diff view draws it.
fn buffer_with_every_new_viewnode (
  f : &Fixture,
) -> Result<String, Box<dyn Error>> {
  let straddle : GraphStraddle =
    GraphStraddle::from_head_and_live (&f . config, f . graph . load_full ()) ?;
  let (_, _, mut tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view (&f . config, None, &[ ID::from ("p") ]) ?;
  let p : NodeId = treeid_of (&tree, "p");
  let b : NodeId = treeid_of (&tree, "b");
  { // gone, a removed member of p, where it stood: before b.
    let gone : NodeId = append_historical_phantom (
      &mut tree, p, &straddle, &ID::from ("gone"), Some (Sign::Minus) )
      . expect ("gone is in the before-graph only");
    move_before (&mut tree, b, gone) ?; }
  for kind in ChangesetMarkerKind::ALL . into_iter () . rev () {
    tree . get_mut (p) . unwrap () . prepend (
      mk_changeset_marker_viewnode (kind) ); }
  { let a : NodeId = treeid_of (&tree, "a");
    append_old_text (&mut tree, a, &straddle, &ID::from ("a"))
      . expect ("a's text changed"); }
  { let mut b_mut = tree . get_mut (b) . unwrap ();
    b_mut . append ( Viewnode {
      focused : false, folded : false, body_folded : false,
      kind : ViewnodeKind::Property ( Property::HomeSkgrepoMove {
        old : SkgrepoName::from ("main"), new : SkgrepoName::from ("elsewhere"),
        change_facts : ChangeFacts::default (), readiness_request : None } ) } );
    b_mut . append ( mk_relRepo_move_viewnode ( &Partner {
      id : ID::from ("b"), presence : Presence::Both,
      relRepo_before : Some (SkgrepoName::from ("main")),
      relRepo_after  : Some (SkgrepoName::from ("elsewhere")) } )
      . expect ("the relationship moved") ); }
  { let acq : NodeId = treeid_of (&tree, "acq");
    let folder : NodeId =
      append_acquired_folder (&mut tree, acq, &straddle, &ID::from ("acq"))
      . expect ("acq acquired 'acquired'");
    move_to_first_child (&mut tree, acq, folder) ?; }
  { // The acquiree, a second root.
    let root : NodeId = tree . root () . id ();
    append_historical_phantom (
      &mut tree, root, &straddle, &ID::from ("acquired"), None )
      . expect ("'acquired' is in the before-graph only"); }
  Ok ( viewforest_tree_to_string (&tree, &f . config) ? ) }

#[test]
fn a_buffer_of_the_new_viewnodes_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture ("skg-test-tdv-roundtrip") ?;
  let result : Result<(), Box<dyn Error>> = (|| {
    let buffer : String = buffer_with_every_new_viewnode (&f) ?;
    let atoms : [&str; 14] = [
      "(historical (id gone) (repo main) (diff node- rel-))",
      "(historical (id gc) (repo main) (diff node- rel-))",
      "(diff rel-))) kept", // a surviving member under gone
      "(unknown (id ghost) (diff rel-))",
      "(oldText titleAndBody)", "bodyDiff",
      "(homeMove (old main) (new elsewhere))",
      "(relRepoMove (old main) (new elsewhere))",
      "(changesetMarker content)", "(changesetMarker relRepo)",
      "(changesetMarker orderOnly)",
      "acquiredFolder", "acquiredByFolder",
      "(historical (id acquired) (repo main) (diff node-))" ];
    let files_before : BTreeMap<String, String> = skg_files (&f) ?;
    let mut saved : String = buffer;
    for _ in 0 .. 2 {
      for atom in atoms {
        assert! ( saved . contains (atom),
                  "the buffer should hold {}:\n{}", atom, saved ); }
      let response : SaveResponse = save (&mut f, &saved) ?;
      assert! ( response . errors . is_empty (),
                "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
                response . errors, saved );
      assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
      assert_eq! ( skg_files (&f) ?, files_before,
                   "saving the buffer unchanged must not change the graph" );
      saved = response . saved_view; }
    let response : SaveResponse = save (&mut f, &saved) ?;
    assert_eq! ( response . saved_view, saved,
                 "a saved buffer, saved again, comes back the same" );
    Ok (( )) }) ();
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }

/// A readiness request on the old text names the node's text leaf;
/// on a historical phantom's node axis, its existence; on the content
/// changeset marker ('all'), the relationship leaves of its node's
/// content.
#[test]
fn readiness_requests_on_the_new_viewnodes_name_their_leaves (
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture ("skg-test-tdv-roundtrip-readiness") ?;
  let result : Result<(), Box<dyn Error>> = (|| {
    let buffer : String = buffer_with_every_new_viewnode (&f) ?
      . replace ( "(skg (oldText titleAndBody))",
                  "(skg (oldText titleAndBody) (readyRequest))" )
      . replace ( "(historical (id gone) (repo main) (diff node- rel-))",
                  "(historical (id gone) (repo main) (diff node- rel-) (readyRequest node))" );
    let response : SaveResponse = save (&mut f, &buffer) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    let p_contains_gone : LeafKey = LeafKey::Relationship {
      relation : NodeRelation::Contains,
      recorder : ID::from ("p"), member : ID::from ("gone"),
      aspect   : RelationshipAspect::Existence };
    assert_eq! (
      ReadinessStore::load (&f . config . data_root) . marks
        . into_keys () . collect::<HashSet<LeafKey>> (),
      [ LeafKey::Node { pid : ID::from ("a"), aspect : NodeAspect::Text },
        LeafKey::Node { pid : ID::from ("gone"), aspect : NodeAspect::Existence } ]
      . into_iter () . collect () );
    assert! ( ! response . saved_view . contains ("Request"),
              "the save consumes its requests:\n{}", response . saved_view );
    let response : SaveResponse = save ( &mut f,
      &response . saved_view . replace (
        "(skg (changesetMarker content))",
        "(skg (changesetMarker content) (readyRequest all))" )) ?;
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    assert! ( ReadinessStore::load (&f . config . data_root) . marks
              . contains_key (&p_contains_gone) );
    Ok (( )) }) ();
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }

/// The new viewnodes are save-inert, but the validator keeps each
/// where the total diff view draws it.
#[test]
fn the_new_viewnodes_are_refused_out_of_place (
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture ("skg-test-tdv-roundtrip-out-of-place") ?;
  let result : Result<(), Box<dyn Error>> = (|| {
    for (misplaced, error) in [
      ( "** (skg bodyDiff)\n",          "bodyDiff must have an oldText parent" ),
      ( "** (skg acquiredByFolder)\n",  "acquiredByFolder has the wrong kind of parent" ),
      ( "** (skg (changesetMarker content))\n** (skg (changesetMarker content))\n",
        "changesetMarker of each kind must be unique" ), ] {
      let refusal : String = match save ( &mut f, &format! ( "\
* (skg (node (id p) (repo main))) p
{}** (skg (node (id a) (repo main))) a, retitled
** (skg (node (id b) (repo main))) b
** (skg (node (id acq) (repo main))) acq
", misplaced )) {
        Err (e) => format! ("{:?}", e),
        Ok (response) => format! ("{:?}", response . errors), };
      assert! ( refusal . contains (error),
                "expected {:?} for\n{}\ngot {}", error, misplaced, refusal ); }
    Ok (( )) }) ();
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }
