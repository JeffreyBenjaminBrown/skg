// A buffer holding every one of the diff view's viewnodes,
// rendered from the graph straddle of a git fixture, saves back unchanged:
// no graph change, no validation error, and every one of them kept.
// The buffer the save returns is a fixed point: saved again, it comes
// back the same. (The first save's view completion adds the display
// facts the builders leave out, and moves a historical phantom ahead
// of the live content it stood among; positioning removed members is
// the diff view's reconciler's job.)
// The fixture's HEAD and worktree differ so: a is retitled and its
// body edited; gone, which held gc, kept and the dangling ghost, is
// deleted with gc, and p no longer contains it; acq acquired
// 'acquired' by a nodeMerge. (One skgrepo cannot show a skgrepo move,
// so the two move properties are taken from made-up skgrepos.)

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture::{
  Fixture, OwnedSkgrepos, save_view, skg_files, with_git_fixture};

use skg::diff_view::change_leaves::{LeafKey, NodeAspect, RelationshipAspect};
use skg::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use skg::graph_straddle::types::{GraphStraddle, Partner};
use skg::org_to_text::viewforest_tree_to_string;
use skg::diff_view::readiness_store::ReadinessStore;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::diff_view::change_viewnodes::{
  append_acquired_folder, append_historical_phantom_content, append_old_text,
  mk_changeset_marker_viewnode, mk_historical_viewnode, mk_relRepo_move_viewnode};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgrepoChange, SkgrepoName};
use skg::types::tree::moves::{move_before, move_to_first_child};
use skg::types::viewnode::{
  ChangesetMarkerKind, Existence, ChangeViewnode, Viewnode, ViewnodeKind,
  Vognode, viewnode_of};
use skg::types::views_state::ViewforestId;

use ego_tree::{NodeId, Tree};
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::error::Error;
use std::path::Path;

const FIXTURES : &str = "tests/diff_view/roundtrip/fixtures";

/// Save 'buffer' as a content view.
fn save (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  save_view ( f, &ViewforestId::ContentView ("diffView-roundtrip" . to_string ()), buffer ) }

/// Run 'body' on the fixture; see 'with_git_fixture'.
fn with_fixture (
  body : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  with_git_fixture ( Path::new (FIXTURES), OwnedSkgrepos::OnlyMain, &[], body ) }

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
/// content, a deleted member of which with its own. The view is a
/// content view, which has no U, so its removed members are
/// write-protected.
fn append_historical_phantom (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
  rel      : Option<Existence>,
) -> Option<NodeId> {
  let phantom : NodeId = tree . get_mut (parent) ? . append (
    mk_historical_viewnode (straddle, pid, rel) ? ) . id ();
  let no_u : BTreeSet<ID> = BTreeSet::new ();
  append_historical_phantom_content (tree, phantom, straddle, pid, &no_u) ?;
  let deleted_members : Vec<(NodeId, ID)> =
    tree . get (phantom) ? . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Ghost (h)) =>
        Some (( child . id (), h . skgid . clone () )),
      _ => None, } )
    . collect ();
  for (member, skgid) in deleted_members {
    append_historical_phantom_content (tree, member, straddle, &skgid, &no_u) ?; }
  Some (phantom) }

/// The view of p, with every one of the diff view's viewnodes
/// added where the diff view shows it.
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
      &mut tree, p, &straddle, &ID::from ("gone"), Some (Existence::Removed) )
      . expect ("gone is in the before-graph only");
    move_before (&mut tree, b, gone) ?; }
  for kind in ChangesetMarkerKind::ALL . into_iter () . rev () {
    tree . get_mut (p) . unwrap () . prepend (
      mk_changeset_marker_viewnode (kind) ); }
  { let a : NodeId = treeid_of (&tree, "a");
    append_old_text (&mut tree, a, &straddle, &ID::from ("a"))
      . expect ("a's text changed"); }
  { let mut b_mut = tree . get_mut (b) . unwrap ();
    b_mut . append ( viewnode_of (ViewnodeKind::ChangeViewnode ( ChangeViewnode::HomeSkgrepoMove {
        skgrepo_change : SkgrepoChange { old : SkgrepoName::from ("main"),
                                         new : SkgrepoName::from ("elsewhere") } } )) );
    b_mut . append ( mk_relRepo_move_viewnode ( &Partner {
      id : ID::from ("b"), existence : None,
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
  with_fixture ( |f| {
    let buffer : String = buffer_with_every_new_viewnode (f) ?;
    let atoms : [&str; 14] = [
      "(ghost (gnid gone) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed)))",
      "(ghost (gnid gc) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed)))",
      "(diff (rel removed)))) kept", // a surviving member under gone
      "(unknown (id ghost) (diff (rel removed)))",
      "(oldText titleAndBody)", "bodyDiff",
      "(homeSkgrepoMove (old main) (new elsewhere))",
      "(relRepoMove (old main) (new elsewhere))",
      "(changesetMarker content)", "(changesetMarker relRepo)",
      "(changesetMarker orderOnly)",
      "acquiredFolder", "acquiredByFolder",
      "(ghost (gnid acquired) (skgRepos (nodeRepo main)) (diff (node removed)))" ];
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let mut saved : String = buffer;
    for _ in 0 .. 2 {
      for atom in atoms {
        assert! ( without_viewnode_ids (&saved) . contains (atom),
                  "the buffer should hold {}:\n{}", atom, saved ); }
      let response : SaveResponse = save (f, &saved) ?;
      assert! ( response . errors . is_empty (),
                "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
                response . errors, saved );
      assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
      assert_eq! ( skg_files (f) ?, files_before,
                   "saving the buffer unchanged must not change the graph" );
      saved = response . saved_view; }
    let response : SaveResponse = save (f, &saved) ?;
    assert_eq! ( response . saved_view, saved,
                 "a saved buffer, saved again, comes back the same" );
    Ok (( )) }) }

/// A readiness request on the old text names the node's text leaf;
/// on a historical phantom's node object, its existence; on the content
/// changeset marker ('all'), the relationship leaves of its node's
/// content.
#[test]
fn readiness_requests_on_the_new_viewnodes_name_their_leaves (
) -> Result<(), Box<dyn Error>> {
  with_fixture ( |f| {
    let buffer : String = buffer_with_every_new_viewnode (f) ?
      . replace ( "(skg (oldText titleAndBody))",
                  "(skg (oldText titleAndBody) (diff (readyRequest)))" )
      . replace ( "(ghost (gnid gone) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed)))",
                  "(ghost (gnid gone) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed) (readyRequest node)))" );
    let response : SaveResponse = save (f, &buffer) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    let p_contains_gone : LeafKey = LeafKey::Relationship {
      relationship : RelationshipKey {
        relation : NodeRelation::Contains,
        recorder : ID::from ("p"), member : ID::from ("gone") },
      aspect       : RelationshipAspect::Existence };
    assert_eq! (
      ReadinessStore::load (&f . config . data_root) . marks
        . into_keys () . collect::<HashSet<LeafKey>> (),
      [ LeafKey::Node { pid : ID::from ("a"), aspect : NodeAspect::Text },
        LeafKey::Node { pid : ID::from ("gone"), aspect : NodeAspect::Existence } ]
      . into_iter () . collect () );
    assert! ( ! without_viewnode_ids (&response . saved_view) . contains ("Request"),
              "the save consumes its requests:\n{}", response . saved_view );
    let response : SaveResponse = save ( f,
      &response . saved_view . replace (
        "(skg (changesetMarker content) (vnid",
        "(skg (changesetMarker content) (diff (readyRequest all)) (vnid" )) ?;
    assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
    assert! ( ReadinessStore::load (&f . config . data_root) . marks
              . contains_key (&p_contains_gone) );
    Ok (( )) }) }

/// The new viewnodes are save-inert, but the validator keeps each
/// where the diff view shows it.
#[test]
fn the_new_viewnodes_are_refused_out_of_place (
) -> Result<(), Box<dyn Error>> {
  with_fixture ( |f| {
    for (misplaced, error) in [
      ( "** (skg bodyDiff)\n",          "bodyDiff must have an oldText parent" ),
      ( "** (skg acquiredByFolder)\n",  "acquiredByFolder has the wrong kind of parent" ),
      ( "** (skg (changesetMarker content))\n** (skg (changesetMarker content))\n",
        "changesetMarker of each kind must be unique" ), ] {
      let refusal : String = match save ( f, &format! ( "\
* (skg (node (id p) (repo main))) p
{}** (skg (node (id a) (repo main))) a, retitled
** (skg (node (id b) (repo main))) b
** (skg (node (id acq) (repo main))) acq
", misplaced )) {
        Err (e) => format! ("{:?}", e),
        Ok (response) => format! ("{:?}", response . errors), };
      assert! ( refusal . contains (error),
                "expected {:?} for\n{}\ngot {}", error, misplaced, refusal ); }
    Ok (( )) }) }
