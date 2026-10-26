// A buffer holding every one of the diff view's viewnodes, rendered
// from the graph straddle of a git fixture, crosses the wire: it
// parses, and prints back, keeping every one of them; saved as a
// content view, it changes no file.
// The fixture's HEAD and worktree differ so: a is retitled and its
// body edited; gone, which held gc, kept and the dangling ghost, is
// deleted with gc, and p no longer contains it; acq acquired
// 'acquired' by a nodeMerge. (One skgrepo cannot show a skgrepo move,
// so the two move properties are taken from made-up skgrepos.)

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture::{
  Fixture, OwnedSkgrepos, save_view, skg_files, with_git_fixture};

use skg::diff_view::change_leaves::{LeafKey, NodeAspect, RelationshipAspect};
use skg::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationRole, RelationshipKey};
use skg::graph_straddle::types::{GraphStraddle, Partner};
use skg::dbs::in_rust_graph::InRustGraph;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use skg::org_to_text::{viewforest_to_string, viewforest_tree_to_string};
use skg::diff_view::readiness_store::ReadinessStore;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::diff_view::change_viewnodes::{
  append_acquired_folder, append_old_text,
  mk_changeset_marker_viewnode, mk_historical_viewnode, mk_relRepo_move_viewnode};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgrepoChange, SkgrepoName};
use skg::types::tree::moves::{move_before, move_to_first_child};
use skg::types::viewnode::{
  ChangesetMarkerKind, Existence, ChangeViewnode, Folder, Viewnode, ViewnodeKind,
  Vognode, mk_unknown_viewnode, mk_writeProtected_viewnode, viewnode_of};
use skg::types::views_state::ViewforestId;

use ego_tree::{NodeId, Tree};
use std::collections::{BTreeMap, HashSet};
use std::error::Error;
use std::path::Path;
use std::sync::Arc;

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

/// The first vognode of 'tree' that represents 'gnid'.
fn treeid_of (
  tree  : &Tree<Viewnode>,
  gnid : &str,
) -> NodeId {
  tree . root () . descendants ()
    . find ( |node| matches! ( &node . value () . kind,
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) if t . gnid . 0 == gnid ))
    . unwrap_or_else ( || panic! ("no vognode {} in the view", gnid) )
    . id () }

/// Under 'parent', the historical phantom of 'pid' with its old
/// content, a deleted member of which with its own.
fn append_historical_phantom (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
  rel      : Option<Existence>,
) -> Option<NodeId> {
  let phantom : NodeId = tree . get_mut (parent) ? . append (
    mk_historical_viewnode (straddle, pid, rel) ? ) . id ();
  append_old_content (tree, phantom, straddle, pid) ?;
  let deleted_members : Vec<(NodeId, ID)> =
    tree . get (phantom) ? . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Ghost (h)) =>
        Some (( child . id (), h . gnid . clone () )),
      _ => None, } )
    . collect ();
  for (member, gnid) in deleted_members {
    append_old_content (tree, member, straddle, &gnid) ?; }
  Some (phantom) }

/// Under the ghost 'phantom' of 'pid', what the diff view shows there:
/// if a nodeMerge acquired it, an "it was acquired by" folder holding
/// its acquirer; then its content at HEAD, a deleted member as its
/// ghost, a surviving one as a write-protected occurrence whose
/// relationship HEAD alone has, and a dangling one as an unknown.
fn append_old_content (
  tree     : &mut Tree<Viewnode>,
  phantom  : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<()> {
  if let Some (acquirer) = straddle . acquirer_of (pid) {
    let node = straddle . after . get (&acquirer) ?;
    let folder : NodeId = tree . get_mut (phantom) ? . append (
      viewnode_of ( ViewnodeKind::Folder (Folder::AcquiredBy) )) . id ();
    tree . get_mut (folder) ? . append ( mk_writeProtected_viewnode (
      acquirer . clone (), node . home_skgrepo . clone (), node . title . clone (), true )); }
  for partner in straddle . partners (pid, RelationRole::CONTENT) {
    let member : Viewnode =
      if straddle . node_existence (&partner . id) == Some (Existence::Removed) {
        mk_historical_viewnode (straddle, &partner . id, Some (Existence::Removed)) ?
      } else {
        let mut member : Viewnode = match straddle . after . get (&partner . id) {
          Some (node) => mk_writeProtected_viewnode (
            partner . id . clone (), node . home_skgrepo . clone (),
            node . title . clone (), false ),
          None => mk_unknown_viewnode ( partner . id . clone () ), };
        member . change_facts . change =
          straddle . node_change (&partner . id, Some (Existence::Removed));
        member };
    tree . get_mut (phantom) ? . append (member); }
  Some (( )) }

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

/// The diff view's viewnodes cross the wire: the buffer parses
/// without errors, and printing what was parsed gives a text that
/// parses and prints back the same, holding every one of them. The
/// parser skips the change facts the diff view derives, so only the
/// buffer as drawn holds those. Saved as a content view, which keeps
/// no ghost, the buffer changes no file.
#[test]
fn the_new_viewnodes_cross_the_wire (
) -> Result<(), Box<dyn Error>> {
  with_fixture ( |f| {
    let buffer : String = buffer_with_every_new_viewnode (f) ?;
    let atoms : [&str; 14] = [
      "ghost (gnid gone) (skgRepos (nodeRepo main))",
      "ghost (gnid gc) (skgRepos (nodeRepo main))",
      ") kept", // a surviving member under gone
      "unknown (gnid ghost)",
      "(oldText titleAndBody)", "bodyDiff",
      "(homeSkgrepoMove (old main) (new elsewhere))",
      "(relRepoMove (old main) (new elsewhere))",
      "(changesetMarker content)", "(changesetMarker relRepo)",
      "(changesetMarker orderOnly)",
      "acquiredFolder", "acquiredByFolder",
      "ghost (gnid acquired) (skgRepos (nodeRepo main))" ];
    let change_facts : [&str; 4] = [
      "ghost (gnid gone) (skgRepos (nodeRepo main)) noClaim (diff (node removed) (rel removed))",
      "ghost (gnid gc) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed))",
      "(diff (rel removed))) kept",
      "ghost (gnid acquired) (skgRepos (nodeRepo main)) (diff (node removed))" ];
    for facts in change_facts {
      assert! ( without_viewnode_ids (&buffer) . contains (facts),
                "the buffer should hold {}:\n{}", facts, buffer ); }
    let graph : Arc<InRustGraph> = f . graph . load_full ();
    let across_the_wire = |text : &str| -> String {
      let (forest, errors, _warnings, _provenance) =
        org_to_viewforest (text, &graph) . unwrap ();
      assert! ( errors . is_empty (), "{:?}\n\nbuffer:\n{}", errors, text );
      viewforest_to_string (&forest, &f . config) . unwrap () };
    let printed : String = across_the_wire (&buffer);
    for atom in atoms {
      assert! ( without_viewnode_ids (&printed) . contains (atom),
                "the printed buffer should hold {}:\n{}", atom, printed ); }
    assert_eq! ( across_the_wire (&printed), printed,
                 "a printed buffer crosses the wire unchanged" );
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &buffer) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before,
                 "saving the buffer unchanged must not change the graph" );
    assert! ( ! response . saved_view . contains ("ghost (gnid "),
              "a content view keeps no ghost:\n{}", response . saved_view );
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
                  "(skg (oldText titleAndBody) (editRequest (ready)))" )
      . replace ( "ghost (gnid gone) (skgRepos (nodeRepo main)) noClaim (diff (node removed) (rel removed))",
                  "ghost (gnid gone) (skgRepos (nodeRepo main)) noClaim (editRequest (ready node)) (diff (node removed) (rel removed))" );
    let response : SaveResponse = save (f, &buffer) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
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
    // A content view keeps no change viewnode, so the request is made
    // on the buffer as drawn.
    let response : SaveResponse = save ( f,
      &buffer_with_every_new_viewnode (f) ? . replacen (
        "(changesetMarker content))",
        "(changesetMarker content) (editRequest (ready all)))", 1 )) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
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
* (skg node (gnid p) (skgRepos (nodeRepo main))) p
{}** (skg node (gnid a) (skgRepos (nodeRepo main))) a, retitled
** (skg node (gnid b) (skgRepos (nodeRepo main))) b
** (skg node (gnid acq) (skgRepos (nodeRepo main))) acq
", misplaced )) {
        Err (e) => format! ("{:?}", e),
        Ok (response) => format! ("{:?}", response . errors), };
      assert! ( refusal . contains (error),
                "expected {:?} for\n{}\ngot {}", error, misplaced, refusal ); }
    Ok (( )) }) }
