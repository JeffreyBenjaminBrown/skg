// In a buffer of the total diff view every node of U stays editable
// somewhere: a save that write-protects the only editable occurrence
// of a node of U, and requests no other occurrence editable, is
// refused ('TODO/total-diff-view/6_plan-more.org', "A save may not
// leave a changed node without an editable instance"). The 'shared'
// fixture (one skgrepo, main): x and y, whose bodies changed, each
// contain n, which contains a, whose body changed, and b, unchanged.
// So n, a node of U, is drawn editable under x and write-protected
// under y.

use crate::pcsvf::common::{
  Fixture, node_lines, open_all_changes_buffer, requesting_editable_at, save,
  view_id_of, with_fixture};
use crate::pcsvf::common::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::update_from_and_rerender_buffer_test;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::ID;
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::{TdvBuffer, ViewId};
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;
use futures::executor::block_on;
use std::error::Error;
use std::net::TcpStream;

const N_UNDER_Y : &str = "** (skg (node (id n) (repo main) writeProtected ";

/// 'buffer' with the first line at 'depth' (its leading stars) that
/// draws 'node' editable made write-protected, as
/// 'skg-set-write-protected' does.
fn write_protecting_at (
  buffer : &str,
  depth  : &str,
  node   : &str,
) -> String {
  let atom : String = format! ("(node (id {}) (repo main) ", node);
  let mut found : bool = false;
  let lines : Vec<String> = buffer . split_inclusive ('\n')
    . map ( |line|
      if ! found && line . starts_with (&format! ("{} (skg ", depth))
         && line . contains (&atom) && ! line . contains ("writeProtected")
      { found = true;
        line . replacen ( "(repo main) ", "(repo main) writeProtected ", 1 ) }
      else { line . to_string () } )
    . collect ();
  assert! ( found, "no editable {} at depth {:?}:\n{}", node, depth, buffer );
  lines . concat () }

fn the_registered_viewforest (
  f : &Fixture,
) -> ViewForest {
  f . views_state . open_views
    . viewid_to_view ( &view_id_of (TdvBuffer::AllChanges) )
    . expect ("the buffer is registered")
    . clone () }

/// Write-protecting n under x leaves n, a node of U, with no editable
/// occurrence, so the save is refused, naming n, and the buffer's
/// registered viewforest is as the last rendering left it.
#[test]
fn write_protecting_the_only_editable_occurrence_of_a_node_of_u_is_refused (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let registered : ViewForest = the_registered_viewforest (f);
    let errors : Vec<BufferValidationError> =
      match save ( f, &write_protecting_at (&buffer, "**", "n") ) {
        Err (error) => match error . downcast::<SaveError> () {
          Ok (save_error) => match *save_error {
            SaveError::BufferValidationErrors { errors, .. } => errors,
            other => panic! ( "expected validation errors, got {:?}", other ), },
          Err (other) => panic! ( "expected a SaveError, got {:?}", other ), },
        Ok (response) => panic! (
          "the save must be refused, got errors {:?}:\n{}",
          response . errors, response . saved_view ), };
    assert! ( matches! (
      &errors [..],
      [ BufferValidationError::NodeOfU_LeftWithoutEditableOccurrence {
          skgid, title } ]
      if *skgid == ID::from ("n") && title == "n" ),
      "{:?}", errors );
    assert! ( errors [0] . to_string () . contains (
                "n (n) would have no editable instance left" ),
              "{}", errors [0] );
    assert! ( the_registered_viewforest (f) == registered,
              "a refused save leaves the registered viewforest alone" );
    Ok (( )) }) }

/// The same save, with a request on n under y, moves editability
/// there: n is editable under y and write-protected under x.
#[test]
fn write_protecting_one_occurrence_and_requesting_another_moves_editability (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = requesting_editable_at (
      &write_protecting_at (&buffer, "**", "n"), N_UNDER_Y );
    let response : SaveResponse = save (f, &edited) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    let saved : String = response . saved_view;
    let n_lines : Vec<&str> = node_lines (&saved, "n");
    assert! ( saved . find ("* (skg (node (id x) ")
              < saved . find ("* (skg (node (id y) ")
              && n_lines . len () == 2,
              "n must be drawn under x, then under y:\n{}", saved );
    assert! ( n_lines [0] . contains ("writeProtected"), "{}", saved );
    assert! ( ! n_lines [1] . contains ("writeProtected"), "{}", saved );
    assert! ( ! saved . contains ("viewRequests"), "{}", saved );
    Ok (( )) }) }

/// b is outside U. Once a request on n under y made b editable there
/// (the cascade), write-protecting that occurrence, b's only editable
/// one, saves.
#[test]
fn write_protecting_a_node_outside_u_saves (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let moved : String = save ( f, &requesting_editable_at (&buffer, N_UNDER_Y) ) ?
      . saved_view;
    assert! ( node_lines (&moved, "b") . iter ()
                . any ( |line| ! line . contains ("writeProtected") ),
              "b must be editable under y:\n{}", moved );
    let response : SaveResponse =
      save ( f, &write_protecting_at (&moved, "***", "b") ) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert! ( node_lines (&response . saved_view, "b") . iter ()
                . all ( |line| line . contains ("writeProtected") ),
              "{}", response . saved_view );
    Ok (( )) }) }

/// An ordinary content view is not a buffer of the total diff view:
/// write-protecting n, its root and n's only editable occurrence,
/// saves, though n is a node of U.
#[test]
fn in_a_content_view_write_protecting_the_only_editable_occurrence_saves (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let view_id : ViewId = ViewId::ContentView ("content-view-of-n" . to_string ());
    let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from ("n") ] ) ?;
    f . views_state . open_views . register_view (
      &f . graph . load_full (), view_id . clone (), tree, &pids );
    assert! ( text . starts_with ("* (skg (node (id n) (repo main) ")
              && ! text . lines () . next () . unwrap () . contains ("writeProtected"),
              "n must be the editable root:\n{}", text );
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    let response : SaveResponse = block_on (
      update_from_and_rerender_buffer_test (
        &mut stream, &write_protecting_at (&text, "*", "n"),
        &f . config, &f . tantivy, &f . graph,
        &Ok (view_id), &mut f . views_state )) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert! ( response . saved_view . lines () . next () . unwrap ()
                . contains ("writeProtected"),
              "{}", response . saved_view );
    Ok (( )) }) }
