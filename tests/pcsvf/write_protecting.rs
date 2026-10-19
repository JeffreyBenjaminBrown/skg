// A save judges a write-protected occurrence by the graph alone
// ('errors_for_writeProtected_occurrences'), so in a buffer of the
// total diff view a save may write-protect the only editable
// occurrence of a node of U. The 'shared' fixture (one skgrepo,
// main): x and y, whose bodies changed, each contain n, which
// contains a, whose body changed, and b, unchanged. So n, a node of
// U, is drawn editable under x and write-protected under y.

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
/// draws 'node' editable made write-protected, without its body, and
/// its child vognodes made non-members, since a write-protected
/// occurrence holds none.
fn write_protecting_at (
  buffer : &str,
  depth  : &str,
  node   : &str,
) -> String {
  let atom : String = format! ("(node (id {}) (repo main) ", node);
  let child_depth : String = format! ("{}*", depth);
  let mut found : bool = false;
  let mut in_subtree : bool = false;
  let lines : Vec<String> = buffer . split_inclusive ('\n')
    . map ( |line| {
      let stars : &str = line . split (' ') . next () . unwrap_or ("");
      if in_subtree && stars . len () <= depth . len ()
         && line . starts_with ('*')
      { in_subtree = false; }
      if ! found && line . starts_with (&format! ("{} (skg ", depth))
         && line . contains (&atom) && ! line . contains ("writeProtected")
      { found = true;
        in_subtree = true;
        line . replacen ( "(repo main) ", "(repo main) writeProtected ", 1 ) }
      else if in_subtree && stars == child_depth
              && line . contains ("(skg (node ")
      { line . replacen ( "(repo main) ", "(repo main) (affectsParent false) ", 1 ) }
      else { line . to_string () } } )
    . collect ();
  assert! ( found, "no editable {} at depth {:?}:\n{}", node, depth, buffer );
  let mut in_body : bool = false;
  lines . into_iter ()
    . filter ( |line| {
      if line . starts_with ('*') {
        in_body = line . contains ("writeProtected ")
                  && line . contains (&atom . replacen (
                       "(repo main) ", "(repo main) writeProtected ", 1 ));
        return true; }
      ! in_body } )
    . collect::<Vec<String>> () . concat () }

fn the_registered_viewforest (
  f : &Fixture,
) -> ViewForest {
  f . views_state . open_views
    . viewid_to_view ( &view_id_of (TdvBuffer::AllChanges) )
    . expect ("the buffer is registered")
    . clone () }

/// The errors of a save that must be refused.
fn refusal_errors (
  f    : &mut Fixture,
  text : &str,
) -> Vec<BufferValidationError> {
  match save (f, text) {
    Err (error) => match error . downcast::<SaveError> () {
      Ok (save_error) => match *save_error {
        SaveError::BufferValidationErrors { errors, .. } => errors,
        other => panic! ( "expected validation errors, got {:?}", other ), },
      Err (other) => panic! ( "expected a SaveError, got {:?}", other ), },
    Ok (response) => panic! (
      "the save must be refused, got errors {:?}:\n{}",
      response . errors, response . saved_view ), } }

/// c, typed under a and so created, is a node of U whose creation is
/// marked ready. Write-protecting its only editable occurrence saves,
/// and c keeps its readiness facts.
#[test]
fn write_protecting_the_only_editable_occurrence_of_a_node_of_u_saves (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let b_under_x : usize = buffer . find ("*** (skg (node (id b) ")
      . expect ("b is drawn under n under x");
    let created : String = save ( f, &format! (
      "{}**** (skg (node (id c) (repo main))) c\n{}",
      &buffer [.. b_under_x], &buffer [b_under_x ..] )) ? . saved_view;
    let c_line : &str = node_lines (&created, "c") [0];
    let marked : String = save ( f, &created . replacen (
      c_line, &c_line . replacen (
        "(repo main) ", "(repo main) (readyRequest all) ", 1 ), 1 )) ?
      . saved_view;
    assert! ( node_lines (&marked, "c") [0] . contains ("(ready "), "{}", marked );
    let response : SaveResponse =
      save ( f, &write_protecting_at (&marked, "****", "c") ) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert! ( node_lines (&response . saved_view, "c") . iter ()
                . all ( |line| line . contains ("(ready ") ),
              "{}", response . saved_view );
    Ok (( )) }) }

/// A child typed under n under y, a write-protected occurrence, is a
/// member, so the save is refused, naming n, and the buffer's
/// registered viewforest is as the last rendering left it. Marked as
/// not affecting its parent, the child saves.
#[test]
fn a_member_child_typed_under_a_writeProtected_occurrence_is_refused (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let registered : ViewForest = the_registered_viewforest (f);
    let n_under_y_line : &str = buffer . lines ()
      . find ( |line| line . starts_with (N_UNDER_Y) )
      . expect ("n is drawn write-protected under y");
    let typing = |child : &str| -> String {
      buffer . replacen ( n_under_y_line,
                          &format! ("{}\n{}", n_under_y_line, child), 1 ) };
    let errors : Vec<BufferValidationError> =
      refusal_errors ( f, &typing ("*** typed child") );
    assert! ( matches! (
      &errors [..],
      [ BufferValidationError::MemberChildOfWriteProtectedOccurrence {
          skgid, title } ]
      if *skgid == ID::from ("n") && title == "n" ),
      "{:?}", errors );
    assert! ( errors [0] . to_string () . contains (
                "cannot become its content" ),
              "{}", errors [0] );
    assert! ( the_registered_viewforest (f) == registered,
              "a refused save leaves the registered viewforest alone" );
    let response : SaveResponse = save (
      f, &typing ("*** (skg (node (affectsParent false))) typed child") ) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    Ok (( )) }) }

/// Write-protecting n under x (its children becoming non-members)
/// while requesting n under y moves editability there: n is editable
/// under y and write-protected under x, and a is drawn once under each.
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
    assert_eq! ( node_lines (&saved, "a") . len (), 2, "{}", saved );
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

/// In an ordinary content view, write-protecting n, its root, whose
/// content a and b stay members, is refused, naming n.
#[test]
fn in_a_content_view_write_protecting_an_occurrence_with_members_is_refused (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let view_id : ViewId = ViewId::ContentView ("content-view-of-n" . to_string ());
    let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from ("n") ] ) ?;
    f . views_state . open_views . register_view (
      &f . graph . load_full (), view_id . clone (), tree, &pids );
    assert! ( text . starts_with ("* (skg (node (id n) (repo main) ")
              && ! text . lines () . next () . unwrap () . contains ("writeProtected")
              && node_lines (&text, "a") . len () == 1,
              "n must be the editable root, with a under it:\n{}", text );
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    let write_protected : String = text . replacen (
      "(repo main) ", "(repo main) writeProtected ", 1 );
    let result : Result<SaveResponse, Box<dyn Error>> = block_on (
      update_from_and_rerender_buffer_test (
        &mut stream, &write_protected,
        &f . config, &f . tantivy, &f . graph,
        &Ok (view_id), &mut f . views_state ));
    let Err (error) = result else {
      panic! ("the save must be refused") };
    let Ok (save_error) = error . downcast::<SaveError> () else {
      panic! ("expected a SaveError") };
    let SaveError::BufferValidationErrors { errors, .. } = *save_error else {
      panic! ("expected validation errors") };
    assert! ( matches! (
      &errors [..],
      [ BufferValidationError::MemberChildOfWriteProtectedOccurrence {
          skgid, .. } ]
      if *skgid == ID::from ("n") ),
      "{:?}", errors );
    Ok (( )) }) }
