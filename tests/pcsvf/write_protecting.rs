// A save judges a write-protected occurrence by the graph alone
// ('errors_for_writeProtected_occurrences'), so in a buffer of the
// diff view a save may write-protect the only editable
// occurrence of a node of U. The 'shared' fixture (one skgrepo,
// main): x and y, whose bodies changed, each contain n, which
// contains a, whose body changed, and b, unchanged. So n, a node of
// U, is drawn editable under x and write-protected under y.

use crate::git_fixture::{Fixture, skg_files};
use crate::pcsvf::common::{
  node_lines, open_all_changes_buffer, requesting_editable_at, save,
  view_id_of, with_fixture};
use crate::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::update_from_and_rerender_buffer_test;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::ID;
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::{DiffViewBufferKind, ViewId};
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;
use std::collections::BTreeMap;
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
    . viewid_to_view ( &view_id_of (DiffViewBufferKind::AllChanges) )
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
/// c keeps its readiness facts, and the summary counts its change
/// as not editable.
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
    assert_eq! ( response . readiness_summary . unwrap () . changes_not_editable_here, 1 );
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

/// c, typed under n under x and so created, carries presence facts. A
/// write-protect request on n under x and an editable-view request
/// on n under y, in one save, move editability: a, b and c are drawn
/// once under x, as non-members, c keeping its presence facts, and
/// editable under y. An editable-view request on n under x moves it
/// back: n under x adopts them as its members again.
#[test]
fn a_writeProtect_request_and_an_editable_request_move_editability_and_back (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let b_under_x : usize = buffer . find ("*** (skg (node (id b) ")
      . expect ("b is drawn under n under x");
    let created : String = save ( f, &format! (
      "{}*** (skg (node (id c) (repo main))) c\n{}",
      &buffer [.. b_under_x], &buffer [b_under_x ..] )) ? . saved_view;
    assert! ( node_lines (&created, "c") [0] . contains ("(diff "), "{}", created );
    // c, new, draws its containers, so n and its content recur deeper.
    let at_depth = |buffer : &'_ str, stars : &str, node : &str| -> Vec<String> {
      node_lines (buffer, node) . into_iter ()
        . filter ( |line| line . starts_with (&format! ("{} (skg ", stars)) )
        . map ( |line| line . to_string () ) . collect () };
    let n_under_x : &str = "\n** (skg bodyFolded (node (id n) (repo main) ";
    let edited : String = requesting_editable_at (
      &created . replacen (
        n_under_x,
        &format! ( "{}(viewRequests writeProtectedView) ", n_under_x ), 1 ),
      &format! ("\n{}", N_UNDER_Y) );
    let response : SaveResponse = save (f, &edited) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    let moved : String = response . saved_view;
    let n_lines : Vec<String> = at_depth (&moved, "**", "n");
    assert! ( n_lines . len () == 2
              && n_lines [0] . contains ("writeProtected")
              && ! n_lines [1] . contains ("writeProtected"),
              "n must be write-protected under x and editable under y:\n{}", moved );
    assert! ( ! moved . contains ("viewRequests"), "{}", moved );
    for child in ["a", "b", "c"] {
      let lines : Vec<String> = at_depth (&moved, "***", child);
      assert! ( lines . len () == 2
                && lines [0] . contains ("(affectsParent false)")
                && lines [0] . contains ("writeProtected")
                && ! lines [1] . contains ("affectsParent"),
                "{} must be drawn once under each n:\n{}", child, moved ); }
    assert! ( at_depth (&moved, "***", "c") [0] . contains ("(diff "),
              "c keeps its presence facts under x:\n{}", moved );
    let moved_back : String = save (
      f, &requesting_editable_at (&moved, "\n** (skg bodyFolded (node (id n) (repo main) writeProtected ") ) ?
      . saved_view;
    let n_lines : Vec<String> = at_depth (&moved_back, "**", "n");
    assert! ( n_lines . len () == 2
              && ! n_lines [0] . contains ("writeProtected")
              && n_lines [1] . contains ("writeProtected"),
              "n must be editable under x again:\n{}", moved_back );
    for child in ["a", "b", "c"] {
      let lines : Vec<String> = at_depth (&moved_back, "***", child);
      assert! ( lines . len () == 2
                && ! lines [0] . contains ("affectsParent"),
                "{} must be n's member under x again:\n{}", child, moved_back ); }
    Ok (( )) }) }

/// In the 'kinds' fixture the worktree removed sub's subscription to
/// s1, so the buffer draws s1's subscriber folder holding sub, marked
/// '(diff rel-)'. Write-protected, s1 saves unchanged: the check of a
/// write-protected occurrence's partner folders does not count a
/// member shown as removed.
#[test]
fn a_writeProtected_occurrence_showing_a_removed_partner_saves_unchanged (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let s1_root : &str = buffer . lines ()
      . find ( |line| line . starts_with ("* (skg (node (id s1) (repo public) ") )
      . expect ("s1 is a root");
    let text : String = buffer . replacen ( s1_root, &s1_root . replacen (
      "(repo public) ", "(repo public) writeProtected ", 1 ), 1 );
    let s1_and_folder : Vec<&str> = text . lines ()
      . skip_while ( |line| ! line . starts_with ("* (skg (node (id s1) ") )
      . take (3) . collect ();
    assert! ( s1_and_folder [0] . contains ("writeProtected")
              && s1_and_folder [1] == "** (skg (subscriberFolder changed))"
              && s1_and_folder [2] . starts_with ("*** (skg (node (id sub) ")
              && s1_and_folder [2] . ends_with ("(diff rel-))) sub")
              && ! s1_and_folder [2] . contains ("affectsParent"),
              "s1 must be write-protected, showing sub as a removed member:\n{}",
              text );
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &text) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before );
    assert! ( response . saved_view . contains ("(diff rel-))) sub"),
              "{}", response . saved_view );
    Ok (( )) }) }

/// The kinds buffer's line drawing ctx, unchanged and write-protected
/// in ct's content, with 'replace' applied to it.
fn editing_ctx (
  buffer  : &str,
  replace : impl Fn (&str) -> String,
) -> String {
  let ctx : &str = buffer . lines ()
    . find ( |line| line . starts_with (
      "** (skg (node (id ctx) (repo public) writeProtected ") )
    . expect ("ctx is drawn write-protected in ct's content");
  buffer . replacen ( ctx, &replace (ctx), 1 ) }

/// In the 'kinds' fixture ctx, unchanged, is drawn write-protected in
/// ct's content. Drawn there with another home skgrepo, it is
/// refused, naming the home skgrepo.
#[test]
fn a_writeProtected_occurrence_with_another_home_skgrepo_is_refused (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let errors : Vec<BufferValidationError> = refusal_errors ( f, &editing_ctx (
      &buffer, |line| line . replacen ("(repo public)", "(repo private)", 1) ));
    assert! ( matches! (
      &errors [..],
      [ BufferValidationError::EditedWriteProtectedOccurrence {
          skgid, changes, .. } ]
      if *skgid == ID::from ("ctx")
         && *changes == vec! [
           "home skgrepo private differs from the graph's public" . to_string () ] ),
      "{:?}", errors );
    Ok (( )) }) }

/// An ID folder and a flags folder under the write-protected ctx
/// (see the previous test) are checked as under any recorder: saying
/// what the graph says, they save; saying more, they are refused.
#[test]
fn id_and_flags_folders_under_a_writeProtected_occurrence_are_checked (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let with_folder = |folder : &str| -> String {
      editing_ctx ( &buffer, |line| format! ("{}\n{}", line, folder) ) };
    for agreeing in [ "*** (skg idFolder)\n**** (skg id) ctx",
                      "*** (skg flagsFolder)" ] {
      let response : SaveResponse = save ( f, &with_folder (agreeing) ) ?;
      assert! ( response . errors . is_empty (),
                "{:?} must save, got {:?}", agreeing, response . errors ); }
    let errors : Vec<BufferValidationError> = refusal_errors (
      f, &with_folder ("*** (skg idFolder)\n**** (skg id) ctx\n**** (skg id) ctx-too") );
    assert! ( matches! (
      &errors [..],
      [ BufferValidationError::IDFolder_Edited (recorder, _, _) ]
      if *recorder == ID::from ("ctx") ),
      "{:?}", errors );
    let errors : Vec<BufferValidationError> = refusal_errors (
      f, &with_folder ("*** (skg flagsFolder)\n**** (skg (flag noSearchMatching))") );
    assert! ( matches! (
      &errors [..],
      [ BufferValidationError::FlagsSurfaceEdited { recorder_skgid, .. } ]
      if *recorder_skgid == ID::from ("ctx") ),
      "{:?}", errors );
    Ok (( )) }) }

/// Typed under n under y as non-members, b (folded) then a are n's
/// content in the wrong order. An editable-view request there makes
/// that occurrence editable; it adopts them as its members, keeping
/// their viewnodes (b stays folded), and moves them into content
/// order: a, then b.
#[test]
fn an_adopted_child_moves_into_content_order (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let n_under_y_line : &str = buffer . lines ()
      . find ( |line| line . starts_with (N_UNDER_Y) )
      . expect ("n is drawn write-protected under y");
    let typed : String = buffer . replacen ( n_under_y_line, &format! (
      "{}\n{}\n{}", n_under_y_line,
      "*** (skg folded (node (id b) (repo main) (affectsParent false) writeProtected)) b",
      "*** (skg (node (id a) (repo main) (affectsParent false) writeProtected)) a" ), 1 );
    let response : SaveResponse =
      save ( f, &requesting_editable_at (&typed, N_UNDER_Y) ) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    let saved : String = response . saved_view;
    let under_y : Vec<&str> = saved . lines ()
      . skip_while ( |line| ! line . starts_with ("* (skg (node (id y) ") )
      . filter ( |line| line . starts_with ("** (skg ")
                        || line . starts_with ("*** (skg ") )
      . filter ( |line| line . contains ("(node (id ") )
      . collect ();
    assert! ( under_y . len () == 3
              && under_y [0] . contains ("(node (id n) ")
              && ! under_y [0] . contains ("writeProtected")
              && under_y [1] . starts_with ("*** (skg (node (id a) ")
              && under_y [2] . starts_with ("*** (skg folded (node (id b) ")
              && ! under_y [1 ..] . iter () . any ( |line| line . contains ("affectsParent") ),
              "n under y must be editable, its members a then b, b folded:\n{}",
              saved );
    Ok (( )) }) }
