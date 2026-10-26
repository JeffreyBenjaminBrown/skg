// The view request '(viewRequests tombstone)' on a ghost
// ('docs/diff-view.org', "Dismissing a ghost"): the save leaves the
// graph as it was, and the refresh entombstones the ghost, which, as
// every tombstone does, keeps its children: what the user put under
// it and what the diff view drew there.
//
// In the 'commit_leftovers' fixture h, deleted since HEAD, is a ghost
// root holding its old content hm. In the 'users_notes' fixture h is a
// ghost under its former container, holding its old member m. In the
// 'restore' fixture g, deleted since HEAD, is a ghost under q, which
// contained it at HEAD, holding gc and gd.
//
// A ghost under a container that held it at HEAD is drawn again by a
// later refresh of that container; the request entombstones the one
// it is on. These tests assert only the refresh that carries the
// request.

use crate::pruned_change_view::common::{
  assert_lines_in_order, note, insert_after_line_starting,
  open_all_changes_buffer, parent_line_of, save, with_fixture};
use crate::git_fixture::{edit_line, skg_files};

use skg::metadata_canon::without_viewnode_ids;
use skg::serve::handlers::save_buffer::SaveResponse;

use std::collections::BTreeMap;
use std::error::Error;

const H_AS_A_ROOT : &str = "* (skg ghost (gnid h)";
const G_UNDER_Q : &str = "** (skg ghost (gnid g)";

/// 'buffer' with the tombstone request on its one line starting
/// 'prefix', a ghost's, before that form's '(diff ...)'.
fn requesting_its_tombstone (
  buffer : &str,
  prefix : &str,
) -> String {
  edit_line ( buffer, prefix, |line : &str| line . replacen (
    " (diff ", " (viewRequests tombstone) (diff ", 1 )) }

/// Save 'buffer', which must save cleanly and leave the files as they
/// were; the saved view.
fn saved_leaving_the_graph (
  f      : &mut crate::git_fixture::Fixture,
  buffer : &str,
) -> Result<String, Box<dyn Error>> {
  let files_before : BTreeMap<String, String> = skg_files (f) ?;
  let response : SaveResponse = save (f, buffer) ?;
  assert! ( response . errors . is_empty (),
            "the save must succeed, got {:?}\n{}", response . errors, buffer );
  assert! ( response . warnings . is_empty (), "{:?}", response . warnings );
  assert_eq! ( skg_files (f) ?, files_before,
               "a view request changes no file" );
  Ok (response . saved_view) }

/// The ghost root goes; the tombstone that replaced it holds what
/// the diff view drew under it, h's old content hm.
#[test]
fn a_ghost_root_entombstoned_goes () -> Result<(), Box<dyn Error>> {
  with_fixture ("commit_leftovers", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_lines_in_order (&buffer, &[ H_AS_A_ROOT, "** (skg node (gnid hm)" ]);
    let saved : String = saved_leaving_the_graph ( f,
      &requesting_its_tombstone (&buffer, H_AS_A_ROOT) ) ?;
    assert! ( ! saved . lines () . any ( |line|
                without_viewnode_ids (line) . starts_with (H_AS_A_ROOT) ),
              "h's ghost root is gone:\n{}", saved );
    assert_lines_in_order (&saved, &[ "* (skg tombstone)", "** (skg node (gnid hm)" ]);
    Ok (( )) }) }

/// What the user typed under the ghost (a new node, which the save
/// creates) stays, under the tombstone, and so does what the diff
/// view drew there, its old member m.
#[test]
fn a_note_under_an_entombstoned_ghost_stays () -> Result<(), Box<dyn Error>> {
  with_fixture ("users_notes", &["main"], |f| {
    let ghost : &str = "** (skg ghost (gnid h)";
    let old_member : &str =
      "*** (skg node (gnid m) (skgRepos (nodeRepo main)) (view ghostWarrant) (diff (rel removed)";
    let buffer : String = open_all_changes_buffer (f) ?;
    let with_note : String =
      insert_after_line_starting (&buffer, ghost, &note (3, "a note under h"));
    assert_lines_in_order (&with_note, &[ ghost, " a note under h\n", old_member ]);
    let response : SaveResponse =
      save ( f, &requesting_its_tombstone (&with_note, ghost) ) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    let saved : String = response . saved_view;
    assert_eq! ( parent_line_of (&saved, "a note under h"), "** (skg tombstone)",
                 "{}", saved );
    assert_lines_in_order (&saved, &[
      "** (skg tombstone)", " a note under h\n",
      "*** (skg node (gnid m) (skgRepos (nodeRepo main))) m\n" ]);
    Ok (( )) }) }

/// A ghost under its former container is entombstoned where it stood,
/// the tombstone holding its old content; the other ghosts of the same
/// node stay.
#[test]
fn a_ghost_member_entombstoned_goes_from_its_container (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("restore", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let ghosts_of_g : usize =
      without_viewnode_ids (&buffer) . matches ("ghost (gnid g)") . count ();
    let saved : String = saved_leaving_the_graph ( f,
      &requesting_its_tombstone (&buffer, G_UNDER_Q) ) ?;
    assert! ( ! saved . lines () . any ( |line|
                without_viewnode_ids (line) . starts_with (G_UNDER_Q) ),
              "g is gone from under q:\n{}", saved );
    assert_eq! ( without_viewnode_ids (&saved) . matches ("ghost (gnid g)") . count (),
                 ghosts_of_g - 1, "g's other ghosts stay:\n{}", saved );
    assert_lines_in_order (&saved, &[
      "* (skg node (gnid q)", "** (skg tombstone)", "*** (skg node (gnid gc)" ]);
    Ok (( )) }) }
