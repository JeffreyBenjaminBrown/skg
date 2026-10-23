// What the user adds under what the diff view rendered survives every
// rerender ('docs/diff-view.org', "What a reconciler keeps").
//
// The 'users_notes' fixture: a (retitled) contains x, which b no
// longer contains; b holds x as a removed member, x's first
// occurrence, so editable there, and x's container folder holds a,
// with a's member role tree (p contains a), and b as a removed
// member; r held h, which is deleted, with m in its old content; acq
// acquired acqd.

use skg::metadata_canon::without_vids;
use crate::pruned_change_view::common::{
  assert_lines_in_order, insert_after_line_starting, note, open_all_changes_buffer,
  parent_line_of, rerendered_view_content, saved_and_rerendered,
  send_stage_and_commit_request, with_fixture};

use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;

use std::error::Error;

const MEMBER_A : &str = "**** (skg (node (id a) (repo main) writeProtected";
const GRAFT_P  : &str =
  "***** (skg (node (id p) (repo main) (affectsParent false) (birth roleGraft container) writeProtected";

/// A note under a member of a changed container folder survives, and
/// the member's role tree is still shown around it, once.
#[test]
fn a_note_under_a_changed_folders_member_survives (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("users_notes", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_lines_in_order (&buffer, &[ MEMBER_A, GRAFT_P ]);
    let saved : String = saved_and_rerendered ( f,
      &insert_after_line_starting ( &buffer, MEMBER_A, &note (5, "a note under a") )) ?;
    assert! ( parent_line_of (&saved, "a note under a") . starts_with (MEMBER_A), "{}", saved );
    assert_lines_in_order (&saved, &[ MEMBER_A, " a note under a\n", GRAFT_P ]);
    assert_eq! ( saved . matches (GRAFT_P) . count (), 1, "{}", saved );
    Ok (( )) }) }

/// A note under a historical phantom survives beside its old content,
/// and survives the phantom going stale: once stage-and-commit makes
/// the deletion part of HEAD, the phantom is deadened, holding the
/// note but not its old content.
#[test]
fn a_note_under_a_historical_phantom_survives (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("users_notes", &["main"], |f| {
    let phantom : &str = "** (skg (historical (id h)";
    let old_member : &str =
      "*** (skg (node (id m) (repo main) (affectsParent false) (diff (rel removed)";
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = saved_and_rerendered ( f,
      &insert_after_line_starting ( &buffer, phantom, &note (3, "a note under h") )) ?;
    assert! ( parent_line_of (&saved, "a note under h") . starts_with (phantom), "{}", saved );
    assert_lines_in_order (&saved, &[ phantom, " a note under h\n", old_member ]);
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let response : String = send_stage_and_commit_request ( f, &env,
      "((request . \"stage and commit\") (message . \"commit\") (confirmed . t))\n" ) ?;
    let committed : String = rerendered_view_content (&response);
    assert_eq! ( parent_line_of (&committed, "a note under h"), "** (skg deadViewnode)",
                 "{}", committed );
    assert! ( ! without_vids (&committed) . contains (old_member), "{}", committed );
    Ok (( )) }) }

/// A note parked in the "it acquired" folder survives beside the
/// acquiree's phantom.
#[test]
fn a_note_in_the_acquired_folder_survives (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("users_notes", &["main"], |f| {
    let folder : &str = "** (skg acquiredFolder)";
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = saved_and_rerendered ( f,
      &insert_after_line_starting ( &buffer, folder, &note (3, "a parked note") )) ?;
    assert! ( parent_line_of (&saved, "a parked note") . starts_with (folder), "{}", saved );
    assert_lines_in_order (&saved, &[
      folder, " a parked note\n", "*** (skg (historical (id acqd)" ]);
    Ok (( )) }) }

/// A note under a removed member survives the member going stale:
/// once stage-and-commit makes the removal part of HEAD, the member
/// is a plain non-member occurrence, still holding the note.
#[test]
fn a_note_under_a_removed_member_survives_it_going_stale (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("users_notes", &["main"], |f| {
    let removed : &str = "** (skg (node (id x) (repo main) (affectsParent false) ";
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = saved_and_rerendered ( f,
      &insert_after_line_starting ( &buffer, removed, &note (3, "a note under x") )) ?;
    let parent : String = parent_line_of (&saved, "a note under x");
    assert! ( parent . starts_with (removed) && parent . contains ("(diff (rel removed)"),
              "{}", saved );
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let response : String = send_stage_and_commit_request ( f, &env,
      "((request . \"stage and commit\") (message . \"commit\") (confirmed . t))\n" ) ?;
    assert! ( response . contains ("(commits ((main "), "{}", response );
    let committed : String = rerendered_view_content (&response);
    let parent : String = parent_line_of (&committed, "a note under x");
    assert! ( parent . starts_with (removed) && ! parent . contains ("(diff"),
              "the note stands under a plain non-member occurrence of x:\n{}", committed );
    assert_lines_in_order (&committed, &[
      "* (skg (node (id b)", removed, " a note under x\n" ]);
    Ok (( )) }) }
