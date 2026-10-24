// What a commit's rerender leaves of the change viewnodes
// ('docs/diff-view.org', "The shallow model subtree, and what it
// keeps"): the diff view's reconciler converts what the graph no
// longer warrants ('convert_unwarranted_subtree'), never detaches.
//
// The 'commit_leftovers' fixture is 'removed_partners' (b, d and e
// drop x or c from their content; s drops its subscribee s2) with p,
// a container of b, and h, which contains hm at HEAD and is deleted.
// Its "all changes" buffer holds, among other roots: e, holding x as
// a removed member, editable, with x's content xc and its changed
// container folder; p, holding b, holding x as a write-protected,
// childless removed member; and h, a historical phantom root holding
// its old content hm.

use crate::pruned_change_view::common::{
  all_changes_view, assert_lines_in_order, insert_after_line_starting, note,
  open_all_changes_buffer, parent_line_of, rerendered_view_content,
  saved_and_rerendered, send_stage_and_commit_request, with_fixture};
use crate::git_fixture::{Fixture, save_cleanly};

use skg::metadata_canon::without_viewnode_ids;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;

use std::error::Error;

const REMOVED_X_UNDER_E : &str = "** (skg (node (id x) (repo main) nonMember ";
const REMOVED_X_UNDER_B : &str = // not c's x, under d: that one is x's container
  "*** (skg (node (id x) (repo main) nonMember writeProtected (rels (contains (in 2) (out 1))";
const RETAINED_X_UNDER_E : &str = // retained as non-member, still editable
  "** (skg (node (id x) (repo main) nonMember (rels ";

/// Commit everything, confirmed; the "all changes" buffer as the
/// rerender stream that follows sends it.
fn committed (
  f : &mut Fixture,
) -> Result<String, Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let response : String = send_stage_and_commit_request ( f, &env,
    "((request . \"stage and commit\") (message . \"commit\") (confirmed . t))\n" ) ?;
  assert! ( response . contains ("(commits ((main "), "{}", response );
  Ok ( rerendered_view_content (&response) ) }

/// The viewnode ID on 'line', a headline.
fn viewnode_id_on (
  line : &str,
) -> &str {
  let start : usize = line . find ("(vnid ") . expect ("a headline has a viewnode ID") + "(vnid " . len ();
  line [start ..] . split (')') . next () . unwrap () }

/// The one line of 'buffer' that starts, viewnode ID aside, with 'prefix'.
fn line_starting<'a> (
  buffer : &'a str,
  prefix : &str,
) -> &'a str {
  let matching : Vec<&str> = buffer . lines ()
    . filter ( |line| without_viewnode_ids (line) . starts_with (prefix) ) . collect ();
  assert_eq! ( matching . len (), 1, "one line starts {:?} in\n{}", prefix, buffer );
  matching [0] }

/// After the commit: the editable removed member with content is
/// retained as non-member, still editable, holding it as its content,
/// without facts; the
/// childless removed member is gone; the deleted root stays as a
/// deleted phantom holding nothing; the changed folders are gone; no
/// change fact remains; and the buffer saves back to itself.
#[test]
fn a_commit_converts_what_the_diff_view_showed (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("commit_leftovers", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_lines_in_order (&buffer, &[
      "* (skg (node (id e)", REMOVED_X_UNDER_E, "(containerFolder changed)",
      "*** (skg (node (id xc) ",
      "* (skg (historical (id h)", "** (skg (node (id hm)",
      "* (skg bodyFolded (node (id p)", "** (skg (node (id b)", REMOVED_X_UNDER_B ]);
    let x_under_e_before : &str = line_starting (&buffer, REMOVED_X_UNDER_E);
    let committed : String = committed (f) ?;
    let x_under_e : &str = line_starting (&committed, RETAINED_X_UNDER_E);
    assert! ( ! x_under_e . contains ("(diff"), "{}", committed );
    assert_eq! ( viewnode_id_on (x_under_e), viewnode_id_on (x_under_e_before),
                 "the removed member is kept, not made again:\n{}", committed );
    assert_lines_in_order (&committed, &[
      "* (skg (node (id e) (repo main))) e\n",
      RETAINED_X_UNDER_E, ") x\n",
      "*** (skg (node (id xc) (repo main) writeProtected " ]);
    assert_lines_in_order (&committed, &[ // b, under p, holds nothing
      "* (skg bodyFolded (node (id p)", "** (skg (node (id b) ", ") b\n* (skg (node (id s)" ]);
    assert_lines_in_order (&committed, &[
      "\n* (skg (deleted (id h) (repo main))) h\n* (skg bodyFolded (node (id p)" ]);
    assert! ( ! committed . contains ("(id hm)"), "{}", committed );
    assert! ( ! committed . contains ("containerFolder")
              && ! committed . contains ("subscriberFolder"),
              "the changed folders are gone:\n{}", committed );
    for fact in [ "(diff ", "changed)", "changesetMarker", "oldText",
                  "historical", "homeSkgrepoMove", "(ready" ] {
      assert! ( ! committed . contains (fact),
                "no change fact {:?} remains:\n{}", fact, committed ); }
    let saved : String =
      save_cleanly (f, &all_changes_view (), &committed) ? . saved_view;
    assert_eq! ( saved, committed );
    Ok (( )) }) }

/// A note typed under an editable removed member survives the commit
/// under the converted member, retained as non-member and still
/// editable.
#[test]
fn a_note_under_a_removed_member_survives_under_the_retained_member (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("commit_leftovers", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    saved_and_rerendered ( f,
      &insert_after_line_starting ( &buffer, REMOVED_X_UNDER_E,
                                    &note (3, "a note under x") )) ?;
    let committed : String = committed (f) ?;
    let parent : String = parent_line_of (&committed, "a note under x");
    assert! ( parent . starts_with (RETAINED_X_UNDER_E) && ! parent . contains ("(diff"),
              "{}", committed );
    assert_lines_in_order (&committed, &[
      "* (skg (node (id e)", RETAINED_X_UNDER_E, " a note under x\n" ]);
    Ok (( )) }) }

/// A note typed under a write-protected removed member keeps that
/// occurrence through the commit: the same viewnode, now a plain
/// write-protected non-member, holds it where it stood.
#[test]
fn a_note_under_a_write_protected_removed_member_keeps_that_occurrence (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("commit_leftovers", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = saved_and_rerendered ( f,
      &insert_after_line_starting ( &buffer, REMOVED_X_UNDER_B,
                                    &note (4, "a note under x") )) ?;
    let x_before : &str = line_starting (&saved, REMOVED_X_UNDER_B);
    let committed : String = committed (f) ?;
    let x_after : &str = line_starting (&committed, REMOVED_X_UNDER_B);
    assert! ( ! x_after . contains ("(diff"), "{}", committed );
    assert_eq! ( viewnode_id_on (x_after), viewnode_id_on (x_before), "{}", committed );
    assert! ( parent_line_of (&committed, "a note under x") . starts_with (REMOVED_X_UNDER_B),
              "{}", committed );
    assert_lines_in_order (&committed, &[
      "* (skg bodyFolded (node (id p)", "** (skg (node (id b)", REMOVED_X_UNDER_B,
      " a note under x\n" ]);
    Ok (( )) }) }

/// A write-protected copy the user pasted under an editable removed
/// member stays where it stood through the commit, write-protected,
/// under the member retained as non-member: it is neither a change
/// viewnode nor a removed member, so the conversion of the stale
/// member leaves it.
#[test]
fn a_pasted_write_protected_copy_under_a_stale_child_stays (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("commit_leftovers", &["main"], |f| {
    let copy : &str = "*** (skg (node (id s1) (repo main) nonMember writeProtected";
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = saved_and_rerendered ( f,
      &insert_after_line_starting (
        &buffer, REMOVED_X_UNDER_E,
        "*** (skg (node (id s1) (repo main) nonMember writeProtected)) s1\n" )) ?;
    let copy_before : &str = line_starting (&saved, copy);
    assert! ( headline_parent_of (&saved, copy_before) . starts_with (REMOVED_X_UNDER_E),
              "{}", saved );
    let committed : String = committed (f) ?;
    let copy_after : &str = line_starting (&committed, copy);
    assert_eq! ( viewnode_id_on (copy_after), viewnode_id_on (copy_before),
                 "the copy is kept, not made again:\n{}", committed );
    assert! ( headline_parent_of (&committed, copy_after) . starts_with (RETAINED_X_UNDER_E),
              "{}", committed );
    Ok (( )) }) }

/// The headline above 'line' one level up, viewnode IDs aside.
fn headline_parent_of (
  buffer : &str,
  line   : &str,
) -> String {
  let depth = |line : &str| line . chars () . take_while ( |c| *c == '*' ) . count ();
  let headlines : Vec<&str> =
    buffer . lines () . filter ( |line| line . starts_with ('*') ) . collect ();
  let at : usize = headlines . iter () . position ( |headline| *headline == line )
    . expect ("the line is a headline of the buffer");
  without_viewnode_ids ( headlines [.. at] . iter () . rev ()
    . find ( |headline| depth (headline) + 1 == depth (line) )
    . expect ("the headline has a parent") ) }
