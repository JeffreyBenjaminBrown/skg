// Saving a buffer of the total diff view: the save consumes its
// readiness requests, the buffer is reconciled over the fresh graph
// straddle with its readiness read from the store, and the response
// carries the readiness summary
// ('TODO/total-diff-view/2_plan.org', "Saving a TDV buffer").

use crate::pcsvf::common::{
  Fixture, open_all_changes_buffer, save, with_fixture};

use skg::change_forest::{BufferReadinessSummary, ReadinessSummary};
use skg::serve::handlers::save_buffer::{SaveResponse, readiness_summary_sexp};

use std::error::Error;

/// 'buffer' with the headline line starting 'prefix' (the only one)
/// replaced by 'replace (line)'.
fn edit_line (
  buffer  : &str,
  prefix  : &str,
  replace : impl Fn (&str) -> String,
) -> String {
  let matching : Vec<&str> = buffer . lines ()
    . filter ( |line| line . starts_with (prefix) ) . collect ();
  assert_eq! ( matching . len (), 1,
               "exactly one line should start {:?}:\n{}", prefix, buffer );
  buffer . lines ()
    . map ( |line| if line . starts_with (prefix) { replace (line) }
                   else { line . to_string () } )
    . collect::<Vec<String>> () . join ("\n") + "\n" }

/// Save 'buffer'; it must save without errors.
fn save_cleanly (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let response : SaveResponse = save (f, buffer) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, buffer );
  Ok (response) }

/// The relationship by which sub no longer subscribes to s1 is one
/// leaf. Marked on its image under sub, it shows ready on its image
/// under s1 too.
#[test]
fn a_mark_shows_on_both_images_of_a_relationship (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let marked : String = edit_line (
      &buffer, "*** (skg (node (id s1)",
      |line| line . replace ( "(diff rel-))) s1",
                              "(diff rel-) (readyRequest rel))) s1" ));
    let saved : String = save_cleanly (f, &marked) ? . saved_view;
    assert! ( saved . contains (
      "*** (skg (node (id s1) (repo public) (affectsParent false) writeProtected (diff rel-) (ready rel))) s1" ),
      "the image under sub is ready:\n{}", saved );
    assert! ( saved . lines () . any ( |line|
      line . starts_with ("*** (skg (node (id sub)")
      && line . ends_with ("(diff rel-) (ready rel))) sub") ),
      "the image under s1 is ready:\n{}", saved );
    assert! ( ! saved . contains ("readyRequest"),
              "the request was consumed:\n{}", saved );
    Ok (( )) }) }

/// A mark approves one version of a change. Editing the node changes
/// the change, and the mark lapses.
#[test]
fn editing_a_node_lapses_its_mark (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let marked : String = edit_line (
      &buffer, "** (skg (oldText titleAndBody)) txt",
      |_| "** (skg (oldText titleAndBody) (readyRequest)) txt" . to_string () );
    let saved : String = save_cleanly (f, &marked) ? . saved_view;
    assert! ( saved . contains ("** (skg (oldText titleAndBody) (ready)) txt"),
              "the old text is ready:\n{}", saved );
    let edited : String = edit_line (
      &saved, "* (skg (node (id txt)",
      |line| line . replace ( "txt, retitled", "txt, retitled again" ));
    let saved : String = save_cleanly (f, &edited) ? . saved_view;
    assert! ( saved . contains ("** (skg (oldText titleAndBody)) txt"),
              "the edit lapsed the mark:\n{}", saved );
    Ok (( )) }) }

/// The summary counts the changed files of the owned skgrepos and
/// the ready ones, and the leaves owned by nodes the saved buffer does
/// not draw. The "sets" fixture changes six files: cy2 and mc
/// (retitled), del and delc (deleted), ln (gains lo) and r (loses
/// del); their seven leaves are the two text changes, the two
/// deletions, and the relationships ln-lo, r-del and del-delc.
#[test]
fn the_save_reports_the_readiness_summary (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let summary_of = |response : &SaveResponse| -> BufferReadinessSummary {
      response . readiness_summary . clone ()
      . expect ("a save of a TDV buffer reports the readiness summary") };
    let summary = |ready_files : usize, unshown_changes : usize|
      -> BufferReadinessSummary {
      BufferReadinessSummary {
        summary : ReadinessSummary {
          ready_files, changed_files : 6,
          foreign_changed_files : 0, order_only_files : 0 },
        unshown_changes } };
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_eq! ( summary_of ( &save_cleanly (f, &buffer) ? ), summary (0, 0) );
    let marked : String = edit_line (
      &buffer, "** (skg (oldText title)) mc",
      |_| "** (skg (oldText title) (readyRequest)) mc" . to_string () );
    let response : SaveResponse = save_cleanly (f, &marked) ?;
    assert_eq! ( summary_of (&response), summary (1, 0),
                 "mc's file records only its text change" );
    let only_r : String = { // drop the roots cy2, ln and mc, which precede r
      let lines : Vec<&str> = response . saved_view . lines () . collect ();
      let start : usize = lines . iter ()
        . position ( |line| line . starts_with ("* (skg (node (id r)") )
        . expect ("r is a root");
      lines [start ..] . join ("\n") + "\n" };
    let response : SaveResponse = save_cleanly (f, &only_r) ?;
    assert_eq! ( summary_of (&response), summary (1, 3),
                 "the leaves of cy2, ln and mc are not shown" );
    assert_eq! ( readiness_summary_sexp (&summary_of (&response)),
                 "(readiness-summary (ready 1) (changed 6) (foreign 0) (order-only 0) (unshown 3))" );
    Ok (( )) }) }

/// Save 'buffer' with 'pid' (an editable occurrence on a line
/// starting 'prefix') marked for deletion; return the reconciled
/// buffer, after checking that saving it again changes nothing.
fn save_deleting (
  f      : &mut Fixture,
  buffer : &str,
  prefix : &str,
  pid    : &str,
) -> Result<String, Box<dyn Error>> {
  let node : String = format! ("(node (id {})", pid);
  let deleting : String = edit_line (
    buffer, prefix,
    |line| line . replacen (
      &node, &format! ("{} (editRequest delete)", node), 1 ));
  let saved : String = save_cleanly (f, &deleting) ? . saved_view;
  assert_eq! ( save_cleanly (f, &saved) ? . saved_view, saved,
               "the reconciled buffer is a fixed point" );
  Ok (saved) }

/// A node deleted in the buffer is drawn as the PCSVF draws a
/// deleted node: where it stood among its container's content, and
/// in the changed container folder of what it contained.
#[test]
fn a_member_deleted_in_the_buffer_is_redrawn_as_removed (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = save_deleting (
      f, &buffer, "** (skg bodyFolded (node (id cy1)", "cy1" ) ?;
    assert! ( ! saved . contains ("(deleted (id cy1)"), "{}", saved );
    let historical : &str =
      "(skg (historical (id cy1) (repo main) (diff node- rel-))) cy1";
    assert_eq! ( saved . matches (historical) . count (), 2,
                 "in cy2's content and in its container folder:\n{}", saved );
    Ok (( )) }) }

#[test]
fn a_root_deleted_in_the_buffer_is_redrawn_as_removed (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = save_deleting (
      f, &buffer, "* (skg (node (id flg)", "flg" ) ?;
    assert! ( saved . contains (
      "* (skg (historical (id flg) (repo public) (diff node-))) flg" ),
      "{}", saved );
    assert! ( ! saved . contains ("(deleted (id flg)"), "{}", saved );
    Ok (( )) }) }
