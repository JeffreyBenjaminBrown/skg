// Saving a buffer of the total diff view: the save consumes its
// readiness requests, the buffer is reconciled over the fresh graph
// straddle with its readiness read from the store, and the response
// carries the readiness summary
// ('TODO/total-diff-view/02_plan.org', "Saving a TDV buffer").

use crate::pcsvf::common::{
  Fixture, open_all_changes_buffer, open_buffer, save, save_buffer,
  show_unshown_changes, with_fixture};

use skg::change_forest::{BufferReadinessSummary, ReadinessSummary};
use skg::serve::handlers::save_buffer::{SaveResponse, readiness_summary_sexp};
use skg::types::views_state::TdvBuffer;

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

/// Marking a removed alias or a removed ID marks its leaf: ali's lost
/// alias a1 and lost extra ID ali-old show ready after the save.
#[test]
fn marking_a_removed_entry_marks_its_leaf (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let marked : String = [ "*** (skg removedAlias (diff rel-)) a1",
                            "*** (skg removedID (diff rel-)) ali-old" ]
      . into_iter () . fold ( buffer, |marked, line|
        edit_line ( &marked, line, |line| line . replacen (
          "(diff rel-))", "(diff rel-) (readyRequest))", 1 )) );
    let saved : String = save_cleanly (f, &marked) ? . saved_view;
    for line in [ "*** (skg removedAlias (diff rel-) (ready)) a1",
                  "*** (skg removedID (diff rel-) (ready)) ali-old" ] {
      assert! ( saved . lines () . any ( |l| l == line ),
                "{:?} must be ready:\n{}", line, saved ); }
    assert! ( ! saved . contains ("readyRequest"),
              "the requests were consumed:\n{}", saved );
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
/// deletions, and the relationships ln-lo, r-del and del-delc. Roots
/// the user drops from the buffer are origins it no longer draws: the
/// save leaves them out and counts their changes unshown, and the
/// "show unshown changes" request appends them again.
#[test]
fn the_save_reports_the_readiness_summary (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_eq! ( summary_of ( &save_cleanly (f, &buffer) ? ), summary (0, 6, 0) );
    let marked : String = edit_line (
      &buffer, "** (skg (oldText title)) mc",
      |_| "** (skg (oldText title) (readyRequest)) mc" . to_string () );
    let response : SaveResponse = save_cleanly (f, &marked) ?;
    assert_eq! ( summary_of (&response), summary (1, 6, 0),
                 "mc's file records only its text change" );
    let only_r : String = { // drop the roots cy2, ln and mc, which precede r
      let lines : Vec<&str> = response . saved_view . lines () . collect ();
      let start : usize = lines . iter ()
        . position ( |line| line . starts_with ("* (skg (node (id r)") )
        . expect ("r is a root");
      lines [start ..] . join ("\n") + "\n" };
    let response : SaveResponse = save_cleanly (f, &only_r) ?;
    assert_eq! ( response . saved_view . lines ()
                   . filter ( |line| line . starts_with ("* ") ) . count (), 1,
                 "the dropped roots stay dropped:\n{}", response . saved_view );
    assert_eq! ( summary_of (&response), summary (1, 6, 3) );
    let (shown, unshown) : (String, usize) = show_unshown_changes (f) ?;
    let roots : Vec<&str> = shown . lines ()
      . filter ( |line| line . starts_with ("* ") ) . collect ();
    assert! ( roots . len () == 4 && roots [0] . starts_with ("* (skg (node (id r)")
              && roots [1] . contains ("(id cy2)") && roots [2] . contains ("(id ln)")
              && roots [3] . contains ("(id mc)"),
              "the dropped roots return, after r:\n{}", shown );
    assert_eq! ( unshown, 0 );
    Ok (( )) }) }

/// A save of the merges buffer of the 'buffers' fixture (described in
/// 'other_buffers.rs') leaves unshown the eleven leaves of the nodes
/// it does not draw editable: txt's text, the home and relRepo moves
/// of mvc, mvg, mvp, mvr and mvt, and the creation of pres, which it
/// draws as a write-protected member of acq's content.
#[test]
fn the_summary_counts_the_changes_the_saved_buffer_does_not_show (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &["public", "private"], |f| {
    let text : String = open_buffer (f, TdvBuffer::Merges) ?
      . ok_or ("the merges buffer is empty") ?;
    let response : SaveResponse = save_buffer (f, TdvBuffer::Merges, &text) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    let summary : BufferReadinessSummary = summary_of (&response);
    assert_eq! ( summary . unshown_changes, 11, "{:?}", summary );
    assert_eq! ( readiness_summary_sexp (&summary),
                 format! ( "(readiness-summary (ready {}) (changed {}) (foreign 0) (order-only 1) (unshown 11))",
                           summary . summary . ready_files,
                           summary . summary . changed_files ));
    Ok (( )) }) }

fn summary_of (
  response : &SaveResponse,
) -> BufferReadinessSummary {
  response . readiness_summary . clone ()
    . expect ("a save of a TDV buffer reports the readiness summary") }

fn summary (
  ready_files     : usize,
  changed_files   : usize,
  unshown_changes : usize,
) -> BufferReadinessSummary {
  BufferReadinessSummary {
    summary : ReadinessSummary {
      ready_files, changed_files,
      foreign_changed_files : 0, order_only_files : 0 },
    unshown_changes } }

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
/// in the changed container folder of what it contained. Where it
/// stood, its viewnode stays, with its folded body.
#[test]
fn a_member_deleted_in_the_buffer_is_redrawn_as_removed (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = save_deleting (
      f, &buffer, "** (skg bodyFolded (node (id cy1)", "cy1" ) ?;
    assert! ( ! saved . contains ("(deleted (id cy1)"), "{}", saved );
    let historical : &str =
      "(historical (id cy1) (repo main) (diff node- rel-))) cy1";
    assert_eq! ( saved . matches (historical) . count (), 2,
                 "in cy2's content and in its container folder:\n{}", saved );
    assert! ( saved . contains ( &format! ("** (skg bodyFolded {}", historical) ),
              "cy1, context before, keeps its folded body:\n{}", saved );
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
