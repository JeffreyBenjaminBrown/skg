// Saving a buffer of the diff view: the save consumes its
// readiness requests, the buffer is reconciled over the fresh graph
// straddle with its readiness read from the store, and the response
// carries the readiness summary
// ('docs/diff-view.org', "The readiness summary").

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture::{Fixture, edit_line, save_cleanly};
use crate::git_fixture_helpers::without_lines_containing;
use crate::pruned_change_view::common::{
  all_changes_view, insert_after_line_starting, integrate_unshown_changes,
  open_all_changes_buffer, open_buffer, save_buffer, with_fixture,
  with_fixture_and_foreign};

use skg::diff_view::change_leaves::{BufferReadinessSummary, ReadinessSummary};
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::serve::util::sexp_to_client_string;
use skg::types::views_state::DiffViewBufferKind;

use std::error::Error;

/// A headline that names change-forest leaves says which, in
/// '(diff (leaves N OBJECT...))'. With ctx removed from ct in the buffer
/// too, ct's content changeset marker gathers three content
/// relationships (k1 and ctx removed, k2 added); the historical
/// phantom of gone, in orph's subscriber folder, names a leaf on each
/// of its objects; an old text names its one leaf.
#[test]
fn a_headline_carries_the_leaves_it_names (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let without_ctx : String =
      without_lines_containing (&buffer, "** (skg node (gnid ctx)");
    let saved : String =
      save_cleanly (f, &all_changes_view (), &without_ctx) ? . saved_view;
    for line in [
      "** (skg (changesetMarker content) (diff (leaves 3 all)))",
      "*** (skg ghost (gnid gone) (skgRepos (nodeRepo public)) (diff (node removed) (rel removed) (leaves 2 node rel))) gone",
      "** (skg (oldText titleAndBody) (diff (leaves 1))) txt" ] {
      assert! ( without_viewnode_ids (&saved) . lines () . any ( |l| l == line ),
                "{:?} in:\n{}", line, saved ); }
    Ok (( )) }) }

/// The relationship by which sub no longer subscribes to s1 is one
/// leaf. Marked on its image under sub, it shows ready on its image
/// under s1 too.
#[test]
fn a_mark_shows_on_both_images_of_a_relationship (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let marked : String = edit_line (
      &buffer, "*** (skg node (gnid s1)",
      |line| line . replace ( "(diff (rel removed) (leaves 1 rel))) s1",
                              "(editRequest (ready rel)) (diff (rel removed) (leaves 1 rel))) s1" ));
    let saved : String = save_cleanly (f, &all_changes_view (), &marked) ? . saved_view;
    assert! ( without_viewnode_ids (&saved) . contains (
      "*** (skg node (gnid s1) (skgRepos (nodeRepo public)) noClaim writeProtected (view ghostWarrant) (diff (rel removed) (leaves 1 rel) (ready rel))) s1" ),
      "the image under sub is ready:\n{}", saved );
    assert! ( without_viewnode_ids (&saved) . lines () . any ( |line|
      without_viewnode_ids (line) . starts_with ("*** (skg node (gnid sub)")
      && line . ends_with ("(diff (rel removed) (properties changed) (leaves 2 rel all) (ready rel))) sub") ),
      "the image under s1 is ready:\n{}", saved );
    assert! ( ! without_viewnode_ids (&saved) . contains ("(editRequest"),
              "the request was consumed:\n{}", saved );
    Ok (( )) }) }

/// A folder member carries its node's other properties as a content
/// member does: sub, in s1's subscriber folder, whose file records
/// two leaves (its subscription to s1 removed, to s2 added), shows
/// '(properties changed)' there, and one gesture on it, naming 'rel'
/// and 'all', marks its file: its root shows '(ready all)', and its
/// image in s2's subscriber folder is ready on both objects too.
#[test]
fn a_gesture_on_a_modified_folder_member_marks_its_file (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let member : &str =
      "*** (skg node (gnid sub) (skgRepos (nodeRepo public)) writeProtected";
    let removed_ending : &str =
      "(diff (rel removed) (properties changed) (leaves 2 rel all))) sub";
    assert_eq! ( buffer . matches (removed_ending) . count (), 1,
                 "sub is shown once as removed:\n{}", buffer );
    let marked : String = buffer . replacen (
      removed_ending,
      "(editRequest (ready rel all)) (diff (rel removed) (properties changed) (leaves 2 rel all))) sub",
      1 );
    assert_ne! ( marked, buffer, "the member shows its properties:\n{}", buffer );
    let saved : String = without_viewnode_ids (
      & save_cleanly (f, &all_changes_view (), &marked) ? . saved_view );
    let lines_of_sub : Vec<&str> = saved . lines ()
      . filter ( |line| line . contains ("node (gnid sub) ") ) . collect ();
    for (prefix, ending) in [
      ( member,
        "(diff (rel removed) (properties changed) (leaves 2 rel all) (ready rel all))) sub" ),
      ( "*** (skg node (gnid sub) (skgRepos (nodeRepo public)) writeProtected",
        "(diff (rel added) (properties changed) (leaves 2 rel all) (ready rel all))) sub" ),
      ( "* (skg node (gnid sub)",
        "(diff (properties changed) (leaves 2 all) (ready all))) sub" ) ] {
      assert! ( lines_of_sub . iter () . any ( |line|
                  without_viewnode_ids (line) . starts_with (prefix) && line . ends_with (ending) ),
                "{:?} ... {:?} in:\n{}", prefix, ending, saved ); }
    Ok (( )) }) }

/// A node of the containerward chain carries no change facts and
/// takes no readiness gesture: d, which contained c at HEAD only,
/// stands in the chain under c, in x's container folder, with no
/// '(diff ...)' part. A gesture typed on it names nothing, so c, as
/// d's removed member, stays unready.
#[test]
fn a_chain_node_carries_no_change_and_takes_no_gesture (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let chain_node : &str =
      "***** (skg node (gnid d) (skgRepos (nodeRepo main)) writeProtected";
    let marked : String = edit_line (
      &buffer, chain_node,
      |line| { assert! ( ! line . contains ("(diff"), "{}", line );
               format! ( "{} (editRequest (ready rel))) d", // beside the node's parts
                         line . strip_suffix (") d") . unwrap () ) } );
    assert_ne! ( marked, buffer, "the gesture was typed:\n{}", buffer );
    let saved : String = without_viewnode_ids (
      & save_cleanly (f, &all_changes_view (), &marked) ? . saved_view );
    assert! ( saved . lines () . any ( |line|
                without_viewnode_ids (line) . starts_with (chain_node) && ! line . contains ("(diff") ),
              "the chain node shows no change:\n{}", saved );
    assert! ( saved . lines () . any ( |line|
                without_viewnode_ids (line) . starts_with ("** (skg node (gnid c) (skgRepos (nodeRepo main)) noClaim")
                && line . ends_with ("(diff (rel removed) (leaves 1 rel))) c") ),
              "c, as d's removed member, is not ready:\n{}", saved );
    Ok (( )) }) }

/// Marking a removed alias or a removed ID marks its leaf: ali's lost
/// alias a1 and lost extra ID ali-old show ready after the save.
#[test]
fn marking_a_removed_entry_marks_its_leaf (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let marked : String = [ "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1",
                            "*** (skg id (view ghostWarrant) (diff (rel removed) (leaves 1))) ali-old" ]
      . into_iter () . fold ( buffer, |marked, line|
        edit_line ( &marked, line, |line| line . replacen (
          "(diff (rel removed) (leaves 1)))", "(editRequest (ready)) (diff (rel removed) (leaves 1)))", 1 )) );
    let saved : String = save_cleanly (f, &all_changes_view (), &marked) ? . saved_view;
    for line in [ "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (ready))) a1",
                  "*** (skg id (view ghostWarrant) (diff (rel removed) (leaves 1) (ready))) ali-old" ] {
      assert! ( without_viewnode_ids (&saved) . lines () . any ( |l| l == line ),
                "{:?} must be ready:\n{}", line, saved ); }
    assert! ( ! without_viewnode_ids (&saved) . contains ("(editRequest"),
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
      &buffer, "** (skg (oldText titleAndBody) (diff (leaves 1))) txt",
      |_| "** (skg (oldText titleAndBody) (editRequest (ready)) (diff (leaves 1))) txt" . to_string () );
    let saved : String = save_cleanly (f, &all_changes_view (), &marked) ? . saved_view;
    assert! ( without_viewnode_ids (&saved) . contains ("** (skg (oldText titleAndBody) (diff (leaves 1) (ready))) txt"),
              "the old text is ready:\n{}", saved );
    let edited : String = edit_line (
      &saved, "* (skg node (gnid txt)",
      |line| line . replace ( "txt, retitled", "txt, retitled again" ));
    let saved : String = save_cleanly (f, &all_changes_view (), &edited) ? . saved_view;
    assert! ( without_viewnode_ids (&saved) . contains ("** (skg (oldText titleAndBody) (diff (leaves 1))) txt"),
              "the edit lapsed the mark:\n{}", saved );
    Ok (( )) }) }

/// The summary counts the changed files of the owned skgrepos and
/// the ready ones, and the leaves owned by nodes the saved buffer does
/// not show. The "sets" fixture changes six files: cy2 and mc
/// (retitled), del and delc (deleted), ln (gains lo) and r (loses
/// del); their seven leaves are the two text changes, the two
/// deletions, and the relationships ln-lo, r-del and del-delc. Roots
/// the user drops from the buffer are origins it no longer shows: the
/// save leaves them out and counts their changes as not editable, and the
/// "integrate unshown changes" request appends them again.
#[test]
fn the_save_reports_the_readiness_summary (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_eq! ( summary_of ( &save_cleanly (f, &all_changes_view (), &buffer) ? ), summary (0, 6, 0) );
    let marked : String = edit_line (
      &buffer, "** (skg (oldText title) (diff (leaves 1))) mc",
      |_| "** (skg (oldText title) (editRequest (ready)) (diff (leaves 1))) mc" . to_string () );
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &marked) ?;
    assert_eq! ( summary_of (&response), summary (1, 6, 0),
                 "mc's file records only its text change" );
    let only_r : String = { // drop the roots cy2, ln and mc, which precede r
      let lines : Vec<&str> = response . saved_view . lines () . collect ();
      let start : usize = lines . iter ()
        . position ( |line| without_viewnode_ids (line) . starts_with ("* (skg node (gnid r)") )
        . expect ("r is a root");
      lines [start ..] . join ("\n") + "\n" };
    let response : SaveResponse = save_cleanly (f, &all_changes_view (), &only_r) ?;
    assert_eq! ( response . saved_view . lines ()
                   . filter ( |line| line . starts_with ("* ") ) . count (), 1,
                 "the dropped roots stay dropped:\n{}", response . saved_view );
    assert_eq! ( summary_of (&response), summary (1, 6, 3) );
    let (shown, not_editable) : (String, usize) = integrate_unshown_changes (f) ?;
    let roots : Vec<&str> = shown . lines ()
      . filter ( |line| line . starts_with ("* ") ) . collect ();
    assert! ( roots . len () == 4 && without_viewnode_ids (roots [0]) . starts_with ("* (skg node (gnid r)")
              && roots [1] . contains ("(gnid cy2)") && roots [2] . contains ("(gnid ln)")
              && roots [3] . contains ("(gnid mc)"),
              "the dropped roots return, after r:\n{}", shown );
    assert_eq! ( not_editable, 0 );
    Ok (( )) }) }

/// A save of the merges buffer of the 'buffers' fixture (described in
/// 'other_buffers.rs') counts as not editable the eleven leaves of the nodes
/// it does not show editable: txt's text, the home and relRepo moves
/// of mvc, mvg, mvp, mvr and mvt, and the creation of pres, which it
/// shows as a write-protected member of acq's content.
#[test]
fn the_summary_counts_the_changes_the_saved_buffer_does_not_show (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &["public", "private"], |f| {
    let text : String = open_buffer (f, DiffViewBufferKind::Merges) ?
      . ok_or ("the merges buffer is empty") ?;
    let response : SaveResponse = save_buffer (f, DiffViewBufferKind::Merges, &text) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    let summary : BufferReadinessSummary = summary_of (&response);
    assert_eq! ( summary . changes_not_editable_here, 11, "{:?}", summary );
    assert_eq! ( sexp_to_client_string (&summary . to_sexp ()),
                 format! ( "(readiness-summary (ready {}) (changed {}) (foreign 0) (order-only 1) (notEditable 11))",
                           summary . summary . ready_files,
                           summary . summary . changed_files ));
    Ok (( )) }) }

/// The 'foreign' fixture has the owned skgrepo main, which retitles
/// m, and the foreign skgrepo foreign, a gitrepo too, which retitles
/// f and deletes g. The summary counts m's file as changed and the
/// foreign skgrepo's two files apart, as foreign.
#[test]
fn the_summary_counts_the_changed_files_of_the_foreign_skgrepos (
) -> Result<(), Box<dyn Error>> {
  with_fixture_and_foreign ("foreign", &["main"], &["foreign"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let summary : BufferReadinessSummary =
      summary_of ( &save_cleanly (f, &all_changes_view (), &buffer) ? );
    assert_eq! ( summary . summary, ReadinessSummary {
      ready_files : 0, changed_files : 1,
      foreign_changed_files : 2, order_only_files : 0 } );
    assert! ( sexp_to_client_string (&summary . to_sexp ())
                . contains ("(foreign 2)"),
              "{:?}", summary );
    Ok (( )) }) }

fn summary_of (
  response : &SaveResponse,
) -> BufferReadinessSummary {
  response . readiness_summary . clone ()
    . expect ("a save of a diff-view buffer reports the readiness summary") }

fn summary (
  ready_files     : usize,
  changed_files   : usize,
  changes_not_editable_here : usize,
) -> BufferReadinessSummary {
  BufferReadinessSummary {
    summary : ReadinessSummary {
      ready_files, changed_files,
      foreign_changed_files : 0, order_only_files : 0 },
    changes_not_editable_here } }

/// Save 'buffer' with 'pid' (an editable occurrence on a line
/// starting 'prefix') marked for deletion; return the reconciled
/// buffer, after checking that saving it again changes nothing.
fn save_deleting (
  f      : &mut Fixture,
  buffer : &str,
  prefix : &str,
  pid    : &str,
) -> Result<String, Box<dyn Error>> {
  let node : String = format! ("node (gnid {})", pid);
  let deleting : String = edit_line (
    buffer, prefix,
    |line| line . replacen (
      &node, &format! ("{} (editRequest delete)", node), 1 ));
  let saved : String = save_cleanly (f, &all_changes_view (), &deleting) ? . saved_view;
  assert_eq! ( save_cleanly (f, &all_changes_view (), &saved) ? . saved_view, saved,
               "the reconciled buffer is a fixed point" );
  Ok (saved) }

/// A node deleted in the buffer is shown as the pruned change view shows a
/// deleted node: where it stood among its container's content, and
/// in the changed container folder of what it contained. Where it
/// stood, its viewnode stays, with its folded body.
#[test]
fn a_member_deleted_in_the_buffer_is_rerendered_as_removed (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = save_deleting (
      f, &buffer, "** (skg bodyFolded node (gnid cy1)", "cy1" ) ?;
    assert! ( ! without_viewnode_ids (&saved) . contains ("(deleted (gnid cy1)"), "{}", saved );
    let historical : &str = "ghost (gnid cy1) (skgRepos (nodeRepo main)) ";
    let facts : &str = "(diff (node removed) (rel removed) (leaves 2 node rel))) cy1";
    assert_eq! ( without_viewnode_ids (&saved) . matches (historical) . count (), 2,
                 "in cy2's content and in its container folder:\n{}", saved );
    assert! ( without_viewnode_ids (&saved) . contains (
                &format! ("** (skg bodyFolded {}noClaim {}", historical, facts) ),
              "cy1, context before, keeps its folded body, claiming nothing:\n{}", saved );
    assert! ( without_viewnode_ids (&saved) . contains ( &format! ("{}{}", historical, facts) ),
              "in the container folder it carries no claim atom:\n{}", saved );
    Ok (( )) }) }

#[test]
fn a_root_deleted_in_the_buffer_is_rerendered_as_removed (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let saved : String = save_deleting (
      f, &buffer, "* (skg node (gnid flg)", "flg" ) ?;
    assert! ( without_viewnode_ids (&saved) . contains (
      "* (skg ghost (gnid flg) (skgRepos (nodeRepo public)) (diff (node removed) (leaves 1 node))) flg" ),
      "{}", saved );
    assert! ( ! without_viewnode_ids (&saved) . contains ("(deleted (gnid flg)"), "{}", saved );
    Ok (( )) }) }

/// A node in both graphs that records a leaf is modified: its change is
/// '(properties changed)' (herald 'M'), and its leaves fact lists
/// 'all', every leaf it records. cy2, retitled, stands as a root
/// and, write-protected, in cy1's content, an unchanged
/// relationship. One request naming 'all' on the write-protected
/// image marks its file: both images then show '(ready all)', and
/// its old text shows ready.
#[test]
fn a_modified_node_lists_all_and_one_mark_readies_its_file (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let modified : &str = "(diff (properties changed) (leaves 1 all))) cy2, retitled";
    assert! ( without_viewnode_ids (&buffer) . lines () . any ( |line|
      without_viewnode_ids (line) . starts_with ("*** (skg node (gnid cy2) (skgRepos (nodeRepo main)) writeProtected")
      && line . ends_with (modified) ),
      "cy2 is modified in cy1's content:\n{}", buffer );
    let marked : String = edit_line (
      &buffer, "*** (skg node (gnid cy2)",
      |line| line . replace ( "(diff (properties changed) (leaves 1 all)))",
                              "(editRequest (ready all)) (diff (properties changed) (leaves 1 all)))" ));
    let saved : String = save_cleanly (f, &all_changes_view (), &marked) ? . saved_view;
    let ready : &str =
      "(diff (properties changed) (leaves 1 all) (ready all))) cy2, retitled";
    for prefix in [ "* (skg node (gnid cy2)", "*** (skg node (gnid cy2)" ] {
      assert! ( without_viewnode_ids (&saved) . lines () . any ( |line|
        without_viewnode_ids (line) . starts_with (prefix) && line . ends_with (ready) ),
        "{:?} is ready:\n{}", prefix, saved ); }
    assert! ( without_viewnode_ids (&saved) . contains ("** (skg (oldText title) (diff (leaves 1) (ready))) cy2"),
              "cy2's old text is ready:\n{}", saved );
    Ok (( )) }) }

/// A modified node in a new relationship lists 'rel' beside 'all',
/// and N counts the distinct leaves the two name: lo, new in ln's
/// content, once retitled records its text, so N is 2.
#[test]
fn a_modified_node_in_a_new_relationship_lists_rel_and_all (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( without_viewnode_ids (&buffer) . contains ("(diff (rel added) (leaves 1 rel))) lo\n"),
              "lo, unmodified, is new in ln's content:\n{}", buffer );
    let retitled : String = edit_line (
      &buffer, "** (skg node (gnid lo)",
      |line| format! ( "{}, retitled", line ));
    let saved : String = save_cleanly (f, &all_changes_view (), &retitled) ? . saved_view;
    assert! ( without_viewnode_ids (&saved) . lines () . any ( |line|
      without_viewnode_ids (line) . starts_with ("** (skg node (gnid lo)")
      && line . ends_with ("(diff (rel added) (properties changed) (leaves 2 rel all))) lo, retitled") ),
      "lo is modified, in a new relationship:\n{}", saved );
    Ok (( )) }) }

/// A new node is tagged and counted as before: its existence and its
/// relationship, one leaf each.
#[test]
fn a_new_node_lists_node_and_rel (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = insert_after_line_starting (
      &buffer, "* (skg node (gnid ct)", "** a new child" );
    let saved : String = save_cleanly (f, &all_changes_view (), &edited) ? . saved_view;
    assert! ( without_viewnode_ids (&saved) . lines () . any ( |line|
      without_viewnode_ids (line) . starts_with ("** (skg node (gnid ")
      && line . ends_with ("(diff (node added) (rel added) (leaves 2 node rel))) a new child") ),
      "the new child is new, in a new relationship:\n{}", saved );
    Ok (( )) }) }

/// A root is tagged at its own visit, as a member is at its
/// viewparent's: txt, retitled, is a modified root.
#[test]
fn a_modified_root_carries_its_change (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( without_viewnode_ids (&buffer) . lines () . any ( |line|
      without_viewnode_ids (line) . starts_with ("* (skg node (gnid txt)")
      && line . ends_with ("(diff (properties changed) (leaves 1 all))) txt, retitled") ),
      "txt is a modified root:\n{}", buffer );
    Ok (( )) }) }
