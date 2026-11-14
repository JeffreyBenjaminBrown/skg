// Marking by what and which way ('docs/diff-view.org', "Processed
// requests and gestures"): 'node', 'rel' or both, processed or
// unprocessed, on each kind of headline that names leaves, and beside
// each walk. Each case saves a request and reads the '(diff ...)' parts
// of the rerendered buffer.

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture::{Fixture, edit_line, save_cleanly};
use crate::pruned_change_view::common::{
  all_changes_view, insert_after_line_starting, open_all_changes_buffer,
  open_buffer, save_buffer, with_fixture};

use skg::diff_view::change_leaves::BufferProcessedSummary;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::DiffViewBufferKind;

use std::error::Error;

/// Which line of a buffer a step reads or marks: the one whose text,
/// its viewnode IDs ignored, satisfies it.
type LineTest = &'static dyn Fn (&str) -> bool;

/// One step: the request written on the line 'marked' and saved, then
/// for each line test the '(diff ...)' part that line must show.
struct Step {
  request  : &'static str,
  expected : Vec<(LineTest, &'static str)>,
}

/// Run 'steps' in order over 'buffer', each a save of the buffer the
/// last returned; return the last.
fn run_steps (
  f      : &mut Fixture,
  buffer : String,
  marked : LineTest,
  steps  : Vec<Step>,
) -> Result<String, Box<dyn Error>> {
  let mut buffer : String = buffer;
  for step in steps {
    let response : SaveResponse = save_cleanly (
      f, &all_changes_view (), &with_request (&buffer, marked, step . request) ) ?;
    assert! ( response . warnings . is_empty (),
              "{}: {:?}", step . request, response . warnings );
    buffer = response . saved_view;
    for (line_test, diff) in step . expected {
      assert_eq! ( diff_of ( &the_line (&buffer, line_test) ), diff,
                   "after {}:\n{}", step . request, buffer ); }}
  Ok (buffer) }

/// The one line of 'buffer' that satisfies 'line_test', its viewnode
/// IDs ignored.
fn the_line (
  buffer    : &str,
  line_test : LineTest,
) -> String {
  let lines : Vec<String> = without_viewnode_ids (buffer) . lines ()
    . filter ( |line| line_test (line) ) . map (String::from) . collect ();
  assert_eq! ( lines . len (), 1, "one line should match in:\n{}", buffer );
  lines [0] . clone () }

/// The '(diff ...)' part of 'line', or "" when it has none.
fn diff_of (
  line : &str,
) -> String {
  let Some (start) : Option<usize> = line . find ("(diff ") else {
    return String::new (); };
  let mut depth : usize = 0;
  for (offset, character) in line [start ..] . char_indices () {
    match character {
      '(' => depth += 1,
      ')' => { depth -= 1;
               if depth == 0 {
                 return line [start ..= start + offset] . to_string (); }},
      _ => {}, }}
  panic! ( "an unbalanced '(diff ...)' in {:?}", line ) }

/// 'buffer' with '(editRequest REQUEST)' on the line 'marked' picks,
/// before its '(diff ...)', or after its '(skgRepos ...)' if it has none.
fn with_request (
  buffer  : &str,
  marked  : LineTest,
  request : &str,
) -> String {
  let lines : Vec<String> = buffer . lines ()
    . map ( |line| {
      if ! marked ( &without_viewnode_ids (line) ) { return line . to_string (); }
      let edit_request : String = format! ("(editRequest {}) ", request);
      match line . find ("(diff ") {
        Some (at) => format! ("{}{}{}", &line [.. at], edit_request, &line [at ..]),
        None => {
          let after_skgRepos : usize = line . find ("(skgRepos ")
            . and_then ( |start| line [start ..] . find (")) ") . map ( |end| start + end + 3 ))
            . expect ("a vognode names its skgrepo");
          format! ("{}{}{}", &line [.. after_skgRepos], edit_request,
                   &line [after_skgRepos ..] ) }, }} )
    . collect ();
  assert_eq! ( lines . iter () . filter ( |line| line . contains ("(editRequest ") ) . count (), 1,
               "one line should be marked in:\n{}", buffer );
  lines . join ("\n") + "\n" }

/// Each letter each way on a new node, a new child of ct: 'node' marks
/// its existence, 'rel' its relationship to ct.
#[test]
fn each_letter_each_way_on_a_new_node (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let opened : String = open_all_changes_buffer (f) ?;
    let buffer : String = save_cleanly ( f, &all_changes_view (),
      &insert_after_line_starting (&opened, "* (skg node (gnid ct)", "** a new child") ) ?
      . saved_view;
    let child : LineTest = &|line| line . ends_with (") a new child");
    let base : &str = "(diff (node added) (rel added) (leaves 2 node rel)";
    assert_eq! ( diff_of ( &the_line (&buffer, child) ), format! ("{})", base) );
    run_steps ( f, buffer, child, vec! [
      Step { request : "(processed node)",
             expected : vec! [ (child, "(diff (node added) (rel added) (leaves 2 node rel) (processed node))") ] },
      Step { request : "(processed rel)",
             expected : vec! [ (child, "(diff (node added) (rel added) (leaves 2 node rel) (processed node rel))") ] },
      Step { request : "(unprocessed node)",
             expected : vec! [ (child, "(diff (node added) (rel added) (leaves 2 node rel) (processed rel))") ] },
      Step { request : "(unprocessed rel)",
             expected : vec! [ (child, "(diff (node added) (rel added) (leaves 2 node rel))") ] },
      Step { request : "(processed node rel)",
             expected : vec! [ (child, "(diff (node added) (rel added) (leaves 2 node rel) (processed node rel))") ] },
      Step { request : "(unprocessed node rel)",
             expected : vec! [ (child, "(diff (node added) (rel added) (leaves 2 node rel))") ] },
    ] ) ?;
    Ok (( )) }) }

/// Each letter each way on a removed node, the ghost of del under r:
/// 'node' marks its existence, 'rel' r's containment of it. Neither
/// marks del's containment of delc, which delc's own headline shows.
#[test]
fn each_letter_each_way_on_a_removed_node (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let del  : LineTest = &|line| line . starts_with ("** (skg ghost (gnid del)");
    let delc : LineTest = &|line| line . starts_with ("*** (skg ghost (gnid delc)");
    let delc_unmarked : &str = "(diff (node removed) (rel removed) (leaves 2 node rel))";
    run_steps ( f, buffer, del, vec! [
      Step { request : "(processed node)",
             expected : vec! [ (del,  "(diff (node removed) (rel removed) (leaves 2 node rel) (processed node))"),
                               (delc, delc_unmarked) ] },
      Step { request : "(processed rel)",
             expected : vec! [ (del,  "(diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))"),
                               (delc, delc_unmarked) ] },
      Step { request : "(unprocessed node)",
             expected : vec! [ (del,  "(diff (node removed) (rel removed) (leaves 2 node rel) (processed rel))") ] },
      Step { request : "(unprocessed rel)",
             expected : vec! [ (del,  "(diff (node removed) (rel removed) (leaves 2 node rel))") ] },
      Step { request : "(processed node rel)",
             expected : vec! [ (del,  "(diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))"),
                               (delc, delc_unmarked) ] },
      Step { request : "(unprocessed node rel)",
             expected : vec! [ (del,  "(diff (node removed) (rel removed) (leaves 2 node rel))") ] },
    ] ) ?;
    Ok (( )) }) }

/// Each letter each way on a modified node: sub, which unsubscribed
/// from s1 and subscribed to s2, in s1's subscriber folder. There 'rel'
/// is its subscription to s1 and 'node' the one to s2, which no
/// headline under it shows; in s2's subscriber folder the two swap.
#[test]
fn each_letter_each_way_on_a_modified_node (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let under_s1 : LineTest = &|line|
      line . contains ("node (gnid sub) ") && line . contains ("(rel removed)");
    let under_s2 : LineTest = &|line|
      line . contains ("node (gnid sub) ") && line . contains ("(rel added)");
    let s1 = |processed : &str| -> &'static str { Box::leak ( format! (
      "(diff (rel removed) (properties changed) (leaves 2 node rel){})", processed )
      . into_boxed_str () ) };
    let s2 = |processed : &str| -> &'static str { Box::leak ( format! (
      "(diff (rel added) (properties changed) (leaves 2 node rel){})", processed )
      . into_boxed_str () ) };
    run_steps ( f, buffer, under_s1, vec! [
      Step { request : "(processed node)",
             expected : vec! [ (under_s1, s1 (" (processed node)")),
                               (under_s2, s2 (" (processed rel)")) ] },
      Step { request : "(processed rel)",
             expected : vec! [ (under_s1, s1 (" (processed node rel)")),
                               (under_s2, s2 (" (processed node rel)")) ] },
      Step { request : "(unprocessed node)",
             expected : vec! [ (under_s1, s1 (" (processed rel)")),
                               (under_s2, s2 (" (processed node)")) ] },
      Step { request : "(unprocessed rel)",
             expected : vec! [ (under_s1, s1 ("")), (under_s2, s2 ("")) ] },
      Step { request : "(processed node rel)",
             expected : vec! [ (under_s1, s1 (" (processed node rel)")),
                               (under_s2, s2 (" (processed node rel)")) ] },
      Step { request : "(unprocessed node rel)",
             expected : vec! [ (under_s1, s1 ("")), (under_s2, s2 ("")) ] },
    ] ) ?;
    Ok (( )) }) }

/// Marking both a modified node and its relationship leaves its
/// children's containment changes to their own headlines: ln, once
/// retitled, records its text and its containment of lo; 'node rel'
/// on ln marks its text alone, and 'rel' on lo marks the containment,
/// which its changeset marker then shows processed.
#[test]
fn marking_a_modified_node_leaves_its_childrens_containment (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let opened : String = open_all_changes_buffer (f) ?;
    let retitled : String =
      edit_line (&opened, "* (skg node (gnid ln)", |line| format! ("{}, retitled", line));
    let only_ln : String = // r's content marker would match too
      only_the_root (&retitled, "* (skg node (gnid ln)");
    let buffer : String = save_cleanly (f, &all_changes_view (), &only_ln) ? . saved_view;
    let ln     : LineTest = &|line| line . starts_with ("* (skg node (gnid ln)");
    let marker : LineTest = &|line| line . starts_with ("** (skg (changesetMarker content)");
    let lo     : LineTest = &|line| line . starts_with ("** (skg node (gnid lo)");
    assert_eq! ( the_line (&buffer, marker), "** (skg (changesetMarker content) (diff (leaves 1 rel)))",
                 "{}", buffer );
    let buffer : String = run_steps ( f, buffer, ln, vec! [
      Step { request : "(processed node rel)",
             expected : vec! [ (ln,     "(diff (properties changed) (leaves 1 node) (processed node))"),
                               (lo,     "(diff (rel added) (leaves 1 rel))"),
                               (marker, "(diff (leaves 1 rel))") ] },
    ] ) ?;
    run_steps ( f, buffer, lo, vec! [
      Step { request : "(processed rel)",
             expected : vec! [ (lo,     "(diff (rel added) (leaves 1 rel) (processed rel))"),
                               (marker, "(diff (leaves 1 rel) (processed rel))") ] },
    ] ) ?;
    Ok (( )) }) }

/// A changed folder is acted on through its members, each an ordinary
/// headline: in sub's subscribee folder, s1 and s2 each name their
/// subscription by 'rel', and 'node' on either names nothing, since
/// neither node changed. 'node rel' marks the subscription; 'node'
/// alone is dropped with a warning.
#[test]
fn each_letter_on_the_members_of_a_changed_folder (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let s1 : LineTest = &|line| line . starts_with ("*** (skg node (gnid s1)");
    let s2 : LineTest = &|line| line . starts_with ("*** (skg node (gnid s2)");
    let buffer : String = run_steps ( f, buffer, s1, vec! [
      Step { request : "(processed rel)",
             expected : vec! [ (s1, "(diff (rel removed) (leaves 1 rel) (processed rel))"),
                               (s2, "(diff (rel added) (leaves 1 rel))") ] },
      Step { request : "(unprocessed node rel)",
             expected : vec! [ (s1, "(diff (rel removed) (leaves 1 rel))") ] },
    ] ) ?;
    let buffer : String = run_steps ( f, buffer, s2, vec! [
      Step { request : "(processed node rel)",
             expected : vec! [ (s2, "(diff (rel added) (leaves 1 rel) (processed rel))") ] },
    ] ) ?;
    let response : SaveResponse = save_cleanly (
      f, &all_changes_view (), &with_request (&buffer, s1, "(processed node)") ) ?;
    assert! ( response . warnings . iter () . any ( |warning|
                warning . starts_with ("A processed request names no current change") ),
              "{:?}", response . warnings );
    assert_eq! ( diff_of ( &the_line (&response . saved_view, s1) ),
                 "(diff (rel removed) (leaves 1 rel))" );
    Ok (( )) }) }

/// On a changeset marker 'rel' names the relationships it gathers and
/// 'node' the own leaves of their members, one level down, though its
/// '(leaves ...)' lists only 'rel', what it shows; a bare request names
/// 'rel'. r's content marker gathers r's containment of del, whose
/// ghost shows 'node' as del's existence and 'rel' as that containment.
#[test]
fn each_letter_each_way_on_a_changeset_marker (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let del : LineTest = &|line| line . starts_with ("** (skg ghost (gnid del)");
    let only_r : String = buffer . lines () // dropping cy2, ln and mc, whose marker is ln's
      . skip_while ( |line| ! without_viewnode_ids (line) . starts_with ("* (skg node (gnid r)") )
      . collect::<Vec<&str>> () . join ("\n") + "\n";
    let buffer : String = save_cleanly (f, &all_changes_view (), &only_r) ? . saved_view;
    let marker : LineTest = &|line| line . starts_with ("** (skg (changesetMarker content)");
    run_steps ( f, buffer, marker, vec! [
      Step { request : "(processed node)",
             expected : vec! [ (marker, "(diff (leaves 1 rel))"),
                               (del,    "(diff (node removed) (rel removed) (leaves 2 node rel) (processed node))") ] },
      Step { request : "(processed rel)",
             expected : vec! [ (marker, "(diff (leaves 1 rel) (processed rel))"),
                               (del,    "(diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))") ] },
      Step { request : "(unprocessed node)",
             expected : vec! [ (marker, "(diff (leaves 1 rel) (processed rel))"),
                               (del,    "(diff (node removed) (rel removed) (leaves 2 node rel) (processed rel))") ] },
      Step { request : "(unprocessed node rel)",
             expected : vec! [ (marker, "(diff (leaves 1 rel))"),
                               (del,    "(diff (node removed) (rel removed) (leaves 2 node rel))") ] },
      Step { request : "(processed)",
             expected : vec! [ (marker, "(diff (leaves 1 rel) (processed rel))"),
                               (del,    "(diff (node removed) (rel removed) (leaves 2 node rel) (processed rel))") ] },
    ] ) ?;
    Ok (( )) }) }

/// Beside a walk the letters filter what it reaches: 'node' the
/// existence of each new node, 'rel' the relationships they record,
/// and with neither, both. Each letter also names its own object on
/// the headline: the new child's existence for 'node', ct's containment
/// of it for 'rel', which no walk from the child reaches.
#[test]
fn the_letters_filter_each_walk (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let opened : String = open_all_changes_buffer (f) ?;
    let buffer : String = save_cleanly ( f, &all_changes_view (),
      &insert_after_line_starting ( &opened, "* (skg node (gnid ct)",
                                    "** a new child\n*** a new grandchild" ) ) ?
      . saved_view;
    let child      : LineTest = &|line| line . ends_with (") a new child");
    let grandchild : LineTest = &|line| line . ends_with (") a new grandchild");
    let new = |processed : &str| -> &'static str { Box::leak ( format! (
      "(diff (node added) (rel added) (leaves 2 node rel){})", processed )
      . into_boxed_str () ) };
    run_steps ( f, buffer, child, vec! [
      Step { request : "(processed node newContent)",
             expected : vec! [ (child, new (" (processed node)")),
                               (grandchild, new (" (processed node)")) ] },
      Step { request : "(processed rel newContent)",
             expected : vec! [ (child, new (" (processed node rel)")),
                               (grandchild, new (" (processed node rel)")) ] },
      Step { request : "(unprocessed newContent)",
             expected : vec! [ (child, new (" (processed rel)")),
                               (grandchild, new ("")) ] },
      Step { request : "(processed rel newOutward)",
             expected : vec! [ (child, new (" (processed rel)")),
                               (grandchild, new (" (processed rel)")) ] },
      Step { request : "(processed node newOutward)",
             expected : vec! [ (child, new (" (processed node rel)")),
                               (grandchild, new (" (processed node rel)")) ] },
      Step { request : "(unprocessed node rel newOutward)",
             expected : vec! [ (child, new ("")), (grandchild, new ("")) ] },
    ] ) ?;
    Ok (( )) }) }

/// A walk from a node that is not new starts at its new members: from
/// ct, 'node newContent' marks the existence of the new child and
/// grandchild, and 'rel newContent' the child's containment of the
/// grandchild, but not ct's of the child, which ct records, nor
/// anything of ct's own, which other headlines show.
#[test]
fn a_walk_from_a_node_that_is_not_new_starts_at_its_new_members (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let opened : String = open_all_changes_buffer (f) ?;
    let buffer : String = save_cleanly ( f, &all_changes_view (),
      &insert_after_line_starting ( &opened, "* (skg node (gnid ct)",
                                    "** a new child\n*** a new grandchild" ) ) ?
      . saved_view;
    let ct         : LineTest = &|line| line . starts_with ("* (skg node (gnid ct)");
    let child      : LineTest = &|line| line . ends_with (") a new child");
    let grandchild : LineTest = &|line| line . ends_with (") a new grandchild");
    let new = |processed : &str| -> &'static str { Box::leak ( format! (
      "(diff (node added) (rel added) (leaves 2 node rel){})", processed )
      . into_boxed_str () ) };
    run_steps ( f, buffer, ct, vec! [
      Step { request : "(processed node newContent)",
             expected : vec! [ (ct, ""),
                               (child, new (" (processed node)")),
                               (grandchild, new (" (processed node)")) ] },
      Step { request : "(processed rel newContent)",
             expected : vec! [ (child, new (" (processed node)")),
                               (grandchild, new (" (processed node rel)")) ] },
      Step { request : "(unprocessed newContent)",
             expected : vec! [ (child, new ("")), (grandchild, new ("")) ] },
    ] ) ?;
    Ok (( )) }) }

/// 'node' on a content changeset marker names the own leaves of its
/// members: ln's marker gathers its containment of lo, and once lo is
/// retitled, 'node' there marks lo's text, which lo's M shows.
#[test]
fn node_on_a_changeset_marker_marks_a_modified_members_own_leaves (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let opened : String = open_all_changes_buffer (f) ?;
    let lo_retitled : String =
      edit_line (&opened, "** (skg node (gnid lo)", |line| format! ("{}, retitled", line));
    let buffer : String = save_cleanly ( f, &all_changes_view (),
      &only_the_root (&lo_retitled, "* (skg node (gnid ln)") ) ? . saved_view;
    let marker : LineTest = &|line| line . starts_with ("** (skg (changesetMarker content)");
    let lo     : LineTest = &|line| line . starts_with ("** (skg node (gnid lo)");
    let lo_diff = |processed : &str| -> &'static str { Box::leak ( format! (
      "(diff (rel added) (properties changed) (leaves 2 node rel){})", processed )
      . into_boxed_str () ) };
    run_steps ( f, buffer, marker, vec! [
      Step { request : "(processed node)",
             expected : vec! [ (marker, "(diff (leaves 1 rel))"),
                               (lo, lo_diff (" (processed node)")) ] },
      Step { request : "(processed rel)",
             expected : vec! [ (marker, "(diff (leaves 1 rel) (processed rel))"),
                               (lo, lo_diff (" (processed node rel)")) ] },
      Step { request : "(unprocessed rel)",
             expected : vec! [ (marker, "(diff (leaves 1 rel))"),
                               (lo, lo_diff (" (processed node)")) ] },
      Step { request : "(unprocessed node rel)",
             expected : vec! [ (marker, "(diff (leaves 1 rel))"),
                               (lo, lo_diff ("")) ] },
    ] ) ?;
    Ok (( )) }) }

/// The relRepo changeset marker gathers the relRepo moves of its
/// node's content relationships: 'rel' on rr's marker marks rm's move,
/// shown under rm; 'node' names rm's own leaves, of which it has none,
/// so it is dropped with a warning.
#[test]
fn each_letter_on_the_relRepo_changeset_marker (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let marker : LineTest = &|line| line . starts_with ("** (skg (changesetMarker relRepo)");
    let moved  : LineTest = &|line| line . starts_with ("*** (skg (relRepoMove ");
    let buffer : String = run_steps ( f, buffer, marker, vec! [
      Step { request : "(processed rel)",
             expected : vec! [ (marker, "(diff (leaves 1 rel) (processed rel))"),
                               (moved,  "(diff (leaves 1) (processed))") ] },
      Step { request : "(unprocessed rel)",
             expected : vec! [ (marker, "(diff (leaves 1 rel))"),
                               (moved,  "(diff (leaves 1))") ] },
    ] ) ?;
    let response : SaveResponse = save_cleanly (
      f, &all_changes_view (), &with_request (&buffer, marker, "(processed node)") ) ?;
    assert! ( response . warnings . iter () . any ( |warning|
                warning . starts_with ("A processed request names no current change") ),
              "{:?}", response . warnings );
    Ok (( )) }) }

/// The root starting 'prefix' and its subtree, the other roots
/// dropped, as a user drops roots from the buffer.
fn only_the_root (
  buffer : &str,
  prefix : &str,
) -> String {
  let lines : Vec<&str> = buffer . lines () . collect ();
  let start : usize = lines . iter ()
    . position ( |line| without_viewnode_ids (line) . starts_with (prefix) )
    . expect ("the root is shown");
  let end : usize = lines [start + 1 ..] . iter ()
    . position ( |line| line . starts_with ("* ") )
    . map_or ( lines . len (), |offset| start + 1 + offset );
  lines [start .. end] . join ("\n") + "\n" }

/// Every leaf a gesture without a prefix could reach before 'all' went
/// is reached by some mark without a prefix: in each fixture's buffer,
/// one save marking every headline that lists leaves, on the objects
/// it lists (bare where it lists none), leaves every changed file
/// processed. A reordering is no leaf: an order-only file counts as
/// processed ('buffers' has one).
#[test]
fn marking_every_headline_on_its_listed_objects_marks_every_change (
) -> Result<(), Box<dyn Error>> {
  for (fixture, skgrepos) in [
    ( "kinds",   &["public", "private"][..] ),
    ( "sets",    &["main"][..] ),
    ( "buffers", &["public", "private"][..] ),
    ( "historical_content", &["main"][..] ),
    ( "removed_partners",   &["main"][..] ), ] {
    with_fixture (fixture, skgrepos, |f| {
      let buffer : String = open_buffer (f, DiffViewBufferKind::AllChanges) ?
        . ok_or ("the buffer is empty") ?;
      let response : SaveResponse = save_buffer (
        f, DiffViewBufferKind::AllChanges, &every_headline_marked (&buffer) ) ?;
      assert! ( response . errors . is_empty () && response . warnings . is_empty (),
                "{}: {:?} {:?}", fixture, response . errors, response . warnings );
      let summary : BufferProcessedSummary = response . processed_summary
        . expect ("a save of a diff-view buffer reports the processed summary");
      assert_eq! ( summary . changes_not_editable_here, 0, "{}: {:?}", fixture, summary );
      assert_eq! ( summary . summary . processed_nodes, summary . summary . changed_nodes,
                   "{}: {:?}\n{}", fixture, summary, response . saved_view );
      Ok (( )) }) ?; }
  Ok (( )) }

/// 'buffer' with a processed request on every headline that carries a
/// '(leaves N OBJECT...)' fact, naming those objects.
fn every_headline_marked (
  buffer : &str,
) -> String {
  buffer . lines ()
    . map ( |line| {
      let (Some (leaves_at), Some (diff_at)) = ( line . find ("(leaves "), line . find ("(diff ") )
        else { return line . to_string (); };
      assert! ( ! line . contains ("(editRequest "), "{}", line );
      let leaves : &str = &line [ leaves_at + "(leaves " . len () .. ];
      let objects : Vec<&str> = leaves [ .. leaves . find (')') . unwrap () ]
        . split (' ') . skip (1) . collect ();
      let request : String =
        [ vec! [ "(editRequest (processed" ], objects ] . concat () . join (" ") + ")) ";
      format! ( "{}{}{}", &line [.. diff_at], request, &line [diff_at ..] ) } )
    . collect::<Vec<String>> () . join ("\n") + "\n" }
