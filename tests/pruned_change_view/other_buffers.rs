// The merges, skgrepo-changes and order-only buffers of the
// diff view. In the 'buffers' fixture (owned skgrepos public and
// private):
// - top contains acq, which acquired acqd by a nodeMerge; pres, new
//   in acq's content, is acqd's text preserver. txt is retitled.
// - mvp moved from public to private. It contains mvc (moved the same
//   way, as did mvc's content mvg), mvr (moved the other way) and mvs
//   (unmoved), whose content mvt moved like mvp.
// - ord's content changed only in order: o1, o2 became o2, o1.
// The 'kinds' fixture, described in 'buffer.rs', has a relRepo-only
// move (rr's containment of rm) and a text-only change (txt).

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture::{Fixture, graph_straddle};
use crate::pruned_change_view::common::{
  assert_lines_in_order, ids, open_buffer, saves_back_unchanged,
  with_fixture};

use skg::graph_straddle::types::GraphStraddle;
use skg::diff_view::pruned_change_view::{
  PrunedChangeViewSets, prunedChangeView_sets_for_skgrepo_changes,
  prunedChangeView_sets_for_merges};
use skg::types::views_state::DiffViewBufferKind;

use std::error::Error;

const SKGREPOS : [&str; 2] = [ "public", "private" ];

/// The text of a buffer of the 'buffers' fixture.
fn buffer (
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<String, Box<dyn Error>> {
  buffer_of_case ("buffers", diffView_buffer_kind) }

/// The text of a buffer of the fixture 'case'.
fn buffer_of_case (
  case                 : &str,
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<String, Box<dyn Error>> {
  let mut text : String = String::new ();
  with_fixture (case, &SKGREPOS, |f : &mut Fixture| {
    text = open_buffer (f, diffView_buffer_kind) ?
      . ok_or ("the fixture's buffer is empty") ?;
    Ok (( )) }) ?;
  Ok (text) }

fn roots (
  text : &str,
) -> Vec<&str> {
  text . lines () . filter ( |line| line . starts_with ("* ") ) . collect () }

/// The headline of the node 'id', which the buffer must hold.
fn headline_of<'a> (
  text : &'a str,
  id   : &str,
) -> &'a str {
  let node : String = format! ( "node (gnid {}) ", id );
  text . lines () . find ( |line| line . contains (&node) )
    . unwrap_or_else ( || panic! ( "no headline of {} in\n{}", id, text )) }

/// The number of leading asterisks of a headline.
fn level_of (
  headline : &str,
) -> usize {
  headline . chars () . take_while ( |c| *c == '*' ) . count () }

/// The headlines of the children of the editable occurrence of the
/// node 'id', which the buffer must hold.
fn child_headlines_of_editable<'a> (
  text : &'a str,
  id   : &str,
) -> Vec<&'a str> {
  let node : String = format! ( "node (gnid {}) ", id );
  let headline : &str =
    text . lines ()
    . find ( |line| line . contains (&node)
                    && ! line . contains ("writeProtected") )
    . unwrap_or_else ( || panic! ( "no editable {} in\n{}", id, text ));
  let level : usize = level_of (headline);
  text . lines ()
    . skip_while ( |line| *line != headline )
    . skip (1)
    . filter ( |line| level_of (line) > 0 )
    . take_while ( |line| level_of (line) > level )
    . filter ( |line| level_of (line) == level + 1 )
    . collect () }

#[test]
fn the_merges_sets_hold_the_acquiree_and_the_acquirer (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let straddle : GraphStraddle = graph_straddle (f) ?;
    let sets : PrunedChangeViewSets = prunedChangeView_sets_for_merges (&straddle);
    assert_eq! ( sets . c, ids (&[ "acq", "acqd" ]) );
    assert_eq! ( sets . d, ids (&[ "acqd", "top" ]) );
    Ok (( )) }) }

#[test]
fn the_skgrepo_changes_sets_hold_the_moved_nodes (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let straddle : GraphStraddle = graph_straddle (f) ?;
    let sets : PrunedChangeViewSets = prunedChangeView_sets_for_skgrepo_changes (&straddle);
    assert_eq! ( sets . c, ids (&[ "mvc", "mvg", "mvp", "mvr", "mvt" ]) );
    assert_eq! ( sets . d, ids (&[ "mvp" ]) );
    assert! ( sets . u . contains (&"mvs" . into ()),
              "mvs is on the walk from mvt to mvp" );
    Ok (( )) }) }

#[test]
fn the_skgrepo_changes_sets_hold_the_recorder_of_a_relRepo_move (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &SKGREPOS, |f| {
    let straddle : GraphStraddle = graph_straddle (f) ?;
    let sets : PrunedChangeViewSets = prunedChangeView_sets_for_skgrepo_changes (&straddle);
    assert_eq! ( sets . c, ids (&[ "mv", "rr" ]) );
    Ok (( )) }) }

#[test]
fn the_skgrepo_changes_buffer_shows_the_recorder_of_a_relRepo_move (
) -> Result<(), Box<dyn Error>> {
  let text : String =
    buffer_of_case ("kinds", DiffViewBufferKind::SkgrepoChanges) ?;
  assert_lines_in_order ( &text, &[
    "* (skg node (gnid rr)",
    "** (skg (changesetMarker relRepo) (diff (leaves 1 rel)))",
    "** (skg node (gnid rm)",
    "*** (skg (relRepoMove (old public) (new private)) (diff (leaves 1)))" ] );
  assert! ( ! headline_of (&text, "rr") . contains ("homeSkgrepoMove"),
            "rr's home did not move:\n{}", text );
  assert! ( ! text . contains ("(gnid txt)"),
            "txt's only change is its text:\n{}", text );
  Ok (( )) }

#[test]
fn the_merges_buffer_shows_only_the_merge (
) -> Result<(), Box<dyn Error>> {
  let text : String = buffer (DiffViewBufferKind::Merges) ?;
  let roots : Vec<&str> = roots (&text);
  assert_eq! ( roots . len (), 2, "{}", text );
  assert! ( roots [0] . contains ("ghost (gnid acqd)"), "{}", text );
  assert! ( roots [1] . contains ("node (gnid top)"), "{}", text );
  assert! ( child_headlines_of_editable (&text, "acq") . iter ()
            . any ( |line| line . contains ("node (gnid pres)") ),
            "the text preserver is shown in acq's content:\n{}", text );
  assert! ( headline_of (&text, "pres") . contains ("(diff (node added) (rel added)"),
            "the text preserver is new in acq's content:\n{}", text );
  assert! ( text . contains ("acquiredByFolder")
            && text . contains ("acquiredFolder"), "{}", text );
  for unrelated in [ "txt", "mvp", "ord" ] {
    assert! ( ! text . contains (&format! ( "(gnid {})", unrelated )),
              "{} is not involved in a merge:\n{}", unrelated, text ); }
  Ok (( )) }

#[test]
fn the_skgrepo_changes_buffer_heralds_each_move_and_moves_like_an_ancestor (
) -> Result<(), Box<dyn Error>> {
  let text : String = buffer (DiffViewBufferKind::SkgrepoChanges) ?;
  assert_eq! ( roots (&text) . len (), 1, "{}", text );
  for (id, facts) in [
    ( "mvp", "(homeSkgrepoMove (old public) (new private)))" ),
    ( "mvc", "(homeSkgrepoMove (old public) (new private)) (movedLikeAncestor a))" ),
    ( "mvg", "(homeSkgrepoMove (old public) (new private)) (movedLikeAncestor a))" ),
    ( "mvr", "(homeSkgrepoMove (old private) (new public)))" ),
    ( "mvt", "(homeSkgrepoMove (old public) (new private)) (movedLikeAncestor b))" ),
  ] {
    assert! ( headline_of (&text, id) . contains (facts),
              "{} should carry {}:\n{}", id, facts, text ); }
  assert! ( ! headline_of (&text, "mvs") . contains ("homeSkgrepoMove"),
            "mvs did not move:\n{}", text );
  for unrelated in [ "txt", "acq", "ord" ] {
    assert! ( ! text . contains (&format! ( "(gnid {})", unrelated )),
              "{} did not move:\n{}", unrelated, text ); }
  Ok (( )) }

#[test]
fn the_homeSkgrepoMove_heralds_are_only_in_the_skgrepo_changes_buffer (
) -> Result<(), Box<dyn Error>> {
  let text : String = buffer (DiffViewBufferKind::AllChanges) ?;
  assert! ( ! headline_of (&text, "mvc") . contains ("homeSkgrepoMove"), "{}", text );
  Ok (( )) }

/// ord only reorders its content: the order-only buffer lists it, with
/// its other changes as their one headline, whose body shows the old
/// order against the new, and its content below in the new order.
#[test]
fn the_order_only_buffer_is_a_flat_list_of_reorders_to_mark (
) -> Result<(), Box<dyn Error>> {
  let text : String = without_viewnode_ids ( &buffer (DiffViewBufferKind::OrderOnlyChanges) ? );
  let lines : Vec<&str> = text . lines () . collect ();
  assert_eq! ( roots (&text) . len (), 1, "{}", text );
  assert! ( lines [0] . starts_with ("* (skg bodyFolded node (gnid ord)")
            && lines [0] . contains ("(diff (properties changed) (leaves 1 node))"),
            "{}", text );
  assert_eq! ( lines [1 .. 6], [
    "** (skg (otherChanges contentOrder) (diff (leaves 1)))",
    "The order of its content (- at HEAD, + now):",
    "+o2", " o1", "-o2" ], "{}", text );
  assert! ( lines [6] . contains ("node (gnid o2) (skgRepos (nodeRepo public)) writeProtected")
            && lines [7] . contains ("node (gnid o1) (skgRepos (nodeRepo public)) writeProtected"),
            "the content is shown in its new order, as leaves:\n{}", text );
  assert_eq! ( lines . len (), 8, "{}", text );
  Ok (( )) }

/// The all-changes buffer shows ord's reorder too, by the same headline.
#[test]
fn the_all_changes_buffer_shows_a_reorder_as_other_changes (
) -> Result<(), Box<dyn Error>> {
  let text : String = without_viewnode_ids ( &buffer (DiffViewBufferKind::AllChanges) ? );
  let lines : Vec<&str> = text . lines () . collect ();
  let ord : usize = lines . iter ()
    . position ( |line| line . contains ("node (gnid ord)") )
    . unwrap_or_else ( || panic! ("no ord:\n{}", text) );
  assert! ( lines [ord + 1] . ends_with ("(otherChanges contentOrder) (diff (leaves 1)))"),
            "{}", text );
  Ok (( )) }

#[test]
fn the_merges_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("buffers", &SKGREPOS, DiffViewBufferKind::Merges) }

#[test]
fn the_skgrepo_changes_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("buffers", &SKGREPOS, DiffViewBufferKind::SkgrepoChanges) }

#[test]
fn a_skgrepo_changes_buffer_with_a_relRepo_move_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("kinds", &SKGREPOS, DiffViewBufferKind::SkgrepoChanges) }

#[test]
fn the_order_only_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("buffers", &SKGREPOS, DiffViewBufferKind::OrderOnlyChanges) }

#[test]
fn the_all_changes_buffer_of_this_fixture_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("buffers", &SKGREPOS, DiffViewBufferKind::AllChanges) }
