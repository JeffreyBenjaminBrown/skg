// The merges, skgrepo-changes and order-only buffers of the
// diff view. In the 'buffers' fixture (owned skgrepos public and
// private):
// - top contains acq, which acquired acqd by a nodeMerge; pres, new
//   in acq's content, is acqd's text preserver. txt is retitled.
// - mvp moved from public to private. It contains mvc (moved the same
//   way, as did mvc's content mvg), mvr (moved the other way) and mvs
//   (unmoved), whose content mvt moved like mvp.
// - ord's content changed only in order: o1, o2 became o2, o1.

use crate::git_fixture::{Fixture, graph_straddle};
use crate::pcsvf::common::{
  ids, open_buffer, saves_back_unchanged, with_fixture};

use skg::graph_straddle::types::GraphStraddle;
use skg::diff_view::pcsvf::{PcsvfSets, pcsvf_sets_for_home_moves, pcsvf_sets_for_merges};
use skg::types::views_state::DiffViewBufferKind;

use std::error::Error;

const SKGREPOS : [&str; 2] = [ "public", "private" ];

/// The text of a buffer of the 'buffers' fixture.
fn buffer (
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<String, Box<dyn Error>> {
  let mut text : String = String::new ();
  with_fixture ("buffers", &SKGREPOS, |f : &mut Fixture| {
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
  let node : String = format! ( "(node (id {}) ", id );
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
  let node : String = format! ( "(node (id {}) ", id );
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
    let sets : PcsvfSets = pcsvf_sets_for_merges (&straddle);
    assert_eq! ( sets . c, ids (&[ "acq", "acqd" ]) );
    assert_eq! ( sets . d, ids (&[ "acqd", "top" ]) );
    Ok (( )) }) }

#[test]
fn the_home_move_sets_hold_the_moved_nodes (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &SKGREPOS, |f| {
    let straddle : GraphStraddle = graph_straddle (f) ?;
    let sets : PcsvfSets = pcsvf_sets_for_home_moves (&straddle);
    assert_eq! ( sets . c, ids (&[ "mvc", "mvg", "mvp", "mvr", "mvt" ]) );
    assert_eq! ( sets . d, ids (&[ "mvp" ]) );
    assert! ( sets . u . contains (&"mvs" . into ()),
              "mvs is on the walk from mvt to mvp" );
    Ok (( )) }) }

#[test]
fn the_merges_buffer_draws_only_the_merge (
) -> Result<(), Box<dyn Error>> {
  let text : String = buffer (DiffViewBufferKind::Merges) ?;
  let roots : Vec<&str> = roots (&text);
  assert_eq! ( roots . len (), 2, "{}", text );
  assert! ( roots [0] . contains ("(historical (id acqd)"), "{}", text );
  assert! ( roots [1] . contains ("(node (id top)"), "{}", text );
  assert! ( child_headlines_of_editable (&text, "acq") . iter ()
            . any ( |line| line . contains ("(node (id pres)") ),
            "the text preserver is drawn in acq's content:\n{}", text );
  assert! ( headline_of (&text, "pres") . contains ("(diff node+ rel+)"),
            "the text preserver is new in acq's content:\n{}", text );
  assert! ( text . contains ("acquiredByFolder")
            && text . contains ("acquiredFolder"), "{}", text );
  for unrelated in [ "txt", "mvp", "ord" ] {
    assert! ( ! text . contains (&format! ( "(id {})", unrelated )),
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
    assert! ( ! text . contains (&format! ( "(id {})", unrelated )),
              "{} did not move:\n{}", unrelated, text ); }
  Ok (( )) }

#[test]
fn the_home_move_heralds_are_only_in_the_skgrepo_changes_buffer (
) -> Result<(), Box<dyn Error>> {
  let text : String = buffer (DiffViewBufferKind::AllChanges) ?;
  assert! ( ! headline_of (&text, "mvc") . contains ("homeSkgrepoMove"), "{}", text );
  Ok (( )) }

#[test]
fn the_order_only_buffer_is_a_flat_list_with_markers (
) -> Result<(), Box<dyn Error>> {
  let text : String = buffer (DiffViewBufferKind::OrderOnlyChanges) ?;
  let lines : Vec<&str> = text . lines () . collect ();
  assert_eq! ( roots (&text) . len (), 1, "{}", text );
  assert! ( lines [0] . starts_with ("* (skg bodyFolded (node (id ord)"),
            "{}", text );
  assert_eq! ( lines [1], "** (skg (changesetMarker orderOnly) (ready))" );
  assert! ( lines [2] . contains ("(node (id o2) (repo public) writeProtected")
            && lines [3] . contains ("(node (id o1) (repo public) writeProtected"),
            "the content is drawn in its new order, as leaves:\n{}", text );
  assert_eq! ( lines . len (), 4, "{}", text );
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
fn the_order_only_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("buffers", &SKGREPOS, DiffViewBufferKind::OrderOnlyChanges) }

#[test]
fn the_all_changes_buffer_of_this_fixture_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("buffers", &SKGREPOS, DiffViewBufferKind::AllChanges) }
