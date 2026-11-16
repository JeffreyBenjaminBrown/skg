// cargo nextest run --test grouped_saves -E 'test(members_as_such_and_stale_buffers::)'
//
// Members as such of a forks folder ('docs/glossary.org', "member as
// such") in views saved with their baselines, as a client saves
// them, so that the stale-buffer check
// ('server/serve/handlers/save_staleness.rs') judges each save:
// - A forkee as such nested under an owned, editable member as such
//   reads that member's content as on disk: a fresh view saves
//   unchanged with no warning, and edits of the member save.
// - Every view the server draws after a save gives each member of a
//   forks folder the claim the graph gives it ('docs/forks.org',
//   "Claims in a redrawn view"): one the note now forks claims, in a
//   stale view another client saves and in a collateral view, so
//   that view can then edit the note.
// - A child placed under an owned member as such joins that member's
//   content ('docs/forks.org', "Adding a child under a member as
//   such"), but a stale buffer showing a child another view removed
//   does not put it back.
// The fixture ('tests/members_as_such_and_stale_buffers/fixtures/'),
// all owned but for byx, bw1 and bw2:
// - V forks W; W forks U and contains u1 and w1; U contains u1, u2.
// - P contains pa, pb, forks Pe and hides pw, ph2; Pe contains pe1;
//   Pf forks P and contains pa, pw, pf1.
// - Box contains bx; byx (foreign) forks bx and contains bw1, bw2.
// - A forks E and contains a1; E contains e1, e2.

use crate::save_handler_helpers::{
  Fixture, collateral_view, errors_of, fixture_from, left_as_the_graph_has_it,
  new_connection, open_view, save, save_with_other_views, saved, saved_view,
  warnings_of};
use crate::view_and_save_helpers::{line_of, under, without_subtree_of};
use skg::serve::ViewsState;

use std::collections::BTreeMap;
use std::error::Error;
use std::fs;
use std::path::PathBuf;

fn fixture (
) -> Result<Fixture, Box<dyn Error>> {
  fixture_from ( "tests/members_as_such_and_stale_buffers/fixtures",
                 "members-as-such-and-stale-buffers", &["main", "theirs"] ) }

/// Every .skg file of the fixture, by skgrepo and name.
fn skg_files (
  f : &Fixture,
) -> BTreeMap<String, String> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for skgrepo in ["main", "theirs"] {
    for entry in fs::read_dir ( f . dir . path () . join (skgrepo) ) . unwrap () {
      let path : PathBuf = entry . unwrap () . path ();
      files . insert (
        format! ( "{}/{}", skgrepo, path . file_name () . unwrap () . to_string_lossy () ),
        fs::read_to_string (&path) . unwrap () ); }}
  files }

fn file_text (
  f       : &Fixture,
  skgrepo : &str,
  gnid    : &str,
) -> String {
  fs::read_to_string (
    f . dir . path () . join (skgrepo) . join (format! ("{}.skg", gnid)) )
    . unwrap () }

/// The members of the list 'key' ('contains', 'forks', 'hides') in
/// the text of a .skg file, in order.
fn list_in (
  skg_text : &str,
  key      : &str,
) -> Vec<String> {
  skg_text . lines ()
    . skip_while ( |line| *line != format! ("{}:", key) )
    . skip (1)
    . take_while ( |line| line . starts_with ("- ") )
    . map ( |line| line . trim_start_matches ("- ") . to_string () )
    . collect () }

/// 'buf' with the title of the first line showing 'gnid' made 'title'.
fn retitled (
  buf   : &str,
  gnid  : &str,
  title : &str,
) -> String {
  let line : &str = line_of (buf, gnid);
  let metadata_end : usize = line . rfind (") ") . unwrap () + 2;
  buf . replacen ( line, &format! ("{}{}", &line [.. metadata_end], title), 1 ) }

/// Save 'view' unchanged as 'view_id', 'view' its baseline: it saves
/// with no warning of a stale node and writes nothing; the view it
/// draws.
fn saves_unchanged_writing_nothing (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
  view_id     : &str,
  view        : &str,
) -> String {
  let before : BTreeMap<String, String> = skg_files (f);
  let result : String = save (f, views_state, view_id, view, Some (view));
  assert! ( saved (&result), "{}", result );
  assert! ( ! warnings_of (&result) . contains ("Left as the graph has it"),
            "no node is stale:\n{}", result );
  assert_eq! ( skg_files (f), before, "an unchanged save writes nothing" );
  saved_view (&result) }

//
// A forkee as such nested under an owned member as such
//

#[test]
fn a_view_of_a_forkee_nested_under_an_owned_member_saves_unchanged
  () -> Result<(), Box<dyn Error>> {
  // V's view shows W, V's forkee as such, editable; under W, its
  // forkee U, editable, showing u2 (W contains u1). W's title is
  // saved, so W has a Save of its own; its contains is still the one
  // on disk, so nothing under U was cut.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let v : String = open_view (&mut f, &mut connection, "V", "V");
  assert! ( ! line_of (&v, "U") . contains ("writeProtected")
            && under (&v, "U") . iter () . any ( |line| line . contains ("(gnid u2)") ),
            "U stands editable under W, showing u2:\n{}", v );
  saves_unchanged_writing_nothing (&mut f, &mut connection, "V", &v);
  Ok (()) }

#[test]
fn an_owned_member_holding_a_forkee_as_such_is_retitled
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let v : String = open_view (&mut f, &mut connection, "V", "V");
  let result : String = save (
    &mut f, &mut connection, "V", &retitled (&v, "W", "W retitled"), Some (&v) );
  assert! ( saved (&result), "{}", result );
  let w : String = file_text (&f, "main", "W");
  assert! ( w . contains ("title: W retitled") && list_in (&w, "hides") . is_empty (),
            "{}", w );
  Ok (()) }

#[test]
fn a_cut_from_a_forkee_nested_under_an_owned_member_hides_it_there
  () -> Result<(), Box<dyn Error>> {
  // u2 cut from U, which stands under W: W hides u2, and nothing
  // else.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let v : String = open_view (&mut f, &mut connection, "V", "V");
  let result : String = save (
    &mut f, &mut connection, "V", &without_subtree_of (&v, "u2"), Some (&v) );
  assert! ( saved (&result), "{}", result );
  let w : String = file_text (&f, "main", "W");
  assert! ( list_in (&w, "hides") == vec! ["u2"]
            && list_in (&w, "contains") == vec! ["u1", "w1"],
            "W hides u2 alone:\n{}", w );
  Ok (()) }
