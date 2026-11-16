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
  Fixture, collateral_view, fixture_from, left_as_the_graph_has_it,
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

//
// Claims in a redrawn view
//

#[test]
fn another_clients_stale_view_redraws_a_fork_made_elsewhere_as_a_claim
  () -> Result<(), Box<dyn Error>> {
  // Two clients each hold a view of P, where Pf, a forker as such,
  // claims nothing. The second cuts pf1 from Pf, so P forks Pf. The
  // first saves its view unchanged: P is left as the graph has it,
  // and the view it draws shows Pf claiming. Saved again, that view
  // finds nothing stale and writes nothing, and an edit of P there
  // saves, P still forking Pf.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let mut second : ViewsState = new_connection ();
  let p_in_first : String = open_view (&mut f, &mut first, "P", "P-first");
  let p_in_second : String = open_view (&mut f, &mut second, "P", "P-second");
  assert! ( line_of (&p_in_first, "Pf") . contains ("noClaim"), "{}", p_in_first );
  let cut : String = save (
    &mut f, &mut second, "P-second",
    &without_subtree_of (&p_in_second, "pf1"), Some (&p_in_second) );
  assert! ( saved (&cut), "{}", cut );
  assert_eq! ( list_in (&file_text (&f, "main", "P"), "forks"), vec! ["Pe", "Pf"] );
  let result : String = save (
    &mut f, &mut first, "P-first", &p_in_first, Some (&p_in_first) );
  assert! ( saved (&result)
            && warnings_of (&result) . contains (&left_as_the_graph_has_it ("P", "P")),
            "{}", result );
  let redrawn : String = saved_view (&result);
  assert! ( ! line_of (&redrawn, "Pf") . contains ("noClaim"),
            "Pf, which P now forks, claims:\n{}", redrawn );
  let redrawn : String = saves_unchanged_writing_nothing (
    &mut f, &mut first, "P-first", &redrawn );
  let edited : String = save (
    &mut f, &mut first, "P-first", &retitled (&redrawn, "P", "P retitled"),
    Some (&redrawn) );
  assert! ( saved (&edited), "{}", edited );
  let p : String = file_text (&f, "main", "P");
  assert! ( p . contains ("title: P retitled")
            && list_in (&p, "forks") == vec! ["Pe", "Pf"],
            "{}", p );
  Ok (()) }

#[test]
fn a_collateral_view_redraws_a_fork_the_save_made_as_a_claim
  () -> Result<(), Box<dyn Error>> {
  // One client holds a view of Box and one of bx. In bx's view, bw1
  // is cut from byx, a foreign forker as such, so bx forks byx. The
  // save redraws Box's view, a collateral view, showing byx claiming
  // under bx. Saved unchanged, that view finds nothing stale and
  // writes nothing; and an edit of bx there saves.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let box_view : String = open_view (&mut f, &mut connection, "Box", "Box");
  let bx_view : String = open_view (&mut f, &mut connection, "bx", "bx");
  assert! ( line_of (&box_view, "byx") . contains ("noClaim"), "{}", box_view );
  let messages : Vec<String> = save_with_other_views (
    &mut f, &mut connection, "bx", &without_subtree_of (&bx_view, "bw1"),
    Some (&bx_view), &["Box"] );
  let result : &String = messages . last () . unwrap ();
  assert! ( saved (result), "{}", result );
  assert_eq! ( list_in (&file_text (&f, "main", "bx"), "forks"), vec! ["byx"] );
  let collateral : String = collateral_view (&messages, "Box")
    . unwrap_or_else ( || panic! ( "Box's view is redrawn:\n{:?}", messages ));
  assert! ( ! line_of (&collateral, "byx") . contains ("noClaim"),
            "byx, which bx now forks, claims:\n{}", collateral );
  let collateral : String = saves_unchanged_writing_nothing (
    &mut f, &mut connection, "Box", &collateral );
  let edited : String = save (
    &mut f, &mut connection, "Box", &retitled (&collateral, "bx", "bx retitled"),
    Some (&collateral) );
  assert! ( saved (&edited), "{}", edited );
  let bx : String = file_text (&f, "main", "bx");
  assert! ( bx . contains ("title: bx retitled")
            && list_in (&bx, "forks") == vec! ["byx"],
            "{}", bx );
  Ok (()) }

//
// A child placed under a member as such, and a stale buffer
//

#[test]
fn a_stale_buffer_does_not_put_back_a_child_removed_from_a_member_as_such
  () -> Result<(), Box<dyn Error>> {
  // The first client's view of A shows E, A's forkee as such, with e1
  // and e2 under it. The second removes e2 from E in E's own view.
  // The first view still shows e2 under E, and its baseline does too,
  // so E is stale and unedited there: saved unchanged, or with
  // another node edited, E is left as the graph has it, without e2;
  // with E edited, the save is refused.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let mut second : ViewsState = new_connection ();
  let a_in_first : String = open_view (&mut f, &mut first, "A", "A-first");
  assert! ( under (&a_in_first, "E") . iter () . any ( |line| line . contains ("(gnid e2)") ),
            "{}", a_in_first );
  let e_in_second : String = open_view (&mut f, &mut second, "E", "E-second");
  let removal : String = save (
    &mut f, &mut second, "E-second",
    &without_subtree_of (&e_in_second, "e2"), Some (&e_in_second) );
  assert! ( saved (&removal), "{}", removal );
  assert_eq! ( list_in (&file_text (&f, "main", "E"), "contains"), vec! ["e1"] );
  let unchanged : String = save (
    &mut f, &mut first, "A-first", &a_in_first, Some (&a_in_first) );
  assert! ( saved (&unchanged)
            && warnings_of (&unchanged) . contains (&left_as_the_graph_has_it ("E", "E")),
            "{}", unchanged );
  assert_eq! ( list_in (&file_text (&f, "main", "E"), "contains"), vec! ["e1"],
               "e2 is not put back" );
  let other_edit : String = save (
    &mut f, &mut first, "A-first", &retitled (&a_in_first, "a1", "a1 retitled"),
    Some (&a_in_first) );
  assert! ( saved (&other_edit), "{}", other_edit );
  assert! ( file_text (&f, "main", "a1") . contains ("title: a1 retitled") );
  assert_eq! ( list_in (&file_text (&f, "main", "E"), "contains"), vec! ["e1"],
               "e2 is not put back" );
  let e_edited : String = save (
    &mut f, &mut first, "A-first", &retitled (&a_in_first, "E", "E retitled"),
    Some (&a_in_first) );
  assert! ( ! saved (&e_edited)
            && e_edited . contains ("changed after the buffer was rendered"),
            "{}", e_edited );
  assert_eq! ( list_in (&file_text (&f, "main", "E"), "contains"), vec! ["e1"] );
  Ok (()) }
