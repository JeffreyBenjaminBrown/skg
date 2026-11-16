// cargo nextest run --test grouped_saves -E 'test(members_as_such_and_stale_buffers::)'
//
// Members as such of a forks folder ('docs/glossary.org', "member as
// such") in views saved with their baselines, as a client saves
// them, so that the stale-buffer check
// ('server/serve/handlers/save_staleness.rs') judges each save:
// - An indirect member ('docs/glossary.org', "direct member, indirect
//   member") stands in the note's flat forks folder, editable,
//   claiming nothing: a fresh view saves unchanged with no warning,
//   edits of a direct member save, and a cut from the indirect member
//   makes the note fork it and hide what was cut.
// - Every view the server draws after a save gives each member of a
//   forks folder the claim the graph gives it ('docs/forks.org',
//   "Claims in a redrawn view"): one the note now forks claims, in a
//   stale view another client saves and in a collateral view, so
//   that view can then edit the note.
// - A child placed under an owned member as such joins that member's
//   content ('docs/forks.org', "Adding a child under a member as
//   such"), but a stale buffer showing a child another view removed
//   does not put it back.
// - A child a stale buffer shows in a member's folder of what the user
//   keeps elsewhere, or of what the note hides in it, was not cut
//   there, though another view since stopped keeping it, or unhid it:
//   the save hides nothing, and the note is not stale. These use the
//   fixture of 'tests/hides_by_integration.rs'.
// - A member standing write-protected and bare, extended by an
//   editable-view request on its note, is drawn as a fresh view draws
//   it, its containers folder included.
// The fixture ('tests/members_as_such_and_stale_buffers/fixtures/'),
// all owned but for byx, bw1 and bw2:
// - V forks W; W forks U and contains u1 and w1; U contains u1, u2.
// - P contains pa, pb, forks Pe and hides pw, ph2; Pe contains pe1;
//   Pf forks P and contains pa, pw, pf1.
// - Box contains bx; byx (foreign) forks bx and contains bw1, bw2.
// - A forks E and contains a1; E contains e1, e2; Shelf contains E.

use crate::save_handler_helpers::{
  Fixture, collateral_view, fixture_from, left_as_the_graph_has_it,
  new_connection, open_view, save, save_with_other_views, saved, saved_view,
  warnings_of};
use crate::view_and_save_helpers::{line_of, under, without_subtree_of};
use skg::metadata_canon::without_viewnode_ids;
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

/// The first line showing 'gnid' as a member of a forks folder of
/// the view's root (at depth 3), and the lines under it: a member may
/// stand deeper first, in a containers folder under what another
/// member keeps.
fn forks_folder_member<'a> (
  buf  : &'a str,
  gnid : &str,
) -> (&'a str, Vec<&'a str>) {
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| line . starts_with ("*** ")
                        && line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no member {} in:\n{}", gnid, buf ));
  ( lines [at],
    lines [at + 1 ..] . iter ()
      . take_while ( |line| line . starts_with ("****") )
      . copied () . collect () ) }

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
// An indirect member beside an owned member as such
//

#[test]
fn a_view_with_an_indirect_member_saves_unchanged
  () -> Result<(), Box<dyn Error>> {
  // V's view shows W, V's forkee as such, editable; then U, which W
  // forks, an indirect member, editable, claiming nothing, showing u2
  // (V contains and hides nothing), and u1, which W, owned, contains
  // too, in its folder of what the user keeps elsewhere (as W shows
  // u1, which U keeps); W shows no forks folder.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let v : String = open_view (&mut f, &mut connection, "V", "V");
  let (u_line, under_u) : (&str, Vec<&str>) = forks_folder_member (&v, "U");
  assert! ( ! u_line . contains ("writeProtected")
            && u_line . contains ("noClaim")
            && under_u . iter () . any ( |line| line . starts_with ("**** ")
                                                && line . contains ("(gnid u2)") )
            && under_u . iter () . any ( |line| line . contains ("integratedElsewhereFolder") ),
            "U stands editable in V's forks folder, claiming nothing, showing u2:\n{}", v );
  assert_eq! ( v . matches ("forksFolder") . count (), 1, "{}", v );
  saves_unchanged_writing_nothing (&mut f, &mut connection, "V", &v);
  Ok (()) }

#[test]
fn an_owned_member_as_such_is_retitled
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
fn a_cut_from_an_indirect_member_makes_the_note_fork_it_and_hide_the_cut
  () -> Result<(), Box<dyn Error>> {
  // u2 cut from U, an indirect member of V's forks folder: V forks U
  // now, after W, and hides u2; W and U are untouched. The redrawn
  // view shows U claiming, u2 in its folder of what V hides in it.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let v : String = open_view (&mut f, &mut connection, "V", "V");
  let w_before : String = file_text (&f, "main", "W");
  let u_before : String = file_text (&f, "main", "U");
  let result : String = save (
    &mut f, &mut connection, "V", &without_subtree_of (&v, "u2"), Some (&v) );
  assert! ( saved (&result), "{}", result );
  let v_file : String = file_text (&f, "main", "V");
  assert! ( list_in (&v_file, "forks") == vec! ["W", "U"]
            && list_in (&v_file, "hides") == vec! ["u2"],
            "V forks U and hides u2:\n{}", v_file );
  assert_eq! ( file_text (&f, "main", "W"), w_before, "W is untouched" );
  assert_eq! ( file_text (&f, "main", "U"), u_before, "U is untouched" );
  let redrawn : String = saved_view (&result);
  let (u_line, under_u) : (&str, Vec<&str>) = forks_folder_member (&redrawn, "U");
  assert! ( ! u_line . contains ("noClaim")
            && under_u . iter () . any ( |line|
                 line . contains ("hiddenInForkPartnerFolder") ),
            "U claims now, u2 in its folder of what V hides in it:\n{}", redrawn );
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

//
// A stale buffer and what a member as such showed beside its content
//

/// The fixture of 'tests/hides_by_integration.rs': M (public) forks
/// x (theirs: a, b, c, d) and contains d and z; y (private) keeps a.
fn integration_fixture (
) -> Result<Fixture, Box<dyn Error>> {
  fixture_from ( "tests/hides_by_integration/fixtures",
                 "members-as-such-stale-integration",
                 &["public", "private", "theirs"] ) }

#[test]
fn a_stale_buffer_does_not_hide_what_its_integrated_folder_showed
  () -> Result<(), Box<dyn Error>> {
  // The first client's view of M shows a, which y keeps, in x's folder
  // of what the user keeps elsewhere, not as x's content. The second
  // removes a from y in y's own view, so x would show a now. The
  // first view, stale, saves an edit of M's title, then of z's: a
  // stood beside x's content, not in it, so it was not cut there, and
  // M hides nothing. M is not stale either: its baseline writes
  // nothing of it, so its edit saves, and no warning names it.
  for edited in ["M", "z"] {
    let mut f : Fixture = integration_fixture () ?;
    let mut first : ViewsState = new_connection ();
    let mut second : ViewsState = new_connection ();
    let m_in_first : String = open_view (&mut f, &mut first, "M", "M-first");
    assert! ( line_of (&m_in_first, "a") . contains ("writeProtected")
              && m_in_first . contains ("integratedElsewhereFolder)"),
              "a stands in x's folder of what the user keeps elsewhere:\n{}", m_in_first );
    let y_in_second : String = open_view (&mut f, &mut second, "y", "y-second");
    let removal : String = save (
      &mut f, &mut second, "y-second",
      &without_subtree_of (&y_in_second, "a"), Some (&y_in_second) );
    assert! ( saved (&removal), "{}", removal );
    assert_eq! ( list_in (&file_text (&f, "private", "y"), "contains"), vec! ["f2", "g2"] );
    let stale_edit : String = save (
      &mut f, &mut first, "M-first",
      &retitled (&m_in_first, edited, &format! ("{} retitled", edited)),
      Some (&m_in_first) );
    assert! ( saved (&stale_edit), "{}", stale_edit );
    assert! ( ! warnings_of (&stale_edit) . contains (&left_as_the_graph_has_it ("M", "M")),
              "M is not stale:\n{}", stale_edit );
    assert! ( file_text (&f, "public", edited)
              . contains (&format! ("title: {} retitled", edited)) );
    assert! ( list_in (&file_text (&f, "public", "M"), "hides") . is_empty (),
              "M hides nothing:\n{}", file_text (&f, "public", "M") ); }
  Ok (()) }

#[test]
fn a_stale_buffer_does_not_hide_again_what_another_view_unhid
  () -> Result<(), Box<dyn Error>> {
  // The first client cuts c from x in its view of M: M hides c, and
  // the redrawn view shows c in x's folder of what M hides in it. The
  // second, in its own view of M, moves c out of that folder to stand
  // under x, unhiding it. The first view, stale, saves an edit of M's
  // title, then of z's: c stood in the hidden folder, not among x's
  // content, so it was not cut there, and M hides nothing. M is not
  // stale either, so its edit saves, and no warning names it.
  for edited in ["M", "z"] {
    let mut f : Fixture = integration_fixture () ?;
    let mut first : ViewsState = new_connection ();
    let mut second : ViewsState = new_connection ();
    let m_in_first : String = open_view (&mut f, &mut first, "M", "M-first");
    let cut : String = save (
      &mut f, &mut first, "M-first",
      &without_subtree_of (&m_in_first, "c"), Some (&m_in_first) );
    assert! ( saved (&cut), "{}", cut );
    assert_eq! ( list_in (&file_text (&f, "public", "M"), "hides"), vec! ["c"] );
    let m_in_first : String = saved_view (&cut);
    assert! ( line_of (&m_in_first, "c") . contains ("writeProtected")
              && m_in_first . contains ("hiddenInForkPartnerFolder)"),
              "c stands in x's folder of what M hides in it:\n{}", m_in_first );
    let m_in_second : String = open_view (&mut f, &mut second, "M", "M-second");
    let moved_out : String = {
      let x_line : &str = line_of (&m_in_second, "x");
      let without_folder : String = m_in_second . lines ()
        . filter ( |line| ! line . contains ("hiddenInForkPartnerFolder)")
                          && ! line . contains ("(gnid c)") )
        . map ( |line| format! ("{}\n", line) )
        . collect ();
      without_folder . replacen (
        &format! ("{}\n", x_line),
        &format! ("{}\n**** (skg node (gnid c) (skgRepos (nodeRepo theirs))) c\n", x_line),
        1 ) };
    let unhiding : String = save (
      &mut f, &mut second, "M-second", &moved_out, Some (&m_in_second) );
    assert! ( saved (&unhiding), "{}", unhiding );
    assert! ( list_in (&file_text (&f, "public", "M"), "hides") . is_empty () );
    let stale_edit : String = save (
      &mut f, &mut first, "M-first",
      &retitled (&m_in_first, edited, &format! ("{} retitled", edited)),
      Some (&m_in_first) );
    assert! ( saved (&stale_edit), "{}", stale_edit );
    assert! ( ! warnings_of (&stale_edit) . contains (&left_as_the_graph_has_it ("M", "M")),
              "M is not stale:\n{}", stale_edit );
    assert! ( file_text (&f, "public", edited)
              . contains (&format! ("title: {} retitled", edited)) );
    assert! ( list_in (&file_text (&f, "public", "M"), "hides") . is_empty (),
              "M hides nothing:\n{}", file_text (&f, "public", "M") ); }
  Ok (()) }

//
// A member extended by an editable-view request
//

#[test]
fn a_member_extended_on_request_gets_its_containers_folder
  () -> Result<(), Box<dyn Error>> {
  // A's view, with E standing write-protected and bare in A's forks
  // folder (as an older server drew it), rerenders so. With an
  // editable-view request on A (C-c s e), E is extended: editable,
  // with its content, its forks folder, and its containers folder
  // holding Shelf, folded, as in a fresh view of A.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let fresh : String = open_view (&mut f, &mut connection, "A", "A");
  assert! ( under (&fresh, "E") . iter () . any ( |line| line . contains ("containerFolder") ),
            "a fresh view draws E's containers folder:\n{}", fresh );
  let e_line : &str = line_of (&fresh, "E");
  let bare : String = without_subtree_of (&fresh, "E") . replacen (
    &format! ( "{}\n", line_of (&fresh, "a1") ),
    &format! ( "{}\n{}\n",
               e_line . replace ( "(gnid E) (skgRepos (nodeRepo main))",
                                  "(gnid E) (skgRepos (nodeRepo main)) writeProtected" ),
               line_of (&fresh, "a1") ),
    1 );
  let rerendered : String = saved_view ( &save (
    &mut f, &mut connection, "A", &bare, Some (&fresh) ));
  assert! ( line_of (&rerendered, "E") . contains ("writeProtected")
            && under (&rerendered, "E") . is_empty (),
            "a plain rerender leaves E bare:\n{}", rerendered );
  let requested : String = rerendered . replacen (
    "node (gnid A)", "node (viewRequests editableView) (gnid A)", 1 );
  assert_ne! ( requested, rerendered );
  let extended : String = saved_view ( &save (
    &mut f, &mut connection, "A", &requested, Some (&rerendered) ));
  assert_eq! ( without_viewnode_ids (&extended), without_viewnode_ids (&fresh),
               "the extended view is the fresh view" );
  Ok (()) }
