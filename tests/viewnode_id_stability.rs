// cargo nextest run --test grouped_repos -E 'test(viewnode_id_stability::)'
//
// A viewnode keeps its viewnode ID ('docs/api-and-formats.org', "The
// metadata headers") across a save, a view request fulfilled by a
// save, and a skgrepo-set switch and back: no headline the buffer
// still shows loses its ID or takes another's.
//
// The fixture, written by the test:
//   public  : r contains a, b, c, s; a contains a1; c subscribes to d;
//             x contains r; s1.
//   private : s ("s secret") contains s1.
// The view of r shows x in a containers folder once it is requested,
// and d in c's subscribee folder. Under the restriction "public", s is
// a tombstone holding s1.

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, open_content_view, save_cleanly, switch_skgrepo_set,
  with_git_fixture, write_unchanged_skgrepos};

use skg::metadata_canon::{viewnode_id_atom_of, without_viewnode_ids};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::types::views_state::ViewforestId;

use std::collections::BTreeMap;
use std::error::Error;
use std::path::Path;

const VIEW : &str = "view of r";

#[test]
fn viewnode_ids_survive_a_save_a_request_and_a_switch (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
      let opened : String = open_view_of_r (f) ?;
      let typed : String = { // a typed child, saved
        let saved : String = save (f, &opened . replacen (
          ")) r\n", ")) r\n** a typed child\n", 1 )) ?;
        assert_every_viewnode_id_kept ("a save", &opened, &saved, &[]);
        saved };
      let requested : String = { // a folder request on r
        let with_request : String = typed . replacen (
          "node (gnid r) ", "node (gnid r) (viewRequests (folder containers)) ", 1 );
        assert_ne! ( with_request, typed );
        let saved : String = save (f, &with_request) ?;
        assert_every_viewnode_id_kept ("a request", &typed, &saved, &[]);
        assert! ( headline_labels (&saved) . values ()
                  . any ( |label| label == "*** (gnid x) x" ),
                  "the requested folder holds x:\n{}", saved );
        saved };
      let public : String = { // switch to "public"
        let switched : String = switch (f, &mut restriction, "public") ?;
        assert_every_viewnode_id_kept (
          "a switch to public", &requested, &switched, &["(gnid s)"] );
        let s_viewnode_id : String = headline_labels (&requested) . into_iter ()
          . find ( |(_, label)| label . contains ("(gnid s)") )
          . map ( |(viewnode_id, _)| viewnode_id ) . unwrap ();
        assert_eq! ( headline_labels (&switched) . get (&s_viewnode_id)
                     . map (String::as_str),
                     Some ("** tombstone"),
                     "s's tombstone keeps its viewnode ID:\n{}", switched );
        switched };
      let all : String = switch (f, &mut restriction, "all") ?;
      assert_every_viewnode_id_kept ("a switch back to all", &public, &all, &[]);
      Ok (( )) } ) }

//
// The fixture and the operations
//

fn write_fixture (
  dir : &Path,
) -> Result<(), Box<dyn Error>> {
  let public : &[(&str, &str)] = &[
    ("r",  "pid: r\ntitle: r\ncontains:\n- a\n- b\n- c\n- s\n"),
    ("a",  "pid: a\ntitle: a\ncontains:\n- a1\n"),
    ("a1", "pid: a1\ntitle: a1\n"),
    ("b",  "pid: b\ntitle: b\n"),
    ("c",  "pid: c\ntitle: c\nsubscribesTo:\n- d\n"),
    ("d",  "pid: d\ntitle: d\n"),
    ("x",  "pid: x\ntitle: x\ncontains:\n- r\n"),
    ("s1", "pid: s1\ntitle: s1\n"), ];
  let private : &[(&str, &str)] = &[
    ("s", "pid: s\ntitle: s secret\ncontains:\n- s1\n"), ];
  write_unchanged_skgrepos (dir, &[("public", public), ("private", private)]) }

fn open_view_of_r (
  f : &mut Fixture,
) -> Result<String, Box<dyn Error>> {
  open_content_view (f, "r", VIEW, None) }

fn save (
  f    : &mut Fixture,
  text : &str,
) -> Result<String, Box<dyn Error>> {
  Ok ( save_cleanly ( f, &ViewforestId::ContentView (VIEW . into ()), text )
       ? . saved_view ) }

/// Switch to the skgrepo-set 'name'; return the view as rerendered.
fn switch (
  f           : &mut Fixture,
  restriction : &mut SkgrepoRestriction,
  name        : &str,
) -> Result<String, Box<dyn Error>> {
  Ok ( switch_skgrepo_set (f, restriction, name, VIEW) ? . 0 ) }

//
// Comparing buffers by viewnode ID
//

/// Each headline's viewnode ID, with a label that does not depend on
/// it or on the headline's stats: its depth, its node's ID or the head
/// word of its '(skg ...)', and its title.
fn headline_labels (
  buffer : &str,
) -> BTreeMap<String, String> {
  let mut labels : BTreeMap<String, String> = BTreeMap::new ();
  for line in buffer . lines () . filter ( |line| line . starts_with ('*') ) {
    let viewnode_id : String = viewnode_id_atom_of (line)
      . unwrap_or_else ( || panic! ( "{:?} has no viewnode ID:\n{}", line, buffer ))
      . trim_start_matches ("(vnid ") . trim_end_matches (')') . to_string ();
    let previous : Option<String> = labels . insert ( viewnode_id . clone (), label (line) );
    assert! ( previous . is_none (),
              "two headlines share viewnode ID {}:\n{}", viewnode_id, buffer ); }
  labels }

fn label (
  line : &str,
) -> String {
  let line : &str = &without_viewnode_ids (line);
  let depth : usize = line . chars () . take_while ( |c| *c == '*' ) . count ();
  let sexp_start : usize = line . find ("(skg") . unwrap ();
  let sexp_end : usize = { // past the parenthesis closing '(skg'
    let mut level : i32 = 0;
    let mut end : usize = sexp_start;
    for (i, c) in line [sexp_start ..] . char_indices () {
      if c == '(' { level += 1; }
      if c == ')' { level -= 1;
                    if level == 0 { end = sexp_start + i + 1; break; }}}
    end };
  let sexp : &str = &line [sexp_start .. sexp_end];
  let head : String = match sexp . find ("(gnid ") {
    Some (i) => format! ( "{})", sexp [i ..] . split (')') . next () . unwrap () ),
    None => sexp ["(skg" . len () ..]
      . split ( |c : char| c == ' ' || c == ')' || c == '(' )
      . find ( |word| ! word . is_empty () && *word != "folded" )
      . unwrap_or ("?") . to_string (), };
  let title : &str = line [sexp_end ..] . trim ();
  if title . is_empty () { format! ("{} {}", "*" . repeat (depth), head) }
  else { format! ("{} {} {}", "*" . repeat (depth), head, title) }}

/// Every viewnode ID of 'before' labels the same headline in 'after',
/// but those of a headline whose label contains one of 'changed'.
fn assert_every_viewnode_id_kept (
  case    : &str,
  before  : &str,
  after   : &str,
  changed : &[&str],
) {
  let after_labels : BTreeMap<String, String> = headline_labels (after);
  for (viewnode_id, label) in headline_labels (before) {
    if changed . iter () . any ( |needle| label . contains (needle) ) { continue; }
    assert_eq! ( after_labels . get (&viewnode_id), Some (&label),
                 "after {}, viewnode ID {} still labels {:?}:\n\nbefore:\n{}\nafter:\n{}",
                 case, viewnode_id, label, before, after ); }}
