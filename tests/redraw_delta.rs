// cargo nextest run --test grouped_repos -E 'test(redraw_delta::)'
//
// The save-result's redraw delta ('docs/api-and-formats.org', "The
// redraw delta"): the headlines whose text the save changed, against
// the saved buffer's text.

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, open_content_view, save_cleanly,
  with_git_fixture, write_unchanged_skgrepos};

use skg::metadata_canon::viewnode_id_atom_of;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::types::viewnode::ViewnodeId;
use skg::types::views_state::ViewforestId;

use std::error::Error;
use tempfile::TempDir;

/// r contains a and b, and both contain c, so a content view of r
/// shows c twice: once editable, once write-protected.
fn with_c_shown_twice (
  body : impl FnOnce (&mut Fixture, String) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let case : TempDir = TempDir::new () ?;
  write_unchanged_skgrepos ( case . path (), &[ ( "main", &[
    ("r", "pid: r\ntitle: r\ncontains:\n- a\n- b\n"),
    ("a", "pid: a\ntitle: a\ncontains:\n- c\n"),
    ("b", "pid: b\ntitle: b\ncontains:\n- c\n"),
    ("c", "pid: c\ntitle: c\n") ] ) ] ) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["main"]), &[],
    |f| {
      let restriction : SkgrepoRestriction = SkgrepoRestriction::unrestricted (&f . config);
      let view : String = open_content_view (f, "r", "delta", &restriction) ?;
      body (f, view) } ) }

/// The viewnode ID of the one line of 'text' that 'is_the_line' picks.
fn viewnode_id_of_line (
  text        : &str,
  is_the_line : impl Fn (&str) -> bool,
) -> ViewnodeId {
  let lines : Vec<&str> = text . lines () . filter ( |line| is_the_line (line) ) . collect ();
  assert_eq! ( lines . len (), 1, "one such line in:\n{}", text );
  let atom : &str = viewnode_id_atom_of (lines [0]) . expect ("a viewnode ID");
  ViewnodeId ( atom . trim_start_matches ("(vnid ") . trim_end_matches (')') . to_string () ) }

/// Saved as drawn, with the fold and focus atoms a client adds, the
/// view comes back with no headline to redraw.
#[test]
fn a_view_saved_as_drawn_has_an_empty_delta (
) -> Result<(), Box<dyn Error>> {
  with_c_shown_twice ( |f, view| {
    let with_atoms : String = view
      . replacen ("(vnid 0a)", "(vnid 0a) focused", 1)
      . replacen ("(vnid 0c)", "(vnid 0c) folded", 1)
      . replacen ("(vnid 0b)", "(vnid 0b) bodyFolded", 1);
    let response : SaveResponse = save_cleanly (
      f, &ViewforestId::ContentView ("delta" . into ()), &with_atoms ) ?;
    assert_eq! ( response . changed_headlines, Some (Vec::new ()),
                 "saved:\n{}\nanswered:\n{}", with_atoms, response . saved_view );
    Ok (( )) } ) }

/// Retitling the editable occurrence of c leaves its own line as the
/// server draws it; the write-protected occurrence takes the new title,
/// and is the one headline to redraw.
#[test]
fn a_retitle_lists_the_other_occurrence_alone (
) -> Result<(), Box<dyn Error>> {
  with_c_shown_twice ( |f, view| {
    let is_c : fn (&str) -> bool = |line : &str| line . contains ("(gnid c)");
    let editable_c : ViewnodeId = viewnode_id_of_line (
      &view, |line| is_c (line) && ! line . contains ("writeProtected") );
    let protected_c : ViewnodeId = viewnode_id_of_line (
      &view, |line| is_c (line) && line . contains ("writeProtected") );
    let retitled : String = view . lines ()
      . map ( |line| if is_c (line) && ! line . contains ("writeProtected")
              { format! ("{}, retitled", line) } else { line . to_string () } )
      . collect::<Vec<String>> () . join ("\n") + "\n";
    let response : SaveResponse = save_cleanly (
      f, &ViewforestId::ContentView ("delta" . into ()), &retitled ) ?;
    assert_eq! ( viewnode_id_of_line (
                   &response . saved_view,
                   |line| is_c (line) && line . contains ("writeProtected")
                          && line . ends_with (" c, retitled") ),
                 protected_c, "answered:\n{}", response . saved_view );
    assert_eq! ( response . changed_headlines, Some (vec! [protected_c]),
                 "the editable occurrence {} is drawn as saved:\n{}",
                 editable_c, response . saved_view );
    Ok (( )) } ) }
