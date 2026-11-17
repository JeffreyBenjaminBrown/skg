// cargo nextest run --test grouped_forks -E 'test(forkee_content_named_by_an_extra_id::)'
//
// A forkee whose list names a child by an extra ID (a nodeMerge folded
// m into k, so the list says m and the view shows k): the
// forkee-visibility step reads the child by its pid, as the view shows
// it ('resolve_visibility.rs', module doc).
// The fixture: "main" is owned, "theirs" foreign. N (main) forks F
// (theirs), which contains m and j; k (theirs) has the extra ID m. O
// (main) forks P (main), which contains n and i; l (main) has the extra
// ID n. Q (main) forks F2 and G2 (theirs); F2 contains m2 and G2
// contains k2, which has the extra ID m2. Each sub-test runs for the
// foreign forkee (N, F, k, j) and the owned one (O, P, l, i):
// - an unchanged save hides nothing, and writes nothing;
// - cutting the sibling hides it alone, and cutting the child shown by
//   its pid hides it, as its forkee's list names it;
// - a hide of the child, as the list names it, moved out of the
//   forkee's folder of what the note hides in it, is removed;
// - so is it when the child is moved from that folder into the note's
//   own content (the contains-removal step, 'resolve_visibility.rs').
// And one for the conflict check: in Q's view k2 stands under F2 and
// under G2; cut from G2 alone, the save is refused as conflicting.

use std::collections::BTreeMap;
use std::error::Error;

use crate::view_and_save_helpers::{
  contains_of, content_of, depth, hides_of, ids, is_a_fixpoint, line_of, refusal_of,
  save_cleanly, skg_files, under, view_of, without_subtree_of};
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

const FIXTURES : &str = "tests/forkee_content_named_by_an_extra_id/fixtures";

/// A note, its forkee, the forkee's child shown by its pid (the list
/// names it by 'extra'), and that child's sibling.
struct Case {
  note    : &'static str,
  forkee  : &'static str,
  child   : &'static str,
  extra   : &'static str,
  sibling : &'static str,
}

const FOREIGN_FORKEE : Case =
  Case { note : "N", forkee : "F", child : "k", extra : "m", sibling : "j" };
const OWNED_FORKEE : Case =
  Case { note : "O", forkee : "P", child : "l", extra : "n", sibling : "i" };

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forkee-content-named-by-an-extra-id",
    |s| Box::pin ( async move {
      for case in [FOREIGN_FORKEE, OWNED_FORKEE] {
        s . reset ("an_unchanged_save_hides_nothing", FIXTURES) ?;
        an_unchanged_save_hides_nothing (&case, &s . config, &mut s . tantivy) . await ?;
        s . reset ("a_cut_hides_what_was_cut_alone", FIXTURES) ?;
        a_cut_hides_what_was_cut_alone (&case, &s . config, &mut s . tantivy) . await ?;
        s . reset_with_fixture_prep (
          "a_hidden_child_moved_back_out_is_unhidden", FIXTURES,
          |root| { std::fs::write (
                     root . join ( format! ("main/{}.skg", case . note) ),
                     format! ( "pid: {}\ntitle: {}\nforks:\n- {}\nhides:\n- {}\n",
                               case . note, case . note, case . forkee, case . extra )) ?;
                   Ok (( )) } ) ?;
        a_hidden_child_moved_back_out_is_unhidden (&case, &s . config, &mut s . tantivy) . await ?;
        s . reset_with_fixture_prep (
          "a_hidden_child_moved_into_the_note_is_unhidden", FIXTURES,
          |root| { std::fs::write (
                     root . join ( format! ("main/{}.skg", case . note) ),
                     format! ( "pid: {}\ntitle: {}\nforks:\n- {}\nhides:\n- {}\n",
                               case . note, case . note, case . forkee, case . extra )) ?;
                   Ok (( )) } ) ?;
        a_hidden_child_moved_into_the_note_is_unhidden (&case, &s . config, &mut s . tantivy) . await ?; }
      s . reset ("a_cut_from_one_forkee_of_a_child_another_shows_by_an_extra_id_conflicts", FIXTURES) ?;
      a_cut_from_one_forkee_of_a_child_another_shows_by_an_extra_id_conflicts (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// A fresh view of the case's note, checked: the forkee shows the
/// child by its pid, and its sibling.
fn view_of_note (
  case    : &Case,
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let view : String = view_of (config, tantivy, case . note) ?;
  let shown : Vec<&str> = content_of (&view, case . forkee);
  assert! ( shown . len () == 2
            && shown [0] . contains (&format! ("(gnid {})", case . child))
            && shown [1] . contains (&format! ("(gnid {})", case . sibling)),
            "{} shows {} and {}:\n{}", case . forkee, case . child, case . sibling, view );
  Ok (view) }

/// 'buf' less the folder 'atom' directly under the first line showing
/// 'member', and what stands under that folder.
fn without_folder_under (
  buf    : &str,
  member : &str,
  atom   : &str,
) -> String {
  let member_depth : usize = depth (line_of (buf, member));
  let below : Vec<&str> = under (buf, member);
  let at : usize = below . iter ()
    . position ( |line| depth (line) == member_depth + 1
                        && line . contains (&format! ("(skg {})", atom)) )
    . unwrap_or_else ( || panic! ( "no {} under {} in:\n{}", atom, member, buf ));
  let mut removed : String = format! ("{}\n", below [at]);
  for line in below [at + 1 ..] . iter ()
    . take_while ( |line| depth (line) > member_depth + 1 )
  { removed . push_str (&format! ("{}\n", line)); }
  buf . replacen (&removed, "", 1) }

//
// The sub-tests
//

/// Saving the note's view unchanged hides nothing: the forkee's list
/// names the child by its extra ID, the view shows it by its pid, and
/// both name one child.
async fn an_unchanged_save_hides_nothing (
  case    : &Case,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_note (case, config, tantivy) ?;
  is_a_fixpoint (&view, case . note, config, tantivy) . await ?;
  assert_eq! ( hides_of (config, case . note) ?, Vec::<ID>::new () );
  Ok (( )) }

/// Cutting the sibling alone hides the sibling alone. Cutting the
/// child too hides it, as the forkee's list names it (by its extra
/// ID), and the redrawn view shows it in the forkee's folder of what
/// the note hides in it.
async fn a_cut_hides_what_was_cut_alone (
  case    : &Case,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_note (case, config, tantivy) ?;
  let saved : String = save_cleanly (
    &without_subtree_of (&view, case . sibling), case . note, config, tantivy ) . await ?;
  assert_eq! ( hides_of (config, case . note) ?, ids (&[case . sibling]),
               "the sibling alone is hidden" );
  let shown : Vec<&str> = content_of (&saved, case . forkee);
  assert! ( shown . len () == 1 && shown [0] . contains (&format! ("(gnid {})", case . child)),
            "{} still shows {}:\n{}", case . forkee, case . child, saved );
  save_cleanly (
    &without_subtree_of (&saved, case . child), case . note, config, tantivy ) . await ?;
  assert_eq! ( hides_of (config, case . note) ?, ids (&[case . sibling, case . extra]),
               "the child is hidden as the forkee's list names it" );
  Ok (( )) }

/// The note hides the child, as the forkee's list names it. The view
/// shows the child in the forkee's folder of what the note hides in
/// it; moved out of that folder to stand under the forkee, it is
/// unhidden.
async fn a_hidden_child_moved_back_out_is_unhidden (
  case    : &Case,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, case . note) ?;
  assert! ( under (&view, case . forkee) . iter () . any ( |line|
              line . contains ("hiddenInForkPartnerFolder") ),
            "{} has a folder of what {} hides in it:\n{}", case . forkee, case . note, view );
  let forkee_line : &str = line_of (&view, case . forkee);
  let moved_out : String = without_folder_under (
    &view, case . forkee, "hiddenInForkPartnerFolder" )
    . replacen (
      &format! ("{}\n", forkee_line),
      &format! ( "{}\n{} (skg node (gnid {})) {}\n",
                 forkee_line, "*" . repeat ( depth (forkee_line) + 1 ),
                 case . child, case . child ),
      1 );
  let saved : String = save_cleanly (&moved_out, case . note, config, tantivy) . await ?;
  assert_eq! ( hides_of (config, case . note) ?, Vec::<ID>::new (),
               "the hide is removed:\n{}", saved );
  let shown : Vec<&str> = content_of (&saved, case . forkee);
  assert! ( shown . iter () . any ( |line| line . contains (&format! ("(gnid {})", case . child)) ),
            "{} shows {} again:\n{}", case . forkee, case . child, saved );
  Ok (( )) }

/// In Q's view k2 stands under F2, whose list names it m2, and under
/// G2, whose list names it k2. Cutting it from G2 alone says "hide
/// it" of one child that F2 says "show": the save is refused, and
/// nothing is written.
async fn a_cut_from_one_forkee_of_a_child_another_shows_by_an_extra_id_conflicts (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "Q") ?;
  for forkee in ["F2", "G2"] {
    let shown : Vec<&str> = content_of (&view, forkee);
    assert! ( shown . len () == 1 && shown [0] . contains ("(gnid k2)"),
              "{} shows k2:\n{}", forkee, view ); }
  let g2_line : &str = line_of (&view, "G2");
  let k2_under_g2 : String = format! ( "{}\n{}\n", g2_line, content_of (&view, "G2") [0] );
  let cut : String = view . replacen ( &k2_under_g2, &format! ("{}\n", g2_line), 1 );
  assert_ne! ( cut, view, "the cut changed the buffer" );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&cut, "Q", config, tantivy) . await ?;
  assert! ( refusal . contains ("Conflicting forkee visibility edits for forker Q and content k2"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// The note hides the child, as the forkee's list names it. The view
/// shows the child in the forkee's folder of what the note hides in
/// it; moved from there into the note's own content, it is unhidden:
/// the note contains it, and its hide, as the list names it, is
/// removed.
async fn a_hidden_child_moved_into_the_note_is_unhidden (
  case    : &Case,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, case . note) ?;
  let note_line : &str = line_of (&view, case . note);
  let child_line : &str = line_of (&view, case . child); // in the forkee's folder of what the note hides in it
  let moved_in : String = without_folder_under (
    &view, case . forkee, "hiddenInForkPartnerFolder" )
    . replacen (
      &format! ("{}\n", note_line),
      &format! ( "{}\n{} {}\n",
                 note_line, "*" . repeat ( depth (note_line) + 1 ),
                 child_line . trim_start_matches ('*') . trim_start () ),
      1 );
  let saved : String = save_cleanly (&moved_in, case . note, config, tantivy) . await ?;
  assert_eq! ( contains_of (config, case . note) ?, ids (&[case . child]),
               "the note contains the child:\n{}", saved );
  assert_eq! ( hides_of (config, case . note) ?, Vec::<ID>::new (),
               "the hide is removed:\n{}", saved );
  Ok (( )) }
