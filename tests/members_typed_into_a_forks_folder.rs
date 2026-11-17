// cargo nextest run --test grouped_forks -E 'test(members_typed_into_a_forks_folder::)'
//
// A note typed into a forks folder in a save ('docs/forks.org',
// "Typing a note into the folder"): the view drew nothing under it,
// so nothing can be cut from it, and the save hides nothing for it,
// whatever children are typed under it. A member the folder showed
// before the save keeps its reading: what is cut from it is hidden.
// The fixture: "main" is owned, "theirs" foreign. N (main) forks G
// (empty) and contains O, which contains x. E (main) contains a, b and
// x, and is no mirror of N. H and E2 (theirs) each contain h. Each
// sub-test reads a view of N, edits it and saves; the first six are
// the probe's runs:
// - E typed claiming, with a and a new d under it: N forks E and hides
//   nothing; d joins E's content (2a);
// - E typed claiming, with nothing under it: N forks E and hides
//   nothing (2b);
// - E typed claiming, with a and b under it, x cut from O: N forks E
//   and hides nothing (2c);
// - E typed claiming nothing, with a and b under it, x cut from O: N
//   neither forks E nor hides anything, and E stands in the folder
//   unwarranted (2g);
// - E typed claiming nothing, with a and a new d under it: likewise,
//   and d joins E's content (2e);
// - E typed claiming nothing, with nothing under it: nothing is
//   written (2f);
// - with N forking E before the save, b cut from E is hidden;
// - with N forking H before the save, E2 typed claiming with nothing
//   under it says nothing of h, which H shows: no conflict, and N
//   hides nothing.

use std::collections::BTreeMap;
use std::error::Error;

use crate::view_and_save_helpers::{
  contains_of, content_of, forks_of, hides_of, ids, line_of, save_cleanly,
  skg_files, view_of, with_line_after_subtree_of, without_subtree_of};
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

const FIXTURES : &str = "tests/members_typed_into_a_forks_folder/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-members-typed-into-a-forks-folder",
    |s| Box::pin ( async move {
      s . reset ("a_claiming_member_typed_with_some_children_hides_nothing", FIXTURES) ?;
      a_claiming_member_typed_with_some_children_hides_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_claiming_member_typed_with_no_children_hides_nothing", FIXTURES) ?;
      a_claiming_member_typed_with_no_children_hides_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_claiming_member_typed_while_its_child_leaves_an_owned_note_hides_nothing", FIXTURES) ?;
      a_claiming_member_typed_while_its_child_leaves_an_owned_note_hides_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_member_typed_claiming_nothing_while_its_child_leaves_an_owned_note_hides_nothing", FIXTURES) ?;
      a_member_typed_claiming_nothing_while_its_child_leaves_an_owned_note_hides_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_member_typed_claiming_nothing_with_some_children_hides_nothing", FIXTURES) ?;
      a_member_typed_claiming_nothing_with_some_children_hides_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_member_typed_claiming_nothing_with_no_children_writes_nothing", FIXTURES) ?;
      a_member_typed_claiming_nothing_with_no_children_writes_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset_with_fixture_prep (
        "a_cut_from_a_member_shown_before_the_save_hides_it", FIXTURES,
        |root| { std::fs::write (
                   root . join ("main/N.skg"),
                   "pid: N\ntitle: N\ncontains:\n- O\nforks:\n- G\n- E\n" ) ?;
                 Ok (( )) } ) ?;
      a_cut_from_a_member_shown_before_the_save_hides_it (&s . config, &mut s . tantivy) . await ?;
      s . reset_with_fixture_prep (
        "a_typed_member_says_nothing_of_what_another_member_shows", FIXTURES,
        |root| { std::fs::write (
                   root . join ("main/N.skg"),
                   "pid: N\ntitle: N\ncontains:\n- O\nforks:\n- G\n- H\n" ) ?;
                 Ok (( )) } ) ?;
      a_typed_member_says_nothing_of_what_another_member_shows (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// N's view with 'typed' (one or more lines) typed into its forks
/// folder, after G; and, where 'cut_x', x cut from O.
fn typed_into_the_folder (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
  typed   : &str,
  cut_x   : bool,
) -> Result<String, Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let typed_in : String = with_line_after_subtree_of (&view, "G", typed);
  Ok ( if cut_x { without_subtree_of (&typed_in, "x") } else { typed_in } ) }

//
// The sub-tests
//

/// The probe's run 2a.
async fn a_claiming_member_typed_with_some_children_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let edited : String = typed_into_the_folder (
    config, tantivy,
    "*** (skg node (gnid E)) E\n**** (skg node (gnid a)) a\n**** d", false ) ?;
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["G", "E"]), "N forks E" );
  assert_eq! ( hides_of (config, "N") ?, Vec::<ID>::new (), "N hides nothing:\n{}", saved );
  let e_contains : Vec<ID> = contains_of (config, "E") ?;
  assert_eq! ( e_contains . len (), 4, "d joins E's content: {:?}", e_contains );
  assert_eq! ( e_contains [0], ID::from ("a") );
  assert_eq! ( &e_contains [2 ..], &ids (&["b", "x"]) [..] );
  Ok (( )) }

/// The probe's run 2b.
async fn a_claiming_member_typed_with_no_children_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let edited : String = typed_into_the_folder (
    config, tantivy, "*** (skg node (gnid E)) E", false ) ?;
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["G", "E"]), "N forks E" );
  assert_eq! ( hides_of (config, "N") ?, Vec::<ID>::new (), "N hides nothing:\n{}", saved );
  assert_eq! ( contains_of (config, "E") ?, ids (&["a", "b", "x"]), "E is as it was" );
  let shown : Vec<&str> = content_of (&saved, "E");
  assert! ( shown . len () == 2
            && shown [0] . contains ("(gnid a)") && shown [1] . contains ("(gnid b)"),
            "E shows a and b, which N now sees from it:\n{}", saved );
  Ok (( )) }

/// The probe's run 2c.
async fn a_claiming_member_typed_while_its_child_leaves_an_owned_note_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let edited : String = typed_into_the_folder (
    config, tantivy,
    "*** (skg node (gnid E)) E\n**** (skg node (gnid a)) a\n**** (skg node (gnid b)) b", true ) ?;
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["G", "E"]), "N forks E" );
  assert_eq! ( hides_of (config, "N") ?, Vec::<ID>::new (), "N hides nothing:\n{}", saved );
  assert_eq! ( contains_of (config, "O") ?, Vec::<ID>::new (), "x left O" );
  assert! ( content_of (&saved, "E") . iter () . any ( |line| line . contains ("(gnid x)") ),
            "E shows x, which nothing of the user's keeps now:\n{}", saved );
  Ok (( )) }

/// The probe's run 2g. A member typed claiming nothing, that is no
/// mirror of N, stands in the folder unwarranted, as one typed with
/// nothing cut from it always did.
async fn a_member_typed_claiming_nothing_while_its_child_leaves_an_owned_note_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let edited : String = typed_into_the_folder (
    config, tantivy,
    "*** (skg node (gnid E) noClaim) E\n**** (skg node (gnid a)) a\n**** (skg node (gnid b)) b", true ) ?;
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["G"]), "N does not fork E" );
  assert_eq! ( hides_of (config, "N") ?, Vec::<ID>::new (), "N hides nothing:\n{}", saved );
  assert_eq! ( contains_of (config, "O") ?, Vec::<ID>::new (), "x left O" );
  let e_line : &str = line_of (&saved, "E");
  assert! ( e_line . contains ("noClaim") && e_line . contains ("noWarrant"),
            "E stands unwarranted, claiming nothing:\n{}", saved );
  Ok (( )) }

/// The probe's run 2e.
async fn a_member_typed_claiming_nothing_with_some_children_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let edited : String = typed_into_the_folder (
    config, tantivy,
    "*** (skg node (gnid E) noClaim) E\n**** (skg node (gnid a)) a\n**** d", false ) ?;
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["G"]), "N does not fork E" );
  assert_eq! ( hides_of (config, "N") ?, Vec::<ID>::new (), "N hides nothing:\n{}", saved );
  assert_eq! ( contains_of (config, "E") ? . len (), 4, "d joins E's content" );
  Ok (( )) }

/// The probe's run 2f.
async fn a_member_typed_claiming_nothing_with_no_children_writes_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let edited : String = typed_into_the_folder (
    config, tantivy, "*** (skg node (gnid E) noClaim) E", false ) ?;
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "nothing is written:\n{}", saved );
  Ok (( )) }

/// N forks E before the save, so the folder shows E, with a and b (x
/// is kept in O). Cutting b from E hides it.
async fn a_cut_from_a_member_shown_before_the_save_hides_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let shown : Vec<&str> = content_of (&view, "E");
  assert! ( shown . len () == 2
            && shown [0] . contains ("(gnid a)") && shown [1] . contains ("(gnid b)"),
            "E shows a and b:\n{}", view );
  save_cleanly (&without_subtree_of (&view, "b"), "N", config, tantivy) . await ?;
  assert_eq! ( hides_of (config, "N") ?, ids (&["b"]), "b is hidden" );
  Ok (( )) }

/// N forks H before the save, so the folder shows H, with h. E2,
/// which contains h too, typed claiming with nothing under it, says
/// nothing of h, since the view drew nothing under E2: the save is no
/// conflict, N forks E2, and hides nothing.
async fn a_typed_member_says_nothing_of_what_another_member_shows (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  assert! ( content_of (&view, "H") . iter () . any ( |line| line . contains ("(gnid h)") ),
            "H shows h:\n{}", view );
  let edited : String = with_line_after_subtree_of (
    &view, "H", "*** (skg node (gnid E2) (skgRepos (nodeRepo theirs))) E2" );
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["G", "H", "E2"]), "N forks E2" );
  assert_eq! ( hides_of (config, "N") ?, Vec::<ID>::new (), "N hides nothing:\n{}", saved );
  Ok (( )) }
