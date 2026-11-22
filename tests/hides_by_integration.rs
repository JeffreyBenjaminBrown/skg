// cargo nextest run --test grouped_forks -E 'test(hides_by_integration::)'
//
// Hides by integration ('docs/forks.org', "What you keep elsewhere"):
// a member as such of a note's forks folder shows what neither the
// note nor any owned note but the member contains, and the note does
// not hide; what the user keeps elsewhere stands in the member's
// folder of it ('integratedElsewhereFolder'), first, each member there
// with a containers folder saying where. A cut records a hide only
// where no owned note but the member contains the child after the
// save.
// The fixture: "public" and "private" are owned, declared in that
// order (most public first), and "theirs" is foreign. Your x (theirs)
// contains a, b, c and d. My M (public) forks x and contains d and z
// (public, empty). My y (private) contains a, f2 and g2. F (theirs)
// forks M and contains f1 and f2; G (theirs) forks x and contains g1
// and g2. So M's forks folder holds x, its forkee, F, a forker-only
// member, and G, an indirect member; a, f2 and g2 are kept in y, and
// d in M itself. y stands in no view of M, so that the containers
// folder under each kept child is no repeat and shows y.
// Each sub-test reads a view of M, and most edit it and save:
// - what the user keeps elsewhere stands in its own folder;
// - moving b from x to z hides nothing, and b joins that folder;
// - cutting c from x hides c;
// - moving d from M's content to z hides nothing, and the save says
//   that z keeps it; removing it from M alone hides it;
// - removing a from y makes a show under x again, with no hide;
// - an existing hide of a, which y keeps, stands in the folder of
//   what M hides in x, and moving a out of it unhides it;
// - under a narrowing that keeps out y's records, what y keeps shows;
// - moving f1 from F to z makes M fork F and hides nothing;
// - cutting g1 from G makes M fork G and hides g1; moving it to z
//   instead hides nothing;
// - d, kept by M alone and so drawing no containers folder, kept in y
//   too and then removed from M: the saved view names y, as a fresh
//   view does; put back under M, it names none again.
// One reads a view of x, foreign, instead: what x contains of M's
// content is x's own, so M draws it neither as content nor in its
// folder of what the user keeps elsewhere.

use std::error::Error;

use crate::view_and_save_helpers::{
  contains_of, content_of, depth, first_forks_folders_members, forks_of, hides_of,
  ids, is_a_fixpoint, line_of, save, save_cleanly, under, view_of,
  with_line_after_subtree_of, without_subtree_of};
use skg::metadata_canon::without_viewnode_ids;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::reset_then_run_subtest;
use skg::test_utils::{multi_root_view_with_skgrepo_set, run_with_shared_test_stores};
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, SkgrepoName, TantivyIndex};

const FIXTURES : &str = "tests/hides_by_integration/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-hides-by-integration",
    |s| Box::pin ( async move {
      reset_then_run_subtest! (s . reset (FIXTURES),
        what_the_user_keeps_elsewhere_stands_in_its_own_folder (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        a_move_into_an_owned_note_hides_nothing (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES), a_cut_hides_the_child (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        a_child_moved_from_the_note_into_an_owned_note_hides_nothing (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        a_child_removed_from_the_note_alone_is_hidden (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        a_child_no_owned_note_keeps_shows_again (&s . config, &mut s . tantivy) . await);
      s . reset_with_fixture_prep (
        "an_existing_hide_of_a_kept_child_stands_in_the_hidden_in_folder", FIXTURES,
        |root| { std::fs::write (
                   root . join ("public/M.skg"),
                   "pid: M\ntitle: M\ncontains:\n- d\n- z\nforks:\n- x\nhides:\n- a\n" ) ?;
                 Ok (( )) } ) ?;
      an_existing_hide_of_a_kept_child_stands_in_the_hidden_in_folder (&s . config, &mut s . tantivy) . await ?;
      reset_then_run_subtest! (s . reset (FIXTURES),
        under_a_narrowing_what_it_keeps_out_shows (&s . config, &s . tantivy));
      reset_then_run_subtest! (s . reset (FIXTURES),
        a_move_from_a_forker_as_such_into_an_owned_note_hides_nothing (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        a_cut_from_an_indirect_member_hides_the_child (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        a_move_from_an_indirect_member_into_an_owned_note_hides_nothing (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        a_member_standing_in_the_folder_names_its_new_keeper (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        under_a_foreign_note_what_it_contains_is_not_kept_by_the_user (&s . config, &mut s . tantivy) . await);
      Ok (( )) } )) }

//
// Helpers
//

/// The gnids of the children of the first folder of kind 'atom'
/// directly under the first line showing 'member', in order.
fn members_of_folder_under (
  view   : &str,
  member : &str,
  atom   : &str,
) -> Vec<String> {
  let member_depth : usize = depth (line_of (view, member));
  let below : Vec<&str> = under (view, member);
  let Some (at) : Option<usize> = below . iter ()
    . position ( |line| depth (line) == member_depth + 1
                        && line . contains (&format! ("(skg {})", atom)) )
    else { return Vec::new (); };
  below [at + 1 ..] . iter ()
    . take_while ( |line| depth (line) > member_depth + 1 )
    . filter ( |line| depth (line) == member_depth + 2 )
    . map ( |line| gnid_of (line) )
    . collect () }

/// The gnids of the content shown under the first line showing
/// 'member', in order.
fn content_gnids_of (
  view   : &str,
  member : &str,
) -> Vec<String> {
  content_of (view, member) . into_iter () . map (gnid_of) . collect () }

fn gnid_of (
  line : &str,
) -> String {
  line . split ("(gnid ") . nth (1) . unwrap ()
    . split (')') . next () . unwrap () . to_string () }

/// 'view' with the first line showing 'moved' at 'from_depth' (a leaf
/// there) cut, and pasted as the last child of the first line showing
/// 'new_parent'.
fn moved_under (
  view       : &str,
  moved      : &str,
  from_depth : usize,
  new_parent : &str,
) -> String {
  let lines : Vec<&str> = view . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| line . contains (&format! ("(gnid {})", moved))
                        && depth (line) == from_depth )
    . unwrap_or_else ( || panic! ( "no {} at depth {} in:\n{}", moved, from_depth, view ));
  let moved_line : String = lines [at] . trim_start_matches ('*') . trim_start () . to_string ();
  let without : String = lines . iter () . enumerate ()
    . filter ( |(i, _)| *i != at )
    . map ( |(_, line)| format! ("{}\n", line) )
    . collect ();
  let stars : String = "*" . repeat ( depth (line_of (&without, new_parent)) + 1 );
  with_line_after_subtree_of (
    &without, new_parent, &format! ("{} {}", stars, moved_line )) }

/// 'view' with the first line showing 'gone' at 'at_depth' removed,
/// with what stands under it.
fn without_line_at_depth (
  view     : &str,
  gone     : &str,
  at_depth : usize,
) -> String {
  let lines : Vec<&str> = view . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| line . contains (&format! ("(gnid {})", gone))
                        && depth (line) == at_depth )
    . unwrap_or_else ( || panic! ( "no {} at depth {} in:\n{}", gone, at_depth, view ));
  lines . iter () . enumerate ()
    . filter ( |(i, line)| *i != at
               && ! ( *i > at
                      && lines [at + 1 .. *i + 1] . iter ()
                         . all ( |between| depth (between) > at_depth )
                      && depth (line) > at_depth ))
    . map ( |(_, line)| format! ("{}\n", line) )
    . collect () }

//
// The sub-tests
//

/// x, M's forkee, shows b and c, and, first among its children, its
/// folder of what the user keeps elsewhere: a, which y keeps, and d,
/// which M contains, in x's order, each folded. a's containers folder
/// names y (x, its view-ancestor, left out); d's would name only
/// view-ancestors, and is not drawn. F, a forker-only member, shows
/// f1, and f2 in that folder; G, an indirect member, g1, and g2 in it.
/// Saved unchanged, the view writes nothing.
async fn what_the_user_keeps_elsewhere_stands_in_its_own_folder (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  assert_eq! ( content_gnids_of (&view, "x"), vec! ["b", "c"], "{}", view );
  assert! ( under (&view, "x") [0] . contains ("(skg integratedElsewhereFolder)"),
            "the folder stands first under x:\n{}", view );
  assert_eq! ( members_of_folder_under (&view, "x", "integratedElsewhereFolder"),
               vec! ["a", "d"], "{}", view );
  let a_line : &str = line_of (&view, "a");
  assert! ( a_line . contains ("folded") && a_line . contains ("writeProtected")
            && a_line . contains ("(birth (contains in 2))"),
            "a stands folded and write-protected, born of x's contains:\n{}", view );
  assert_eq! ( members_of_folder_under (&view, "a", "containerFolder"), vec! ["y"],
               "a's containers folder names y:\n{}", view );
  assert! ( members_of_folder_under (&view, "d", "containerFolder") . is_empty (),
            "{}", view );
  assert_eq! ( content_gnids_of (&view, "F"), vec! ["f1"], "{}", view );
  assert_eq! ( members_of_folder_under (&view, "F", "integratedElsewhereFolder"),
               vec! ["f2"], "{}", view );
  assert_eq! ( content_gnids_of (&view, "G"), vec! ["g1"], "{}", view );
  assert_eq! ( members_of_folder_under (&view, "G", "integratedElsewhereFolder"),
               vec! ["g2"], "{}", view );
  assert! ( line_of (&view, "x") . contains ("(unintegrated 2)"),
            "x counts b and c alone:\n{}", view );
  is_a_fixpoint (&view, "M", config, tantivy) . await ?;
  Ok (( )) }

/// b moved from under x to under z, my note: z contains b, M hides
/// nothing and forks x alone. The redrawn view shows b in x's folder
/// of what the user keeps elsewhere, between a and d, and c alone as
/// x's content. Saved unchanged, it writes nothing.
async fn a_move_into_an_owned_note_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  let saved : String =
    save_cleanly (&moved_under (&view, "b", 4, "z"), "M", config, tantivy) . await ?;
  assert_eq! ( contains_of (config, "z") ?, ids (&["b"]), "z contains b" );
  assert_eq! ( hides_of (config, "M") ?, ids (&[]), "M hides nothing" );
  assert_eq! ( forks_of (config, "M") ?, ids (&["x"]), "M forks x alone" );
  assert_eq! ( members_of_folder_under (&saved, "x", "integratedElsewhereFolder"),
               vec! ["a", "b", "d"], "{}", saved );
  assert_eq! ( content_gnids_of (&saved, "x"), vec! ["c"], "{}", saved );
  is_a_fixpoint (&saved, "M", config, tantivy) . await ?;
  Ok (( )) }

/// c cut from under x, kept nowhere: M hides c, which stands in x's
/// folder of what M hides in it. Saved unchanged, the view writes
/// nothing.
async fn a_cut_hides_the_child (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  let saved : String =
    save_cleanly (&without_subtree_of (&view, "c"), "M", config, tantivy) . await ?;
  assert_eq! ( hides_of (config, "M") ?, ids (&["c"]), "M hides c" );
  assert_eq! ( members_of_folder_under (&saved, "x", "hiddenInForkPartnerFolder"),
               vec! ["c"], "{}", saved );
  assert_eq! ( members_of_folder_under (&saved, "x", "integratedElsewhereFolder"),
               vec! ["a", "d"], "{}", saved );
  is_a_fixpoint (&saved, "M", config, tantivy) . await ?;
  Ok (( )) }

/// d, which M and x both contain, moved from M's content to under z:
/// M contains z alone, z contains d, and M hides nothing, since z
/// keeps d; the save says so. d still stands in x's folder of what the
/// user keeps elsewhere.
async fn a_child_moved_from_the_note_into_an_owned_note_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  let response : SaveResponse =
    save (&moved_under (&view, "d", 2, "z"), "M", config, tantivy) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  let saved : String = without_viewnode_ids (&response . saved_view);
  assert_eq! ( contains_of (config, "M") ?, ids (&["z"]), "M contains z alone" );
  assert_eq! ( contains_of (config, "z") ?, ids (&["d"]), "z contains d" );
  assert_eq! ( hides_of (config, "M") ?, ids (&[]), "M hides nothing" );
  assert! ( response . hides . iter () . any ( |line|
              line == "Hides nothing: 'd' left 'M', but 'z' keeps it." ),
            "the save says that z keeps d: {:?}", response . hides );
  assert_eq! ( members_of_folder_under (&saved, "x", "integratedElsewhereFolder"),
               vec! ["a", "d"], "{}", saved );
  is_a_fixpoint (&saved, "M", config, tantivy) . await ?;
  Ok (( )) }

/// d removed from M's content and kept nowhere else: M hides d, as
/// before, since x would show it again.
async fn a_child_removed_from_the_note_alone_is_hidden (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  let saved : String =
    save_cleanly (&without_line_at_depth (&view, "d", 2), "M", config, tantivy) . await ?;
  assert_eq! ( contains_of (config, "M") ?, ids (&["z"]), "M contains z alone" );
  assert_eq! ( hides_of (config, "M") ?, ids (&["d"]), "M hides d" );
  assert_eq! ( members_of_folder_under (&saved, "x", "hiddenInForkPartnerFolder"),
               vec! ["d"], "{}", saved );
  is_a_fixpoint (&saved, "M", config, tantivy) . await ?;
  Ok (( )) }

/// a removed from y, in a view of y: no owned note keeps a now, so a
/// fresh view of M shows it as x's content, out of the folder of what
/// the user keeps elsewhere, and M hides nothing.
async fn a_child_no_owned_note_keeps_shows_again (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let y_view : String = view_of (config, tantivy, "y") ?;
  save_cleanly (&without_subtree_of (&y_view, "a"), "y", config, tantivy) . await ?;
  assert_eq! ( contains_of (config, "y") ?, ids (&["f2", "g2"]), "y keeps a no longer" );
  let view : String = view_of (config, tantivy, "M") ?;
  assert_eq! ( content_gnids_of (&view, "x"), vec! ["a", "b", "c"], "{}", view );
  assert_eq! ( members_of_folder_under (&view, "x", "integratedElsewhereFolder"),
               vec! ["d"], "{}", view );
  assert_eq! ( hides_of (config, "M") ?, ids (&[]), "M hides nothing" );
  is_a_fixpoint (&view, "M", config, tantivy) . await ?;
  Ok (( )) }

/// M hides a, which y keeps too: a stands in x's folder of what M
/// hides in it, not in the folder of what the user keeps elsewhere,
/// and an unchanged save writes nothing. Moved out of that folder to
/// stand under x, a is unhidden, and the redrawn view shows it in the
/// folder of what the user keeps elsewhere, which holds a and d.
async fn an_existing_hide_of_a_kept_child_stands_in_the_hidden_in_folder (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  assert_eq! ( members_of_folder_under (&view, "x", "hiddenInForkPartnerFolder"),
               vec! ["a"], "{}", view );
  assert_eq! ( members_of_folder_under (&view, "x", "integratedElsewhereFolder"),
               vec! ["d"], "{}", view );
  is_a_fixpoint (&view, "M", config, tantivy) . await ?;
  let x_line : &str = line_of (&view, "x");
  let moved_out : String = {
    let without_folder : String = view . lines ()
      . filter ( |line| ! line . contains ("(skg hiddenInForkPartnerFolder)") )
      . map ( |line| format! ("{}\n", line) )
      . collect ();
    let without_a : String = without_line_at_depth (&without_folder, "a", 5);
    without_a . replacen (
      &format! ("{}\n", x_line),
      &format! ("{}\n**** (skg node (gnid a) (skgRepos (nodeRepo theirs))) a\n", x_line),
      1 ) };
  let saved : String = save_cleanly (&moved_out, "M", config, tantivy) . await ?;
  assert_eq! ( hides_of (config, "M") ?, ids (&[]), "M hides a no longer" );
  assert_eq! ( members_of_folder_under (&saved, "x", "integratedElsewhereFolder"),
               vec! ["a", "d"], "{}", saved );
  assert! ( members_of_folder_under (&saved, "x", "hiddenInForkPartnerFolder") . is_empty (),
            "{}", saved );
  assert_eq! ( content_gnids_of (&saved, "x"), vec! ["b", "c"], "{}", saved );
  is_a_fixpoint (&saved, "M", config, tantivy) . await ?;
  Ok (( )) }

/// Under "public" alone, y (private) and its records are out of
/// sight, so nothing the user keeps only there is integrated: x shows
/// a, b and c, keeping d alone in its folder; F shows f1 and f2, G g1
/// and g2, neither drawing the folder.
fn under_a_narrowing_what_it_keeps_out_shows (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let public : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoName::from ("public")) ?;
  let (view, _, _) = multi_root_view_with_skgrepo_set (
    config, Some (tantivy), &[ ID::from ("M") ], &public ) ?;
  let view : String = without_viewnode_ids (&view);
  assert_eq! ( content_gnids_of (&view, "x"), vec! ["a", "b", "c"], "{}", view );
  assert_eq! ( members_of_folder_under (&view, "x", "integratedElsewhereFolder"),
               vec! ["d"], "{}", view );
  assert_eq! ( content_gnids_of (&view, "F"), vec! ["f1", "f2"], "{}", view );
  assert_eq! ( content_gnids_of (&view, "G"), vec! ["g1", "g2"], "{}", view );
  assert! ( members_of_folder_under (&view, "F", "integratedElsewhereFolder") . is_empty ()
            && members_of_folder_under (&view, "G", "integratedElsewhereFolder") . is_empty (),
            "{}", view );
  Ok (( )) }

/// f1 moved from under F, a forker-only member, to under z: M comes
/// to fork F, as any cut from a member claiming nothing makes it, but
/// hides nothing, since z keeps f1. F, a mutual now, keeps f1 and f2
/// in its folder of what the user keeps elsewhere.
async fn a_move_from_a_forker_as_such_into_an_owned_note_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  let saved : String =
    save_cleanly (&moved_under (&view, "f1", 4, "z"), "M", config, tantivy) . await ?;
  assert_eq! ( contains_of (config, "z") ?, ids (&["f1"]), "z contains f1" );
  assert_eq! ( forks_of (config, "M") ?, ids (&["x", "F"]), "M forks F" );
  assert_eq! ( hides_of (config, "M") ?, ids (&[]), "M hides nothing" );
  assert_eq! ( members_of_folder_under (&saved, "F", "integratedElsewhereFolder"),
               vec! ["f1", "f2"], "{}", saved );
  is_a_fixpoint (&saved, "M", config, tantivy) . await ?;
  Ok (( )) }

/// g1 cut from under G, an indirect member, kept nowhere: M forks G
/// and hides g1.
async fn a_cut_from_an_indirect_member_hides_the_child (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  let saved : String =
    save_cleanly (&without_subtree_of (&view, "g1"), "M", config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "M") ?, ids (&["x", "G"]), "M forks G" );
  assert_eq! ( hides_of (config, "M") ?, ids (&["g1"]), "M hides g1" );
  is_a_fixpoint (&saved, "M", config, tantivy) . await ?;
  Ok (( )) }

/// g1 moved from under G, an indirect member, to under z: M forks G
/// and hides nothing.
async fn a_move_from_an_indirect_member_into_an_owned_note_hides_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  let saved : String =
    save_cleanly (&moved_under (&view, "g1", 4, "z"), "M", config, tantivy) . await ?;
  assert_eq! ( contains_of (config, "z") ?, ids (&["g1"]), "z contains g1" );
  assert_eq! ( forks_of (config, "M") ?, ids (&["x", "G"]), "M forks G" );
  assert_eq! ( hides_of (config, "M") ?, ids (&[]), "M hides nothing" );
  assert_eq! ( members_of_folder_under (&saved, "G", "integratedElsewhereFolder"),
               vec! ["g1", "g2"], "{}", saved );
  is_a_fixpoint (&saved, "M", config, tantivy) . await ?;
  Ok (( )) }

/// d, which M and x both contain, stands in x's folder of what the
/// user keeps elsewhere with no containers folder: M, its only keeper,
/// is a view-ancestor. Another view puts d under y too; then M's view,
/// drawn before that, removes d from M's content. The saved view
/// draws d's containers folder, naming y, as a fresh view of M does,
/// though d stood in the folder without one. Put back under M, d is a
/// repeat there again, and its containers folder goes, as a fresh
/// view draws none.
async fn a_member_standing_in_the_folder_names_its_new_keeper (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "M") ?;
  assert! ( members_of_folder_under (&view, "d", "containerFolder") . is_empty (),
            "{}", view );
  let y_view : String = view_of (config, tantivy, "y") ?;
  save_cleanly ( &with_line_after_subtree_of (
                   &y_view, "y", "** (skg node (gnid d) (skgRepos (nodeRepo theirs))) d" ),
                 "y", config, tantivy ) . await ?;
  assert_eq! ( contains_of (config, "y") ?, ids (&["a", "f2", "g2", "d"]), "y keeps d" );
  let saved : String = save_cleanly (
    &without_line_at_depth (&view, "d", 2), "M", config, tantivy ) . await ?;
  assert_eq! ( contains_of (config, "M") ?, ids (&["z"]), "M contains z alone" );
  assert_eq! ( hides_of (config, "M") ?, ids (&[]), "M hides nothing" );
  assert_eq! ( members_of_folder_under (&saved, "d", "containerFolder"), vec! ["y"],
               "d's containers folder names y:\n{}", saved );
  assert_eq! ( saved, view_of (config, tantivy, "M") ?,
               "the saved view is a fresh one" );
  let again : String = save_cleanly (
    &with_line_after_subtree_of (
      &saved, "z", "** (skg node (gnid d) (skgRepos (nodeRepo theirs))) d" ),
    "M", config, tantivy ) . await ?;
  assert_eq! ( contains_of (config, "M") ?, ids (&["z", "d"]), "M contains d again" );
  assert! ( members_of_folder_under (&again, "d", "containerFolder") . is_empty (),
            "d, a repeat of M's own d again, names no keeper:\n{}", again );
  assert_eq! ( again, view_of (config, tantivy, "M") ?,
               "and that saved view is a fresh one too" );
  Ok (( )) }

/// A view of x, foreign: M, a forker-only member of its forks folder,
/// contains d, which x contains too. That is x's own content, not
/// something the user keeps elsewhere: M draws only z, and no folder
/// of what the user keeps elsewhere. G and F, whose f2 and g2 y keeps,
/// still draw theirs. Saved unchanged, the view writes nothing.
async fn under_a_foreign_note_what_it_contains_is_not_kept_by_the_user (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "x") ?;
  assert_eq! ( first_forks_folders_members (&view), vec! ["M", "G", "F"], "{}", view );
  assert_eq! ( content_gnids_of (&view, "M"), vec! ["z"], "{}", view );
  assert! ( members_of_folder_under (&view, "M", "integratedElsewhereFolder") . is_empty (),
            "{}", view );
  assert! ( line_of (&view, "M") . contains ("(unintegrated 1)"),
            "M counts z alone:\n{}", view );
  assert_eq! ( members_of_folder_under (&view, "G", "integratedElsewhereFolder"),
               vec! ["g2"], "{}", view );
  assert_eq! ( members_of_folder_under (&view, "F", "integratedElsewhereFolder"),
               vec! ["f2"], "{}", view );
  is_a_fixpoint (&view, "x", config, tantivy) . await ?;
  Ok (( )) }
