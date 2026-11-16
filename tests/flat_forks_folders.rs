// cargo nextest run --test grouped_forks -E 'test(flat_forks_folders::)'
//
// Flat forks folders ('docs/forks.org', "The forks folder"): a note's
// forks folder lists every mirror of it, each once, its direct fork
// partners first, then its indirect members ('docs/glossary.org',
// "direct member, indirect member"), which claim nothing; no member
// shows a forks folder unless asked.
// The fixture: "main" (owned) is declared before "theirs" (foreign).
// A, in main, contains a1, forks B and hides d2. B, in main, contains
// b1 and forks C, in theirs, which contains c1. D, in main, forks B,
// contains d1 and d2, and hides a1. AA, in theirs, forks B. So B's
// folder holds C, its forkee, then A, D and AA, its forkers; and A's
// holds B, its forkee, then D, AA and C, its indirect members.
// Each sub-test reads a view, and some edit it and save:
// - B's folder, in the order of 'docs/forks.org': the forkee, then
//   the forkers by the declaration order of their homes (D before AA,
//   though AA's pid is the lesser);
// - A's folder is flat and saves unchanged, writing nothing;
// - a forks request on a member (C-c l f there) draws its folder;
// - deleting D in A's view hides D in A;
// - cutting d1 from D in A's view makes A fork D and hide d1.

use std::error::Error;
use std::collections::BTreeMap;

use crate::view_and_save_helpers::{
  content_of, first_forks_folders_members, ids, line_of, node, save_cleanly,
  skg_files, under, view_of, without_subtree_of};
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{SkgConfig, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;

const FIXTURES : &str = "tests/flat_forks_folders/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-flat-forks-folders",
    |s| Box::pin ( async move {
      s . reset ("a_forks_folder_takes_the_graphs_and_the_configs_order", FIXTURES) ?;
      a_forks_folder_takes_the_graphs_and_the_configs_order (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_forks_folder_is_flat_and_saves_unchanged", FIXTURES) ?;
      a_forks_folder_is_flat_and_saves_unchanged (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_forks_request_on_a_member_draws_its_forks_folder", FIXTURES) ?;
      a_forks_request_on_a_member_draws_its_forks_folder (&s . config, &mut s . tantivy) . await ?;
      s . reset ("deleting_an_indirect_member_hides_it", FIXTURES) ?;
      deleting_an_indirect_member_hides_it (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_cut_from_an_indirect_member_makes_the_note_fork_it", FIXTURES) ?;
      a_cut_from_an_indirect_member_makes_the_note_fork_it (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// Save 'view' unchanged as a view of 'root': no file is written, and
/// the saved view is 'view' again.
async fn is_a_fixpoint (
  view    : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let saved : String = save_cleanly (view, root, config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  assert_eq! ( saved, view, "and draws the view again as it was" );
  Ok (( )) }

/// Whether the headline 'line' stands for a member claiming nothing,
/// born of nothing, with no fork toward the note two levels up: an
/// indirect member.
fn is_drawn_as_an_indirect_member (
  line : &str,
) -> bool {
  line . contains ("noClaim") && ! line . contains ("birth")
  && ! line . contains ("ancestors 2") }

//
// The sub-tests
//

/// B's forks folder holds C, which B forks, claiming; then A, D and
/// AA, which fork B, claiming nothing, by the declaration order of
/// their homes ("main", then "theirs"), then pid; no indirect member,
/// since every mirror of B is a fork partner of it.
async fn a_forks_folder_takes_the_graphs_and_the_configs_order (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "B") ?;
  assert_eq! ( first_forks_folders_members (&view), vec! ["C", "A", "D", "AA"],
               "{}", view );
  assert! ( ! line_of (&view, "C") . contains ("noClaim"), "{}", view );
  for forker in ["A", "D", "AA"] {
    let line : &str = line_of (&view, forker);
    assert! ( line . contains ("noClaim") && line . contains ("(birth (forks out 2))"),
              "{} is a forker-only member:\n{}", forker, view ); }
  is_a_fixpoint (&view, "B", config, tantivy) . await ?;
  Ok (( )) }

/// A's forks folder holds B, which A forks ('(birth (forks in 2))',
/// herald bK), then D, AA and C, the indirect members, nearest first
/// (all two forks away), then by the declaration order of their homes
/// (D, in "main", before AA and C, in "theirs"), then pid. Each
/// indirect member claims nothing, is born of nothing and shows no
/// fork toward A. The view holds that one forks folder alone. D, drawn
/// editable, shows d1, and d2, which A hides, in its folder of what A
/// hides in it; so d2 is no member of the folder of what A hides
/// outside its fork partners, which is not drawn. D hides a1, which A
/// contains, but its folder of what it hides of A's content is not
/// drawn: D does not fork A. Saved unchanged, the view writes nothing.
async fn a_forks_folder_is_flat_and_saves_unchanged (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "A") ?;
  assert_eq! ( first_forks_folders_members (&view), vec! ["B", "D", "AA", "C"],
               "{}", view );
  assert_eq! ( view . matches ("forksFolder") . count (), 1,
               "one forks folder, nothing nested:\n{}", view );
  let b_line : &str = line_of (&view, "B");
  assert! ( ! b_line . contains ("noClaim") && b_line . contains ("(birth (forks in 2))")
            && b_line . contains ("(forks (in 3 (ancestors 2))"),
            "B is A's forkee, herald bK:\n{}", view );
  for indirect in ["D", "AA", "C"] {
    assert! ( is_drawn_as_an_indirect_member (line_of (&view, indirect)),
              "{} is an indirect member:\n{}", indirect, view ); }
  assert! ( ! line_of (&view, "D") . contains ("writeProtected"), "{}", view );
  let d_shows : Vec<&str> = content_of (&view, "D");
  assert! ( d_shows . len () == 1 && d_shows [0] . contains ("(gnid d1)"),
            "D shows d1 alone:\n{}", view );
  assert! ( under (&view, "D") . iter () . any ( |line|
              line . contains ("hiddenInForkPartnerFolder") )
            && under (&view, "D") . iter () . any ( |line|
              line . contains ("(gnid d2)") && line . contains ("folded") ),
            "d2 stands in D's folder of what A hides in it:\n{}", view );
  assert! ( ! view . contains ("hiddenOutsideOfForkPartnersFolder"),
            "a hide that D's content explains stands under D alone:\n{}", view );
  assert! ( ! view . contains ("hiddenByForkerFolder"),
            "D's hides do not apply to A's content:\n{}", view );
  is_a_fixpoint (&view, "A", config, tantivy) . await ?;
  Ok (( )) }

/// In A's view, B asked for its forks folder ('(folder forks)', as
/// C-c l f there asks) draws it: C, which B forks, then A, D and AA,
/// its forkers. The save writes nothing, and the redrawn view keeps
/// the folder.
async fn a_forks_request_on_a_member_draws_its_forks_folder (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "A") ?;
  let asked : String = view . replacen (
    "(skg node (gnid B)", "(skg node (viewRequests (folder forks)) (gnid B)", 1 );
  assert_ne! ( asked, view );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let saved : String = save_cleanly (&asked, "A", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "the request writes nothing" );
  assert_eq! ( saved . matches ("forksFolder") . count (), 2,
               "B shows its forks folder:\n{}", saved );
  let b_folder : Vec<String> = first_forks_folders_members (
    &under (&saved, "B") . join ("\n") );
  assert_eq! ( b_folder, vec! ["C", "A", "D", "AA"], "{}", saved );
  is_a_fixpoint (&saved, "A", config, tantivy) . await ?;
  Ok (( )) }

/// In A's view, D deleted from the forks folder: A hides D, which
/// leaves the folder for the folder of what A hides outside its fork
/// partners; d2, which only D contained among the members, joins it
/// there. D's file is untouched. Saved unchanged, the redrawn view
/// writes nothing.
async fn deleting_an_indirect_member_hides_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "A") ?;
  let d_before : String = skg_files (config) ? ["main/D.skg"] . clone ();
  let saved : String =
    save_cleanly (&without_subtree_of (&view, "D"), "A", config, tantivy) . await ?;
  let a : Graphnode = node (config, "A") ?;
  assert_eq! ( members_of ( a . hides . or_default () ), ids (&["d2", "D"]),
               "A hides D" );
  assert_eq! ( members_of ( a . forks . or_default () ), ids (&["B"]),
               "A forks B alone" );
  assert_eq! ( skg_files (config) ? ["main/D.skg"], d_before, "D's file is untouched" );
  assert_eq! ( first_forks_folders_members (&saved), vec! ["B", "AA", "C"],
               "D leaves the folder:\n{}", saved );
  let at : usize = saved . lines ()
    . position ( |line| line . contains ("hiddenOutsideOfForkPartnersFolder") )
    . unwrap_or_else ( || panic! ( "A has a hidden-outside folder:\n{}", saved ));
  let in_it : Vec<&str> = saved . lines () . skip (at + 1) . take (2) . collect ();
  assert! ( in_it . iter () . any ( |line| line . contains ("(gnid D)") )
            && in_it . iter () . any ( |line| line . contains ("(gnid d2)") ),
            "D and d2 stand in A's folder of what it hides outside its fork partners:\n{}",
            saved );
  is_a_fixpoint (&saved, "A", config, tantivy) . await ?;
  Ok (( )) }

/// In A's view, d1 cut from D, an indirect member: A comes to fork D,
/// after B, and hides d1; D's file is untouched. The redrawn view
/// shows D claiming, a forkee as such now ('(birth (forks in 2))'),
/// showing nothing, with d2 and d1 in its folder of what A hides in
/// it. Saved unchanged, it writes nothing.
async fn a_cut_from_an_indirect_member_makes_the_note_fork_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "A") ?;
  let d_before : String = skg_files (config) ? ["main/D.skg"] . clone ();
  let saved : String =
    save_cleanly (&without_subtree_of (&view, "d1"), "A", config, tantivy) . await ?;
  let a : Graphnode = node (config, "A") ?;
  assert_eq! ( members_of ( a . forks . or_default () ), ids (&["B", "D"]),
               "A forks D now" );
  assert_eq! ( members_of ( a . hides . or_default () ), ids (&["d2", "d1"]),
               "A hides d1" );
  assert_eq! ( skg_files (config) ? ["main/D.skg"], d_before, "D's file is untouched" );
  let d_line : &str = line_of (&saved, "D");
  assert! ( ! d_line . contains ("noClaim") && d_line . contains ("(birth (forks in 2))"),
            "D claims, a forkee as such:\n{}", saved );
  assert! ( content_of (&saved, "D") . is_empty (), "D shows nothing:\n{}", saved );
  let hidden_in_d : Vec<&str> = under (&saved, "D") . into_iter ()
    . filter ( |line| line . contains ("(gnid d1)") || line . contains ("(gnid d2)") )
    . collect ();
  assert_eq! ( hidden_in_d . len (), 2,
               "d1 and d2 stand in D's folder of what A hides in it:\n{}", saved );
  is_a_fixpoint (&saved, "A", config, tantivy) . await ?;
  Ok (( )) }
