// cargo nextest run --test grouped_forks -E 'test(cuts_from_forkers::)'
//
// A cut from a forker as such ('TODO/every-mirror/2_plan.org', slice
// 5; 'docs/forks.org', "Cutting from a forker"). The fixture is
// Jeff's example, "forking a forker": N, owned, contains nothing; F,
// foreign ("you"), forks N and contains y and z. E, owned, forks N
// too, and contains e1; M, owned, already forks F (monogamy is gone,
// so N may fork F as well). G, foreign, is forked by H, foreign,
// which contains h. Each sub-test edits a view and saves it:
// - Jeff's example: in N's view, y cut from F and pasted under N.
//   N comes to fork F, contains y and hides nothing; the redrawn view
//   shows F first in N's forks folder, claiming, editable, showing z,
//   then E, and y as N's content. Saved again unchanged, it writes
//   nothing and draws itself again;
// - the same cut without the paste: N forks F and hides y, which the
//   redrawn view shows in F's folder of what N hides in it;
// - a cut from H in G's view is refused, naming G, which is foreign;
// - a cut that makes N fork F while the same save places F in N's
//   content is refused by the fork rules, and nothing is written;
// - a cut from F, a mutual, whose claim the same save removes (a
//   detachment): N stops forking F, and hides what was cut.

use std::error::Error;
use std::collections::BTreeMap;

use crate::view_and_save_helpers::{
  content_of, first_forks_folders_members, ids, line_of, node, refusal_of,
  save_cleanly, skg_files, under, view_of, without_subtree_of};
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;

const FIXTURES : &str = "tests/cuts_from_forkers/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-cuts-from-forkers",
    |s| Box::pin ( async move {
      s . reset ("jeffs_example_forking_a_forker", FIXTURES) ?;
      jeffs_example_forking_a_forker (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_cut_without_a_paste_hides_what_was_cut", FIXTURES) ?;
      a_cut_without_a_paste_hides_what_was_cut (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_cut_under_a_foreign_note_is_refused", FIXTURES) ?;
      a_cut_under_a_foreign_note_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_cut_that_would_make_n_fork_what_it_contains_is_refused", FIXTURES) ?;
      a_cut_that_would_make_n_fork_what_it_contains_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_cut_from_a_mutual_detached_in_the_same_save_only_hides", FIXTURES) ?;
      a_cut_from_a_mutual_detached_in_the_same_save_only_hides (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// A fresh view of N, checked: its forks folder holds E then F, both
/// forkers as such (claiming nothing), editable, and F shows y and z.
fn view_of_n_before_any_cut (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  assert_eq! ( first_forks_folders_members (&view), vec! ["E", "F"], "{}", view );
  let f_line : &str = line_of (&view, "F");
  assert! ( f_line . contains ("noClaim") && ! f_line . contains ("writeProtected"),
            "F is a forker as such, editable:\n{}", view );
  let shown : Vec<&str> = content_of (&view, "F");
  assert! ( shown . len () == 2
            && shown [0] . contains ("(gnid y)") && shown [1] . contains ("(gnid z)"),
            "F shows y and z:\n{}", view );
  Ok (view) }

//
// The sub-tests
//

/// Jeff's example, "forking a forker". In N's view, y is cut from F
/// and pasted under N. After the save N forks F (which M forks too:
/// monogamy is gone), N contains y, and N hides nothing, since it
/// contains what it cut. F's file is untouched. The redrawn view is
/// Jeff's "after": F stands first in N's forks folder, a mutual,
/// claiming, editable, showing z; then E; and y as N's content. Saved
/// again unchanged, it writes nothing and draws itself again.
async fn jeffs_example_forking_a_forker (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n_before_any_cut (config, tantivy) ?;
  let y_line : String = line_of (&view, "y") . to_string ();
  let pasted : String = format! (
    "{}** {}\n",
    without_subtree_of (&view, "y"),
    y_line . trim_start_matches ('*') . trim_start () );
  let f_before : String = std::fs::read_to_string (
    format! ("{}/theirs/F.skg", FIXTURES) ) ?;
  let saved : String = save_cleanly (&pasted, "N", config, tantivy) . await ?;
  let n : Graphnode = node (config, "N") ?;
  assert_eq! ( members_of ( n . forks . or_default () ), ids (&["F"]),
               "N forks F" );
  assert_eq! ( members_of (&n . contains), ids (&["y"]), "N contains y" );
  assert_eq! ( members_of ( n . hides . or_default () ), Vec::<ID>::new (),
               "N hides nothing: it contains what it cut" );
  assert_eq! ( members_of (& node (config, "F") ? . contains), ids (&["y", "z"]),
               "F's list is as it was" );
  assert_eq! ( skg_files (config) ? . get ("theirs/F.skg"), Some (&f_before),
               "F's file is untouched" );
  assert_eq! ( members_of ( node (config, "M") ? . forks . or_default () ), ids (&["F"]),
               "M still forks F" );
  assert_eq! ( first_forks_folders_members (&saved), vec! ["F", "E"],
               "F, now a forkee, leads:\n{}", saved );
  let f_line : &str = line_of (&saved, "F");
  assert! ( ! f_line . contains ("noClaim") && ! f_line . contains ("writeProtected"),
            "F claims, editable:\n{}", saved );
  let f_shows : Vec<&str> = content_of (&saved, "F");
  assert! ( f_shows . len () == 1 && f_shows [0] . contains ("(gnid z)"),
            "F shows z alone:\n{}", saved );
  let n_shows : Vec<&str> = content_of (&saved, "N");
  assert! ( n_shows . len () == 1 && n_shows [0] . contains ("(gnid y)"),
            "N shows y:\n{}", saved );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let again : String = save_cleanly (&saved, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  assert_eq! ( again, saved, "and draws the view again as it was" );
  Ok (( )) }

/// The same cut, without the paste: N forks F and hides y, and the
/// redrawn view shows y in F's folder of what N hides in it.
async fn a_cut_without_a_paste_hides_what_was_cut (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n_before_any_cut (config, tantivy) ?;
  let saved : String =
    save_cleanly (&without_subtree_of (&view, "y"), "N", config, tantivy) . await ?;
  let n : Graphnode = node (config, "N") ?;
  assert_eq! ( members_of ( n . forks . or_default () ), ids (&["F"]) );
  assert_eq! ( members_of ( n . hides . or_default () ), ids (&["y"]) );
  assert_eq! ( members_of (&n . contains), Vec::<ID>::new () );
  assert! ( ! line_of (&saved, "F") . contains ("noClaim"), "{}", saved );
  let f_shows : Vec<&str> = content_of (&saved, "F");
  assert! ( f_shows . len () == 1 && f_shows [0] . contains ("(gnid z)"),
            "F shows z alone:\n{}", saved );
  assert! ( under (&saved, "F") . iter () . any ( |line|
              line . contains ("hiddenInForkPartnerFolder") ),
            "F has a folder of what N hides in it:\n{}", saved );
  assert! ( under (&saved, "F") . iter () . any ( |line|
              line . contains ("(gnid y)") && line . contains ("folded") ),
            "which holds y:\n{}", saved );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  save_cleanly (&saved, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  Ok (( )) }

/// In a view of G, foreign, H, which forks G, stands editable as a
/// forker as such, showing h. Cutting h would make G fork H and hide
/// h, which only G's owner can record: the save is refused, naming G,
/// in the words a cut from a forkee under a foreign note uses.
async fn a_cut_under_a_foreign_note_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "G") ?;
  assert! ( ! line_of (&view, "H") . contains ("writeProtected")
            && content_of (&view, "H") . iter () . any ( |line| line . contains ("(gnid h)") ),
            "H is drawn editable under G, showing h:\n{}", view );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String =
    refusal_of (&without_subtree_of (&view, "h"), "G", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot change what G ('G') shows of H ('H'), its forker: G is foreign"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// Cutting y from F while the same save places F in N's content would
/// make N both fork and contain F, which the fork rules forbid: the
/// save is refused, and nothing is written.
async fn a_cut_that_would_make_n_fork_what_it_contains_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n_before_any_cut (config, tantivy) ?;
  let edited : String = format! (
    "{}** (skg node (gnid F) (skgRepos (nodeRepo theirs)) writeProtected) F\n",
    without_subtree_of (&view, "y") );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&edited, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("both forks and contains node F"), "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// After a plain cut has made F a mutual of N, a save that removes F's
/// claim there (detaching it) and cuts z from it ends N's fork of F,
/// as a detachment does, and hides z: the cut does not fork F again.
async fn a_cut_from_a_mutual_detached_in_the_same_save_only_hides (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n_before_any_cut (config, tantivy) ?;
  let mutual : String =
    save_cleanly (&without_subtree_of (&view, "y"), "N", config, tantivy) . await ?;
  let f_line : &str = line_of (&mutual, "F");
  let detached : String = without_subtree_of ( &mutual . replacen (
      f_line, &f_line . replacen ("(skgRepos (nodeRepo theirs))", "(skgRepos (nodeRepo theirs)) noClaim", 1 ), 1 ),
    "z" );
  assert! ( line_of (&detached, "F") . contains ("noClaim"), "{}", detached );
  save_cleanly (&detached, "N", config, tantivy) . await ?;
  let n : Graphnode = node (config, "N") ?;
  assert_eq! ( members_of ( n . forks . or_default () ), Vec::<ID>::new (),
               "N no longer forks F" );
  assert_eq! ( members_of ( n . hides . or_default () ), ids (&["y", "z"]),
               "N hides what it cut" );
  Ok (( )) }
