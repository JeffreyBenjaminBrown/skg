// cargo nextest run --test grouped_forks -E 'test(forks_meet::)'
//
// Forks of one note meet ('docs/forks.org', "Forks of one note
// meet"), end to end: both
// views and every save, each saved view saved again unchanged (the
// fixpoint). The fixture is the one of 'tests/forkers_drawn_editable.rs':
// N, owned, contains X1 to X4 and hides Z; R and T, owned, fork N;
// R contains X1, X2, X3; T contains X1, X3, Y, Z and hides X2. Beside
// the doc's picture, N forks K (which contains k1), and FF, foreign,
// forks N and contains X1 and f1; they stand in N's forks folder too.
// - R's view: R's forks folder is flat. N stands editable in it, a
//   forkee (its birth '(forks in 2)', herald bK), showing X4, what R
//   neither contains nor hides; then K, T and FF, the other mirrors
//   of N, indirect members, claiming nothing, born of nothing; T
//   editable, showing Y and Z, what R neither contains nor hides. No
//   member shows a forks folder. Saved unchanged, it writes nothing
//   and draws itself again.
// - Cutting X4 from N there hides X4 in R, and nothing else changes;
//   the redrawn view shows X4 in N's folder of what R hides in it.
// - In N's view, cutting Y from T and pasting it under N makes N fork
//   T and contain Y, hiding nothing new. The redrawn view shows T
//   among N's forkees, a mutual (both births, herald bKb), claiming,
//   editable, showing nothing; its folder of what it hides of N's
//   content goes, since T is no forker as such now; R and FF follow, and Y
//   stands in N's content. It is the fresh view of N, and saved
//   unchanged it writes nothing and draws itself again.

use std::error::Error;
use std::collections::BTreeMap;

use crate::view_and_save_helpers::{
  content_of, depth, ids, is_a_fixpoint, line_of, node, save_cleanly, skg_files,
  under, view_of, with_line_after_subtree_of, without_subtree_of};
use skg::reset_then_run_subtest;
use skg::test_utils::run_with_shared_test_stores;
use skg::types::config::SkgConfig;
use skg::types::misc::{TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;

const FIXTURES : &str = "tests/forkers_drawn_editable/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forks-meet",
    |s| Box::pin ( async move {
      reset_then_run_subtest! (s . reset (FIXTURES),
        rs_view_shows_n_and_its_forks (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        cutting_x4_from_n_in_rs_view_hides_it_in_r (&s . config, &mut s . tantivy) . await);
      reset_then_run_subtest! (s . reset (FIXTURES),
        moving_y_from_t_into_n_makes_n_fork_t (&s . config, &mut s . tantivy) . await);
      Ok (( )) } )) }

//
// Helpers
//

/// The gnids of the members of the forks folder standing directly
/// under the first line showing 'gnid', in order.
fn forks_folder_members_of (
  buf  : &str,
  gnid : &str,
) -> Vec<String> {
  let note_depth : usize = depth (line_of (buf, gnid));
  let below : Vec<&str> = under (buf, gnid);
  let at : usize = below . iter ()
    . position ( |line| depth (line) == note_depth + 1 && line . contains ("forksFolder") )
    . unwrap_or_else ( || panic! ( "{} has no forks folder in:\n{}", gnid, buf ));
  below [at + 1 ..] . iter ()
    . take_while ( |line| depth (line) > note_depth + 1 )
    . filter ( |line| depth (line) == note_depth + 2 )
    . filter_map ( |line| line . split ("(gnid ") . nth (1) )
    . map ( |rest| rest . split (')') . next () . unwrap () . to_string () )
    . collect () }

/// The members of the folder of kind 'folder_atom' standing directly
/// under the first line showing 'gnid', each line in full.
fn folder_members_under<'a> (
  buf         : &'a str,
  gnid        : &str,
  folder_atom : &str,
) -> Vec<&'a str> {
  let member_depth : usize = depth (line_of (buf, gnid));
  let below : Vec<&str> = under (buf, gnid);
  let Some (at) : Option<usize> = below . iter ()
    . position ( |line| depth (line) == member_depth + 1 && line . contains (folder_atom) )
    else { return Vec::new (); };
  below [at + 1 ..] . iter () . copied ()
    . take_while ( |line| depth (line) > member_depth + 1 )
    . filter ( |line| depth (line) == member_depth + 2 )
    . collect () }

//
// The sub-tests
//

/// R's view. R, the root, is editable. Its forks folder is flat: N,
/// editable, a forkee ('(birth (forks in 2))', herald bK), showing X4
/// alone (N's content less what R contains); then K, T and FF, the
/// other mirrors of N, each two forks from R, an indirect member
/// claiming nothing, with no birth fact and no fork toward R, by the
/// declaration order of their homes, then pid. T, editable, shows Y
/// and Z, what R neither contains nor hides, and neither folder of
/// hides: those are N's hides and T's hides of N's content, which do
/// not apply to R. No member shows a forks folder. Saved unchanged,
/// the view writes nothing and draws itself again.
async fn rs_view_shows_n_and_its_forks (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "R") ?;
  assert! ( ! line_of (&view, "R") . contains ("writeProtected"), "{}", view );
  assert_eq! ( forks_folder_members_of (&view, "R"), vec! ["N", "K", "T", "FF"],
               "{}", view );
  assert_eq! ( view . matches ("forksFolder") . count (), 1,
               "one forks folder, nothing nested:\n{}", view );
  let n_line : &str = line_of (&view, "N");
  assert! ( ! n_line . contains ("writeProtected") && ! n_line . contains ("noClaim")
            && n_line . contains ("(birth (forks in 2))"),
            "N stands in R's forks folder, an editable forkee:\n{}", view );
  let n_content : Vec<&str> = content_of (&view, "N");
  assert! ( n_content . len () == 1 && n_content [0] . contains ("(gnid X4)")
            && ! n_content [0] . contains ("writeProtected"),
            "N shows X4 alone, editable:\n{}", view );
  let t_line : &str = line_of (&view, "T");
  assert! ( ! t_line . contains ("writeProtected") && t_line . contains ("noClaim")
            && ! t_line . contains ("birth") && ! t_line . contains ("ancestors 2"),
            "T stands in R's forks folder, an editable indirect member:\n{}", view );
  let t_content : Vec<&str> = content_of (&view, "T");
  assert! ( t_content . len () == 2 && t_content [0] . contains ("(gnid Y)")
            && t_content [1] . contains ("(gnid Z)"),
            "T shows Y and Z:\n{}", view );
  assert! ( ! under (&view, "T") . iter () . any ( |line|
              line . contains ("hiddenInForkPartnerFolder")
              || line . contains ("hiddenByForkerFolder") ),
            "T has no folder of hides:\n{}", view );
  is_a_fixpoint (&view, "R", config, tantivy) . await ?;
  Ok (( )) }

/// In R's view, X4 cut from N (a forkee as such) and the view saved:
/// R hides X4, and no list changes. The redrawn view shows N showing
/// nothing, with X4, folded, in N's folder of what R hides in it.
/// Saved unchanged, it writes nothing and draws itself again.
async fn cutting_x4_from_n_in_rs_view_hides_it_in_r (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "R") ?;
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let cut : String = without_subtree_of (&view, "X4");
  assert_ne! ( cut, view );
  let saved : String = save_cleanly (&cut, "R", config, tantivy) . await ?;
  assert_eq! ( members_of ( node (config, "R") ? . hides . or_default () ),
               ids (&["X4"]), "R hides X4" );
  assert_eq! ( members_of (& node (config, "R") ? . contains),
               ids (&["X1", "X2", "X3"]), "R's content is as it was" );
  let changed : Vec<String> = skg_files (config) ? . into_iter ()
    . filter ( |(file, text)| before . get (file) != Some (text) )
    . map ( |(file, _)| file ) . collect ();
  assert_eq! ( changed, vec! ["main/R.skg"], "only R's file is written" );
  assert! ( content_of (&saved, "N") . is_empty (), "N shows nothing now:\n{}", saved );
  let hidden_in_n : Vec<&str> =
    folder_members_under (&saved, "N", "hiddenInForkPartnerFolder");
  assert! ( hidden_in_n . len () == 1 && hidden_in_n [0] . contains ("(gnid X4)")
            && hidden_in_n [0] . contains ("folded"),
            "N's folder of what R hides in it holds X4, folded:\n{}", saved );
  is_a_fixpoint (&saved, "R", config, tantivy) . await ?;
  Ok (( )) }

/// In N's view, Y cut from T (a forker as such) and pasted into N's
/// own content, after X4. N comes to fork T, after K, and contains Y;
/// it hides Z alone, as before, since it contains what it cut. T's
/// file is untouched. The redrawn view shows N's forks folder holding
/// K and T, its forkees, then R and FF, its forkers; T a mutual
/// (births '(forks in 2)' and '(forks out 2)', herald bKb), claiming,
/// editable, showing nothing, with Z in its folder of what N hides in
/// it and no folder of what it hides of N's content (X2 there is
/// drawn under a forker as such only); and Y last in N's content. It
/// is the fresh view of N, and saved unchanged it writes nothing and
/// draws itself again.
async fn moving_y_from_t_into_n_makes_n_fork_t (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  assert_eq! ( forks_folder_members_of (&view, "N"), vec! ["K", "R", "T", "FF"],
               "{}", view );
  assert! ( content_of (&view, "R") . is_empty (), "R adds nothing:\n{}", view );
  assert! ( ! folder_members_under (&view, "T", "hiddenByForkerFolder") . is_empty (),
            "before the cut, T, a forker, shows what it hides of N's content:\n{}", view );
  let t_before : String = skg_files (config) ? ["main/T.skg"] . clone ();
  let y_line : &str = line_of (&view, "Y");
  assert! ( under (&view, "Y") . is_empty (), "{}", view );
  let moved : String = with_line_after_subtree_of (
    &without_subtree_of (&view, "Y"), "X4",
    &format! ( "** {}", y_line . trim_start_matches ('*') . trim_start () ));
  let saved : String = save_cleanly (&moved, "N", config, tantivy) . await ?;
  let n : Graphnode = node (config, "N") ?;
  assert_eq! ( members_of (n . forks . or_default ()), ids (&["K", "T"]),
               "N forks T now" );
  assert_eq! ( members_of (&n . contains), ids (&["X1", "X2", "X3", "X4", "Y"]),
               "N contains Y" );
  assert_eq! ( members_of (n . hides . or_default ()), ids (&["Z"]),
               "N hides nothing new" );
  assert_eq! ( skg_files (config) ? ["main/T.skg"], t_before, "T's file is untouched" );
  assert_eq! ( forks_folder_members_of (&saved, "N"), vec! ["K", "T", "R", "FF"],
               "T stands among the forkees:\n{}", saved );
  let t_line : &str = line_of (&saved, "T");
  assert! ( ! t_line . contains ("noClaim") && ! t_line . contains ("writeProtected")
            && t_line . contains ("(birth (forks in 2) (forks out 2))"),
            "T is a mutual, claiming, editable:\n{}", saved );
  assert! ( content_of (&saved, "T") . is_empty (), "T shows nothing new:\n{}", saved );
  let hidden_in_t : Vec<&str> =
    folder_members_under (&saved, "T", "hiddenInForkPartnerFolder");
  assert! ( hidden_in_t . len () == 1 && hidden_in_t [0] . contains ("(gnid Z)"),
            "T's folder of what N hides in it holds Z:\n{}", saved );
  assert! ( folder_members_under (&saved, "T", "hiddenByForkerFolder") . is_empty ()
            && ! under (&saved, "T") . iter ()
                 . any ( |line| line . contains ("hiddenByForkerFolder") ),
            "T, now a forkee, has no folder of what it hides of N's content:\n{}", saved );
  let n_content : Vec<&str> = content_of (&saved, "N");
  assert! ( n_content . last () . is_some_and ( |line| line . contains ("(gnid Y)") ),
            "Y stands last in N's content:\n{}", saved );
  assert_eq! ( saved, view_of (config, tantivy, "N") ?,
               "the redrawn view is the fresh view of N" );
  is_a_fixpoint (&saved, "N", config, tantivy) . await ?;
  { // R's view now shows T, N's mutual, in R's flat forks folder, an
    // indirect member, showing what R neither contains nor hides of
    // its content. Nothing under T was cut: saved unchanged, it writes
    // nothing.
    let rs_view : String = view_of (config, tantivy, "R") ?;
    is_a_fixpoint (&rs_view, "R", config, tantivy) . await ?; }
  Ok (( )) }
