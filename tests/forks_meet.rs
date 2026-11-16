// cargo nextest run --test grouped_forks -E 'test(forks_meet::)'
//
// Forks of one note meet ('TODO/every-mirror/2_plan.org', behavior
// 5; 'docs/forks.org', "Forks of one note meet"), end to end: both
// views and every save, each saved view saved again unchanged (the
// fixpoint). The fixture is the one of 'tests/forkers_drawn_editable.rs':
// N, owned, contains X1 to X4 and hides Z; R and T, owned, fork N;
// R contains X1, X2, X3; T contains X1, X3, Y, Z and hides X2. Beside
// the plan's picture, N forks K (which contains k1), and FF, foreign,
// forks N and contains X1 and f1; they stand in N's forks folder too.
// - R's view: N stands editable in R's forks folder, a forkee (its
//   birth '(forks in 2)', herald bK), showing X4, what R neither
//   contains nor hides; in N's forks folder R is a repeat, showing
//   nothing, and T a forker (birth '(forks out 2)', herald Kb),
//   editable, showing Y, with Z in its folder of what N hides in it.
//   Saved unchanged, it writes nothing and draws itself again.
// - Cutting X4 from N there hides X4 in R, and nothing else changes;
//   the redrawn view shows X4 in N's folder of what R hides in it.
// - In N's view, cutting Y from T and pasting it under N makes N fork
//   T and contain Y, hiding nothing new. The redrawn view shows T
//   among N's forkees, a mutual (both births, herald bKb), claiming,
//   editable, showing nothing; its folder of what it hides of N's
//   content goes, since T is no forker as such now; R follows, and Y
//   stands in N's content. It is the fresh view of N, and saved
//   unchanged it writes nothing and draws itself again.

use std::error::Error;
use std::collections::BTreeMap;

use crate::view_and_save_helpers::{
  content_of, depth, ids, line_of, node, save_cleanly, skg_files, under, view_of,
  with_line_after_subtree_of, without_subtree_of};
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{SkgConfig, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;

const FIXTURES : &str = "tests/forkers_drawn_editable/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forks-meet",
    |s| Box::pin ( async move {
      s . reset ("rs_view_shows_n_and_its_forks", FIXTURES) ?;
      rs_view_shows_n_and_its_forks (&s . config, &mut s . tantivy) . await ?;
      s . reset ("cutting_x4_from_n_in_rs_view_hides_it_in_r", FIXTURES) ?;
      cutting_x4_from_n_in_rs_view_hides_it_in_r (&s . config, &mut s . tantivy) . await ?;
      s . reset ("moving_y_from_t_into_n_makes_n_fork_t", FIXTURES) ?;
      moving_y_from_t_into_n_makes_n_fork_t (&s . config, &mut s . tantivy) . await ?;
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

/// The line showing 'gnid' as the 'nth' (from 0) occurrence in 'buf'.
fn nth_line_of<'a> (
  buf  : &'a str,
  gnid : &str,
  nth  : usize,
) -> &'a str {
  buf . lines ()
    . filter ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . nth (nth)
    . unwrap_or_else ( || panic! ( "no occurrence {} of {} in:\n{}", nth, gnid, buf )) }

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

//
// The sub-tests
//

/// R's view. R, the root, is editable. Its forks folder holds N,
/// editable, a forkee ('(birth (forks in 2))', herald bK), showing X4
/// alone (N's content less what R contains). N's forks folder holds
/// K, its forkee, then its forkers FF, R and T: R a repeat,
/// write-protected, showing nothing; T editable, claiming nothing
/// ('(birth (forks out 2))', herald Kb), showing Y, with Z (which N
/// hides) in its folder of what N hides in it. Saved unchanged, the
/// view writes nothing and draws itself again.
async fn rs_view_shows_n_and_its_forks (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "R") ?;
  assert! ( ! line_of (&view, "R") . contains ("writeProtected"), "{}", view );
  assert_eq! ( forks_folder_members_of (&view, "R"), vec! ["N"], "{}", view );
  let n_line : &str = line_of (&view, "N");
  assert! ( ! n_line . contains ("writeProtected") && ! n_line . contains ("noClaim")
            && n_line . contains ("(birth (forks in 2))"),
            "N stands in R's forks folder, an editable forkee:\n{}", view );
  let n_content : Vec<&str> = content_of (&view, "N");
  assert! ( n_content . len () == 1 && n_content [0] . contains ("(gnid X4)")
            && ! n_content [0] . contains ("writeProtected"),
            "N shows X4 alone, editable:\n{}", view );
  assert_eq! ( forks_folder_members_of (&view, "N"), vec! ["K", "FF", "R", "T"],
               "{}", view );
  let repeat : &str = nth_line_of (&view, "R", 1);
  assert! ( repeat . contains ("writeProtected") && repeat . contains ("noClaim")
            && repeat . contains ("(birth (forks out 2))"),
            "R stands in N's forks folder, a write-protected forker:\n{}", view );
  let after_repeat : &str = view . lines ()
    . skip_while ( |line| *line != repeat ) . nth (1) . unwrap ();
  assert! ( depth (after_repeat) <= depth (repeat), "the repeat shows nothing:\n{}", view );
  let t_line : &str = line_of (&view, "T");
  assert! ( ! t_line . contains ("writeProtected") && t_line . contains ("noClaim")
            && t_line . contains ("(birth (forks out 2))"),
            "T stands in N's forks folder, an editable forker:\n{}", view );
  let t_content : Vec<&str> = content_of (&view, "T");
  assert! ( t_content . len () == 1 && t_content [0] . contains ("(gnid Y)"),
            "T shows Y alone:\n{}", view );
  let hidden_in_t : Vec<&str> =
    folder_members_under (&view, "T", "hiddenInForkPartnerFolder");
  assert! ( hidden_in_t . len () == 1 && hidden_in_t [0] . contains ("(gnid Z)")
            && hidden_in_t [0] . contains ("folded"),
            "T's folder of what N hides in it holds Z, folded:\n{}", view );
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
/// K and T, its forkees, then FF and R, its forkers; T a mutual
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
  assert_eq! ( forks_folder_members_of (&view, "N"), vec! ["K", "FF", "R", "T"],
               "{}", view );
  assert! ( content_of (&view, "R") . is_empty (), "R adds nothing:\n{}", view );
  assert! ( ! folder_members_under (&view, "T", "hiddenByForkPartnerFolder") . is_empty (),
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
  assert_eq! ( forks_folder_members_of (&saved, "N"), vec! ["K", "T", "FF", "R"],
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
  assert! ( folder_members_under (&saved, "T", "hiddenByForkPartnerFolder") . is_empty ()
            && ! under (&saved, "T") . iter ()
                 . any ( |line| line . contains ("hiddenByForkPartnerFolder") ),
            "T, now a forkee, has no folder of what it hides of N's content:\n{}", saved );
  let n_content : Vec<&str> = content_of (&saved, "N");
  assert! ( n_content . last () . is_some_and ( |line| line . contains ("(gnid Y)") ),
            "Y stands last in N's content:\n{}", saved );
  assert_eq! ( saved, view_of (config, tantivy, "N") ?,
               "the redrawn view is the fresh view of N" );
  is_a_fixpoint (&saved, "N", config, tantivy) . await ?;
  { // R's view now shows T, N's mutual, under N, and under T what it
    // shows of its own forkee N. N's contains there is the one on
    // disk, so nothing under T was cut: saved unchanged, it writes
    // nothing.
    let rs_view : String = view_of (config, tantivy, "R") ?;
    is_a_fixpoint (&rs_view, "R", config, tantivy) . await ?; }
  Ok (( )) }
