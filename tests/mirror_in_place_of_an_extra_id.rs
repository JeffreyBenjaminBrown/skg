// cargo nextest run --test grouped_forks -E 'test(mirror_in_place_of_an_extra_id::)'
//
// A mirror shown '(inPlaceOf X)', where X has since become an extra ID
// of K (a nodeMerge folded X into K, made from another view): the
// refresh reads the original by its pid, as its parent's goal names it
// ('member_of_child' in 'server/refresh/goal.rs').
// The fixture: "main" and "base", both owned. P (main) contains X; R
// (main) forks X and contains W; X is in base. A view of P, drawn from
// main, shows R in X's place. Each sub-test draws a view, then folds
// X into K on disk (K, in base, titled "X", has the extra ID X):
// - saved unchanged, R keeps its place, viewnode ID, fold state and
//   mark, and P still contains X;
// - a root showing R in X's place, saved under the root homes, shows
//   K itself, unmarked;
// - with X containing W too, a containers folder typed under W (under
//   R) omits K, which R stands in place of, as it omits X before.

use std::error::Error;
use std::path::PathBuf;

use crate::view_and_save_helpers::{contains_of, depth, ids, line_of, save};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  multi_root_view_from_perspective, run_with_shared_test_stores, single_root_view};
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, SkgrepoName, TantivyIndex};
use skg::types::views_state::Perspective;

const FIXTURES : &str = "tests/mirror_in_place_of_an_extra_id/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-mirror-in-place-of-an-extra-id",
    |s| Box::pin ( async move {
      s . reset ("a_mirror_in_place_of_an_extra_id_keeps_its_headline", FIXTURES) ?;
      a_mirror_in_place_of_an_extra_id_keeps_its_headline (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_root_in_place_of_an_extra_id_of_what_it_shows_is_unmarked", FIXTURES) ?;
      a_root_in_place_of_an_extra_id_of_what_it_shows_is_unmarked (&s . config, &mut s . tantivy) . await ?;
      s . reset_with_fixture_prep (
        "a_containers_folder_omits_the_original_a_mirror_above_stands_for", FIXTURES,
        |root| { std::fs::write ( root . join ("owned/base/X.skg"),
                                  "pid: X\ntitle: X\ncontains:\n- W\n" ) ?;
                 Ok (( )) } ) ?;
      a_containers_folder_omits_the_original_a_mirror_above_stands_for (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// Fold X into K on disk, as a nodeMerge made from another view would:
/// K has X's title and lists, and the extra ID X.
fn fold_x_into_k (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let base : &PathBuf =
    &config . skgrepos . get (&SkgrepoName::from ("base")) . unwrap () . path;
  let x : String = std::fs::read_to_string ( base . join ("X.skg") ) ?;
  std::fs::remove_file ( base . join ("X.skg") ) ?;
  std::fs::write ( base . join ("K.skg"),
                   format! ( "{}extra_ids:\n- X\n", x . replacen ("pid: X", "pid: K", 1) )) ?;
  Ok (( )) }

//
// The sub-tests
//

/// P's view shows R in X's place, folded by the user. After X is
/// folded into K, saving the view unchanged keeps R's headline as it
/// was: its viewnode ID, its fold and its '(inPlaceOf X)'. P's list
/// still names X.
async fn a_mirror_in_place_of_an_extra_id_keeps_its_headline (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("P")) ?;
  let r_line : &str = line_of (&view, "R");
  assert! ( r_line . contains ("(inPlaceOf X)"), "R stands in X's place:\n{}", view );
  let folded_r_line : String = r_line . replacen (" node (gnid R)", " folded node (gnid R)", 1);
  let buffer : String = view . replacen (r_line, &folded_r_line, 1);
  fold_x_into_k (config) ?;
  let saved : String = save (&buffer, "P", config, tantivy) . await ?
    . saved_view;
  assert_eq! ( line_of (&saved, "R"), folded_r_line, "R's headline is kept:\n{}", saved );
  assert_eq! ( contains_of (config, "P") ?, ids (&["X"]) );
  Ok (( )) }

/// A view of X drawn from main shows R as its root, in X's place.
/// After X is folded into K, the view saved under the root homes shows
/// K itself, which is what the root stands for: unmarked.
async fn a_root_in_place_of_an_extra_id_of_what_it_shows_is_unmarked (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (view, _) = multi_root_view_from_perspective (
    config, &[ ID::from ("X") ], &SkgrepoRestriction::unrestricted (config),
    Perspective::Skgrepo ( SkgrepoName::from ("main") )) ?;
  let root_line : &str = view . lines () . next () . unwrap ();
  assert! ( root_line . contains ("(gnid R)") && root_line . contains ("(inPlaceOf X)"),
            "the root shows R in X's place:\n{}", view );
  fold_x_into_k (config) ?;
  let saved : String = save (&view, "X", config, tantivy) . await ?
    . saved_view;
  let root_line : &str = saved . lines () . next () . unwrap ();
  assert! ( root_line . contains ("(gnid K)") && ! root_line . contains ("inPlaceOf"),
            "the root shows K, unmarked:\n{}", saved );
  Ok (( )) }

/// X contains W, as R does. In P's view R stands in X's place, so a
/// containers folder typed under W (R's child) omits X, a
/// view-ancestor, and holding nothing, is not drawn. After X is folded
/// into K, it omits K likewise.
async fn a_containers_folder_omits_the_original_a_mirror_above_stands_for (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("P")) ?;
  let w_line : &str = view . lines () // R's own child, after X's in R's forks folder
    . filter ( |line| line . contains ("(gnid W)") ) . last () . unwrap ();
  let typed : String = format! ( "{}\n{} (skg containerFolder)\n",
                                 w_line, "*" . repeat ( depth (w_line) + 1 ));
  let buffer : String = view . replacen ( &format! ("{}\n", w_line), &typed, 1 );
  let before_the_fold : String = save (&buffer, "P", config, tantivy) . await ? . saved_view;
  assert! ( ! before_the_fold . contains ("containerFolder"),
            "W's containers folder omits X, and is not drawn:\n{}", before_the_fold );
  fold_x_into_k (config) ?;
  let saved : String = save (&buffer, "P", config, tantivy) . await ? . saved_view;
  assert! ( ! saved . contains ("containerFolder"),
            "W's containers folder omits K, and is not drawn:\n{}", saved );
  Ok (( )) }
