// Kept children keep their order ('docs/diff-view.org', "What the
// refresh keeps"): the refresh never moves a child it keeps, and
// the containerward walk visits containers in ID order, so a first
// render is deterministic.
//
// The 'two_containers' fixture (one skgrepo, main): at HEAD b and d
// contain c, which contains x; since then a contains x too. So a is a
// root holding x, whose changed container folder holds a and c, and
// under c its containerward tree has two branches, b and d.

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture::{Fixture, save_cleanly};
use crate::pruned_change_view::common::{
  all_changes_buffer, all_changes_view, assert_lines_in_order,
  integrate_unshown_changes, open_all_changes_buffer, with_fixture};

use std::error::Error;

const C_IN_THE_FOLDER : &str =
  "**** (skg node (gnid c) (skgRepos (nodeRepo main)) writeProtected";
const B_UNDER_C : &str =
  "***** (skg node (gnid b) (skgRepos (nodeRepo main)) writeProtected";
const D_UNDER_C : &str =
  "***** (skg node (gnid d) (skgRepos (nodeRepo main)) writeProtected";

/// Save 'buffer' as the "all changes" buffer, and require that it
/// comes back as it went.
fn assert_saves_back_unchanged (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<(), Box<dyn Error>> {
  let saved : String = save_cleanly (f, &all_changes_view (), buffer) ? . saved_view;
  assert_eq! ( saved, buffer, "the buffer must save back unchanged" );
  Ok (( )) }

/// The branches b and d stand in ID order, and the buffer saves back
/// unchanged, as it stands and as "integrate unshown changes"
/// rerenders it, each several times (an unordered walk would put the
/// branches in either order at each rerender).
#[test]
fn a_two_container_role_tree_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("two_containers", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_lines_in_order ( &buffer, &[ C_IN_THE_FOLDER, B_UNDER_C, D_UNDER_C ] );
    for _ in 0 .. 5 {
      assert_saves_back_unchanged (f, &buffer) ?; }
    for _ in 0 .. 5 {
      let (shown, _) : (String, usize) = integrate_unshown_changes (f) ?;
      assert_eq! ( shown, buffer, "integrating changes nothing here" );
      assert_saves_back_unchanged (f, &shown) ?; }
    Ok (( )) }) }

/// The user puts d above b; a save keeps that order, and the result
/// saves back unchanged.
#[test]
fn a_role_tree_reordered_by_the_user_keeps_that_order_through_a_save (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("two_containers", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let lines : Vec<&str> = buffer . lines () . collect ();
    let position = |prefix : &str| -> usize {
      lines . iter () . position ( |line| without_viewnode_ids (line) . starts_with (prefix) )
        . unwrap_or_else ( || panic! ( "no line starts {:?}:\n{}", prefix, buffer )) };
    let (b, d) : (usize, usize) = ( position (B_UNDER_C), position (D_UNDER_C) );
    let mut swapped_lines : Vec<&str> = lines . clone ();
    swapped_lines . swap (b, d);
    let swapped : String = swapped_lines . join ("\n") + "\n";
    let saved : String = save_cleanly (f, &all_changes_view (), &swapped) ? . saved_view;
    assert_lines_in_order ( &saved, &[ C_IN_THE_FOLDER, D_UNDER_C, B_UNDER_C ] );
    assert_eq! ( saved, swapped, "the reordered buffer must save back unchanged" );
    assert_saves_back_unchanged (f, &saved) ?;
    Ok (( )) }) }

/// The first render of a fixture's "all changes" buffer, ten times
/// over, is the same text each time.
fn assert_first_render_is_deterministic (
  case : &str,
) -> Result<(), Box<dyn Error>> {
  with_fixture (case, &["main"], |f| {
    let first : String = all_changes_buffer (f) ? . ok_or ("the buffer is empty") ?;
    for _ in 1 .. 10 {
      assert_eq! ( all_changes_buffer (f) ? . ok_or ("the buffer is empty") ?,
                   first,
                   "every first render of {} must be the same", case ); }
    Ok (( )) }) }

#[test]
fn the_first_render_of_removed_partners_is_the_same_every_time (
) -> Result<(), Box<dyn Error>> {
  assert_first_render_is_deterministic ("removed_partners") }

#[test]
fn the_first_render_of_two_containers_is_the_same_every_time (
) -> Result<(), Box<dyn Error>> {
  assert_first_render_is_deterministic ("two_containers") }
