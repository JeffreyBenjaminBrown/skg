// cargo nextest run --test grouped_repos -E 'test(fork_merge_delete_under_a_narrowing::)'
//
// Writes built from the graph rather than the buffer leave alone
// what the skgrepo restriction keeps out ('docs/api-and-formats.org',
// "Restricted-skgrepo nodes" and "Save buffer"):
// - a delete whose propagation would strip records the skgrepo
//   restriction keeps out stops for the user's choice, the
//   delete-from confirmation: cancel, the visible records only, or
//   every owned record.
//
// Every fixture has the skgrepos "public" and "private", both owned.

use crate::git_fixture::{
  Fixture, edit_line, list_in, open_content_view, save_under, skg_files, under_public};

use skg::save::DeleteFrom;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::skgrepo_sets::SkgrepoRestriction;

use std::collections::BTreeMap;
use std::error::Error;

const VIEW : &str = "view";

//
// The delete
//

/// x (public, no private section) is deleted under "public". v
/// contains x by a public record; w (public) contains x by a private
/// record; w2 (home "private") contains x. Two records out of sight
/// name x, so the save stops with the delete-from confirmation and
/// writes nothing.
#[test]
fn a_delete_whose_propagation_reaches_restricted_records_asks_first (
) -> Result<(), Box<dyn Error>> {
  delete_fixture ( |f, public| {
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let requested : String = with_x_deleted (f, public) ?;
    let response : SaveResponse =
      save_under (f, VIEW, &requested, Some (public), true,
                  DeleteFrom::NotChosen) ?;
    assert_eq! ( response . delete_from_confirmation, Some (2),
                 "the confirmation names the two restricted records" );
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
    Ok (( )) } ) }

/// Delete from the visible records only: x goes, v's public record of
/// it goes, and the two restricted records stay as they were.
#[test]
fn a_delete_from_the_visible_records_leaves_the_restricted_ones (
) -> Result<(), Box<dyn Error>> {
  delete_fixture ( |f, public| {
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let requested : String = with_x_deleted (f, public) ?;
    let response : SaveResponse =
      save_under (f, VIEW, &requested, Some (public), true,
                  DeleteFrom::VisibleRecords) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert_eq! ( response . delete_from_confirmation, None );
    let files : BTreeMap<String, String> = skg_files (f) ?;
    assert! ( ! files . contains_key ("public/x.skg"), "x is deleted" );
    assert_eq! ( list_in (f, "public/v.skg", "contains"), Vec::<String>::new (),
                 "v's public record of x goes" );
    for restricted in ["private/w.skg", "private/w2.skg", "public/w.skg"] {
      assert_eq! ( files . get (restricted), files_before . get (restricted),
                   "{} is as it was", restricted ); }
    Ok (( )) } ) }

/// Delete from all owned records: every record of x goes, the
/// restricted ones too, as with no skgrepo restriction.
#[test]
fn a_delete_from_all_owned_records_strips_the_restricted_ones_too (
) -> Result<(), Box<dyn Error>> {
  delete_fixture ( |f, public| {
    let requested : String = with_x_deleted (f, public) ?;
    let response : SaveResponse =
      save_under (f, VIEW, &requested, Some (public), true,
                  DeleteFrom::AllOwnedRecords) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    let files : BTreeMap<String, String> = skg_files (f) ?;
    assert! ( ! files . contains_key ("public/x.skg"), "x is deleted" );
    assert_eq! ( list_in (f, "public/v.skg", "contains"), Vec::<String>::new () );
    assert_eq! ( list_in (f, "private/w.skg", "contains"), Vec::<String>::new (),
                 "w's private record of x goes" );
    assert_eq! ( list_in (f, "private/w2.skg", "contains"), Vec::<String>::new (),
                 "w2's record of x goes" );
    assert! ( files . contains_key ("private/w2.skg"), "w2 stays" );
    Ok (( )) } ) }

/// With no skgrepo restriction the delete asks nothing and strips
/// every record of x.
#[test]
fn a_delete_with_no_restriction_asks_nothing (
) -> Result<(), Box<dyn Error>> {
  delete_fixture ( |f, _public| {
    let opened : String = open_content_view (f, "x", VIEW, None) ?;
    let response : SaveResponse =
      save_under (f, VIEW, &with_delete_requested (&opened), None, true,
                  DeleteFrom::NotChosen) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert_eq! ( response . delete_from_confirmation, None );
    assert_eq! ( list_in (f, "private/w2.skg", "contains"), Vec::<String>::new () );
    Ok (( )) } ) }

//
// The fixtures and the operations
//

/// The delete's fixture: x, v, w (public) and w2 (home "private"),
/// each of v, w and w2 containing x, w by a private record.
fn delete_fixture (
  body : impl FnOnce (&mut Fixture, &SkgrepoRestriction) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("x", "pid: x\ntitle: x\n"),
       ("v", "pid: v\ntitle: v\ncontains:\n- x\n"),
       ("w", "pid: w\ntitle: w\n") ],
    &[ ("w",  "pid: w\ncontains:\n- x\n"),
       ("w2", "pid: w2\ntitle: w2 secret\ncontains:\n- x\n") ],
    body ) }

/// The view of x under 'restriction', its root carrying a delete
/// request.
fn with_x_deleted (
  f           : &mut Fixture,
  restriction : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  let opened : String = open_content_view (f, "x", VIEW, Some (restriction)) ?;
  Ok ( with_delete_requested (&opened) ) }

/// 'view' with a delete request on its root, x.
fn with_delete_requested (
  view : &str,
) -> String {
  edit_line ( view, "* (skg node (gnid x)", |line|
    line . replacen ( "(skg ", "(skg (editRequest delete) ", 1 )) }
