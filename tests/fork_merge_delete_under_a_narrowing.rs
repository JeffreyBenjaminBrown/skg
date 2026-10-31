// cargo nextest run --test grouped_repos -E 'test(fork_merge_delete_under_a_narrowing::)'
//
// Three writes built from the graph rather than the buffer leave alone
// what the skgrepo restriction keeps out ('docs/api-and-formats.org',
// "Restricted-skgrepo nodes" and "Save buffer"):
// - a fork copies into its clone only the records the skgrepo
//   restriction shows, each no more public than the record it copies;
// - a delete whose propagation would strip records the skgrepo
//   restriction keeps out stops for the user's choice, the
//   delete-from confirmation: cancel, the visible records only, or
//   every owned record;
// - a nodeMerge is refused when either graphnode has a section the
//   skgrepo restriction keeps out.
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
// The fork
//

/// n contains a by a public record and p by a private one. Under
/// "public" n is forked, and the fork approved: the clone, in
/// "public", contains a alone, and no section of it names p.
#[test]
fn a_fork_under_a_narrowing_copies_only_the_visible_records (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("n", "pid: n\ntitle: n\ncontains:\n- a\n"),
       ("a", "pid: a\ntitle: a\n"),
       ("p", "pid: p\ntitle: p\n") ],
    &[ ("n", "pid: n\ncontains:\n- p\n") ],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let opened : String = open_content_view (f, "n", VIEW, public) ?;
      save_cleanly (f, &with_fork_requested (&opened), public) ?;
      let clone : String = the_clone (f, &files_before) ?;
      assert_eq! ( list_in (f, &format! ("public/{}.skg", clone), "contains"), vec! ["a"],
                   "the clone contains a alone" );
      assert_eq! ( list_in (f, &format! ("public/{}.skg", clone), "subscribesTo"), vec! ["n"] );
      assert! ( ! skg_files (f) ? . contains_key (&format! ("private/{}.skg", clone)),
                "the clone has no private section" );
      assert_eq! ( skg_files (f) ? . get ("private/n.skg"), files_before . get ("private/n.skg"),
                   "n's private section is as it was" );
      Ok (( )) } ) }

/// The same fork with no skgrepo restriction: the clone copies both
/// records, and p's stays private, in the clone's private section.
#[test]
fn a_fork_keeps_a_private_record_private (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("n", "pid: n\ntitle: n\ncontains:\n- a\n"),
       ("a", "pid: a\ntitle: a\n"),
       ("p", "pid: p\ntitle: p\n") ],
    &[ ("n", "pid: n\ncontains:\n- p\n") ],
    |f, _public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let unrestricted : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      let opened : String = open_content_view (f, "n", VIEW, &unrestricted) ?;
      save_cleanly (f, &with_fork_requested (&opened), &unrestricted) ?;
      let clone : String = the_clone (f, &files_before) ?;
      assert_eq! ( list_in (f, &format! ("public/{}.skg", clone), "contains"), vec! ["a"],
                   "the clone's public file contains a alone" );
      assert_eq! ( list_in (f, &format! ("private/{}.skg", clone), "contains"), vec! ["p"],
                   "the clone's private section contains p" );
      Ok (( )) } ) }

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
      save_under (f, VIEW, &requested, public, true,
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
      save_under (f, VIEW, &requested, public, true,
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
      save_under (f, VIEW, &requested, public, true,
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
    let unrestricted : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config);
    let opened : String = open_content_view (f, "x", VIEW, &unrestricted) ?;
    let response : SaveResponse =
      save_under (f, VIEW, &with_delete_requested (&opened), &unrestricted, true,
                  DeleteFrom::NotChosen) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert_eq! ( response . delete_from_confirmation, None );
    assert_eq! ( list_in (f, "private/w2.skg", "contains"), Vec::<String>::new () );
    Ok (( )) } ) }

//
// The nodeMerge
//

/// a (public; its private section hides h) acquires b (public;
/// contains h publicly and q by a private record). Under "public"
/// both homes are visible, but both graphnodes have private
/// sections: the nodeMerge is refused, and no file changes.
#[test]
fn a_nodeMerge_of_graphnodes_with_restricted_sections_is_refused (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("a", "pid: a\ntitle: a\n"),
       ("b", "pid: b\ntitle: b\ncontains:\n- h\n"),
       ("h", "pid: h\ntitle: h\n"),
       ("q", "pid: q\ntitle: q\n") ],
    &[ ("a", "pid: a\nhidesFromSubs:\n- h\n"),
       ("b", "pid: b\ncontains:\n- q\n") ],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let opened : String = open_content_view (f, "a", VIEW, public) ?;
      let requested : String = edit_line ( &opened, "* (skg node (gnid a)", |line|
        line . replacen ( "(skg ", "(skg (editRequest (merge b)) ", 1 ));
      let refusal : String = match save_under (
        f, VIEW, &requested, public, true, DeleteFrom::NotChosen ) {
        Err (e) => e . to_string (),
        Ok (response) => panic! ( "the nodeMerge must be refused; errors {:?}",
                                  response . errors ), };
      assert! ( refusal . contains ("Cannot merge 'b' into 'a'")
                && refusal . contains ("telescope sections in restricted repos"),
                "the refusal says why: {}", refusal );
      assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
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
  let opened : String = open_content_view (f, "x", VIEW, restriction) ?;
  Ok ( with_delete_requested (&opened) ) }

/// 'view' with a delete request on its root, x.
fn with_delete_requested (
  view : &str,
) -> String {
  edit_line ( view, "* (skg node (gnid x)", |line|
    line . replacen ( "(skg ", "(skg (editRequest delete) ", 1 )) }

/// 'view' with a fork request on its root, n.
fn with_fork_requested (
  view : &str,
) -> String {
  edit_line ( view, "* (skg node (gnid n)", |line|
    line . replacen ( "(skg ", "(skg (viewRequests fork) ", 1 )) }

/// The pid of the one graphnode whose public file is new since
/// 'files_before': the clone.
fn the_clone (
  f            : &Fixture,
  files_before : &BTreeMap<String, String>,
) -> Result<String, Box<dyn Error>> {
  let new_public_files : Vec<String> = skg_files (f) ? . into_keys ()
    . filter ( |path| path . starts_with ("public/") && ! files_before . contains_key (path) )
    . collect ();
  assert_eq! ( new_public_files . len (), 1,
               "one new public file, the clone's: {:?}", new_public_files );
  Ok ( new_public_files [0] . trim_start_matches ("public/")
       . trim_end_matches (".skg") . to_string () ) }

/// Save 'text' as VIEW under 'restriction', its forks approved; it
/// must save without errors.
fn save_cleanly (
  f           : &mut Fixture,
  text        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<SaveResponse, Box<dyn Error>> {
  let response : SaveResponse =
    save_under (f, VIEW, text, restriction, true, DeleteFrom::NotChosen) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, text );
  Ok (response) }
