// cargo nextest run --test grouped_repos -E 'test(hide_inference_under_a_narrowing::)'
//
// A save infers a hide from a removal only by what the skgrepo
// restriction shows, and records a new hide no more publicly than
// the records that explain it ('docs/api-and-formats.org',
// "Restricted-skgrepo nodes"). Deleting a member's line from a
// subscriber hides the member from its subscriptions when a
// subscribee contains it; the subscriptions, the subscribees'
// content and the subscriber's hides are read by their visible
// records, and the hide's relRepo is at least as private as the
// subscription and the contains record that explain it. Re-adding a
// member drops no hide the skgrepo restriction keeps out.
//
// Every fixture has the skgrepos "public" and "private", both owned.

use crate::git_fixture::{
  Fixture, edit_line, list_in, open_content_view, save_under, skg_files, under_public};

use skg::save::DeleteFrom;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::skgrepo_sets::SkgrepoRestriction;

use std::error::Error;

const VIEW : &str = "view";

/// r contains y and subscribes to e, both by public records; e
/// contains c publicly and y only privately. Under "public" y's line
/// under r is deleted: e's content, as the narrowing shows it, is c
/// alone, so nothing explains a hide of y, and r's files name no
/// hide.
#[test]
fn a_member_only_a_restricted_record_explains_is_not_hidden (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- y\nsubscribesTo:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("y", "pid: y\ntitle: y\n") ],
    &[ ("e", "pid: e\ncontains:\n- y\n") ],
    |f, public| {
      let opened : String = open_content_view (f, "r", VIEW, Some (public)) ?;
      save_cleanly (f, &without_line (&opened, "** (skg node (gnid y)"), Some (public)) ?;
      assert_eq! ( list_in (f, "public/r.skg", "contains"), Vec::<String>::new (),
                   "y's line is gone from r" );
      assert_eq! ( list_in (f, "public/r.skg", "hidesFromSubs"), Vec::<String>::new (),
                   "r's public file hides nothing" );
      assert! ( ! skg_files (f) ? . contains_key ("private/r.skg"),
                "no private section of r appears" );
      Ok (( )) } ) }

/// The same deletion with no skgrepo restriction: e's private record
/// explains the hide, so the hide of y lands in r's private section,
/// not in its public file.
#[test]
fn a_hide_lands_no_more_publicly_than_the_contains_record_that_explains_it (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- y\nsubscribesTo:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("y", "pid: y\ntitle: y\n") ],
    &[ ("e", "pid: e\ncontains:\n- y\n") ],
    |f, _public| {
      let opened : String = open_content_view (f, "r", VIEW, None) ?;
      save_cleanly (f, &without_line (&opened, "** (skg node (gnid y)"), None) ?;
      assert_eq! ( list_in (f, "public/r.skg", "hidesFromSubs"), Vec::<String>::new (),
                   "r's public file names no hide of y" );
      assert_eq! ( list_in (f, "private/r.skg", "hidesFromSubs"), vec! ["y"],
                   "r's private section hides y" );
      Ok (( )) } ) }

/// r2 contains z by a public record, and its one subscription, to e2
/// (which contains z publicly), is recorded in r2's private section.
/// Under "public" z's line is deleted: the subscription is out of
/// sight, so no hide is inferred, and r2's private section is left
/// as it was.
#[test]
fn a_restricted_subscription_explains_no_hide (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r2", "pid: r2\ntitle: r2\ncontains:\n- z\n"),
       ("e2", "pid: e2\ntitle: e2\ncontains:\n- z\n"),
       ("z",  "pid: z\ntitle: z\n") ],
    &[ ("r2", "pid: r2\nsubscribesTo:\n- e2\n") ],
    |f, public| {
      let private_before : String =
        skg_files (f) ? . get ("private/r2.skg") . cloned () . unwrap ();
      let opened : String = open_content_view (f, "r2", VIEW, Some (public)) ?;
      save_cleanly (f, &without_line (&opened, "** (skg node (gnid z)"), Some (public)) ?;
      assert_eq! ( list_in (f, "public/r2.skg", "contains"), Vec::<String>::new (),
                   "z's line is gone from r2" );
      assert_eq! ( skg_files (f) ? . get ("private/r2.skg") . cloned (),
                   Some (private_before), "r2's private section is as it was" );
      Ok (( )) } ) }

/// r subscribes to e publicly and hides h by a record in its private
/// section; e contains h. Under "public" h is typed as r's content
/// and saved: r's public file contains h, and the restricted hide
/// stays.
#[test]
fn re_adding_a_member_drops_no_restricted_hide (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- c\nsubscribesTo:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- h\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("h", "pid: h\ntitle: h\n") ],
    &[ ("r", "pid: r\nhidesFromSubs:\n- h\n") ],
    |f, public| {
      let opened : String = open_content_view (f, "r", VIEW, Some (public)) ?;
      let with_h : String = edit_line ( &opened, "** (skg node (gnid c)", |line|
        format! ("{}\n** (skg node (gnid h)) h", line) );
      save_cleanly (f, &with_h, Some (public)) ?;
      assert_eq! ( list_in (f, "public/r.skg", "contains"), vec! ["c", "h"],
                   "r's public file contains h" );
      assert_eq! ( list_in (f, "private/r.skg", "hidesFromSubs"), vec! ["h"],
                   "r's private section still hides h" );
      Ok (( )) } ) }

//
// The operations
//

/// Save 'text' as VIEW under 'restriction'; it must save without
/// errors.
fn save_cleanly (
  f           : &mut Fixture,
  text        : &str,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<SaveResponse, Box<dyn Error>> {
  let response : SaveResponse =
    save_under (f, VIEW, text, restriction, true, DeleteFrom::NotChosen) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, text );
  Ok (response) }

/// 'view' without its one line starting 'prefix'.
fn without_line (
  view   : &str,
  prefix : &str,
) -> String {
  edit_line ( view, prefix, |_| String::new () )
    . lines () . filter ( |line| ! line . is_empty () )
    . map ( |line| format! ("{}\n", line) ) . collect () }
