// cargo nextest run --test grouped_repos -E 'test(hide_inference_under_a_narrowing::)'
//
// A save infers a hide from a removal only by what the skgrepo
// restriction shows, and records a new hide no more publicly than
// the records that explain it ('docs/api-and-formats.org',
// "Restricted-skgrepo nodes"). Deleting a member's line from a
// forker hides the member from what it forks when a
// forkee contains it; the 'forks' relationships, the forkees'
// content and the forker's hides are read by their visible
// records, and the hide's relRepo is at least as private as the
// 'forks' relationship and the contains record that explain it. Re-adding a
// member drops no hide the skgrepo restriction keeps out.
//
// A forker of the hider explains a hide as a forkee does
// ('docs/glossary.org', "hides"): deleting a member's line
// that a forker still contains hides it, by the same visible records
// and at the same floor, the 'forks' relationship read in the
// forker's file.
//
// Every fixture has the skgrepos "public" and "private", both owned.

use crate::git_fixture::{
  Fixture, edit_line, list_in, open_content_view, save_under, skg_files, under_public};

use skg::save::DeleteFrom;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::skgrepo_sets::SkgrepoRestriction;

use std::error::Error;

const VIEW : &str = "view";

/// r contains y and forks e, both by public records; e
/// contains c publicly and y only privately. Under "public" y's line
/// under r is deleted: e's content, as the narrowing shows it, is c
/// alone, so nothing explains a hide of y, and r's files name no
/// hide.
#[test]
fn a_member_only_a_restricted_record_explains_is_not_hidden (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- y\nforks:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("y", "pid: y\ntitle: y\n") ],
    &[ ("e", "pid: e\ncontains:\n- y\n") ],
    |f, public| {
      let opened : String = open_content_view (f, "r", VIEW, public) ?;
      save_cleanly (f, &without_line (&opened, "** (skg node (gnid y)"), public) ?;
      assert_eq! ( list_in (f, "public/r.skg", "contains"), Vec::<String>::new (),
                   "y's line is gone from r" );
      assert_eq! ( list_in (f, "public/r.skg", "hides"), Vec::<String>::new (),
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
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- y\nforks:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("y", "pid: y\ntitle: y\n") ],
    &[ ("e", "pid: e\ncontains:\n- y\n") ],
    |f, _public| {
      let unrestricted : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      let opened : String = open_content_view (f, "r", VIEW, &unrestricted) ?;
      save_cleanly (f, &without_line (&opened, "** (skg node (gnid y)"), &unrestricted) ?;
      assert_eq! ( list_in (f, "public/r.skg", "hides"), Vec::<String>::new (),
                   "r's public file names no hide of y" );
      assert_eq! ( list_in (f, "private/r.skg", "hides"), vec! ["y"],
                   "r's private section hides y" );
      Ok (( )) } ) }

/// r2 contains z by a public record, and its one 'forks' relationship, to e2
/// (which contains z publicly), is recorded in r2's private section.
/// Under "public" z's line is deleted: the relationship is out of
/// sight, so no hide is inferred, and r2's private section is left
/// as it was.
#[test]
fn a_restricted_fork_explains_no_hide (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r2", "pid: r2\ntitle: r2\ncontains:\n- z\n"),
       ("e2", "pid: e2\ntitle: e2\ncontains:\n- z\n"),
       ("z",  "pid: z\ntitle: z\n") ],
    &[ ("r2", "pid: r2\nforks:\n- e2\n") ],
    |f, public| {
      let private_before : String =
        skg_files (f) ? . get ("private/r2.skg") . cloned () . unwrap ();
      let opened : String = open_content_view (f, "r2", VIEW, public) ?;
      save_cleanly (f, &without_line (&opened, "** (skg node (gnid z)"), public) ?;
      assert_eq! ( list_in (f, "public/r2.skg", "contains"), Vec::<String>::new (),
                   "z's line is gone from r2" );
      assert_eq! ( skg_files (f) ? . get ("private/r2.skg") . cloned (),
                   Some (private_before), "r2's private section is as it was" );
      Ok (( )) } ) }

/// r forks e publicly and hides h by a record in its private
/// section; e contains h. Under "public" the view draws e, a forkee
/// as such, editable, showing h (the hide that leaves h out is
/// restricted); h is moved from there to r's content and saved: r's
/// public file contains h, and the restricted hide stays.
#[test]
fn re_adding_a_member_drops_no_restricted_hide (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- c\nforks:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- h\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("h", "pid: h\ntitle: h\n") ],
    &[ ("r", "pid: r\nhides:\n- h\n") ],
    |f, public| {
      let opened : String = open_content_view (f, "r", VIEW, public) ?;
      let with_h : String = edit_line (
        &without_line (&opened, "**** (skg node (gnid h)"),
        "** (skg node (gnid c)", |line|
        format! ("{}\n** (skg node (gnid h)) h", line) );
      save_cleanly (f, &with_h, public) ?;
      assert_eq! ( list_in (f, "public/r.skg", "contains"), vec! ["c", "h"],
                   "r's public file contains h" );
      assert_eq! ( list_in (f, "private/r.skg", "hides"), vec! ["h"],
                   "r's private section still hides h" );
      Ok (( )) } ) }

/// r contains y publicly; f forks r publicly, and contains y only
/// by a record in its private section. With no skgrepo restriction,
/// y's line under r is deleted: f still contains y, so y is hidden in
/// r (it would come back under f), and the hide lands in r's private
/// section, no more publicly than f's record that explains it.
#[test]
fn a_hide_a_forker_explains_lands_no_more_publicly_than_its_record (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- y\n- c\n"),
       ("f", "pid: f\ntitle: f\nforks:\n- r\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("y", "pid: y\ntitle: y\n") ],
    &[ ("f", "pid: f\ncontains:\n- y\n") ],
    |f, _public| {
      let unrestricted : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      let opened : String = open_content_view (f, "r", VIEW, &unrestricted) ?;
      save_cleanly (f, &without_line (&opened, "** (skg node (gnid y)"), &unrestricted) ?;
      assert_eq! ( list_in (f, "public/r.skg", "contains"), vec! ["c"],
                   "y's line is gone from r" );
      assert_eq! ( list_in (f, "public/r.skg", "hides"), Vec::<String>::new (),
                   "r's public file names no hide of y" );
      assert_eq! ( list_in (f, "private/r.skg", "hides"), vec! ["y"],
                   "r's private section hides y" );
      Ok (( )) } ) }

/// The same deletion under "public": f's content, as the narrowing
/// shows it, lacks y, so nothing explains a hide of y, and r's files
/// name no hide.
#[test]
fn a_member_only_a_forkers_restricted_record_explains_is_not_hidden (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- y\n- c\n"),
       ("f", "pid: f\ntitle: f\nforks:\n- r\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("y", "pid: y\ntitle: y\n") ],
    &[ ("f", "pid: f\ncontains:\n- y\n") ],
    |f, public| {
      let opened : String = open_content_view (f, "r", VIEW, public) ?;
      save_cleanly (f, &without_line (&opened, "** (skg node (gnid y)"), public) ?;
      assert_eq! ( list_in (f, "public/r.skg", "contains"), vec! ["c"],
                   "y's line is gone from r" );
      assert_eq! ( list_in (f, "public/r.skg", "hides"), Vec::<String>::new (),
                   "r's public file hides nothing" );
      assert! ( ! skg_files (f) ? . contains_key ("private/r.skg"),
                "no private section of r appears" );
      Ok (( )) } ) }

//
// The operations
//

/// Save 'text' as VIEW under 'restriction'; it must save without
/// errors.
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

/// 'view' without its one line starting 'prefix'.
fn without_line (
  view   : &str,
  prefix : &str,
) -> String {
  edit_line ( view, prefix, |_| String::new () )
    . lines () . filter ( |line| ! line . is_empty () )
    . map ( |line| format! ("{}\n", line) ) . collect () }
