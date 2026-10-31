// cargo nextest run --test grouped_repos -E 'test(subscriber_hides_under_a_narrowing::)'
//
// A save under a skgrepo narrowing leaves alone what the narrowing
// keeps out of a subscriber's hides ('docs/api-and-formats.org',
// "Restricted-skgrepo nodes"). Two steps of the save write a
// subscriber's hides from what the buffer shows: the
// subscribee-visibility step, for a subscribee shown editable under
// its subscriber, and the save of the folder of what the subscriber
// hides outside its subscribees. Each reads the subscribee's content
// and the subscriber's hides by the records the skgrepo restriction
// shows, as the view drew them: a hide recorded in a restricted
// section is neither removed nor moved, and no hide is written of a
// member that only a restricted record explains.
//
// Every fixture has the skgrepos "public" and "private", both owned;
// r, home "public", subscribes to e by a record in its public file,
// and every view is of r, under "public" alone.

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, edit_line, open_content_view, skg_files, with_git_fixture,
  write_unchanged_skgrepos};
use crate::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::from_text::strandeds::StrandedsReview;
use skg::metadata_canon::without_viewnode_ids;
use skg::serve::handlers::save_buffer::{
  SaveResponse, update_from_and_rerender_buffer_with_approvals};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;
use skg::types::views_state::ViewforestId;

use futures::executor::block_on;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::error::Error;
use std::net::TcpStream;

const VIEW : &str = "view of r";

/// Under "public", e is made editable as r's subscribee and the view
/// saved unchanged. e contains c by a public record and x and y by
/// private ones, so the view shows c alone under e: the save hides
/// neither x nor y from r, and no file changes.
#[test]
fn an_unchanged_save_hides_nothing_that_only_a_restricted_record_explains (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- y\nsubscribesTo:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("x", "pid: x\ntitle: x\n"),
       ("y", "pid: y\ntitle: y\n") ],
    &[ ("e", "pid: e\ncontains:\n- x\n- y\n") ],
    |f, public| {
      let opened : String = open_content_view (f, "r", VIEW, Some (public)) ?;
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let editable : String = save (f, &with_e_requested_editable (&opened), public) ?;
      assert! ( has_line (&editable, "**** (skg node (gnid c)"),
                "e, editable, shows c:\n{}", editable );
      assert! ( ! editable . contains ("(gnid x)"),
                "e shows nothing of its private record:\n{}", editable );
      save (f, &editable, public) ?;
      assert_eq! ( list_in (f, "public/r.skg", "hidesFromSubs"), Vec::<String>::new (),
                   "r's public file hides nothing" );
      assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
      Ok (( )) } ) }

/// r hides h by a record in its private section, and e contains h by
/// a public record. Under "public" the hide is out of sight, so h is
/// shown as e's content under r. A save of that view, unchanged,
/// leaves the hide where it was.
#[test]
fn an_unchanged_save_keeps_a_restricted_hide_of_what_the_subscribee_shows (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\nsubscribesTo:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- c\n- h\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("h", "pid: h\ntitle: h\n") ],
    &[ ("r", "pid: r\nhidesFromSubs:\n- h\n") ],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let editable : String = e_editable_under_its_subscriber (f, public) ?;
      assert! ( has_line (&editable, "**** (skg node (gnid h)"),
                "h stands as e's content:\n{}", editable );
      assert! ( ! editable . contains ("hiddenInSubscribeeFolder"),
                "no folder shows the restricted hide:\n{}", editable );
      let saved : String = save (f, &editable, public) ?;
      assert_eq! ( without_viewnode_ids (&saved), without_viewnode_ids (&editable),
                   "saved unchanged, the view comes back as it went" );
      assert_eq! ( list_in (f, "private/r.skg", "hidesFromSubs"), vec! ["h"],
                   "r's private section still hides h" );
      assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
      Ok (( )) } ) }

/// As above, but e contains h by a private record too: under
/// "public" h stands nowhere under r, and saves leave every file as it
/// was.
#[test]
fn a_restricted_hide_of_restricted_subscribee_content_stands_nowhere_and_saves_keep_it (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\nsubscribesTo:\n- e\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("h", "pid: h\ntitle: h\n") ],
    &[ ("r", "pid: r\nhidesFromSubs:\n- h\n"),
       ("e", "pid: e\ncontains:\n- h\n") ],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let editable : String = e_editable_under_its_subscriber (f, public) ?;
      assert! ( ! editable . contains ("(gnid h)"), "h stands nowhere:\n{}", editable );
      save (f, &editable, public) ?;
      assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
      Ok (( )) } ) }

/// r hides hpub by a record in its public file and hpriv by one in
/// its private section; e contains neither. Under "public" the folder
/// of what r hides outside its subscribees shows hpub alone. Saved
/// unchanged, then with hpub's line deleted, then with a hide of n
/// added, the folder's edits reach r's public file alone, and the
/// private section keeps its hide of hpriv.
#[test]
fn edits_of_the_hidden_outside_folder_leave_a_restricted_hide_alone (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r",     "pid: r\ntitle: r\nsubscribesTo:\n- e\nhidesFromSubs:\n- hpub\n"),
       ("e",     "pid: e\ntitle: e\ncontains:\n- c\n"),
       ("c",     "pid: c\ntitle: c\n"),
       ("hpub",  "pid: hpub\ntitle: hpub\n"),
       ("hpriv", "pid: hpriv\ntitle: hpriv\n"),
       ("n",     "pid: n\ntitle: n\n") ],
    &[ ("r", "pid: r\nhidesFromSubs:\n- hpriv\n") ],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let requested : String = subscribee_folder_requested (f, public) ?;
      assert_eq! ( members_of_the_hidden_outside_folder (&requested), vec! ["hpub"] );
      let unchanged : String = save (f, &requested, public) ?;
      assert_eq! ( skg_files (f) ?, files_before, "saved unchanged, no file changed" );
      let without_hpub : String = save (
        f, &without_member_line (&unchanged, "hpub"), public) ?;
      assert_eq! ( list_in (f, "public/r.skg", "hidesFromSubs"), Vec::<String>::new (),
                   "hpub's hide goes from r's public file" );
      assert_eq! ( list_in (f, "private/r.skg", "hidesFromSubs"), vec! ["hpriv"],
                   "the private section keeps its hide of hpriv" );
      save (f, &with_member_n (&without_hpub), public) ?;
      assert_eq! ( list_in (f, "public/r.skg", "hidesFromSubs"), vec! ["n"],
                   "n's hide is recorded in r's public file" );
      assert_eq! ( list_in (f, "private/r.skg", "hidesFromSubs"), vec! ["hpriv"],
                   "the private section keeps its hide of hpriv" );
      Ok (( )) } ) }

/// r hides h and o by records in its public file; e contains c by a
/// public record and h by a private one. Under "public" e's content
/// is c alone, so both hides stand outside, in the folder of what r
/// hides outside its subscribees. Saved unchanged, r keeps both;
/// with h's line deleted, r's public file hides o alone.
#[test]
fn a_hide_of_what_the_subscribee_contains_restrictedly_stands_outside_and_its_edits_save (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\nsubscribesTo:\n- e\nhidesFromSubs:\n- h\n- o\n"),
       ("e", "pid: e\ntitle: e\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("h", "pid: h\ntitle: h\n"),
       ("o", "pid: o\ntitle: o\n") ],
    &[ ("e", "pid: e\ncontains:\n- h\n") ],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let requested : String = subscribee_folder_requested (f, public) ?;
      assert_eq! ( members_of_the_hidden_outside_folder (&requested), vec! ["h", "o"] );
      let unchanged : String = save (f, &requested, public) ?;
      assert_eq! ( skg_files (f) ?, files_before, "saved unchanged, no file changed" );
      save (f, &without_member_line (&unchanged, "h"), public) ?;
      assert_eq! ( list_in (f, "public/r.skg", "hidesFromSubs"), vec! ["o"],
                   "h's hide goes from r's public file" );
      assert_eq! ( list_in (f, "private/e.skg", "contains"), vec! ["h"],
                   "e's private section still contains h" );
      Ok (( )) } ) }

//
// The fixture and the operations
//

/// Write 'public' and 'private' as the two skgrepos' files, then run
/// 'body' with the skgrepo restriction "public".
fn under_public (
  public  : &[(&str, &str)],
  private : &[(&str, &str)],
  body    : impl FnOnce (&mut Fixture, &SkgrepoRestriction) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_unchanged_skgrepos (case . path (), &[("public", public), ("private", private)]) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f| {
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (&f . config, "public" . into ()) ?;
      body (f, &restriction) } ) }

/// The view of r with its subscribee folder requested, as saved.
fn subscribee_folder_requested (
  f      : &mut Fixture,
  public : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  let opened : String = open_content_view (f, "r", VIEW, Some (public)) ?;
  let requested : String = edit_line ( &opened, "* (skg node (gnid r)", |line|
    line . replacen ( "(skg ", "(skg (viewRequests (folder subscribesTo)) ", 1 ));
  save (f, &requested, public) }

/// The view of r with its subscribee folder requested and e made
/// editable in it, as saved.
fn e_editable_under_its_subscriber (
  f      : &mut Fixture,
  public : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  let requested : String = subscribee_folder_requested (f, public) ?;
  save (f, &with_e_requested_editable (&requested), public) }

/// 'view' with an editable-view request on e, r's subscribee.
fn with_e_requested_editable (
  view : &str,
) -> String {
  edit_line ( view, "*** (skg node (gnid e)", |line|
    line . replacen ( "writeProtected ", "writeProtected (viewRequests editableView) ", 1 )) }

/// Save 'text' as VIEW under 'restriction'; it must save without
/// errors. Returns the view as saved.
fn save (
  f           : &mut Fixture,
  text        : &str,
  restriction : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let mut env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let response : SaveResponse = block_on (
    update_from_and_rerender_buffer_with_approvals (
      &mut stream, text, &mut env,
      &Ok ( ViewforestId::ContentView (VIEW . into ()) ),
      &mut f . views_state, Some (restriction),
      true, &HashMap::new (), &HashSet::new (), &HashSet::new (),
      &StrandedsReview::NotReviewed, &[] )) ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
            response . errors, text );
  Ok (response . saved_view) }

/// Whether some line of 'view', without its viewnode ID, starts with
/// 'prefix'.
fn has_line (
  view   : &str,
  prefix : &str,
) -> bool {
  without_viewnode_ids (view) . lines () . any ( |line| line . starts_with (prefix) ) }

/// The members 'field' lists in the file 'file' ("SKGREPO/PID.skg"),
/// as written: the "- " lines that follow the line "FIELD:". None
/// when the file or the field is absent.
fn list_in (
  f     : &Fixture,
  file  : &str,
  field : &str,
) -> Vec<String> {
  let files : BTreeMap<String, String> = skg_files (f) . unwrap ();
  let Some (text) : Option<&String> = files . get (file) else { return Vec::new (); };
  let heading : String = format! ("{}:", field);
  text . lines ()
    . skip_while ( |line| *line != heading )
    . skip (1)
    . map_while ( |line| line . strip_prefix ("- ") )
    . map ( |member| member . to_string () )
    . collect () }

/// The gnid of each member of the folder of what r hides outside its
/// subscribees.
fn members_of_the_hidden_outside_folder (
  view : &str,
) -> Vec<String> {
  let lines : Vec<&str> = view . lines () . collect ();
  let folder : usize = lines . iter ()
    . position ( |line| line . contains ("hiddenOutsideOfSubscribeeFolder") )
    . unwrap_or_else ( || panic! ( "no hidden-outside folder:\n{}", view ));
  lines [folder + 1 ..] . iter ()
    . take_while ( |line| line . starts_with ("**** ") )
    . filter_map ( |line| line . split ("(gnid ") . nth (1)
                   . and_then ( |rest| rest . split (')') . next () )
                   . map ( |gnid| gnid . to_string () ))
    . collect () }

/// 'view' without the line of the hidden-outside folder's member
/// 'gnid'.
fn without_member_line (
  view : &str,
  gnid : &str,
) -> String {
  edit_line ( view, &format! ("**** (skg folded node (gnid {})", gnid), |_| String::new () )
    . lines () . filter ( |line| ! line . is_empty () )
    . map ( |line| format! ("{}\n", line) ) . collect () }

/// 'view' with a new member, n, first in the hidden-outside folder.
fn with_member_n (
  view : &str,
) -> String {
  edit_line ( view, "*** (skg hiddenOutsideOfSubscribeeFolder)", |line|
    format! ("{}\n**** (skg node (gnid n)) n", line) ) }
