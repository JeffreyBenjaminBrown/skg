// cargo nextest run --test grouped_repos -E 'test(restricted_graphnode_in_the_buffer::)'
//
// A buffer saved under a skgrepo narrowing can hold what the
// narrowing keeps out: a headline for a restricted graphnode, pasted
// from before the narrowing, or a record that would land in a
// restricted section. The save leaves the restricted graphnode as it
// is, whatever nodeRepo the buffer gives it, and refuses a save whose
// new or moved record would land in a restricted section, naming the
// skgrepo ('docs/api-and-formats.org', "Restricted-skgrepo nodes").
//
// Every fixture has the skgrepos "public" and "private", both owned.

use crate::git_fixture::{
  edit_line, list_in, open_content_view, save_under, skg_files, under_public};

use skg::save::DeleteFrom;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::errors::SaveError;
use skg::types::save::format_save_error_as_org;

use std::collections::BTreeMap;
use std::error::Error;

const VIEW : &str = "view";

const R_AND_P_PUBLIC  : &[(&str, &str)] = &[
  ("r", "pid: r\ntitle: r\ncontains:\n- c\n"),
  ("c", "pid: c\ntitle: c\n") ];
const R_AND_P_PRIVATE : &[(&str, &str)] = &[
  ("p", "pid: p\ntitle: p secret\ncontains:\n- k\n"),
  ("k", "pid: k\ntitle: k\n") ];

/// p's home is "private". Under "public" a root for p with the
/// nodeRepo "public", titled "p typed", is added to a view of r and
/// saved: p keeps its home, its title and its content, and the
/// warning says so.
#[test]
fn a_restricted_graphnode_given_a_visible_nodeRepo_is_left_as_it_is (
) -> Result<(), Box<dyn Error>> {
  under_public ( R_AND_P_PUBLIC, R_AND_P_PRIVATE, |f, public| {
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let opened : String = open_content_view (f, "r", VIEW, public) ?;
    let text : String = format! (
      "{}* (skg node (gnid p) (skgRepos (nodeRepo public))) p typed\n", opened );
    let response : SaveResponse = save_under (
      f, VIEW, &text, public, true, DeleteFrom::NotChosen ) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert! ( response . warnings . iter () . any ( |w| w . contains (
                "Restricted nodes present in saved buffer remain unchanged in graph." )),
              "the warning says p was left alone: {:?}", response . warnings );
    assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
    Ok (( )) } ) }

/// Under "public" a headline for p, with its true nodeRepo, is pasted
/// as r's content. Its record would land in "private", by the default
/// floor: the save is refused, naming the skgrepo, and no file
/// changes.
#[test]
fn a_record_whose_default_floor_is_restricted_is_refused (
) -> Result<(), Box<dyn Error>> {
  under_public ( R_AND_P_PUBLIC, R_AND_P_PRIVATE, |f, public| {
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let opened : String = open_content_view (f, "r", VIEW, public) ?;
    let text : String = edit_line ( &opened, "** (skg node (gnid c)", |line|
      format! ("{}\n** (skg node (gnid p) (skgRepos (nodeRepo private))) p secret", line) );
    assert_refused_naming_private ( save_under (
      f, VIEW, &text, public, true, DeleteFrom::NotChosen ), "contains", "p" );
    assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
    Ok (( )) } ) }

/// Under "public" r's visible record of c is asked, by an explicit
/// relRepo request, to move to "private": refused, naming the
/// skgrepo, and no file changes.
#[test]
fn a_relRepo_request_into_a_restricted_skgrepo_is_refused (
) -> Result<(), Box<dyn Error>> {
  under_public ( R_AND_P_PUBLIC, R_AND_P_PRIVATE, |f, public| {
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let opened : String = open_content_view (f, "r", VIEW, public) ?;
    let text : String = edit_line ( &opened, "** (skg node (gnid c)", |line|
      line . replacen ( "(skg ", "(skg (editRequest (relRepo private)) ", 1 ));
    assert_refused_naming_private ( save_under (
      f, VIEW, &text, public, true, DeleteFrom::NotChosen ), "contains", "c" );
    assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
    Ok (( )) } ) }

/// r subscribes to e1 publicly and to e by a record in its private
/// section; e contains h. Under "public" a hide of h is typed into
/// the folder of what r hides outside its subscribees. The one
/// subscription that explains the hide is restricted, so the hide
/// would land in "private": refused, naming the skgrepo.
#[test]
fn a_hide_only_a_restricted_subscription_explains_is_refused (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r",  "pid: r\ntitle: r\nsubscribesTo:\n- e1\nhidesFromSubs:\n- o\n"),
       ("e1", "pid: e1\ntitle: e1\ncontains:\n- c\n"),
       ("e",  "pid: e\ntitle: e\ncontains:\n- h\n"),
       ("c",  "pid: c\ntitle: c\n"),
       ("h",  "pid: h\ntitle: h\n"),
       ("o",  "pid: o\ntitle: o\n") ],
    &[ ("r", "pid: r\nsubscribesTo:\n- e\n") ],
    |f, public| {
      let opened : String = open_content_view (f, "r", VIEW, public) ?;
      let requested : String = edit_line ( &opened, "* (skg node (gnid r)", |line|
        line . replacen ( "(skg ", "(skg (viewRequests (folder subscribesTo)) ", 1 ));
      let with_folder : SaveResponse = save_under (
        f, VIEW, &requested, public, true, DeleteFrom::NotChosen ) ?;
      assert! ( with_folder . errors . is_empty (), "{:?}", with_folder . errors );
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let with_hide : String = edit_line (
        &with_folder . saved_view, "*** (skg hiddenOutsideOfSubscribeeFolder)", |line|
          format! ("{}\n**** (skg node (gnid h)) h", line) );
      assert_refused_naming_private ( save_under (
        f, VIEW, &with_hide, public, true, DeleteFrom::NotChosen ),
        "hidesFromSubs", "h" );
      assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
      assert_eq! ( list_in (f, "public/r.skg", "hidesFromSubs"), vec! ["o"] );
      Ok (( )) } ) }

/// r (public) contains m (public) by a record in r's private section,
/// so under "public" the view of r shows nothing of m. A headline for
/// m pasted under r with an explicit relRepo request for "public"
/// would move the restricted record into view: refused, naming the
/// skgrepo it would leave, and no file changes.
#[test]
fn a_relRepo_request_that_would_move_a_restricted_record_out_is_refused (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n"),
       ("m", "pid: m\ntitle: m\n") ],
    &[ ("r", "pid: r\ncontains:\n- m\n") ],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let opened : String = open_content_view (f, "r", VIEW, public) ?;
      assert! ( ! opened . contains ("(gnid m)"), "m stands nowhere:\n{}", opened );
      let text : String = edit_line ( &opened, "** (skg node (gnid c)", |line|
        format! ("{}\n** (skg (editRequest (relRepo public)) node (gnid m)) m", line) );
      let refusal : String = refusal_text ( save_under (
        f, VIEW, &text, public, true, DeleteFrom::NotChosen ));
      assert! ( refusal . contains ("Cannot move a record out of a restricted repo")
                && refusal . contains ("- Relation: contains")
                && refusal . contains ("- Member: m")
                && refusal . contains ("- The record's repo: private"),
                "the refusal names the record and its skgrepo:\n{}", refusal );
      assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
      Ok (( )) } ) }

/// 'saved' is the refusal of a save for a record of 'member', by
/// 'relation', that would land in "private", as the client shows it.
fn assert_refused_naming_private (
  saved    : Result<SaveResponse, Box<dyn Error>>,
  relation : &str,
  member   : &str,
) {
  let refusal : String = refusal_text (saved);
  assert! ( refusal . contains ("Cannot save a record into a restricted repo")
            && refusal . contains (&format! ("- Relation: {}", relation))
            && refusal . contains (&format! ("- Member: {}", member))
            && refusal . contains ("- The record's repo: private"),
            "the refusal names the record and its skgrepo:\n{}", refusal ); }

/// The refusal 'saved' must be, as the client shows it.
fn refusal_text (
  saved : Result<SaveResponse, Box<dyn Error>>,
) -> String {
  match saved {
    Err (e) => match e . downcast_ref::<SaveError> () {
      Some (save_error) => format_save_error_as_org (save_error),
      None => e . to_string (), },
    Ok (response) => panic! ( "the save must be refused; errors {:?}", response . errors ), } }
