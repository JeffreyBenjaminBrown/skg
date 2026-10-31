// cargo nextest run --test grouped_repos -E 'test(requests_refused_under_a_narrowing::)'
//
// Requests that rewrite or approve what a skgrepo narrowing may hide
// are refused unless the connection is unrestricted: the cleanup of
// the references to an absent ID, which scans and rewrites every owned
// recorder, and the restore and readiness requests a save carries
// ('docs/api-and-formats.org', "Restricted-skgrepo nodes"). The
// refusal writes nothing. (The import's refusal of a restricted
// destination is tested beside its handler.)
//
// Every fixture has the skgrepos "public" and "private", both owned.

use crate::git_fixture::{
  edit_line, everything_written, open_content_view, save_under, skg_files, under_public};
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;

use skg::save::DeleteFrom;
use skg::serve::handlers::delete_references_to_absent_node::
  handle_delete_references_to_absent_node_request;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;
use skg::types::errors::SaveError;
use skg::types::save::format_save_error_as_org;

use std::collections::BTreeMap;
use std::error::Error;
use std::net::TcpStream;

const VIEW : &str = "view";

/// r (public) and w (private) each contain gone, an ID the graph does
/// not have. Under "public" the cleanup of the references to gone is
/// refused, and neither file changes.
#[test]
fn deleting_the_references_to_an_absent_id_is_refused (
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- gone\n") ],
    &[ ("w", "pid: w\ntitle: w\ncontains:\n- gone\n") ],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let mut env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
      let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
      handle_delete_references_to_absent_node_request (
        &mut stream,
        "((request . \"delete references to absent node\") (id . \"gone\"))\n",
        &mut env, &mut f . views_state, public );
      let written : String = everything_written (stream, read_end);
      assert! ( written . contains (
                  "Deleting the references to an absent ID requires no skgrepo restriction" ),
                "the refusal says why:\n{}", written );
      assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
      Ok (( )) } ) }

/// Under "public" a save carrying a readiness request is refused,
/// before anything is written.
#[test]
fn a_save_carrying_a_readiness_request_is_refused (
) -> Result<(), Box<dyn Error>> {
  refused_with_the_root_carrying ("(editRequest (ready all))", "A readiness request") }

/// Under "public" a save carrying a restore request, on a member
/// shown as removed, is refused, before anything is written.
#[test]
fn a_save_carrying_a_restore_request_is_refused (
) -> Result<(), Box<dyn Error>> {
  refused_after ( "A restore request", |opened| edit_line (
    opened, "** (skg node (gnid c)", |_|
    "** (skg (editRequest restoreRel) node (gnid c) noClaim (diff (rel removed))) c"
    . to_string () )) }

/// The view of r, under "public", saved with r's line carrying
/// 'request', and with its title edited so that the save would write:
/// refused, the refusal opening with 'what'.
fn refused_with_the_root_carrying (
  request : &str,
  what    : &str,
) -> Result<(), Box<dyn Error>> {
  refused_after ( what, |opened| edit_line ( opened, "* (skg node (gnid r)", |line|
    line . replacen ( "(skg ", &format! ("(skg {} ", request), 1 )
    . replacen ( ") r", ") r edited", 1 )) ) }

/// The view of r, under "public", saved as 'edit' makes it: refused,
/// the refusal opening with 'what', and no file changed.
fn refused_after (
  what : &str,
  edit : impl Fn (&str) -> String,
) -> Result<(), Box<dyn Error>> {
  under_public (
    &[ ("r", "pid: r\ntitle: r\ncontains:\n- c\n"),
       ("c", "pid: c\ntitle: c\n") ],
    &[],
    |f, public| {
      let files_before : BTreeMap<String, String> = skg_files (f) ?;
      let opened : String = open_content_view (f, "r", VIEW, public) ?;
      let saved : Result<SaveResponse, Box<dyn Error>> = save_under (
        f, VIEW, &edit (&opened), public, true, DeleteFrom::NotChosen );
      let refusal : String = match saved {
        Err (e) => match e . downcast_ref::<SaveError> () {
          Some (save_error) => format_save_error_as_org (save_error),
          None => e . to_string (), },
        Ok (response) => panic! ( "the save must be refused; errors {:?}",
                                  response . errors ), };
      assert! ( refusal . contains (
                  &format! ("{} requires no skgrepo restriction", what) ),
                "the refusal says why:\n{}", refusal );
      assert_eq! ( skg_files (f) ?, files_before, "no file changed" );
      Ok (( )) } ) }
