// cargo nextest run --test grouped_repos -E 'test(foreign_hidden::)'
//
// Hiding the foreign skgrepos ('docs/api-and-formats.org',
// "skgrepo-sets"): the skgrepo restriction's second part. Hidden, a
// foreign node is neither shown nor editable, and an open view that
// showed it re-renders without it; shown again, it comes back.
//
// The fixture has the owned skgrepo "public" and the foreign one
// "theirs": public's p contains c and the foreign f, which contains
// the foreign g.

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, edit_line, open_content_view, save_under,
  with_git_fixture, write_unchanged_skgrepos};

use skg::save::DeleteFrom;
use skg::serve::handlers::skgrepo_sets::handle_skgrepo_set_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  extract_string_field_from_sexp, read_all_lp_messages, skg_env_from_parts};
use skg::types::env::SkgEnv;
use skg::types::misc::SkgrepoName;

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::collections::BTreeMap;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::TcpStream;
use std::path::PathBuf;
use tempfile::TempDir;

const VIEW : &str = "view";

fn with_public_and_theirs (
  body : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let case : TempDir = TempDir::new () ?;
  write_unchanged_skgrepos ( case . path (), &[
    ("public", &[ ("p", "pid: p\ntitle: p\ncontains:\n- f\n- c\n"),
                  ("c", "pid: c\ntitle: c\n") ]),
    ("theirs", &[ ("f", "pid: f\ntitle: f's foreign title\ncontains:\n- g\n"),
                  ("g", "pid: g\ntitle: g's foreign title\n") ]) ] ) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public"]), &["theirs"], body ) }

/// Every '.skg' file of the foreign skgrepo, by name, with its text.
fn foreign_files (
  f : &Fixture,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for entry in fs::read_dir ( f . root . path () . join ("theirs") ) ? {
    let path : PathBuf = entry ? . path ();
    if path . extension () . is_some_and ( |ext| ext == "skg" ) {
      files . insert ( path . file_name () . unwrap () . to_string_lossy () . into_owned (),
                       fs::read_to_string (&path) ? ); }}
  Ok (files) }

/// Send 'request' to the switch handler; the restriction it confirms
/// (its response's 'foreign-repos'), and 'VIEW' as rerendered.
fn switch (
  f           : &mut Fixture,
  restriction : &mut SkgrepoRestriction,
  request     : &str,
) -> Result<(String, String), Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_skgrepo_set_request (
    &mut stream, request, &env, &mut f . views_state, restriction );
  drop (stream);
  let messages : Vec<String> = read_all_lp_messages ( &mut BufReader::new (read_end) );
  let confirmed : String = messages . iter ()
    . find ( |message| message . contains ("(response-type skgrepo-restriction)") )
    . and_then ( |message| extract_string_field_from_sexp (
      &message [ message . find ('(') ? .. ], "foreign-repos" ))
    . unwrap_or_else ( || panic! ( "no confirmation: {:?}", messages ));
  let rerendered : String = messages . iter ()
    . find ( |message| message . contains ("rerender-view") )
    . and_then ( |message| extract_string_field_from_sexp (
      &message [ message . find ('(') ? .. ], "content" ))
    . unwrap_or_else ( || panic! ( "no rerendered view: {:?}", messages ));
  Ok (( confirmed, rerendered )) }

#[test]
fn hidden_foreign_skgrepos_take_a_foreign_node_out_of_an_open_view_and_out_of_reach (
) -> Result<(), Box<dyn Error>> {
  with_public_and_theirs ( |f| {
    let mut restriction : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config);
    let opened : String = open_content_view (f, "p", VIEW, &restriction) ?;
    assert! ( opened . contains ("f's foreign title")
              && opened . contains ("g's foreign title"),
              "shown, the foreign nodes are drawn:\n{}", opened );
    let (confirmed, hidden_view) : (String, String) = switch (
      f, &mut restriction,
      "((request . \"set skgrepo restriction\") (foreign-repos . \"hidden\"))\n" ) ?;
    assert_eq! ( confirmed, "hidden" );
    assert_eq! ( restriction . most_private_shown, SkgrepoName::from ("public"),
                 "the rung is kept" );
    assert! ( ! restriction . foreign_shown );
    for secret in [ "foreign title", "(gnid g)" ] {
      assert! ( ! hidden_view . contains (secret),
                "hidden, the view shows nothing of f but its ID: {}:\n{}",
                secret, hidden_view ); }
    assert! ( hidden_view . contains ("restricted (gnid f)"),
              "p's own record of f stands as a Restricted vognode:\n{}", hidden_view );
    { // An edit to the Restricted vognode is refused, and nothing is
      // written: neither f nor a clone of it.
      let foreign_before : BTreeMap<String, String> = foreign_files (f) ?;
      let public_before : usize = fs::read_dir ( f . root . path () . join ("public") ) ?
        . count ();
      let edited : String = edit_line (
        &hidden_view, "** (skg restricted (gnid f))",
        |line| format! ("{} retitled", line) );
      let refusal : String = save_under (
        f, VIEW, &edited, &restriction, true, DeleteFrom::NotChosen )
        . err () . map ( |e| format! ("{:?}", e) )
        . expect ("an edit to a hidden foreign node must be refused");
      assert! ( refusal . contains ("restricted content cannot be edited"),
                "{}", refusal );
      assert_eq! ( foreign_files (f) ?, foreign_before );
      assert_eq! ( fs::read_dir ( f . root . path () . join ("public") ) ? . count (),
                   public_before, "no clone of f was written" ); }
    let (confirmed, shown_view) : (String, String) = switch (
      f, &mut restriction,
      "((request . \"set skgrepo restriction\") (foreign-repos . \"shown\"))\n" ) ?;
    assert_eq! ( confirmed, "shown" );
    assert! ( restriction . is_unrestricted () );
    assert! ( shown_view . contains ("f's foreign title")
              && shown_view . contains ("g's foreign title"),
              "shown again, they come back:\n{}", shown_view );
    Ok (( )) } ) }

#[test]
fn naming_a_rung_keeps_foreign_skgrepos_hidden_and_a_foreign_name_is_refused (
) -> Result<(), Box<dyn Error>> {
  with_public_and_theirs ( |f| {
    let mut restriction : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config)
      . with_foreign_shown (&f . config, false) ?;
    open_content_view (f, "p", VIEW, &restriction) ?;
    let (confirmed, _view) : (String, String) = switch (
      f, &mut restriction,
      "((request . \"set skgrepo restriction\") (name . \"public\"))\n" ) ?;
    assert_eq! ( confirmed, "hidden", "naming a rung leaves the flag alone" );
    let before : SkgrepoRestriction = restriction . clone ();
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
    handle_skgrepo_set_request (
      &mut stream,
      "((request . \"set skgrepo restriction\") (name . \"theirs\"))\n",
      &env, &mut f . views_state, &mut restriction );
    drop (stream);
    let messages : Vec<String> = read_all_lp_messages ( &mut BufReader::new (read_end) );
    assert_eq! ( restriction, before, "a foreign repo is no rung" );
    assert! ( messages . iter () . any ( |message|
                message . contains ("names no owned skgrepo") ),
              "{:?}", messages );
    Ok (( )) } ) }
