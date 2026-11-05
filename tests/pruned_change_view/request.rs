// The "diff view" request at the handler: it registers each
// nonempty buffer as a view of the diff view, answers an empty
// buffer with '(empty . t)', and the graph straddle is dropped when the
// last such view closes. A buffer's answer carries the readiness
// summary its save would. A rerender-all writes back the straddle it
// brought up to date.

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use crate::git_fixture::{Fixture, everything_written, save_view};
use crate::pruned_change_view::common::{open_all_changes_buffer, with_fixture};

use skg::graph_straddle::types::GraphStraddle;
use skg::metadata_canon::viewnode_id_atom_of;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::handle_save_buffer_request;
use skg::test_utils::read_all_lp_messages;
use skg::types::views_state::OpenViews;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::serve::util::sexp_to_client_string;
use skg::test_utils::extract_string_field_from_sexp;
use skg::serve::handlers::close_view::handle_close_view_request;
use skg::serve::handlers::rerender_all_views::handle_rerender_all_views_request;
use skg::serve::handlers::diff_view::handle_diffView_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{graph_handle_from_config, skg_env_from_parts};
use skg::types::env::SkgEnv;
use skg::types::views_state::{DiffViewBufferKind, ViewforestId};

use git2::{Commit, Oid, Repository, Signature};
use std::error::Error;
use std::fs;
use std::io::{BufReader, Write};
use std::net::TcpStream;
use std::path::PathBuf;
use std::sync::Arc;

fn request (
  buffer  : &str,
  viewforest_id : &str,
) -> String {
  format! ( "((request . \"diff view\") (buffer . \"{}\") (view-id . \"{}\"))\n",
            buffer, viewforest_id ) }

#[test]
fn a_request_registers_its_buffer_and_the_last_close_drops_the_straddle (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config);
    { let (mut stream, read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_diffView_request (
        &mut stream, &request ("all changes", "diff-view-1"), &env,
        &mut f . views_state, &all );
      let response : String = everything_written (stream, read_end);
      assert! ( response . contains ("diff-view")
                && response . contains ("(content"), "{}", response ); }
    { let (mut stream, read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_diffView_request (
        &mut stream, &request ("merges", "diff-view-2"), &env,
        &mut f . views_state, &all );
      let response : String = everything_written (stream, read_end);
      assert! ( response . contains ("(empty . t)"), "{}", response ); }
    let viewforest_id : ViewforestId = ViewforestId::ContentView ("diff-view-1" . to_string ());
    assert_eq! ( f . views_state . open_views . views . get (&viewforest_id)
                 . and_then ( |state| state . diffView_buffer_kind ),
                 Some (DiffViewBufferKind::AllChanges) );
    assert! ( ! f . views_state . open_views . views
              . contains_key (&ViewforestId::ContentView ("diff-view-2" . to_string ())),
              "an empty buffer is not registered" );
    assert! ( f . views_state . graph_straddle . is_some () );
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    handle_close_view_request (
      &mut stream, "((request . \"close view\") (view-id . \"diff-view-1\"))\n",
      &mut f . views_state );
    assert! ( f . views_state . graph_straddle . is_none () );
    Ok (( )) }) }

/// The answer to a buffer's request ends with the readiness summary
/// that saving the buffer unchanged reports, so a client need not save
/// to learn it.
#[test]
fn a_buffers_answer_carries_the_readiness_summary_of_its_save (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let (response, _registered) : (String, bool) = request_all_changes (f) ?;
    let payload : &str = &response [ response . find ("((") . unwrap () .. ];
    let content : String = extract_string_field_from_sexp (payload, "content")
      . expect ("the buffer's answer has content");
    let saved : SaveResponse = save_view (
      f, &ViewforestId::ContentView ("diff-view-1" . to_string ()), &content ) ?;
    let summary_of_the_save : String = sexp_to_client_string (
      &saved . readiness_summary
        . expect ("a save of a buffer of the diff view reports its summary")
        . to_sexp () );
    assert! ( summary_of_the_save . starts_with ("(readiness-summary (ready 0)"),
              "{}", summary_of_the_save );
    assert! ( payload . trim_end () . ends_with (
                &format! ( "{})", summary_of_the_save )),
              "the answer:\n{}\nthe save's summary:\n{}",
              payload, summary_of_the_save );
    Ok (( )) }) }

/// A save naming a buffer of the diff view its connection does not
/// hold (a tool's connection ends with each command) re-renders it as
/// that buffer, with its readiness summary, when the request names the
/// buffer; each headline keeps the viewnode ID it carries, even one no
/// rendering gave it. Without the name it re-renders as a content view.
#[test]
fn a_save_of_a_diffView_buffer_not_held_keeps_its_viewnode_ids (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let (response, _registered) : (String, bool) = request_all_changes (f) ?;
    let payload : &str = &response [ response . find ("((") . unwrap () .. ];
    let content : String = extract_string_field_from_sexp (payload, "content")
      . expect ("the buffer's answer has content");
    let rendered_at : String = extract_string_field_from_sexp (payload, "rendered-at")
      . expect ("the buffer's answer has rendered-at");
    let first_headline : &str = content . lines ()
      . find ( |line| viewnode_id_atom_of (line) . is_some () )
      . expect ("a headline with a viewnode ID");
    let renumbered : String = content . replacen (
      first_headline,
      & first_headline . replacen (
        viewnode_id_atom_of (first_headline) . unwrap (), "(vnid zz)", 1 ),
      1 );
    let saved_on_a_new_connection = |f : &mut Fixture, fields : &str| -> String {
      let mut views_state : ViewsState = ViewsState {
        open_views : OpenViews::new (), graph_straddle : None };
      save_through_the_handler (
        f, &mut views_state,
        &format! ( "((request . \"save buffer\") (view-id . \"diff-view-1\") (rendered-at . \"{}\"){})",
                   rendered_at, fields ),
        &renumbered ) };
    { let named : String = saved_on_a_new_connection (
        f, " (diff-view-buffer . \"all changes\")" );
      let saved : String = extract_string_field_from_sexp (&named, "content")
        . expect ("a save-result with content");
      assert! ( named . contains ("(readiness-summary"), "{}", named );
      assert_eq! ( viewnode_id_atoms (&saved), viewnode_id_atoms (&renumbered),
                   "every viewnode ID kept:\n{}", saved );
      assert! ( saved . contains ("(vnid zz)"), "{}", saved ); }
    { let unnamed : String = saved_on_a_new_connection (f, "");
      assert! ( ! unnamed . contains ("(readiness-summary"),
                "without the name, a content view:\n{}", unnamed ); }
    { let refused : String = saved_on_a_new_connection (
        f, " (diff-view-buffer . \"no such buffer\")" );
      assert! ( refused . contains ("which the diff view does not have"), "{}", refused ); }
    Ok (( )) }) }

/// The viewnode ID atoms of 'text', in order.
fn viewnode_id_atoms (
  text : &str,
) -> Vec<String> {
  text . lines ()
    . filter_map ( |line| viewnode_id_atom_of (line) . map ( str::to_string ) )
    . collect () }

/// Send 'request' with 'buffer' as its saved buffer through the save
/// handler, on the connection 'views_state'; return the terminal
/// save-result message.
fn save_through_the_handler (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
  request     : &str,
  buffer      : &str,
) -> String {
  let envelope : String = format! (
    "((saved-buffer \"{}\") (baseline \"\") (other-views ()))", // the empty baseline, which writes nothing
    buffer . replace ('\\', "\\\\") . replace ('"', "\\\"") );
  let (mut client_writes, server_reads) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  write! ( client_writes, "Content-Length: {}\r\n\r\n{}",
           envelope . len (), envelope ) . unwrap ();
  let (mut server_writes, client_reads) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let mut env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  handle_save_buffer_request (
    &mut BufReader::new (server_reads), &mut server_writes, request,
    &mut env, views_state, &SkgrepoRestriction::unrestricted (&f . config) );
  drop (server_writes);
  read_all_lp_messages (&mut BufReader::new (client_reads))
    . into_iter ()
    . find ( |message| message . contains ("save-result") )
    . expect ("a save-result") }

#[test]
fn a_request_is_refused_under_a_skgrepo_restriction (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let restriction : SkgrepoRestriction = // 'main' shown, 'other' kept out
      SkgrepoRestriction::of_privacy_order_for_test (&["main", "other"], "main");
    let (mut stream, read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    handle_diffView_request (
      &mut stream, &request ("all changes", "diff-view-1"), &env,
      &mut f . views_state, &restriction );
    let response : String = everything_written (stream, read_end);
    assert! ( response . contains ("requires no skgrepo restriction"), "{}", response );
    assert! ( f . views_state . open_views . views . is_empty () );
    Ok (( )) }) }

#[test]
fn a_rerender_all_writes_its_refreshed_straddle_back (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    open_all_changes_buffer (f) ? ;
    let opened : Arc<GraphStraddle> =
      f . views_state . graph_straddle . clone ()
      . expect ("opening a buffer builds the straddle");
    // A newer live graph, as a save in another connection swaps in.
    f . graph . store ( graph_handle_from_config (&f . config) ? . load_full () );
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config);
    let (mut stream, read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    handle_rerender_all_views_request (
      &mut stream, "((request . \"rerender all views\"))\n", &env,
      &mut f . views_state, &all );
    let response : String = everything_written (stream, read_end);
    assert! ( response . contains ("rerender-view"), "{}", response );
    let held : Arc<GraphStraddle> =
      f . views_state . graph_straddle . clone ()
      . expect ("the connection still holds a straddle");
    assert! ( Arc::ptr_eq (&held . after, &env . runtime_snapshot () . graph),
              "the held straddle pairs HEAD with the live graph the rerender used" );
    assert! ( Arc::ptr_eq (&held . before, &opened . before) );
    Ok (( )) }) }

/// What the client's one command does: the four requests in order,
/// each nonempty buffer registered under its own view-id as the
/// buffer it is, and an empty one answered '(empty . t)'.
fn request_the_four_buffers (
  case     : &str,
  skgrepos : &[&str],
  expected_nonempty : &[DiffViewBufferKind],
) -> Result<(), Box<dyn Error>> {
  with_fixture (case, skgrepos, |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config);
    for diffView_buffer_kind in DiffViewBufferKind::ALL {
      let viewforest_id : String = format! ( "diff-view: {}", diffView_buffer_kind . wire_name () );
      let (mut stream, read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_diffView_request (
        &mut stream, &request (diffView_buffer_kind . wire_name (), &viewforest_id), &env,
        &mut f . views_state, &all );
      let response : String = everything_written (stream, read_end);
      let registered : Option<DiffViewBufferKind> =
        f . views_state . open_views . views
        . get (&ViewforestId::ContentView (viewforest_id))
        . and_then ( |state| state . diffView_buffer_kind );
      if expected_nonempty . contains (&diffView_buffer_kind) {
        assert! ( response . contains ("(content"), "{}", response );
        assert_eq! ( registered, Some (diffView_buffer_kind) );
      } else {
        assert! ( response . contains ("(empty . t)"), "{}", response );
        assert_eq! ( registered, None ); }}
    Ok (( )) }) }

#[test]
fn the_four_requests_open_every_nonempty_buffer (
) -> Result<(), Box<dyn Error>> {
  request_the_four_buffers ( "buffers", &["public", "private"], &DiffViewBufferKind::ALL ) }

#[test]
fn the_four_requests_skip_the_empty_buffers (
) -> Result<(), Box<dyn Error>> {
  request_the_four_buffers ( "sets", &["main"], &[ DiffViewBufferKind::AllChanges ] ) }

/// Send the "all changes" request with no skgrepo restriction; return
/// the response, and whether the buffer was registered.
fn request_all_changes (
  f : &mut Fixture,
) -> Result<(String, bool), Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let all : SkgrepoRestriction =
    SkgrepoRestriction::unrestricted (&f . config);
  let (mut stream, read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  handle_diffView_request (
    &mut stream, &request ("all changes", "diff-view-1"), &env,
    &mut f . views_state, &all );
  let response : String = everything_written (stream, read_end);
  let registered : bool = f . views_state . open_views . views
    . contains_key ( &ViewforestId::ContentView ("diff-view-1" . to_string ()) );
  Ok (( response, registered )) }

/// The 'sets' fixture's one owned skgrepo, main, made a plain folder.
fn main_without_its_gitrepo (
  f : &Fixture,
) -> Result<PathBuf, Box<dyn Error>> {
  let main : PathBuf = f . root . path () . join ("main");
  fs::remove_dir_all ( main . join (".git") ) ?;
  Ok (main) }

#[test]
fn a_request_is_refused_for_a_skgrepo_with_no_gitrepo (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    main_without_its_gitrepo (f) ?;
    let (response, registered) : (String, bool) = request_all_changes (f) ?;
    assert! ( response . contains (
      "Cannot open the diff view: Cannot read HEAD of skgrepo 'main': it is not in a git repository." ),
      "{}", response );
    assert! ( ! registered );
    Ok (( )) }) }

#[test]
fn a_request_is_refused_for_a_skgrepo_with_no_head_commit (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    Repository::init ( main_without_its_gitrepo (f) ? ) ?;
    let (response, registered) : (String, bool) = request_all_changes (f) ?;
    assert! ( response . contains (
      "Cannot open the diff view: Cannot read HEAD of skgrepo 'main': it has no HEAD commit" ),
      "{}", response );
    assert! ( ! registered );
    Ok (( )) }) }

/// HEAD is a merge commit, of the fixture's commit and a commit on
/// its side, both with the same tree: the request renders the buffer as
/// for the fixture's commit.
#[test]
fn a_request_accepts_a_merge_commit_at_head (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let before_the_merge : String = open_all_changes_buffer (f) ?;
    { let gitrepo : Repository =
        Repository::open ( f . root . path () . join ("main") ) ?;
      let signature : Signature = gitrepo . signature () ?;
      let head : Commit = gitrepo . head () ? . peel_to_commit () ?;
      let side : Oid = gitrepo . commit (
        None, &signature, &signature, "a side commit",
        &head . tree () ?, &[&head] ) ?;
      gitrepo . commit (
        Some ("HEAD"), &signature, &signature, "a merge commit",
        &head . tree () ?, &[ &head, &gitrepo . find_commit (side) ? ] ) ?;
      assert_eq! ( gitrepo . head () ? . peel_to_commit () ? . parent_count (), 2 ); }
    let (response, registered) : (String, bool) = request_all_changes (f) ?;
    assert! ( registered, "{}", response );
    assert! ( response . contains ( before_the_merge . lines () . next () . unwrap () ),
              "{}", response );
    Ok (( )) }) }
