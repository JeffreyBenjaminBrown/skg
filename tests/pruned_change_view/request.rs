// The "diff view" request at the handler: it registers each
// nonempty buffer as a view of the diff view, answers an empty
// buffer with '(empty . t)', and the graph straddle is dropped when the
// last such view closes. A rerender-all writes back the straddle it
// brought up to date.

use crate::git_fixture_helpers::mk_test_tcp_stream_pair;
use crate::git_fixture::{Fixture, everything_written};
use crate::pruned_change_view::common::{open_all_changes_buffer, with_fixture};

use skg::graph_straddle::types::GraphStraddle;
use skg::serve::handlers::close_view::handle_close_view_request;
use skg::serve::handlers::rerender_all_views::handle_rerender_all_views_request;
use skg::serve::handlers::diff_view::handle_diffView_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{graph_handle_from_config, skg_env_from_parts};
use skg::types::env::SkgEnv;
use skg::types::views_state::{DiffViewBufferKind, ViewId};

use git2::{Commit, Oid, Repository, Signature};
use std::error::Error;
use std::fs;
use std::net::TcpStream;
use std::path::PathBuf;
use std::sync::Arc;

fn request (
  buffer  : &str,
  view_id : &str,
) -> String {
  format! ( "((request . \"diff view\") (buffer . \"{}\") (view-id . \"{}\"))\n",
            buffer, view_id ) }

#[test]
fn a_request_registers_its_buffer_and_the_last_close_drops_the_straddle (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all : SkgrepoRestriction =
      SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
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
    let view_id : ViewId = ViewId::ContentView ("diff-view-1" . to_string ());
    assert_eq! ( f . views_state . open_views . views . get (&view_id)
                 . and_then ( |state| state . diffView_buffer_kind ),
                 Some (DiffViewBufferKind::AllChanges) );
    assert! ( ! f . views_state . open_views . views
              . contains_key (&ViewId::ContentView ("diff-view-2" . to_string ())),
              "an empty buffer is not registered" );
    assert! ( f . views_state . graph_straddle . is_some () );
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    handle_close_view_request (
      &mut stream, "((request . \"close view\") (view-id . \"diff-view-1\"))\n",
      &mut f . views_state );
    assert! ( f . views_state . graph_straddle . is_none () );
    Ok (( )) }) }

#[test]
fn a_request_is_refused_under_a_skgrepo_restriction (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let mut restriction : SkgrepoRestriction =
      SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
    restriction . name = "some" . into ();
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
      SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
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
      SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
    for diffView_buffer_kind in DiffViewBufferKind::ALL {
      let view_id : String = format! ( "diff-view: {}", diffView_buffer_kind . wire_name () );
      let (mut stream, read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_diffView_request (
        &mut stream, &request (diffView_buffer_kind . wire_name (), &view_id), &env,
        &mut f . views_state, &all );
      let response : String = everything_written (stream, read_end);
      let registered : Option<DiffViewBufferKind> =
        f . views_state . open_views . views
        . get (&ViewId::ContentView (view_id))
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

/// Send the "all changes" request under the skgrepo set 'all'; return
/// the response, and whether the buffer was registered.
fn request_all_changes (
  f : &mut Fixture,
) -> Result<(String, bool), Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let all : SkgrepoRestriction =
    SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
  let (mut stream, read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  handle_diffView_request (
    &mut stream, &request ("all changes", "diff-view-1"), &env,
    &mut f . views_state, &all );
  let response : String = everything_written (stream, read_end);
  let registered : bool = f . views_state . open_views . views
    . contains_key ( &ViewId::ContentView ("diff-view-1" . to_string ()) );
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
