// The "diff view" request at the handler: it registers each
// nonempty buffer as a view of the diff view, answers an empty
// buffer with '(empty . t)', and the graph straddle is dropped when the
// last such view closes. A rerender-all writes back the straddle it
// brought up to date.

use crate::pcsvf::common::git_fixture_helpers::mk_test_tcp_stream_pair;
use crate::pcsvf::common::{open_all_changes_buffer, with_fixture};

use skg::graph_straddle::types::GraphStraddle;
use skg::serve::handlers::close_view::handle_close_view_request;
use skg::serve::handlers::rerender_all_views::handle_rerender_all_views_request;
use skg::serve::handlers::diff_view::handle_diffView_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{graph_handle_from_config, skg_env_from_parts};
use skg::types::env::SkgEnv;
use skg::types::views_state::{DiffViewBufferKind, ViewId};

use std::error::Error;
use std::io::Read;
use std::net::TcpStream;
use std::sync::Arc;

fn request (
  buffer  : &str,
  view_id : &str,
) -> String {
  format! ( "((request . \"diff view\") (buffer . \"{}\") (view-id . \"{}\"))\n",
            buffer, view_id ) }

/// The response the handler wrote to 'stream''s peer.
fn response_from (
  read_end : &mut TcpStream,
) -> String {
  read_end . set_read_timeout ( Some (std::time::Duration::from_secs (5)) ) . unwrap ();
  let mut bytes : Vec<u8> = vec! [ 0; 1 << 20 ];
  let n : usize = read_end . read (&mut bytes) . unwrap ();
  String::from_utf8_lossy (&bytes [.. n]) . to_string () }

#[test]
fn a_request_registers_its_buffer_and_the_last_close_drops_the_straddle (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all : SkgrepoRestriction =
      SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
    { let (mut stream, mut read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_diffView_request (
        &mut stream, &request ("all changes", "diff-view-1"), &env,
        &mut f . views_state, &all );
      let response : String = response_from (&mut read_end);
      assert! ( response . contains ("diff-view")
                && response . contains ("(content"), "{}", response ); }
    { let (mut stream, mut read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_diffView_request (
        &mut stream, &request ("merges", "diff-view-2"), &env,
        &mut f . views_state, &all );
      let response : String = response_from (&mut read_end);
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
    let (mut stream, mut read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    handle_diffView_request (
      &mut stream, &request ("all changes", "diff-view-1"), &env,
      &mut f . views_state, &restriction );
    let response : String = response_from (&mut read_end);
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
    let (mut stream, mut read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    handle_rerender_all_views_request (
      &mut stream, "((request . \"rerender all views\"))\n", &env,
      &mut f . views_state, &all );
    drop (stream);
    let mut response : String = String::new ();
    read_end . read_to_string (&mut response) ?;
    assert! ( response . contains ("rerender-view"), "{}", response );
    let held : Arc<GraphStraddle> =
      f . views_state . graph_straddle . clone ()
      . expect ("the connection still holds a straddle");
    assert! ( Arc::ptr_eq (&held . after, &env . runtime_snapshot () . graph),
              "the held straddle pairs HEAD with the live graph the rerender drew" );
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
      let (mut stream, mut read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_diffView_request (
        &mut stream, &request (diffView_buffer_kind . wire_name (), &view_id), &env,
        &mut f . views_state, &all );
      let response : String = response_from (&mut read_end);
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
