// The "total diff view" request at the handler: it registers each
// nonempty buffer as a view of the total diff view, answers an empty
// buffer with '(empty . t)', and the graph pair is dropped when the
// last such view closes.

use crate::pcsvf::common::git_fixture_helpers::mk_test_tcp_stream_pair;
use crate::pcsvf::common::with_fixture;

use skg::serve::handlers::close_view::handle_close_view_request;
use skg::serve::handlers::total_diff_view::handle_total_diff_view_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;
use skg::types::views_state::{TdvBuffer, ViewId};

use std::error::Error;
use std::io::Read;
use std::net::TcpStream;

fn request (
  buffer  : &str,
  view_id : &str,
) -> String {
  format! ( "((request . \"total diff view\") (buffer . \"{}\") (view-id . \"{}\"))\n",
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
fn a_request_registers_its_buffer_and_the_last_close_drops_the_pair (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let all : SkgrepoRestriction =
      SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
    { let (mut stream, mut read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_total_diff_view_request (
        &mut stream, &request ("all changes", "tdv-1"), &env,
        &mut f . views_state, &all );
      let response : String = response_from (&mut read_end);
      assert! ( response . contains ("total-diff-view")
                && response . contains ("(content"), "{}", response ); }
    { let (mut stream, mut read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      handle_total_diff_view_request (
        &mut stream, &request ("merges", "tdv-2"), &env,
        &mut f . views_state, &all );
      let response : String = response_from (&mut read_end);
      assert! ( response . contains ("(empty . t)"), "{}", response ); }
    let view_id : ViewId = ViewId::ContentView ("tdv-1" . to_string ());
    assert_eq! ( f . views_state . open_views . views . get (&view_id)
                 . and_then ( |state| state . tdv_buffer ),
                 Some (TdvBuffer::AllChanges) );
    assert! ( ! f . views_state . open_views . views
              . contains_key (&ViewId::ContentView ("tdv-2" . to_string ())),
              "an empty buffer is not registered" );
    assert! ( f . views_state . graph_pair . is_some () );
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    handle_close_view_request (
      &mut stream, "((request . \"close view\") (view-id . \"tdv-1\"))\n",
      &mut f . views_state );
    assert! ( f . views_state . graph_pair . is_none () );
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
    handle_total_diff_view_request (
      &mut stream, &request ("all changes", "tdv-1"), &env,
      &mut f . views_state, &restriction );
    let response : String = response_from (&mut read_end);
    assert! ( response . contains ("requires no skgrepo restriction"), "{}", response );
    assert! ( f . views_state . open_views . views . is_empty () );
    Ok (( )) }) }
