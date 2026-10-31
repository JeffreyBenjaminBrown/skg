// A connected pair of TCP streams on the loopback interface, for a
// test that hands a handler a stream and reads what it sent.

use std::net::{SocketAddr, TcpListener, TcpStream};

/// Create a connected TCP stream pair for testing.
/// Returns (write_end, read_end). The write_end is passed
/// to 'update_from_and_rerender_buffer'; the read_end can
/// be used to read streamed collateral-view messages.
pub fn mk_test_tcp_stream_pair ()
  -> (TcpStream, TcpStream)
{ let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : SocketAddr =
    listener . local_addr () . unwrap ();
  let write_end : TcpStream =
    TcpStream::connect (addr) . unwrap ();
  let (read_end, _) =
    listener . accept () . unwrap ();
  (write_end, read_end) }
