/// PURPOSE: Serve the herald rule table (see server/heralds.rs).
/// Emacs requests it once per connection, caches it, and hands it to
/// the lens engine; the table itself lives only in Rust.

use crate::heralds::herald_rules_sexp;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};

use std::net::TcpStream;

pub fn handle_herald_rules_request (
  stream : &mut TcpStream,
) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::HeraldRules,
      & herald_rules_sexp () )); }
