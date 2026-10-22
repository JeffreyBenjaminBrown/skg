/// PURPOSE: Serve the warnings the server raised as it started (see
/// 'unknown_top_level_config_key_warnings' and
/// 'foreign_manifest_order_warnings'). Each client asks once per
/// connection, before it requests any view, and shows a non-empty
/// answer in a buffer of its own.

use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, sexp_to_client_string, tag_sexp_response};

use sexp::{Atom, Sexp};
use std::net::TcpStream;

pub fn handle_server_warnings_request (
  stream   : &mut TcpStream,
  warnings : &[String],
) {
  send_response_with_length_prefix (
    stream,
    & server_warnings_response (warnings) ); }

/// '((response-type server-warnings) (warnings ("..." ...)))'.
/// Every warning holds whitespace, so each prints double-quoted.
pub fn server_warnings_response (
  warnings : &[String],
) -> String {
  let payload : Sexp =
    Sexp::List ( vec! [
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S ( "warnings" . to_string () )),
        Sexp::List (
          warnings . iter ()
          . map ( |warning| Sexp::Atom ( Atom::S ( warning . clone () )) )
          . collect () ), ] ), ] );
  tag_sexp_response (
    TcpToClient::ServerWarnings,
    & sexp_to_client_string (&payload) ) }
