//! PURPOSE: Serve the sentences describing one headline's heralds
//! ('server/herald_descriptions.rs'), for the clients' help on the
//! headline at point. It reads nothing but the request and the
//! config.
//!
//! Request:  ((request . "describe heralds") (metadata . "(skg ...)")
//!            [(folder-change . "processed" | "unprocessed")])
//! Response: ((response-type describe-heralds) (lines "..." ...))
//!           or ((response-type describe-heralds) (error "..."))

use crate::herald_descriptions::describe_heralds;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, value_from_request_sexp};
use crate::sexp_string::quote_sexp_string;
use crate::types::config::SkgConfig;

use std::net::TcpStream;

pub fn handle_describe_heralds_request (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
) {
  send_response_with_length_prefix (
    stream, &describe_heralds_response (request, config) ); }

pub fn describe_heralds_response (
  request : &str,
  config  : &SkgConfig,
) -> String {
  let body : String = match describe_heralds_lines (request, config) {
    Ok (lines) => format! (
      "(lines{})",
      lines . iter () . map ( |line| format! (" {}", quote_sexp_string (line)) )
        . collect::<String> () ),
    Err (message) => format! ("(error {})", quote_sexp_string (&message)), };
  format! ( "((response-type {}) {})",
            TcpToClient::DescribeHeralds . repr_in_client (), body ) }

fn describe_heralds_lines (
  request : &str,
  config  : &SkgConfig,
) -> Result<Vec<String>, String> {
  let metadata : String = value_from_request_sexp ("metadata", request) ?;
  let folder_change : Option<bool> =
    match value_from_request_sexp ("folder-change", request) . ok () . as_deref () {
      Some ("processed")   => Some (true),
      Some ("unprocessed") => Some (false),
      _                    => None, };
  describe_heralds (&metadata, folder_change, config) }
