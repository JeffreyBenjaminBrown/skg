/// PURPOSE: Serve the herald rule table (see server/heralds.rs) and,
/// beside it, the skgrepo label table the clients render a
/// 'repoBoundary' with. Each client requests both once per connection,
/// caches them, and hands the rules to its lens engine; the table
/// itself lives only in Rust.
///
/// Response: (("response-type" "herald-rules") (content "RULES")
///            (skgrepoLabels ("REPO" "LABEL")...))

use crate::heralds::{herald_rules_sexp, skgrepo_labels_sexp};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_sexp_response};
use crate::sexp_string::quote_sexp_string;
use crate::types::misc::SkgConfig;

use std::net::TcpStream;

pub fn handle_herald_rules_request (
  stream : &mut TcpStream,
  config : &SkgConfig,
) {
  send_response_with_length_prefix (
    stream,
    & herald_rules_response (config) ); }

pub fn herald_rules_response (
  config : &SkgConfig,
) -> String {
  tag_sexp_response (
    TcpToClient::HeraldRules,
    & format! ( "((content {}) {})",
                quote_sexp_string ( &herald_rules_sexp () ),
                skgrepo_labels_sexp (config) )) }
