use crate::diff_report::diff_report_as_org_with_overPrivateText_pids;
use crate::diff_report::types::DiffSelection;
use crate::serve::protocol::TcpToClient;
use crate::serve::handlers::text_release::{
  TextReleaseDecision, decide_for_overPrivateText_pids};
use crate::serve::util::{
  format_buffer_response_sexp,
  send_response_with_length_prefix,
  tag_sexp_response,
  value_from_request_sexp};
use crate::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName};
use crate::types::misc::SkgConfig;

use std::net::TcpStream;
use std::collections::HashSet;

pub fn handle_diff_report_request (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
) {
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (
      config,
      SkgrepoSetName::from ("all"))
    . expect ("reserved repo-set all should always resolve");
  handle_diff_report_request_with_repo_set (
    stream, request, config, &restriction ) }

pub fn handle_diff_report_request_with_repo_set (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
  restriction : &SkgrepoRestriction,
) {
  let result : Result<(String, Vec<String>), String> =
    if restriction . is_all () {
      parse_selection (request)
      . and_then ( |selection|
        diff_report_as_org_with_overPrivateText_pids (config, selection) )
      . map ( |(report, overPrivateText_pids)| {
        let warnings = match decide_for_overPrivateText_pids (
          "diff-report", restriction, overPrivateText_pids, &HashSet::new () ) {
          TextReleaseDecision::AllowWithWarning { warning } =>
            vec! [warning],
          _ => Vec::new (), };
        (report, warnings) } )
    } else {
      Err (format! (
        "Diff report requires no skgrepo restriction (the skgrepo-set all); current skgrepo restriction is {}",
        restriction . name )) };
  let (content, errors, warnings)
    : (String, Vec<String>, Vec<String>) =
    match result {
      Ok ((report, warnings)) => (report, Vec::new (), warnings),
      Err (e) => (
        format! ("* diff report failed\n** {}\n", e),
        vec! [e], Vec::new () ), };
  let response : String =
    format_buffer_response_sexp (&content, &errors, &warnings);
  send_response_with_length_prefix (
    stream,
    &tag_sexp_response (TcpToClient::DiffReport, &response) );
}

fn parse_selection (
  request : &str,
) -> Result<DiffSelection, String> {
  let include_staged : bool =
    bool_from_request ("include-staged", request) ?;
  let include_unstaged : bool =
    bool_from_request ("include-unstaged", request) ?;
  Ok ( DiffSelection { include_staged, include_unstaged } )
}

fn bool_from_request (
  key     : &str,
  request : &str,
) -> Result<bool, String> {
  let value : String =
    value_from_request_sexp (key, request) ?;
  match value . as_str () {
    "true"  => Ok (true),
    "false" => Ok (false),
    _ => Err ( format! (
      "Expected '{}' to be \"true\" or \"false\", got {:?}",
      key, value )), } }
