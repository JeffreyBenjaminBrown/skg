use crate::diff_analysis::diff_analysis_report_with_ugly_pids;
use crate::diff_analysis::types::DiffSelection;
use crate::serve::protocol::TcpToClient;
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision, decide_for_ugly_pids};
use crate::serve::util::{
  format_buffer_response_sexp,
  send_response_with_length_prefix,
  tag_sexp_response,
  value_from_request_sexp};
use crate::source_sets::{ActiveSourceSet, SourceSetName};
use crate::types::misc::SkgConfig;

use std::net::TcpStream;
use std::collections::HashSet;

pub fn handle_diff_analysis_request (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
) {
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      config,
      SourceSetName::from ("all"))
    . expect ("reserved source-set all should always resolve");
  handle_diff_analysis_request_with_source_set (
    stream, request, config, &active ) }

pub fn handle_diff_analysis_request_with_source_set (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
  active  : &ActiveSourceSet,
) {
  let result : Result<(String, Vec<String>), String> =
    if active . is_all () {
      parse_selection (request)
      . and_then ( |selection|
        diff_analysis_report_with_ugly_pids (config, selection) )
      . map ( |(report, ugly_pids)| {
        let warnings = match decide_for_ugly_pids (
          "diff-analysis", active, ugly_pids, &HashSet::new () ) {
          ScalarReleaseDecision::AllowWithWarning { warning } =>
            vec! [warning],
          _ => Vec::new (), };
        (report, warnings) } )
    } else {
      Err (format! (
        "Diff analysis requires active source-set all; current active source-set is {}",
        active . name )) };
  let (content, errors, warnings)
    : (String, Vec<String>, Vec<String>) =
    match result {
      Ok ((report, warnings)) => (report, Vec::new (), warnings),
      Err (e) => (
        format! ("* diff analysis failed\n** {}\n", e),
        vec! [e], Vec::new () ), };
  let response : String =
    format_buffer_response_sexp (&content, &errors, &warnings);
  let _ = send_response_with_length_prefix (
    stream,
    &tag_sexp_response (TcpToClient::DiffAnalysis, &response) );
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
