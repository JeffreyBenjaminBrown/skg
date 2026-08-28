use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::export_org::{
  export_candidate_pids, export_to_org, ExportReport};
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision,
  approved_pids_from_request,
  challenge_response,
  decide_for_ugly_pids};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  format_buffer_response_sexp,
  send_response_with_length_prefix,
  tag_sexp_response,
  value_from_request_sexp};
use crate::source_sets::{ActiveSourceSet, SourceSetName};
use crate::types::misc::SkgConfig;
use crate::types::nodes::complete::NodeComplete;

use std::net::TcpStream;
use std::path::PathBuf;

/// Export every export root, limited to the requested source-set,
/// into a chosen directory. Two REQUIRED request fields:
/// `(source-set . "NAME")` -- the set the client picked (with its
/// circular selector) -- and `(output-dir . "PATH")` -- where to
/// write, resolved against the server's working directory (its
/// project root); a relative PATH lands under it, an absolute PATH
/// is used as-is. The server applies no default for either: the
/// client supplies the user a default but always sends a value, so
/// a missing or blank `output-dir` is an error here, not a silent
/// "org-exports". Reads .skg files fresh from disk, so the export
/// reflects current on-disk state. Needs neither TypeDB nor Tantivy.
pub fn handle_export_to_org_request (
  stream  : &mut TcpStream,
  config  : &SkgConfig,
  request : &str,
) {
  let prepared : Result<
    (ActiveSourceSet, Vec<NodeComplete>, PathBuf), String> = ( || {
    let name : String =
      value_from_request_sexp ("source-set", request) ?;
    let active : ActiveSourceSet =
      ActiveSourceSet::named (config, SourceSetName::from (name))
      . map_err ( |e| e . to_string () ) ?;
    let nodes : Vec<NodeComplete> =
      read_all_skg_files_from_sources (config)
      . map_err ( |e| format! ("Reading .skg files: {}", e) ) ?;
    let output_dir : String =
      value_from_request_sexp ("output-dir", request)
      . map_err ( |e| format! ("output-dir is required: {}", e) ) ?;
    if output_dir . trim () . is_empty () {
      return Err ( "output-dir is required but blank" . to_string () ); }
    let output_base : PathBuf =
      std::env::current_dir ()
      . map_err ( |e| format! ("current_dir: {}", e) ) ?
      . join (&output_dir); // join with an absolute PATH yields PATH
    Ok ((active, nodes, output_base)) } ) ();
  let (active, nodes, output_base) = match prepared {
    Ok (prepared) => prepared,
    Err (error) => {
      send_export_result (stream, Err (error));
      return; }};
  let candidate_pids = export_candidate_pids (&active, &nodes);
  let ugly_pids = candidate_pids . into_iter ()
    . filter ( |pid| nodes . iter () . any (
      |node| node . pid == *pid && node . ugly_telescope ) )
    . collect ();
  let release = decide_for_ugly_pids (
    "export-to-org", &active, ugly_pids,
    &approved_pids_from_request (request) );
  if matches! (release, ScalarReleaseDecision::Challenge { .. }) {
    send_response_with_length_prefix (
      stream, &challenge_response (&release) . unwrap () );
    return; }
  let release_warning : Option<String> = match release {
    ScalarReleaseDecision::AllowWithWarning { warning } => Some (warning),
    _ => None, };
  let result : Result<(String, Vec<String>), String> =
    export_to_org (&active, &nodes, &output_base)
    . map ( |report : ExportReport| {
      let summary : String = report . summary ();
      let mut warnings = report . warnings;
      if let Some (warning) = release_warning {
        warnings . insert (0, warning); }
      (summary, warnings) } )
    . map_err ( |error| format! ("Export failed: {}", error) );
  send_export_result (stream, result); }

fn send_export_result (
  stream : &mut TcpStream,
  result : Result<(String, Vec<String>), String>,
) {
  let (content, errors, warnings)
    : (String, Vec<String>, Vec<String>) =
    match result {
      Ok ((content, warnings)) => (content, Vec::new (), warnings),
      Err (e) => (
        format! ("Export to org failed: {}", e),
        vec! [e],
        Vec::new () ), };
  let response : String =
    format_buffer_response_sexp (&content, &errors, &warnings);
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (TcpToClient::ExportToOrg, &response) ); }
