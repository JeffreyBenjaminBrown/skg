use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::export_org::{export_to_org, ExportReport};
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
/// into a user-chosen directory. Two request fields:
/// `(source-set . "NAME")` -- the set the client picked (with its
/// circular selector) -- and `(output-dir . "PATH")` -- where to
/// write, resolved against the server's working directory (its
/// project root); a relative PATH lands under it, an absolute PATH
/// is used as-is. `output-dir` defaults to "org-exports" when
/// absent or blank. Reads .skg files fresh from disk, so the export
/// reflects current on-disk state. Needs neither TypeDB nor Tantivy.
pub fn handle_export_to_org_request (
  stream  : &mut TcpStream,
  config  : &SkgConfig,
  request : &str,
) {
  let result : Result<(String, Vec<String>), String> = ( || {
    let name : String =
      value_from_request_sexp ("source-set", request) ?;
    let active : ActiveSourceSet =
      ActiveSourceSet::named (config, SourceSetName::from (name))
      . map_err ( |e| e . to_string () ) ?;
    let nodes : Vec<NodeComplete> =
      read_all_skg_files_from_sources (config)
      . map_err ( |e| format! ("Reading .skg files: {}", e) ) ?;
    let output_dir : String =
      match value_from_request_sexp ("output-dir", request) {
        Ok (d) if ! d . trim () . is_empty () => d,
        _ => "org-exports" . to_string (), };
    let output_base : PathBuf =
      std::env::current_dir ()
      . map_err ( |e| format! ("current_dir: {}", e) ) ?
      . join (&output_dir); // join with an absolute PATH yields PATH
    let report : ExportReport =
      export_to_org (&active, &nodes, &output_base)
      . map_err ( |e| format! ("Export failed: {}", e) ) ?;
    Ok (( report . summary (), report . warnings )) } ) ();
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
