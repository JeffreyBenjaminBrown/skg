use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  value_from_request_sexp,
  send_response_with_length_prefix,
  tag_text_response};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::util::path_from_pid_and_source;

use std::fs;
use std::net::TcpStream;
use std::path::PathBuf;

/// Resolves the on-disk file path for a node given its id and source.
/// Returns the path relative to the skgconfig.toml directory.
pub fn handle_get_file_path_request (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
) {
  let id : String = match value_from_request_sexp (
    "id", request ) {
    Ok  (v) => v,
    Err (e) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::GetFilePath,
          &format! ( "Error: {}", e ) ));
      return; } };
  let source : String = match value_from_request_sexp (
    "source", request ) {
    Ok  (v) => v,
    Err (e) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::GetFilePath,
          &format! ( "Error: {}", e ) ));
      return; } };
  let raw_path : String = match path_from_pid_and_source (
    config,
    & SourceName (source),
    ID (id) ) {
    Ok  (p) => p,
    Err (e) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::GetFilePath,
          &format! ( "Error: {}", e ) ));
      return; } };
  // We need both paths canonicalized so that strip_prefix works
  // (e.g. resolving symlinks and ".." segments to get matching
  // prefixes).  But canonicalize fails if the file doesn't exist,
  // so we fall back to the un-canonicalized path, which keeps
  // deleted nodes working (the client needs the path to navigate
  // to the deletion in magit).
  let raw_pathbuf : PathBuf = PathBuf::from (&raw_path);
  let data_root : PathBuf =
    fs::canonicalize ( & config . data_root )
    . unwrap_or ( config . data_root . clone () );
  let canonical_raw : PathBuf =
    fs::canonicalize (&raw_pathbuf)
    . unwrap_or (raw_pathbuf);
  let rel_path : String =
    canonical_raw
    . strip_prefix (&data_root)
    . map ( |p| p . to_string_lossy () . into_owned () )
    . unwrap_or_else ( |_| canonical_raw . to_string_lossy ()
                       . into_owned () );
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::GetFilePath, &rel_path )); }
