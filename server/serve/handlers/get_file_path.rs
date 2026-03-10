use crate::serve::protocol::ResponseType;
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
          ResponseType::GetFilePath,
          &format! ( "Error: {}", e ) ));
      return; } };
  let source : String = match value_from_request_sexp (
    "source", request ) {
    Ok  (v) => v,
    Err (e) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          ResponseType::GetFilePath,
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
          ResponseType::GetFilePath,
          &format! ( "Error: {}", e ) ));
      return; } };
  let abs_path : PathBuf = match fs::canonicalize (&raw_path) {
    Ok  (p) => p,
    Err (_) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          ResponseType::GetFilePath,
          & format! ( "File not found: {}", raw_path ) ));
      return; } };
  let data_root : PathBuf = // Canonicalize to match abs_path
    // (both must be absolute for strip_prefix to work).
    fs::canonicalize ( & config . data_root )
    . unwrap_or ( config . data_root . clone () );
  let rel_path : String =
    abs_path
    . strip_prefix (&data_root)
    . map ( |p| p . to_string_lossy () . into_owned () )
    . unwrap_or_else ( |_| abs_path . to_string_lossy ()
                       . into_owned () );
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      ResponseType::GetFilePath, &rel_path )); }
