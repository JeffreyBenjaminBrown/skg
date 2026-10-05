use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  value_from_request_sexp,
  send_response_with_length_prefix,
  tag_text_response};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgConfig, SkgRepoName};
use crate::util::path_from_pid_and_skgrepo;

use std::fs;
use std::net::TcpStream;
use std::path::PathBuf;


pub fn handle_get_file_path_request_with_skgrepo_set (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
  restriction : &SkgrepoRestriction,
) {
  let skgid : ID = match value_from_request_sexp (
    "id", request ) {
    Ok  (v) => ID (v),
    Err (e) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::GetFilePath,
          &format! ( "Error: {}", e ) ));
      return; } };
  let skgrepo : SkgRepoName = match value_from_request_sexp (
    "repo", request ) {
    Ok  (v) => SkgRepoName (v),
    Err (e) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::GetFilePath,
          &format! ( "Error: {}", e ) ));
      return; } };
  if ! restriction . contains_skgrepo (&skgrepo) {
    send_response_with_length_prefix (
      stream,
      & tag_text_response (
        TcpToClient::GetFilePath,
        &format! (
          "Error: repo {} is not in skgrepo restriction {}",
          skgrepo, restriction . name ) ));
    return; }
  let raw_path : String = match path_from_pid_and_skgrepo (
    config,
    & skgrepo,
    skgid ) {
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
