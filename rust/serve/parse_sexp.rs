// Parse S-expressions from Emacs requests into OrgnodeMetadata

use crate::types::OrgnodeMetadata;
use crate::types::orgnode::{parse_metadata_to_headline_md, default_metadata};
use crate::serve::util::extract_v_from_kv_pair_in_sexp;
use sexp::Sexp;

pub fn parse_headline_from_sexp (
  request : &str
) -> Result<( OrgnodeMetadata,
             usize,   // level in org buffer
             String), // title
           String> {
  let sexp : Sexp =
    sexp::parse ( request )
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  let headline_text : String =
    extract_v_from_kv_pair_in_sexp ( &sexp, "headline" ) ?;
  let level : usize =
    count_headline_level ( &headline_text )
    . ok_or ( "Could not count asterisks in (supposed) headline." ) ?;
  let line_after_bullet : &str =
    extract_line_after_bullet ( &headline_text );
  let (metadata, title) : (OrgnodeMetadata, String) =
    parse_separating_metadata_and_title ( line_after_bullet ) ?;
  Ok (( metadata, level, title )) }

/// Count the number of leading asterisks in a headline
fn count_headline_level (
  line : &str
) -> Option<usize> {
  let mut i : usize = 0;
  let bytes : &[u8] = line.as_bytes();
  while i < bytes.len() && bytes[i] == b'*' {
    i += 1; } // count asterisks
  if i == 0 { return None; } // no stars => not a headline
  if i >= bytes.len() || (bytes[i] != b' ' && bytes[i] != b'\t') {
    return None; } // At least one whitespace must follow asterisks.
  Some ( i ) }

/// Extract the part of the headline after the bullet and whitespace
fn extract_line_after_bullet (
  headline_text : &str
) -> &str {
  let mut i : usize = 0;
  let bytes : &[u8] = headline_text.as_bytes();
  while i < bytes.len() && bytes[i] == b'*' {
    i += 1; } // skip asterisks
  while i < bytes.len() && (bytes[i] == b' ' || bytes[i] == b'\t') {
    i += 1; } // skip whitespace
  &headline_text[i..] }

/// Parse a headline string that might contain metadata and a title.
/// Returns (OrgnodeMetadata, title)
fn parse_separating_metadata_and_title (
  line_after_bullet : &str
) -> Result<(OrgnodeMetadata, String), String> {
  let headline_with_metadata : &str =
    line_after_bullet.trim_start ();
  if let Some ( meta_start ) =
    headline_with_metadata . strip_prefix ( "<skg<" ) {
    if let Some ( end ) = meta_start.find ( ">>" ) {
      let inner : &str =
        &meta_start[..end]; // between "<skg<" and ">>"
      let metadata : OrgnodeMetadata =
        parse_metadata_to_headline_md ( inner ) ?;
      let title_rest : &str =
        &meta_start[end + 2..]; // skip ">>"
      let title : String =
        title_rest.trim () . to_string ();
      return Ok (( metadata, title )); }}
  // No metadata found - use defaults
  Ok (( default_metadata (),
        headline_with_metadata.to_string () )) }
