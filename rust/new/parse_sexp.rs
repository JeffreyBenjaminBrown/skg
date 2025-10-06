// Parse S-expressions from Emacs requests into HeadlineMd2

use crate::types::{HeadlineMd2, RelToOrgParent2, ID};
use sexp::{Atom, Sexp};

pub fn parse_headline_from_sexp (
  request : &str
) -> Result<( HeadlineMd2,
             usize,   // level in org buffer
             String), // title
           String> {
  let sexp : Sexp =
    sexp::parse ( request )
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  let headline_text : String =
    extract_headline_from_sexp ( &sexp ) ?;
  let level : usize =
    count_headline_level ( &headline_text )
    . ok_or ( "Could not count asterisks in (supposed) headline." ) ?;
  let line_after_bullet : &str =
    extract_line_after_bullet ( &headline_text );
  let (metadata, title) : (HeadlineMd2, String) =
    parse_separating_metadata_and_title ( line_after_bullet ) ?;
  Ok (( metadata, level, title )) }

/// Extract the headline value from a parsed S-expression
/// Expected format: (.. (headline . "HEADLINE") ..)
fn extract_headline_from_sexp (
  sexp : &Sexp
) -> Result<String, String> {
  match sexp {
    Sexp::List ( items ) => {
      for item in items {
        if let Sexp::List ( pair ) = item {
          if pair.len() == 3 {
            if let ( Sexp::Atom ( Atom::S ( key ) ),
                     Sexp::Atom ( Atom::S ( dot ) ),
                     Sexp::Atom ( Atom::S ( value ) ) ) =
              ( &pair[0], &pair[1], &pair[2] )
            { if key == "headline" && dot == "." {
              return Ok ( value.clone() ); }} }} }
      Err ( "No headline field found in S-expression"
             . to_string() ) },
    _ => Err ( "Expected list as top-level S-expression"
                . to_string() ) }}

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
/// Returns (HeadlineMd2, title)
fn parse_separating_metadata_and_title (
  line_after_bullet : &str
) -> Result<(HeadlineMd2, String), String> {
  let headline_with_metadata : &str =
    line_after_bullet.trim_start ();
  if let Some ( meta_start ) =
    headline_with_metadata . strip_prefix ( "<skg<" ) {
    if let Some ( end ) = meta_start.find ( ">>" ) {
      let inner : &str =
        &meta_start[..end]; // between "<skg<" and ">>"
      let metadata : HeadlineMd2 =
        parse_metadata_to_orgNodeMd2 ( inner ) ?;
      let title_rest : &str =
        &meta_start[end + 2..]; // skip ">>"
      let title : String =
        title_rest.trim () . to_string ();
      return Ok (( metadata, title )); }}
  // No metadata found - use defaults
  let default_metadata : HeadlineMd2 =
    create_default_orgNodeMd2 ();
  Ok (( default_metadata,
        headline_with_metadata.to_string () )) }

/// Parse metadata string into HeadlineMd2.
/// This is a copy of the function from uninterpreted2.rs
/// to avoid circular dependencies.
fn parse_metadata_to_orgNodeMd2 (
  metadata_str : &str
) -> Result<HeadlineMd2, String> {
  let mut result : HeadlineMd2 =
    create_default_orgNodeMd2 ();
  for part in metadata_str.split ( ',' ) {
    let trimmed : &str = part.trim ();
    if trimmed.is_empty () { continue; }
    if let Some (( key_str, value_str )) = trimmed.split_once ( ':' ) {
      // Handle key:value pairs
      let key : &str = key_str.trim ();
      let value : &str = value_str.trim ();
      match key {
        "id" => { result.id = Some ( ID::from ( value )); },
        "relToOrgParent" => {
          result.relToOrgParent = match value {
            "alias"        => RelToOrgParent2::Alias,
            "aliasCol"     => RelToOrgParent2::AliasCol,
            "container"    => RelToOrgParent2::Container,
            "content"      => RelToOrgParent2::Content,
            "none"         => RelToOrgParent2::None,
            "searchResult" => RelToOrgParent2::SearchResult,
            _ => return Err (
              format! ( "Unknown relToOrgParent value: {}", value )),
          }; },
        _ => {
          return Err ( format! ( "Unknown metadata key: {}", key )); }}
    } else {
      // Handle bare values (boolean flags)
      match trimmed {
        "repeated"         => result.repeat = true,
        "folded"           => result.folded = true,
        "focused"          => result.focused = true,
        "cycle"            => result.cycle = true,
        "mightContainMore" => result.mightContainMore = true,
        "toDelete"         => result.toDelete = true,
        _ => {
          return Err ( format! ( "Unknown metadata value: {}", trimmed ));
        }} }}
  Ok ( result ) }

/// Create default HeadlineMd2 with all default values.
fn create_default_orgNodeMd2 () -> HeadlineMd2 {
  HeadlineMd2 {
    id : None,
    relToOrgParent : RelToOrgParent2::Content,
    cycle : false,
    focused : false,
    folded : false,
    mightContainMore : false,
    repeat : false,
    toDelete : false, }}
