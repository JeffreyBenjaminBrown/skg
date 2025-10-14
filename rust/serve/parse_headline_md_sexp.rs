/// PURPOSE: Parse headlines:
/// the bullet, the title, and the (s-expr) metadata.

use crate::types::{OrgnodeMetadata, ID, Treatment};
use crate::types::orgnode::default_metadata;
use crate::serve::util::extract_v_from_kv_pair_in_sexp;
use sexp::{Sexp, Atom};


/// Helper function to extract string value from any Sexp atom.
/// Converts integers and floats to strings as needed.
fn atom_to_string (
  atom : &Sexp
) -> Result<String, String> {
  match atom {
    Sexp::Atom ( Atom::S ( s ) ) => Ok ( s . clone () ),
    Sexp::Atom ( Atom::I ( i ) ) => Ok ( i . to_string () ),
    Sexp::Atom ( Atom::F ( f ) ) => Ok ( f . to_string () ),
    _ => Err ( "Expected atom (string, integer, or float)".to_string () ),
  } }

/// Find the end position of an s-expression in a string.
/// Uses simple parenthesis matching to locate the closing paren.
/// Returns the byte position immediately after the closing paren,
/// or None if parentheses are unbalanced.
pub fn find_sexp_end (
  text : &str
) -> Option<usize> {
  let mut depth : i32 = 0;

  for ( i, ch ) in text . char_indices () {
    match ch {
      '(' => depth += 1,
      ')' => {
        depth -= 1;
        if depth == 0 {
          return Some ( i + 1 ); // Return position after closing paren
        }
      },
      _ => {}
    }
  }

  None // Unbalanced parentheses
}

/// Parse metadata from org-mode headline into OrgnodeMetadata.
/// Format: "(skg (id xyz) repeated folded (treatment parentIgnores))"
/// Now uses the sexp crate for proper s-expression parsing.
/// Takes the full s-expression including the "(skg ...)" wrapper.
pub fn parse_metadata_to_orgnodemd (
  sexp_str : &str
) -> Result<OrgnodeMetadata, String> {
  let mut result : OrgnodeMetadata =
    default_metadata ();

  let parsed : Sexp =
    sexp::parse ( sexp_str )
    . map_err ( |e| format! ( "Failed to parse metadata as s-expression: {}", e ) ) ?;

  // Extract the list of elements from (skg ...)
  let elements : &[Sexp] =
    match &parsed {
      Sexp::List ( items ) => {
        // First element should be the symbol 'skg'
        if items . is_empty () {
          return Err ( "Empty metadata s-expression".to_string () ); }
        // Skip the 'skg' symbol and return the rest
        &items[1..]
      },
      _ => return Err ( "Expected metadata to be a list".to_string () ),
    };

  // Process each element
  for element in elements {
    match element {
      Sexp::List ( kv_pair ) if kv_pair . len () == 2 => {
        // This is a (key value) pair
        let key : String =
          atom_to_string ( &kv_pair[0] ) ?;
        let value : String =
          atom_to_string ( &kv_pair[1] ) ?;
        match key . as_str () {
          "id" => { result.id = Some ( ID::from ( value )); },
          "treatment" => {
            result.treatment = match value . as_str () {
              "alias"         => Treatment::Alias,
              "aliasCol"      => Treatment::AliasCol,
              "content"       => Treatment::Content,
              "parentIgnores" => Treatment::ParentIgnores,
              _ => return Err (
                format! ( "Unknown treatment value: {}", value )),
            }; },
          "numContainers" => {
            result.numContainers = Some (
              value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid numContainers value: {}", value )) ? ); },
          "numContents" => {
            result.numContents = Some (
              value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid numContents value: {}", value )) ? ); },
          "numLinksIn" => {
            result.numLinksIn = Some (
              value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid numLinksIn value: {}", value )) ? ); },
          _ => { return Err ( format! ( "Unknown metadata key: {}",
                                         key )); }} },
      Sexp::Atom ( _ ) => { // This is a bare value
        let bare_value : String =
          atom_to_string ( element ) ?;
        match bare_value . as_str () {
          "repeated"           => result.repeat = true,
          "folded"             => result.folded = true,
          "focused"            => result.focused = true,
          "cycle"              => result.cycle = true,
          "mightContainMore"   => result.mightContainMore = true,
          "toDelete"           => result.toDelete = true,
          "parentIsContainer"  => result.parentIsContainer = true,
          "parentIsContent"    => result.parentIsContent = true,
          _ => {
            return Err ( format! ( "Unknown metadata value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in metadata"
                           . to_string () ); }} }
  Ok ( result ) }

/// Renders OrgnodeMetadata as a metadata string suitable for org-mode display.
/// This is the inverse of parse_metadata_to_orgnodemd.
/// Returns string like "(id abc123) repeated focused" etc.
pub fn orgnodemd_to_string (
  metadata : &OrgnodeMetadata
) -> String {
  let mut parts : Vec<String> =
    Vec::new ();
  if let Some ( ref id ) = metadata.id {
    parts.push ( format! ( "(id {})", id.0 )); }
  if metadata.treatment != Treatment::Content {
    parts.push ( format! (
      "(treatment {})", metadata.treatment )); }
  if metadata.repeat {
    parts.push ( "repeated".to_string () ); }
  if metadata.folded {
    parts.push ( "folded".to_string () ); }
  if metadata.focused {
    parts.push ( "focused".to_string () ); }
  if metadata.cycle {
    parts.push ( "cycle".to_string () ); }
  if metadata.mightContainMore {
    parts.push ( "mightContainMore".to_string () ); }
  if metadata.toDelete {
    parts.push ( "toDelete".to_string () ); }
  if metadata.parentIsContainer {
    parts.push ( "parentIsContainer".to_string () ); }
  if metadata.parentIsContent {
    parts.push ( "parentIsContent".to_string () ); }
  if let Some ( count ) = metadata.numContainers {
    parts.push ( format! ( "(numContainers {})", count )); }
  if let Some ( count ) = metadata.numContents {
    parts.push ( format! ( "(numContents {})", count )); }
  if let Some ( count ) = metadata.numLinksIn {
    parts.push ( format! ( "(numLinksIn {})", count )); }
  parts.join ( " " ) }

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

  if headline_with_metadata.starts_with ( "(skg" ) {
    // Find the end of the s-expression
    if let Some ( sexp_end ) = find_sexp_end ( headline_with_metadata ) {
      // Extract the s-expression substring
      let sexp_str : &str = &headline_with_metadata[..sexp_end];

      // Verify it's valid by attempting to parse it
      if let Err ( e ) = sexp::parse ( sexp_str ) {
        return Err ( format! ( "Invalid s-expression syntax: {}", e ) );
      }

      // Parse the metadata from the s-expression
      let metadata : OrgnodeMetadata =
        parse_metadata_to_orgnodemd ( sexp_str ) ?;

      // The title is everything after the s-expression
      let title : String =
        headline_with_metadata[sexp_end..] . trim () . to_string ();

      return Ok (( metadata, title ));
    } else {
      return Err ( "Unclosed metadata parentheses".to_string () );
    }
  }

  // No metadata found - use defaults
  Ok (( default_metadata (),
        headline_with_metadata.to_string () )) }
