/// PURPOSE: Parse headlines:
/// the bullet, the title, and the (s-expr) metadata.

use crate::types::{OrgnodeMetadata, ID, RelToParent};
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
      Sexp::List ( items ) if items . len () >= 2 => {
        let first : String =
          atom_to_string ( &items[0] ) ?;
        match first . as_str () {
          "rels" => {
            // Parse nested rels s-expr
            for rel_element in &items[1..] {
              match rel_element {
                Sexp::List ( kv_pair ) if kv_pair . len () == 2 => {
                  let key : String =
                    atom_to_string ( &kv_pair[0] ) ?;
                  let value : String =
                    atom_to_string ( &kv_pair[1] ) ?;
                  match key . as_str () {
                    "containers" => {
                      result.relationships.numContainers = Some (
                        value.parse::<usize>()
                          . map_err ( |_| format! (
                            "Invalid containers value: {}", value )) ? ); },
                    "contents" => {
                      result.relationships.numContents = Some (
                        value.parse::<usize>()
                          . map_err ( |_| format! (
                            "Invalid contents value: {}", value )) ? ); },
                    "linksIn" => {
                      result.relationships.numLinksIn = Some (
                        value.parse::<usize>()
                          . map_err ( |_| format! (
                            "Invalid linksIn value: {}", value )) ? ); },
                    _ => { return Err ( format! ( "Unknown rels key: {}",
                                                   key )); }} },
                Sexp::Atom ( _ ) => {
                  let bare_value : String =
                    atom_to_string ( rel_element ) ?;
                  match bare_value . as_str () {
                    "notInParent"    => result.relationships.parentIsContainer = false,
                    "containsParent" => result.relationships.parentIsContent   = true,
                    _ => {
                      return Err ( format! ( "Unknown rels value: {}",
                                              bare_value )); }} },
                _ => { return Err ( "Unexpected element in rels"
                                     . to_string () ); }} }},
          "id" => {
            if items . len () != 2 {
              return Err ( "id requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &items[1] ) ?;
            result.id = Some ( ID::from ( value )); },
          "relToParent" => {
            if items . len () != 2 {
              return Err ( "relToParent requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &items[1] ) ?;
            result.relToParent = match value . as_str () {
              "alias"         => RelToParent::Alias,
              "aliasCol"      => RelToParent::AliasCol,
              "content"       => RelToParent::Content,
              "parentIgnores" => RelToParent::ParentIgnores,
              _ => return Err (
                format! ( "Unknown relToParent value: {}", value )),
            }; },
          _ => { return Err ( format! ( "Unknown metadata key: {}",
                                         first )); }} },
      Sexp::Atom ( _ ) => { // This is a bare value
        let bare_value : String =
          atom_to_string ( element ) ?;
        match bare_value . as_str () {
          "repeated"     => result.repeat = true,
          "folded"       => result.folded = true,
          "focused"      => result.focused = true,
          "cycle"        => result.cycle = true,
          "indefinitive" => result.indefinitive = true,
          "toDelete"     => result.toDelete = true,
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
  if metadata.cycle {
    parts.push ( "cycle".to_string () ); }
  if metadata.focused {
    parts.push ( "focused".to_string () ); }
  if metadata.folded {
    parts.push ( "folded".to_string () ); }

  // Build rels s-expr (only if has non-default values)
  let mut rel_parts : Vec<String> = Vec::new ();
  // Only emit if not default (default is true)
  if ! metadata.relationships.parentIsContainer {
    rel_parts.push ( "notInParent".to_string () ); }
  // Only emit if not default (default is false)
  if metadata.relationships.parentIsContent {
    rel_parts.push ( "containsParent".to_string () ); }
  // Only emit if not default (default is Some(1))
  if metadata.relationships.numContainers != Some ( 1 ) {
    if let Some ( count ) = metadata.relationships.numContainers {
      rel_parts.push ( format! ( "(containers {})", count )); } }
  // Only emit if not default (default is Some(0))
  if metadata.relationships.numContents != Some ( 0 ) {
    if let Some ( count ) = metadata.relationships.numContents {
      rel_parts.push ( format! ( "(contents {})", count )); } }
  // Only emit if not default (default is Some(0))
  if metadata.relationships.numLinksIn != Some ( 0 ) {
    if let Some ( count ) = metadata.relationships.numLinksIn {
      rel_parts.push ( format! ( "(linksIn {})", count )); } }

  if ! rel_parts . is_empty () {
    parts.push ( format! ( "(rels {})", rel_parts . join ( " " ))); }

  if metadata.relToParent != RelToParent::Content {
    parts.push ( format! (
      "(relToParent {})", metadata.relToParent )); }
  if metadata.indefinitive {
    parts.push ( "indefinitive".to_string () ); }
  if metadata.repeat {
    parts.push ( "repeated".to_string () ); }
  if metadata.toDelete {
    parts.push ( "toDelete".to_string () ); }

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
