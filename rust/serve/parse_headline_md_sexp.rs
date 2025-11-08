/// PURPOSE: Parse headlines:
/// the bullet, the title, and the (s-expr) metadata.

use crate::types::orgnode::{OrgnodeMetadata, OrgnodeViewData, OrgnodeCode, OrgnodeRelationships, RelToParent, EditRequest, ViewRequest, default_metadata};
use crate::types::misc::ID;
use crate::serve::util::extract_v_from_kv_pair_in_sexp;
use sexp::{Sexp, Atom};
use std::collections::HashSet;
use std::str::FromStr;


/* -------- Public entry points -------- */

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

/// Renders OrgnodeMetadata as a metadata string suitable for org-mode display.
/// This is the inverse of parse_metadata_to_orgnodemd.
/// Returns string like "(id abc123) (view ...) (code ...)" etc.
pub fn orgnodemd_to_string (
  metadata : &OrgnodeMetadata
) -> String {
  let mut parts : Vec<String> =
    Vec::new ();

  if let Some ( ref id ) = metadata.id {
    parts.push ( format! ( "(id {})", id.0 )); }

  // Build view s-expr
  let mut view_parts : Vec<String> = Vec::new ();
  if metadata.viewData.cycle {
    view_parts.push ( "cycle".to_string () ); }
  if metadata.viewData.focused {
    view_parts.push ( "focused".to_string () ); }
  if metadata.viewData.folded {
    view_parts.push ( "folded".to_string () ); }
  if metadata.viewData.repeat {
    view_parts.push ( "repeated".to_string () ); }

  // Build rels s-expr (only if has non-default values)
  let mut rel_parts : Vec<String> = Vec::new ();
  // Only emit if not default (default is true)
  if ! metadata.viewData.relationships.parentIsContainer {
    rel_parts.push ( "notInParent".to_string () ); }
  // Only emit if not default (default is false)
  if metadata.viewData.relationships.parentIsContent {
    rel_parts.push ( "containsParent".to_string () ); }
  // Only emit if not default (default is Some(1))
  if metadata.viewData.relationships.numContainers != Some ( 1 ) {
    if let Some ( count )
      = metadata.viewData.relationships.numContainers {
        rel_parts.push ( format! ( "(containers {})", count )); }}
  // Only emit if not default (default is Some(0))
  if metadata.viewData.relationships.numContents != Some ( 0 ) {
    if let Some ( count )
      = metadata.viewData.relationships.numContents {
        rel_parts.push ( format! ( "(contents {})", count )); }}
  // Only emit if not default (default is Some(0))
  if metadata.viewData.relationships.numLinksIn != Some ( 0 ) {
    if let Some ( count )
      = metadata.viewData.relationships.numLinksIn {
        rel_parts.push ( format! ( "(linksIn {})", count )); }}

  if ! rel_parts . is_empty () {
    view_parts.push ( format! ( "(rels {})",
                                  rel_parts . join ( " " ))); }

  if ! view_parts . is_empty () {
    parts.push ( format! ( "(view {})",
                             view_parts . join ( " " ))); }

  // Build code s-expr
  let mut code_parts : Vec<String> = Vec::new ();
  if metadata.code.relToParent != RelToParent::Content {
    code_parts.push ( format! (
      "(relToParent {})", metadata.code.relToParent )); }
  if metadata.code.indefinitive {
    code_parts.push ( "indefinitive".to_string () ); }

  // Handle editRequest (toDelete or merge)
  if let Some(ref edit_req) = metadata.code.editRequest {
    code_parts.push ( edit_req . to_string () ); }

  // Build viewRequests s-expr (inside code)
  if ! metadata.code.viewRequests . is_empty () {
    let mut request_strings : Vec<String> =
      metadata.code.viewRequests . iter ()
      . map ( | req | req . to_string () )
      . collect ();
    request_strings . sort (); // Ensure consistent ordering
    code_parts.push ( format! ( "(viewRequests {})",
                                  request_strings . join ( " " ))); }

  if ! code_parts . is_empty () {
    parts.push ( format! ( "(code {})",
                             code_parts . join ( " " ))); }
  parts.join ( " " ) }


/* -------- Level 1: Direct dependencies of entry points -------- */

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


/* -------- Level 2: Dependencies of level 1 -------- */

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
/// Format: "(skg (id xyz) (view ...) (code (requests ...) ...))"
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
      Sexp::List ( items ) if items . len () >= 1 => {
        let first : String =
          atom_to_string ( &items[0] ) ?;
        match first . as_str () {
          "id" => {
            if items . len () != 2 {
              return Err ( "id requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &items[1] ) ?;
            result.id = Some ( ID::from ( value )); },
          "view" => {
            parse_view_sexp ( &items[1..], &mut result . viewData ) ?; },
          "code" => {
            parse_code_sexp ( &items[1..], &mut result . code ) ?; },
          _ => { return Err ( format! ( "Unknown metadata key: {}",
                                         first )); }} },
      _ => { return Err ( format! (
        "Unexpected element '{}' in metadata sexp: {}",
        element, sexp_str )); }} }
  Ok ( result ) }


/* -------- Level 3: Dependencies of level 2 -------- */

/// Parse the (view ...) s-expression and update viewData.
fn parse_view_sexp (
  items : &[Sexp],
  view_data : &mut OrgnodeViewData
) -> Result<(), String> {
  for view_element in items {
    match view_element {
      Sexp::List ( subitems ) if subitems . len () >= 2 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        if key == "rels" {
          parse_rels_sexp ( &subitems[1..], &mut view_data . relationships ) ?;
        } else {
          return Err ( format! ( "Unknown view key: {}", key )); }
      },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( view_element ) ?;
        match bare_value . as_str () {
          "cycle"    => view_data . cycle = true,
          "focused"  => view_data . focused = true,
          "folded"   => view_data . folded = true,
          "repeated" => view_data . repeat = true,
          _ => {
            return Err ( format! ( "Unknown view value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in view"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the (code ...) s-expression and update code.
fn parse_code_sexp (
  items : &[Sexp],
  code : &mut OrgnodeCode
) -> Result<(), String> {
  for code_element in items {
    match code_element {
      Sexp::List ( kv_pair ) if kv_pair . len () == 2 => {
        let key : String =
          atom_to_string ( &kv_pair[0] ) ?;
        match key . as_str () {
          "relToParent" => {
            let value : String =
              atom_to_string ( &kv_pair[1] ) ?;
            code . relToParent = match value . as_str () {
              "alias"         => RelToParent::Alias,
              "aliasCol"      => RelToParent::AliasCol,
              "content"       => RelToParent::Content,
              "parentIgnores" => RelToParent::ParentIgnores,
              _ => return Err (
                format! ( "Unknown relToParent value: {}", value )),
            }; },
          "merge" => {
            // (merge id) sets editRequest to Merge(id)
            let id_str : String = atom_to_string ( &kv_pair[1] ) ?;
            code . editRequest = Some (
              EditRequest::Merge ( ID::from ( id_str ))); },
          "viewRequests" => {
            parse_viewrequests_sexp (
              &kv_pair[1..], &mut code . viewRequests ) ?; },
          _ => { return Err ( format! ( "Unknown code key: {}",
                                         key )); }} },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( code_element ) ?;
        match bare_value . as_str () {
          "indefinitive" => code . indefinitive = true,
          "toDelete"     => code . editRequest =
            Some ( EditRequest::Delete ),
          _ => { return Err ( format! ( "Unknown code value: {}",
                                         bare_value )); }} },
      _ => { return Err ( "Unexpected element in code"
                           . to_string () ); }} }
  Ok (( )) }


/* -------- Level 4: Dependencies of level 3 -------- */

/// Parse the (rels ...) s-expression and update relationships.
fn parse_rels_sexp (
  items : &[Sexp],
  relationships : &mut OrgnodeRelationships
) -> Result<(), String> {
  for rel_element in items {
    match rel_element {
      Sexp::List ( kv_pair ) if kv_pair . len () == 2 => {
        let rel_key : String =
          atom_to_string ( &kv_pair[0] ) ?;
        let rel_value : String =
          atom_to_string ( &kv_pair[1] ) ?;
        match rel_key . as_str () {
          "containers" => {
            relationships . numContainers = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid containers value: {}", rel_value )) ? ); },
          "contents" => {
            relationships . numContents = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid contents value: {}", rel_value )) ? ); },
          "linksIn" => {
            relationships . numLinksIn = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid linksIn value: {}", rel_value )) ? ); },
          _ => { return Err ( format! ( "Unknown rels key: {}",
                                         rel_key )); }} },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( rel_element ) ?;
        match bare_value . as_str () {
          "notInParent"    => relationships . parentIsContainer = false,
          "containsParent" => relationships . parentIsContent   = true,
          _ => {
            return Err ( format! ( "Unknown rels value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in rels"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the (viewRequests ...) s-expression and update viewRequests.
fn parse_viewrequests_sexp (
  items : &[Sexp],
  requests : &mut HashSet<ViewRequest>
) -> Result<(), String> {
  for request_element in items {
    match request_element {
      Sexp::Atom ( _ ) => {
        let request_str : String =
          atom_to_string ( request_element ) ?;
        let request : ViewRequest =
          ViewRequest::from_str ( &request_str )
          . map_err (
            | e | format! ( "Invalid view request: {}", e )) ?;
        requests . insert ( request ); },
      _ => { return Err (
        "Unexpected element in viewRequests (expected atoms)"
          . to_string () ); }} }
  Ok (( )) }


/* -------- Helper functions -------- */

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
