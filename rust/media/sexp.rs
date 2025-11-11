/// General-purpose S-expression utilities.
/// These functions work with any S-expressions,
/// not specific to org-mode or the application domain.

use sexp::{Sexp, Atom};

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
          // Return position after closing paren
          return Some ( i + 1 ); }},
      _ => {} }}
  None // Unbalanced parentheses
}

/// Helper function to extract string value from any Sexp atom.
/// Converts integers and floats to strings as needed.
pub fn atom_to_string (
  atom : &Sexp
) -> Result<String, String> {
  match atom {
    Sexp::Atom ( Atom::S ( s ) ) => Ok ( s . clone () ),
    Sexp::Atom ( Atom::I ( i ) ) => Ok ( i . to_string () ),
    Sexp::Atom ( Atom::F ( f ) ) => Ok ( f . to_string () ),
    _ => Err ( "Expected atom (string, integer, or float)"
                . to_string () ), }}

/// Extract a string value from an S-expression key-value pair.
/// Expected format: (.. (key . "value") ..)
pub fn extract_v_from_kv_pair_in_sexp (
  sexp : &Sexp,
  key  : &str,
) -> Result<String, String> {
  match sexp {
    Sexp::List ( items ) => {
      for item in items {
        if let Sexp::List ( pair ) = item {
          if pair.len() == 3 {
            if let ( Sexp::Atom ( Atom::S ( k ) ),
                     Sexp::Atom ( Atom::S ( dot ) ),
                     Sexp::Atom ( Atom::S ( value ) ) ) =
              ( &pair[0], &pair[1], &pair[2] )
            { if k == key && dot == "." {
              return Ok ( value.clone() ); }} }} }
      Err ( format! (
        "No {} field found in S-expression", key ) ) },
    _ => Err ( "Expected list as top-level S-expression"
                . to_string() ) }}
