//! PURPOSE: a TEST helper for comparing rendered org buffers while
//! ignoring the sibling ORDER inside `(skg ...)` metadata sexps.
//!
//! Metadata is an "unordered tree after the first element": in any
//! metadata list the first element is a keyword that must come first,
//! but the remaining siblings carry no order -- `(node (id x) (source
//! s))` means the same as `(node (source s) (id x))`. This holds
//! recursively, to any depth: `(a (b c d) (e f g))` == `(a (e f g)
//! (b c d))` == `(a (b d c) (e f g))`, but != `(a (c b d) (e f g))`
//! (the inner head `b` is fixed) and != `(b a c ...)` (the outer head).
//!
//! `canonicalize_metadata_ordering` rewrites every `(skg ...)` sub-sexp
//! of a buffer into a canonical form (each list's tail sorted, head
//! left in place), leaving all non-metadata text -- org bullets,
//! titles, bodies, blank lines -- byte-for-byte untouched. Tests then
//! compare two buffers with
//!   assert_eq! ( canonicalize_metadata_ordering (&actual),
//!                canonicalize_metadata_ordering (&expected) );
//! so a change to the ORDER the server emits metadata siblings in never
//! breaks a test, while any change to the CONTENT still does. `assert_eq!`
//! keeps a readable diff (both sides are canonicalized identically, so
//! non-metadata text and structure still line up).
//!
//! CAVEAT: this deliberately makes sibling order invisible EVERYWHERE in
//! metadata, including the ordered relationship-herald spans inside a
//! `(rels ...)` form. Span display-order is pinned separately by the
//! `herald_tokens` unit tests, which assert the assembled order directly.
//!
//! It lives in the library (not under a test module) so every
//! integration-test binary can reach it as
//! `skg::metadata_canon::canonicalize_metadata_ordering`, alongside the
//! other shared test helpers in `skg::test_utils`.

use sexp::{Atom, Sexp};

/// Assert two rendered org buffers equal, ignoring the sibling ORDER
/// inside every `(skg ...)` metadata sexp (see this module's docs).
/// A drop-in for `assert_eq!` at buffer-comparison sites: both sides
/// are run through `canonicalize_metadata_ordering` first, so the diff
/// stays readable and any CONTENT difference still fails. Accepts an
/// optional trailing message like `assert_eq!`.
#[macro_export]
macro_rules! assert_metadata_eq {
  ($actual:expr, $expected:expr $(,)?) => {
    assert_eq! (
      $crate::metadata_canon::canonicalize_metadata_ordering ( &($actual) ),
      $crate::metadata_canon::canonicalize_metadata_ordering ( &($expected) ) ) };
  ($actual:expr, $expected:expr, $($arg:tt)+) => {
    assert_eq! (
      $crate::metadata_canon::canonicalize_metadata_ordering ( &($actual) ),
      $crate::metadata_canon::canonicalize_metadata_ordering ( &($expected) ),
      $($arg)+ ) };
}

/// Rewrite every `(skg ...)` sexp in `buffer` into canonical
/// (tail-sorted) form, leaving all other text untouched. A `(skg ...)`
/// that fails to parse is left verbatim, so the tool never corrupts a
/// buffer it cannot understand -- the comparison simply stays exact
/// there.
pub fn canonicalize_metadata_ordering (
  buffer : &str,
) -> String {
  let mut out : String = String::new ();
  let mut rest : &str = buffer;
  while ! rest . is_empty () {
    if let Some (pos) = rest . find ("(skg") {
      out . push_str ( &rest[.. pos] );
      let after : &str = &rest[pos ..];
      match sexp_byte_len (after) {
        Some (len) => {
          let seg : &str = &after[.. len];
          match sexp::parse (seg) {
            Ok (parsed) => out . push_str ( &canon_sexp (&parsed) ),
            Err (_)     => out . push_str (seg), }
          rest = &after[len ..]; }
        None => { // unbalanced from here: copy the rest verbatim
          out . push_str (after);
          rest = ""; } } }
    else {
      out . push_str (rest);
      rest = ""; } }
  out }

/// Canonical string for a sexp: an atom serialized unambiguously; a
/// list as its head (position 0, unchanged) followed by its remaining
/// elements each canonicalized and then SORTED. Sorting the already-
/// canonical child strings makes two tails that are equal as multisets
/// (order-independent) produce identical output, and keeps duplicates.
pub fn canon_sexp (
  s : &Sexp,
) -> String {
  match s {
    Sexp::Atom (a) => canon_atom (a),
    Sexp::List (items) => {
      if items . is_empty () { return "()" . to_string (); }
      let head : String = canon_sexp ( &items[0] );
      let mut tail : Vec<String> =
        items[1 ..] . iter () . map (canon_sexp) . collect ();
      tail . sort ();
      let mut parts : Vec<String> = Vec::with_capacity (items . len ());
      parts . push (head);
      parts . extend (tail);
      format! ( "({})", parts . join (" ") ) } } }

/// Serialize an atom to a canonical, unambiguous token. Strings are
/// always quoted (via Debug, which escapes quotes/backslashes) so a
/// string can never collide with a differently-typed neighbor; ints and
/// floats print bare. (The sexp parser already collapses a bare symbol
/// and a quoted string to the same `Atom::S`, so we cannot and need not
/// distinguish those -- metadata treats them alike.)
fn canon_atom (
  a : &Atom,
) -> String {
  match a {
    Atom::S (s) => format! ("{:?}", s),
    Atom::I (i) => i . to_string (),
    Atom::F (f) => f . to_string (), } }

/// Byte length of the balanced sexp that `s` STARTS with (s[0] == '('),
/// counting through to and including the matching ')'. Respects
/// double-quoted strings (parens and escaped quotes inside them are
/// literal), so a herald like `(rels (blue "a(b,c)L"))` scans correctly.
/// None if the parens never balance.
fn sexp_byte_len (
  s : &str,
) -> Option<usize> {
  let mut depth   : i32  = 0;
  let mut in_str  : bool = false;
  let mut escaped : bool = false;
  for (idx, ch) in s . char_indices () {
    if in_str {
      if escaped         { escaped = false; }
      else if ch == '\\' { escaped = true; }
      else if ch == '"'  { in_str = false; } }
    else {
      match ch {
        '"' => in_str = true,
        '(' => depth += 1,
        ')' => { depth -= 1;
                 if depth == 0 {
                   return Some ( idx + ch . len_utf8 () ); } }
        _ => {} } } }
  None }

#[cfg(test)]
mod tests {
  use super::*;

  fn canon (s : &str) -> String {
    canon_sexp ( &sexp::parse (s) . unwrap () ) }

  #[test]
  fn tail_is_unordered_but_head_is_fixed () {
    assert_eq! ( canon ("(a b c)"), canon ("(a c b)") );
    assert_ne! ( canon ("(a b c)"), canon ("(b a c)") );
    assert_ne! ( canon ("(a b)"),   canon ("(a c)") ); }

  #[test]
  fn unordering_is_recursive_and_head_fixed_at_each_level () {
    assert_eq! ( canon ("(a (b c d) (e f g))"),
                 canon ("(a (e f g) (b c d))") );
    assert_eq! ( canon ("(a (b c d) (e f g))"),
                 canon ("(a (b d c) (e f g))") );
    // inner head b vs c differs
    assert_ne! ( canon ("(a (b c d) (e f g))"),
                 canon ("(a (c b d) (e f g))") ); }

  #[test]
  fn duplicates_are_kept_as_a_multiset () {
    assert_eq! ( canon ("(a b b c)"), canon ("(a c b b)") );
    assert_ne! ( canon ("(a b b c)"), canon ("(a b c c)") ); }

  #[test]
  fn quoted_string_with_parens_scans_and_canons () {
    // The L-herald token '2(1,1)L' lives inside a quoted string; its
    // parens must not confuse balancing, and it must round-trip.
    let s : &str = r#"(rels (blue "aC3") (sep " ") (purple "2(1,1)L"))"#;
    let c : String = canon (s);
    assert! ( c . contains ("2(1,1)L") );
    // span order inside rels does not matter (documented caveat)
    assert_eq! (
      canon ( r#"(rels (blue "aC3") (purple "2S"))"# ),
      canon ( r#"(rels (purple "2S") (blue "aC3"))"# ) ); }

  #[test]
  fn buffer_reorders_only_skg_metadata_and_preserves_the_rest () {
    let a : &str =
      "* (skg (node (id x) (source main))) A title\nbody line\n";
    let b : &str =
      "* (skg (node (source main) (id x))) A title\nbody line\n";
    assert_eq! ( canonicalize_metadata_ordering (a),
                 canonicalize_metadata_ordering (b) );
    // a differing title (non-metadata text) is still caught
    let c : &str =
      "* (skg (node (source main) (id x))) DIFFERENT\nbody line\n";
    assert_ne! ( canonicalize_metadata_ordering (a),
                 canonicalize_metadata_ordering (c) );
    // a differing id (metadata content) is still caught
    let d : &str =
      "* (skg (node (source main) (id y))) A title\nbody line\n";
    assert_ne! ( canonicalize_metadata_ordering (a),
                 canonicalize_metadata_ordering (d) ); }

  #[test]
  fn multiline_buffer_with_several_headlines () {
    let a : &str = concat! (
      "* (skg (node (id 1) (source main) (parentIs absent))) one\n",
      "** (skg (node (id 2) (source main) indef)) two\n" );
    let b : &str = concat! (
      "* (skg (node (parentIs absent) (source main) (id 1))) one\n",
      "** (skg (node (source main) indef (id 2))) two\n" );
    assert_eq! ( canonicalize_metadata_ordering (a),
                 canonicalize_metadata_ordering (b) );
    // bullets / depth are non-metadata and stay significant
    let c : &str = concat! (
      "* (skg (node (parentIs absent) (source main) (id 1))) one\n",
      "*** (skg (node (source main) indef (id 2))) two\n" );
    assert_ne! ( canonicalize_metadata_ordering (a),
                 canonicalize_metadata_ordering (c) ); }

  #[test]
  fn sourceherald_atom_with_colon_and_nonascii_round_trips () {
    // The ⌂:LABEL sourceHerald and the ☮/⟳ glyphs must survive parse +
    // canon (bytes preserved, just possibly reordered).
    let s : &str =
      "* (skg (node (id 1) (source main) (viewStats cycle (sourceHerald ⌂:main)))) t\n";
    let c : String = canonicalize_metadata_ordering (s);
    assert! ( c . contains ("⌂:main"), "got: {}", c );
    assert! ( c . contains ("cycle") ); }

  #[test]
  fn text_outside_any_skg_form_is_verbatim () {
    let s : &str = "no metadata here, just ( parens ) and text\n";
    assert_eq! ( canonicalize_metadata_ordering (s), s ); }
}
