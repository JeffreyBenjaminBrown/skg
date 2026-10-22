use super::*;

#[test]
fn quote_sexp_string_escapes_the_backslash_and_the_quote () {
  assert_eq! ( quote_sexp_string ("a\\b"),     "\"a\\\\b\"" );
  assert_eq! ( quote_sexp_string ("say \"hi\""), "\"say \\\"hi\\\"\"" ); }

#[test]
fn quote_sexp_string_leaves_a_newline_literal () {
  assert_eq! ( quote_sexp_string ("one\ntwo"), "\"one\ntwo\"" ); }

#[test]
fn quote_sexp_string_leaves_sexp_punctuation_alone () {
  // The quotes alone keep ';' and '(' from being read as a comment or
  // a list.
  assert_eq! ( quote_sexp_string ("dir;x/(a).skg"), "\"dir;x/(a).skg\"" ); }

#[test]
fn quote_sexp_string_quotes_the_empty_string () {
  assert_eq! ( quote_sexp_string (""), "\"\"" ); }
