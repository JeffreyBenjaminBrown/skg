use super::*;

use std::collections::HashSet;

/// Atoms that legitimately appear in the rule table although the
/// server never emits them:
/// - "container" is injected by Emacs's normalization
///   ('heralds--inject-default-parentIs') for implicit
///   parentIs=affected content;
/// - "affected" is accepted by the parser but left implicit by the
///   serializer; its vacuous rule documents that.
const ACCEPTED_NOT_EMITTED_ATOMS : [&str; 2] =
  [ "container", "affected" ];

// The required core of the herald migration: every metadata atom the
// server can emit has a rule, and every rule names a live atom.
// Coverage only -- labels and colors are presentation, free to drift
// without ceremony (decided 2026-06-11).
#[test]
fn herald_rules_cover_the_emittable_vocabulary () {
  let table : HashSet<&'static str> = atoms_in_rule_table ();
  let emittable : HashSet<&'static str> = emittable_metadata_atoms ();
  let missing_rules : Vec<&&str> =
    emittable . iter ()
    . filter ( |atom| ! table . contains (**atom) )
    . collect ();
  assert! ( missing_rules . is_empty (),
            "server-emittable atoms with no herald rule: {:?}",
            missing_rules );
  let dead_rules : Vec<&&str> =
    table . iter ()
    . filter ( |atom| ! emittable . contains (**atom)
               && ! ACCEPTED_NOT_EMITTED_ATOMS . contains (atom) )
    . collect ();
  assert! ( dead_rules . is_empty (),
            "herald rules matching atoms the server cannot emit: {:?}",
            dead_rules ); }

// The string-vs-symbol distinction is load-bearing for the lens
// engine (an unquoted empty INTERC separator would vanish; an
// unquoted prefix string would be misread as an INTERC label), so the
// serializer must quote every string, including the empty one.
#[test]
fn herald_rules_sexp_quotes_strings () {
  let sexp : String = herald_rules_sexp ();
  assert! ( sexp . starts_with ("(skg ") );
  assert! ( sexp . contains ( r#"(GREEN INTERC "" staged "staged:""# ),
            "labelled INTERC with quoted empty separator not found" );
  assert! ( sexp . contains ( r#"(GREEN aliasCol "aliases")"# ));
  assert! ( sexp . contains ( r#"(GREEN indef ABUT "☮")"# ));
  { let mut depth : i64 = 0; // balanced parens (no parens occur inside the table's strings, so plain counting suffices)
    for c in sexp . chars () {
      match c { '(' => depth += 1,
                ')' => depth -= 1,
                _ => () }
      assert! ( depth >= 0, "unbalanced parens in {}", sexp ); }
    assert_eq! ( depth, 0, "unbalanced parens in {}", sexp ); }}

// tests/elisp/herald-rules.sexp lets batch-mode elisp tests inject
// the real table without a running server. It is generated, not
// hand-maintained: regenerate with
//   cargo run --bin emit-herald-rules > tests/elisp/herald-rules.sexp
// This test pins it to the live table so it cannot go stale silently.
#[test]
fn elisp_fixture_matches_the_live_table () {
  let fixture_path : std::path::PathBuf =
    std::path::Path::new ( env! ("CARGO_MANIFEST_DIR") )
    . join ("tests/elisp/herald-rules.sexp");
  let fixture : String =
    std::fs::read_to_string (&fixture_path)
    . unwrap_or_else ( |e| panic! (
        "could not read {:?}: {}. Generate it with: \
         cargo run --bin emit-herald-rules > tests/elisp/herald-rules.sexp",
        fixture_path, e ));
  assert_eq! ( fixture . trim_end (), herald_rules_sexp (),
               "tests/elisp/herald-rules.sexp is stale. Regenerate with: \
                cargo run --bin emit-herald-rules > tests/elisp/herald-rules.sexp" ); }
