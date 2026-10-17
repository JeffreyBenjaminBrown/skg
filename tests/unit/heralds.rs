use super::*;

use std::collections::HashSet;

// The required core of the herald migration: every metadata atom the
// server can emit has a rule, and every rule names a live atom.
// Coverage only -- labels and styles are presentation, free to drift
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
    . filter ( |atom| ! emittable . contains (**atom) )
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
  assert! ( sexp . contains ( r#"(UGLY relRepo (ANY "~" IT))"# ),
            "quoted prefix string not found" );
  assert! ( sexp . contains ( r#"(MESSAGE aliasFolder "aliases")"# ));
  assert! ( sexp . contains ( r#"(CRUCIAL writeProtected ABUT "☮")"# ));
  { let mut depth : i64 = 0; // balanced parens (no parens occur inside the table's strings, so plain counting suffices)
    for c in sexp . chars () {
      match c { '(' => depth += 1,
                ')' => depth -= 1,
                _ => () }
      assert! ( depth >= 0, "unbalanced parens in {}", sexp ); }
    assert_eq! ( depth, 0, "unbalanced parens in {}", sexp ); }}

// tests/shared/herald-rules.sexp lets batch-mode elisp tests inject
// the real table without a running server. It is generated, not
// hand-maintained: regenerate with
//   cargo run --bin emit-herald-rules > tests/shared/herald-rules.sexp
// This test pins it to the live table so it cannot go stale silently.
#[test]
fn elisp_fixture_matches_the_live_table () {
  let fixture_path : std::path::PathBuf =
    std::path::Path::new ( env! ("CARGO_MANIFEST_DIR") )
    . join ("tests/shared/herald-rules.sexp");
  let fixture : String =
    std::fs::read_to_string (&fixture_path)
    . unwrap_or_else ( |e| panic! (
        "could not read {:?}: {}. Generate it with: \
         cargo run --bin emit-herald-rules > tests/shared/herald-rules.sexp",
        fixture_path, e ));
  assert_eq! ( fixture . trim_end (), herald_rules_sexp (),
               "tests/shared/herald-rules.sexp is stale. Regenerate with: \
                cargo run --bin emit-herald-rules > tests/shared/herald-rules.sexp" ); }

// shared/herald-styles.json names only styles it defines, and gives a
// style to every styled atom of the rule table. The server asserts the
// same at startup.
#[test]
fn herald_styles_file_is_internally_consistent () {
  let problems : Vec<String> = herald_styles_inconsistencies ();
  assert! ( problems . is_empty (), "{}", problems . join ("\n") ); }

// The consistency check notices an undefined tier and an unstyled atom.
#[test]
fn herald_styles_consistency_check_reports_problems () {
  let mut file : serde_json::Value = herald_styles () . clone ();
  file ["relationship_heralds"] ["birth_letter"] =
    serde_json::Value::from ("no_such_style");
  file ["node_heralds"] . as_object_mut () . unwrap ()
    . remove ("cycle");
  let problems : Vec<String> = herald_styles_inconsistencies_of (&file);
  assert_eq! ( problems, vec! [
    "relationship_heralds.birth_letter names no_such_style, which is not a key of 'styles'" . to_string (),
    "the rule table styles cycle, which has no entry in 'node_heralds'" . to_string () ] ); }
