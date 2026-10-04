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
  assert! ( sexp . contains ( r#"(GO INTERC "" staged "staged:""# ),
            "labelled INTERC with quoted empty separator not found" );
  assert! ( sexp . contains ( r#"(GO aliasFolder "aliases")"# ));
  assert! ( sexp . contains ( r#"(GO writeProtected ABUT "☮")"# ));
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

fn shared_json (
  file_name : &str,
) -> serde_json::Value {
  let path : std::path::PathBuf =
    std::path::Path::new ( env! ("CARGO_MANIFEST_DIR") )
    . join ("shared") . join (file_name);
  let text : String = std::fs::read_to_string (&path)
    . unwrap_or_else ( |e| panic! ("could not read {:?}: {}", path, e) );
  serde_json::from_str (&text)
    . unwrap_or_else ( |e| panic! ("could not parse {:?}: {}", path, e) ) }

fn styles_in_rule_table () -> HashSet<HeraldStyle> {
  fn collect (
    rule : &HeraldRule,
    out  : &mut HashSet<HeraldStyle>,
  ) {
    if let Some (style) = rule . style { out . insert (style); }
    for child in & rule . children {
      if let RuleChild::Rule (r) = child { collect (r, out); }}}
  let mut out : HashSet<HeraldStyle> = HashSet::new ();
  collect ( &herald_rule_table (), &mut out );
  out }

// The ten styles are defined in shared/herald-styles.json, which both
// clients read, and every style the rule table names is one of them.
#[test]
fn herald_styles_match_the_shared_style_file () {
  let file : serde_json::Value = shared_json ("herald-styles.json");
  let defined : HashSet<String> =
    file ["styles"] . as_object ()
    . expect ("herald-styles.json: 'styles' should be an object")
    . keys () . cloned () . collect ();
  let enumerated : HashSet<String> =
    HeraldStyle::ALL . iter ()
    . map ( |style| style . name () . to_string () )
    . collect ();
  assert_eq! ( defined, enumerated,
               "HeraldStyle and shared/herald-styles.json disagree" );
  for style in styles_in_rule_table () {
    assert! ( defined . contains ( style . name () ),
              "the rule table names undefined style {}", style . name () ); }}
