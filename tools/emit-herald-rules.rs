/// Print the herald rule table as the sexp the Emacs lens engine
/// interprets. Used to regenerate the batch-test fixture:
///   cargo run --bin emit-herald-rules > tests/elisp/herald-rules.sexp
/// (A Rust unit test, elisp_fixture_matches_the_live_table, fails
/// whenever that fixture is stale.) The production client does not
/// use this; it fetches the same sexp over the "herald rules"
/// endpoint at connect time.

fn main () {
  println! ( "{}", skg::heralds::herald_rules_sexp () ); }
