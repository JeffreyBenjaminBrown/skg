//! Per-hit "coverage multiplier" for the search-result ranker.
//!
//! Tantivy's BM25 score is the sum of per-term contributions: a
//! doc that matches every search term gets only a linear bonus
//! per term, which can lose to a doc that matches one rare term.
//! To bias ranking toward "matched more of what the user asked",
//! we multiply each hit's BM25 score by
//! (matched/total)^SEARCH_COVERAGE_EXPONENT.
//!
//! Skipped (factor=1) in operators-mode queries, where the user
//! has expressed explicit MUST/MUSTNOT semantics that don't
//! translate cleanly to "fraction matched". Applied in literal
//! mode (lowercased substring) and regex mode (case-insensitive
//! Regex match).

use crate::consts::SEARCH_COVERAGE_EXPONENT;
use crate::dbs::tantivy::search::SearchOptions;

use regex::{Regex, RegexBuilder};

/// Pre-built per-search machinery for computing the coverage
/// multiplier of one match-doc.
pub(super) enum CoverageMatcher {
  None, // Skip coverage -- factor is always 1.0. Used for operator-mode queries (where the user has expressed explicit semantics that don't translate cleanly to "fraction of terms matched") and for queries that contain no coverage-relevant tokens at all.
  Literal (Vec<String>), // Literal mode: lowercased substring tokens.
  Regex (Vec<Regex>), // Compiled case-insensitive patterns, one per whitespace-separated piece of the original query, mirroring how 'build_regex_query' splits the input.
}

/// Parse a search query the same way the query builders in
/// dbs/tantivy/search.rs split it -- whitespace pieces minus the
/// boolean keywords, with an optional leading +/- stripped.
fn coverage_pieces (
  search_terms : &str,
) -> Vec<&str> {
  search_terms . split_whitespace ()
    . filter_map ( |w| {
      if matches! (w, "AND" | "OR" | "NOT") { return None; }
      let stripped : &str =
        if let Some (rest) = w . strip_prefix ('+') { rest }
        else if let Some (rest) = w . strip_prefix ('-') { rest }
        else { w };
      if stripped . is_empty () { return None; }
      Some (stripped) } )
    . collect () }

pub(super) fn build_coverage_matcher (
  search_terms : &str,
  search_opts  : &SearchOptions,
) -> CoverageMatcher {
  if search_opts . operators { return CoverageMatcher::None; }
  let pieces : Vec<&str> = coverage_pieces (search_terms);
  if pieces . is_empty () { return CoverageMatcher::None; }
  if search_opts . regex {
    // Compile each piece as a case-insensitive regex. If any one
    // fails to compile, fall back to no-coverage rather than
    // partially counting -- a malformed query should still rank
    // by raw BM25 across whatever Tantivy did manage to match.
    let mut compiled : Vec<Regex> =
      Vec::with_capacity (pieces . len ());
    for p in &pieces {
      match RegexBuilder::new (p) . case_insensitive (true) . build () {
        Ok (r)  => compiled . push (r),
        Err (_) => return CoverageMatcher::None, } }
    CoverageMatcher::Regex (compiled)
  } else {
    CoverageMatcher::Literal (
      pieces . iter () . map ( |p| p . to_lowercase () ) . collect ()) } }

/// Compute the coverage multiplier for one match-doc.
/// SEARCHABLE_TITLE is the doc's title_or_alias text -- the field
/// the index actually matched against. Returns 1.0 when MATCHER is
/// None.
pub(super) fn coverage_factor (
  matcher          : &CoverageMatcher,
  searchable_title : &str,
) -> f32 {
  let (matched, total) : (usize, usize) = match matcher {
    CoverageMatcher::None => return 1.0,
    CoverageMatcher::Literal (tokens) => {
      let normalized : String = searchable_title . to_lowercase ();
      let m : usize = tokens . iter ()
        . filter ( |t| normalized . contains ( t as &str ) )
        . count ();
      (m, tokens . len ()) },
    CoverageMatcher::Regex (patterns) => {
      let m : usize = patterns . iter ()
        . filter ( |r| r . is_match (searchable_title) )
        . count ();
      (m, patterns . len ()) }, };
  let coverage : f32 = matched as f32 / total as f32;
  coverage . powf (SEARCH_COVERAGE_EXPONENT) }
