// PURPOSE: The text-search API. Builds Tantivy queries from a string
// (either through the QueryParser with optional operator syntax, or
// through RegexQuery), runs them against the index, and returns
// (score, DocAddress) pairs plus the Searcher so callers can fetch
// stored fields.

use crate::dbs::tantivy::escape::{escape_tantivy_intra_word, escape_tantivy_literal};
use crate::types::misc::TantivyIndex;

use tantivy::{IndexReader, Searcher};
use tantivy::collector::TopDocs;
use tantivy::query::{QueryParser, Query};
use tantivy::schema;

/// Options for `search_index`. Defaults: all false — literal search
/// across titles+aliases only, OR between words, operator chars
/// treated as text.
#[derive (Clone, Copy, Debug, Default)]
pub struct SearchOptions {
  pub regex     : bool, // Interpret the query as a per-token regex. Bypasses QueryParser and builds RegexQuery directly.
  pub body      : bool, // Also search node bodies (titles always searched).
  pub operators : bool, // Honor AND/OR/NOT/+/- operators between words. In non-regex mode these pass through Tantivy's QueryParser; in regex mode they combine per-piece RegexQueries at the document level.
}

/// Returns ALL matching Tantivy "Documents" (see glossary).
/// No limit: context-based reranking (multipliers up to 100x)
/// can reorder results arbitrarily, so the caller must see
/// every match. The caller truncates after reranking.
pub fn search_index (
  tantivy_index : &TantivyIndex,
  query_text    : &str,
  opts          : &SearchOptions,
) -> Result <
    ( Vec< ( f32, // relevance score
             tantivy::DocAddress )>, // ID for a tantivy Document. (Not a filepath.)
      Searcher ),
    Box <dyn std::error::Error> > {

  tracing::info! (
    query = query_text,
    regex = opts . regex,
    body = opts . body,
    operators = opts . operators,
    "Finding matches." );
  let searcher : Searcher = {
    let reader : IndexReader =
      tantivy_index . index . reader () ?;
    reader } . searcher();
  let query : Box < dyn Query > =
    if opts . regex {
      build_regex_query (
        tantivy_index, query_text, opts . body, opts . operators ) ?
    } else {
      build_parser_query (
        tantivy_index, query_text, opts . operators, opts . body ) ? };
  Ok (( {
    let best_matches : Vec < ( f32, tantivy::DocAddress ) > =
      searcher . search (
        &query, &TopDocs::with_limit (
          crate::consts::TANTIVY_SEARCH_LIMIT )
          . order_by_score () )?;
    best_matches },
       searcher )) }

fn build_parser_query (
  tantivy_index : &TantivyIndex,
  query_text    : &str,
  operators     : bool,
  body          : bool,
) -> Result < Box < dyn Query >, Box < dyn std::error::Error >> {
  let preprocessed : String =
    if operators { escape_tantivy_intra_word ( query_text ) }
    else         { escape_tantivy_literal    ( query_text ) };
  let fields : Vec<schema::Field> =
    if body {
      vec! [ tantivy_index . title_or_alias_field,
             tantivy_index . body_field ]
    } else {
      vec! [ tantivy_index . title_or_alias_field ] };
  let query_parser : QueryParser =
    QueryParser::for_index ( &tantivy_index . index, fields );
  Ok ( query_parser . parse_query ( &preprocessed ) ? ) }

/// Tantivy tokenizes titles and bodies on whitespace, so no stored
/// token ever contains a space. A regex pattern spanning spaces
/// could therefore never match. We split on whitespace and treat
/// each piece as its own RegexQuery.
///
/// With operators=false, pieces are SHOULD-combined (OR) — mirroring
/// the OR-between-words default of the parser path.
///
/// With operators=true, the keywords AND / OR / NOT and the
/// prefixes +foo / -foo combine pieces at the document level:
/// `+foo` and a piece adjacent to `AND` become MUST; `-foo` and a
/// piece preceded by `NOT` become MUST_NOT; everything else is
/// SHOULD. Keywords are consumed rather than being treated as
/// patterns.
fn build_regex_query (
  tantivy_index : &TantivyIndex,
  pattern       : &str,
  body          : bool,
  operators     : bool,
) -> Result < Box < dyn Query >, Box < dyn std::error::Error >> {
  use tantivy::query::{BooleanQuery, Occur, RegexQuery};
  let fields : Vec<schema::Field> =
    if body {
      vec! [ tantivy_index . title_or_alias_field,
             tantivy_index . body_field ]
    } else {
      vec! [ tantivy_index . title_or_alias_field ] };
  let assignments : Vec<(Occur, String)> =
    if operators { regex_pieces_with_operators (pattern) }
    else         { regex_pieces_plain         (pattern) };
  // Fall back to the raw pattern if parsing yielded nothing (e.g.
  // pattern is all whitespace or all keywords). Tantivy will then
  // surface whatever error it emits for that input.
  let effective : Vec<(Occur, String)> =
    if assignments . is_empty () {
      vec! [ ( Occur::Should, pattern . to_string () ) ]
    } else { assignments };
  let mut clauses : Vec<(Occur, Box<dyn Query>)> =
    Vec::with_capacity ( effective . len () );
  for (occur, piece) in &effective {
    let mut per_field : Vec<(Occur, Box<dyn Query>)> =
      Vec::with_capacity ( fields . len () );
    for field in &fields {
      let rq : RegexQuery =
        RegexQuery::from_pattern (piece, *field) ?;
      per_field . push (
        ( Occur::Should,
          Box::new (rq) as Box<dyn Query> )); }
    let piece_query : Box<dyn Query> =
      if per_field . len () == 1 {
        per_field . pop () . unwrap () . 1
      } else {
        Box::new ( BooleanQuery::new (per_field) ) };
    clauses . push ( (*occur, piece_query) ); }
  if clauses . len () == 1 && clauses [0] . 0 == Occur::Should {
    let (_, q) : (Occur, Box<dyn Query>) =
      clauses . pop () . unwrap ();
    return Ok (q); }
  Ok ( Box::new ( BooleanQuery::new (clauses) ) ) }

/// Plain split: every non-empty piece becomes a SHOULD clause.
fn regex_pieces_plain (
  pattern : &str,
) -> Vec < (tantivy::query::Occur, String) > {
  pattern . split_whitespace ()
    . map ( |w| (tantivy::query::Occur::Should, w . to_string ()) )
    . collect () }

/// Parse a regex query with operator syntax. Keywords AND / OR /
/// NOT are consumed; +foo and -foo force the occurrence of that
/// piece; piece adjacent to AND becomes MUST; piece preceded by
/// NOT becomes MUST_NOT. Empty patterns (e.g. a bare +) are
/// dropped.
fn regex_pieces_with_operators (
  pattern : &str,
) -> Vec < (tantivy::query::Occur, String) > {
  use tantivy::query::Occur;
  let pieces : Vec<&str> =
    pattern . split_whitespace () . collect ();
  let is_keyword = | w : &str | -> bool {
    w == "AND" || w == "OR" || w == "NOT" };
  let mut result : Vec<(Occur, String)> = Vec::new ();
  let mut i : usize = 0;
  while i < pieces . len () {
    let w : &str = pieces [i];
    if is_keyword (w) { i += 1; continue; }
    let (pat, forced) : (&str, Option<Occur>) =
      if let Some (rest) = w . strip_prefix ('+') {
        (rest, Some (Occur::Must))
      } else if let Some (rest) = w . strip_prefix ('-') {
        (rest, Some (Occur::MustNot))
      } else {
        (w, None) };
    if pat . is_empty () { i += 1; continue; }
    let occur : Occur =
      if let Some (f) = forced { f }
      else {
        let prev_not : bool =
          i > 0 && pieces [i - 1] == "NOT";
        let prev_and : bool =
          i > 0 && pieces [i - 1] == "AND";
        let next_and : bool =
          i + 1 < pieces . len () && pieces [i + 1] == "AND";
        if prev_not          { Occur::MustNot }
        else if prev_and
             || next_and      { Occur::Must }
        else                  { Occur::Should } };
    result . push ( (occur, pat . to_string ()) );
    i += 1; }
  result }
