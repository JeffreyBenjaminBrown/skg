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
  pub regex     : bool, // Interpret the query as a per-token regex.
                         // Bypasses QueryParser and builds RegexQuery
                         // directly.
  pub body      : bool, // Also search node bodies (titles always
                         // searched).
  pub operators : bool, // Preserve Tantivy operator syntax. Only
                         // meaningful when regex=false.
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
      build_regex_query ( tantivy_index, query_text, opts . body ) ?
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

fn build_regex_query (
  tantivy_index : &TantivyIndex,
  pattern       : &str,
  body          : bool,
) -> Result < Box < dyn Query >, Box < dyn std::error::Error >> {
  use tantivy::query::{BooleanQuery, Occur, RegexQuery};
  let title_regex : RegexQuery =
    RegexQuery::from_pattern (
      pattern, tantivy_index . title_or_alias_field ) ?;
  if ! body {
    return Ok ( Box::new ( title_regex ) ); }
  let body_regex : RegexQuery =
    RegexQuery::from_pattern (
      pattern, tantivy_index . body_field ) ?;
  // SHOULD-SHOULD = OR between fields.
  let bq : BooleanQuery =
    BooleanQuery::new ( vec! [
      ( Occur::Should, Box::new (title_regex) as Box<dyn Query> ),
      ( Occur::Should, Box::new (body_regex ) as Box<dyn Query> ),
    ] );
  Ok ( Box::new (bq) ) }
