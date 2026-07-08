// PURPOSE: The text-search API. Builds Tantivy queries from a string
// (either through the QueryParser with optional operator syntax, or
// through RegexQuery), runs them against the index, and returns
// (score, DocAddress) pairs plus the Searcher so callers can fetch
// stored fields.

use crate::consts::TANTIVY_SEARCH_LIMIT;
use crate::dbs::tantivy::escape::{escape_tantivy_intra_word, escape_tantivy_literal};
use crate::types::misc::TantivyIndex;

use tantivy::Searcher;
use tantivy::collector::TopDocs;
use tantivy::query::{BooleanQuery, Occur, Query, QueryParser, RegexQuery};
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
  // Tantivy index writes commit on a background worker (see
  // server/dbs/tantivy/background_writer.rs). The worker reloads the
  // shared reader after each commit, but reloading again here, on the
  // reading thread immediately before we take a searcher, is what makes
  // read-your-writes deterministic: a caller that has
  // 'wait_for_tantivy_writes_idle'd first is then guaranteed to see every
  // committed write. Without this, a search right after a save could
  // occasionally observe a stale reader.
  tantivy_index . reader . reload () ?;
  let searcher : Searcher =
    tantivy_index . reader . searcher ();
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
          TANTIVY_SEARCH_LIMIT )
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
/// With operators=true, the pattern is parsed as a boolean
/// expression over the pieces ('parse_regex_operator_query'):
/// AND / OR / NOT with conventional precedence, +foo / -foo
/// prefixes, and grouping via parens that stand ALONE between
/// whitespace (TODO/fork-fixes.org).
fn build_regex_query (
  tantivy_index : &TantivyIndex,
  pattern       : &str,
  body          : bool,
  operators     : bool,
) -> Result < Box < dyn Query >, Box < dyn std::error::Error >> {
  let fields : Vec<schema::Field> =
    if body {
      vec! [ tantivy_index . title_or_alias_field,
             tantivy_index . body_field ]
    } else {
      vec! [ tantivy_index . title_or_alias_field ] };
  if operators {
    let expr : RegexOpExpr =
      parse_regex_operator_query (pattern)
      . map_err ( |e| -> Box<dyn std::error::Error> { e . into () } ) ?;
    return regex_op_expr_to_query (&expr, &fields); }
  let pieces : Vec<String> =
    pattern . split_whitespace ()
    . map (str::to_string)
    . collect ();
  // Fall back to the raw pattern if splitting yielded nothing (an
  // all-whitespace pattern). Tantivy will then surface whatever
  // error it emits for that input.
  let effective : Vec<String> =
    if pieces . is_empty () { vec! [ pattern . to_string () ] }
    else { pieces };
  let mut clauses : Vec<(Occur, Box<dyn Query>)> =
    Vec::with_capacity ( effective . len () );
  for piece in &effective {
    clauses . push (
      ( Occur::Should,
        regex_piece_query (piece, &fields) ? )); }
  if clauses . len () == 1 {
    let (_, q) : (Occur, Box<dyn Query>) =
      clauses . pop () . unwrap ();
    return Ok (q); }
  Ok ( Box::new ( BooleanQuery::new (clauses) ) ) }

/// Turn a parsed operator expression into a Tantivy query.
/// - An Or group SHOULD-combines its positive children; an And group
///   MUST-combines them. In either group, a Not child contributes a
///   MUST_NOT clause and a Must child ('+piece') a MUST clause.
/// - A group holding ONLY exclusions matches nothing (Tantivy needs
///   a positive clause), same as the pre-grouping behavior of a
///   lone 'NOT x'.
fn regex_op_expr_to_query (
  expr   : &RegexOpExpr,
  fields : &[schema::Field],
) -> Result < Box < dyn Query >, Box < dyn std::error::Error >> {
  let group_clauses =
    | children : &[RegexOpExpr], positive : Occur |
    -> Result < Vec<(Occur, Box<dyn Query>)>,
                Box < dyn std::error::Error >> {
      let mut clauses : Vec<(Occur, Box<dyn Query>)> =
        Vec::with_capacity ( children . len () );
      for child in children {
        clauses . push ( match child {
          RegexOpExpr::Not (inner) =>
            ( Occur::MustNot,
              regex_op_expr_to_query (inner, fields) ? ),
          RegexOpExpr::Must (inner) =>
            ( Occur::Must,
              regex_op_expr_to_query (inner, fields) ? ),
          other =>
            ( positive,
              regex_op_expr_to_query (other, fields) ? ), } ); }
      Ok (clauses) };
  match expr {
    RegexOpExpr::Piece (pat) =>
      regex_piece_query (pat, fields),
    RegexOpExpr::Or (children) =>
      Ok ( Box::new ( BooleanQuery::new (
        group_clauses (children, Occur::Should) ? ))),
    RegexOpExpr::And (children) =>
      Ok ( Box::new ( BooleanQuery::new (
        group_clauses (children, Occur::Must) ? ))),
    RegexOpExpr::Not (inner) =>
      // Reachable only as the whole query (groups intercept their
      // Not children above): a bare exclusion matches nothing.
      Ok ( Box::new ( BooleanQuery::new ( vec! [
        ( Occur::MustNot,
          regex_op_expr_to_query (inner, fields) ? ) ] ))),
    RegexOpExpr::Must (inner) =>
      regex_op_expr_to_query (inner, fields), } }

/// One whitespace-delimited regex piece, matched against every
/// searched field (SHOULD across fields: a hit in any field counts).
fn regex_piece_query (
  piece  : &str,
  fields : &[schema::Field],
) -> Result < Box < dyn Query >, Box < dyn std::error::Error >> {
  let mut per_field : Vec<(Occur, Box<dyn Query>)> =
    Vec::with_capacity ( fields . len () );
  for field in fields {
    let rq : RegexQuery =
      RegexQuery::from_pattern (piece, *field) ?;
    per_field . push (
      ( Occur::Should,
        Box::new (rq) as Box<dyn Query> )); }
  Ok ( if per_field . len () == 1 {
         per_field . pop () . unwrap () . 1
       } else {
         Box::new ( BooleanQuery::new (per_field) ) } ) }

/// The parsed shape of a regex-mode operator query. Pieces are
/// whitespace-delimited regex patterns. 'Must' wraps a '+piece';
/// 'Not' wraps a NOT operand or a '-piece'.
#[derive (Clone, Debug, PartialEq)]
enum RegexOpExpr {
  Or    (Vec<RegexOpExpr>),
  And   (Vec<RegexOpExpr>),
  Not   (Box<RegexOpExpr>),
  Must  (Box<RegexOpExpr>),
  Piece (String),
}

/// Parse a regex-mode operator query (TODO/fork-fixes.org). The
/// pattern is split on whitespace; the grammar, in conventional
/// precedence (NOT binds tightest, then AND, then OR; bare
/// adjacency is OR):
///
///   or      := and ( 'OR'? and )*
///   and     := not ( 'AND' not )*
///   not     := 'NOT' not | primary
///   primary := '(' or ')' | piece
///
/// A paren is a GROUPING token only when it stands alone between
/// whitespace; attached to a pattern it is ordinary (Rust-flavor)
/// regex syntax inside that piece, so '(cat|dog).*' still works.
/// '+piece' / '-piece' force that piece to MUST / MUST_NOT within
/// its group. Double negation cancels. Malformed operator syntax
/// (an unbalanced lone paren, a dangling keyword, an empty query)
/// is a loud error, never silently reinterpreted.
fn parse_regex_operator_query (
  pattern : &str,
) -> Result < RegexOpExpr, String > {
  let tokens : Vec<&str> =
    pattern . split_whitespace () . collect ();
  let mut pos : usize = 0;
  let expr : RegexOpExpr =
    parse_or_expr (&tokens, &mut pos) ?;
  if pos < tokens . len () {
    // parse_or_expr only stops early at a ')' it did not open.
    return Err ( "operator syntax: unmatched ')'. A paren alone \
                  between spaces groups operators; to use ')' inside \
                  a regex, attach it to its pattern." . to_string () ); }
  Ok (expr) }

fn parse_or_expr (
  tokens : &[&str],
  pos    : &mut usize,
) -> Result < RegexOpExpr, String > {
  let mut operands : Vec<RegexOpExpr> =
    vec! [ parse_and_expr (tokens, pos) ? ];
  loop {
    match tokens . get (*pos) {
      None => break,
      Some (&")") => break, // the enclosing parse_primary consumes it
      Some (&"OR") => {
        *pos += 1;
        operands . push ( parse_and_expr (tokens, pos) ? ); }
      Some (_) => {
        // bare adjacency is OR
        operands . push ( parse_and_expr (tokens, pos) ? ); }} }
  Ok ( if operands . len () == 1 { operands . pop () . unwrap () }
       else { RegexOpExpr::Or (operands) } ) }

fn parse_and_expr (
  tokens : &[&str],
  pos    : &mut usize,
) -> Result < RegexOpExpr, String > {
  let mut operands : Vec<RegexOpExpr> =
    vec! [ parse_not_expr (tokens, pos) ? ];
  while tokens . get (*pos) == Some (&"AND") {
    *pos += 1;
    operands . push ( parse_not_expr (tokens, pos) ? ); }
  Ok ( if operands . len () == 1 { operands . pop () . unwrap () }
       else { RegexOpExpr::And (operands) } ) }

fn parse_not_expr (
  tokens : &[&str],
  pos    : &mut usize,
) -> Result < RegexOpExpr, String > {
  if tokens . get (*pos) == Some (&"NOT") {
    *pos += 1;
    let inner : RegexOpExpr =
      parse_not_expr (tokens, pos) ?;
    Ok ( match inner {
      RegexOpExpr::Not (x) => *x, // double negation cancels
      other => RegexOpExpr::Not ( Box::new (other) ), } )
  } else {
    parse_primary_expr (tokens, pos) }}

fn parse_primary_expr (
  tokens : &[&str],
  pos    : &mut usize,
) -> Result < RegexOpExpr, String > {
  match tokens . get (*pos) {
    None =>
      Err ( "operator syntax: expected a pattern, found the end of \
             the query." . to_string () ),
    Some (&"(") => {
      *pos += 1;
      let inner : RegexOpExpr =
        parse_or_expr (tokens, pos) ?;
      if tokens . get (*pos) == Some (&")") {
        *pos += 1;
        Ok (inner)
      } else {
        Err ( "operator syntax: unclosed '('. A paren alone between \
               spaces groups operators; to use '(' inside a regex, \
               attach it to its pattern." . to_string () ) }}
    Some (&")") =>
      Err ( "operator syntax: expected a pattern, found ')'."
            . to_string () ),
    Some (&"AND") | Some (&"OR") =>
      Err ( format! (
        "operator syntax: '{}' needs a pattern on each side.",
        tokens [*pos] )),
    Some (&tok) => {
      *pos += 1;
      if let Some (rest) = tok . strip_prefix ('+') {
        if rest . is_empty () {
          Err ( "operator syntax: '+' needs a pattern attached."
                . to_string () )
        } else {
          Ok ( RegexOpExpr::Must ( Box::new (
            RegexOpExpr::Piece ( rest . to_string () )))) }
      } else if let Some (rest) = tok . strip_prefix ('-') {
        if rest . is_empty () {
          Err ( "operator syntax: '-' needs a pattern attached."
                . to_string () )
        } else {
          Ok ( RegexOpExpr::Not ( Box::new (
            RegexOpExpr::Piece ( rest . to_string () )))) }
      } else {
        Ok ( RegexOpExpr::Piece ( tok . to_string () )) }} }}
