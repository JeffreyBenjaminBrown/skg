// PURPOSE: Creates a Tantivy index from SkgNodes,
// associating each node's primary ID with its processed title and aliases.

// GLOSSARY:
// See the Tantivy section in glossary.md.

use crate::types::textlinks::replace_each_link_with_its_label;
use crate::types::misc::{ID, SourceName, TantivyIndex};
use crate::types::skgnode::{FileProperty, SkgNode};

use tantivy::{Index, IndexWriter, doc, Term, IndexReader, Searcher, TantivyDocument};
use tantivy::schema::document::Value;
use tantivy::query::{QueryParser, Query};
use tantivy::collector::TopDocs;
use tantivy::schema;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::Path;
use std::sync::Arc;


/// Opens an existing Tantivy index at `index_path`.
/// Returns Err if the directory doesn't exist or the
/// index can't be opened (caller falls back to full rebuild).
pub(crate) fn open_existing_tantivy_index (
  index_path : &Path,
) -> Result<TantivyIndex, Box<dyn Error>> {
  let index : Index =
    Index::open_in_dir (index_path) ?;
  let schema : schema::Schema =
    index . schema();
  let id_field : schema::Field =
    schema . get_field ("id") ?;
  let title_or_alias_field : schema::Field =
    schema . get_field ("title_or_alias") ?;
  let source_field : schema::Field =
    schema . get_field ("source") ?;
  let context_origin_type_field : schema::Field =
    schema . get_field ("context_origin_type") ?;
  let is_title_field : schema::Field =
    schema . get_field ("is_title") ?;
  let had_id_field : schema::Field =
    schema . get_field ("had_id") ?;
  let body_field : schema::Field =
    schema . get_field ("body") ?;
  Ok ( TantivyIndex {
    index              : Arc::new (index),
    id_field,
    title_or_alias_field,
    source_field,
    context_origin_type_field,
    is_title_field,
    had_id_field,
    body_field, } ) }

/// The Tantivy schema.
/// Fields:
/// - "id":                  STRING | STORED — the node's primary ID.
/// - "title_or_alias":      TEXT   | STORED — searchable titles and aliases.
/// - "source":              STRING | STORED — the source name.
/// - "context_origin_type": STRING | STORED — Root/CycleMember/Target/…
/// - "is_title":            STRING | STORED — "true" for the primary title,
///                          "false" for alias docs.
/// - "had_id":              STRING | STORED — "true" if the node had an
///                          org-roam ID before import.
/// - "body":                TEXT   | STORED — searchable body text. STORED
///                          so that `update_context_origin_types` can
///                          preserve the body when it does a
///                          delete-and-readd to refresh origin types.
///                          (The body is also on disk in the .skg file,
///                          so this is duplication — revisit once origin
///                          types are computed before indexing.)
pub(super) fn mk_tantivy_schema() -> schema::Schema {
  let mut schema_builder : schema::SchemaBuilder =
    schema::Schema::builder();
  schema_builder . add_text_field(
    "id", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "title_or_alias", schema::TEXT | schema::STORED);
  schema_builder . add_text_field(
    "source", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "context_origin_type", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "is_title", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "had_id", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "body", schema::TEXT | schema::STORED);
  schema_builder . build() }

/// Pre-process `query` so titles and aliases containing Tantivy
/// operator characters are findable by typing the literal text.
///
/// Strategy: split on whitespace. Any word that contains an operator
/// char — or is one of the boolean keywords AND / OR / NOT — is
/// wrapped as a Tantivy phrase ("..."), which treats its contents as
/// literal content (tokenized, but not operator-interpreted). Words
/// with no specials pass through unchanged, preserving the OR-default
/// across terms that
/// [[../../docs/COMMANDS.org][docs/COMMANDS.org]] advertises and
/// existing tests (e.g. `test_aliases`) rely on.
///
/// Why per-word phrasing rather than whole-query:
/// Wrapping the whole query as a phrase would collapse OR-default
/// into phrase-only (AND + adjacency). Per-word wrapping keeps OR
/// between words while making each word's contents literal.
///
/// Why phrase-wrap rather than backslash-escape each char:
/// Tantivy 0.19's QueryParser does not accept `\[` / `\]` etc.
/// outside phrases — it errors with SyntaxError. Phrase-wrapping is
/// the most reliable way to pass operator chars through the parser.
pub fn escape_tantivy_literal (
  query : &str,
) -> String {
  query
    . split_whitespace ()
    . map ( |word| {
      let needs_wrap : bool =
        word == "AND" || word == "OR" || word == "NOT" ||
        word . chars () . any ( is_tantivy_operator_char );
      if needs_wrap {
        wrap_as_phrase ( word )
      } else {
        word . to_string () }} )
    . collect::<Vec<String>> ()
    . join ( " " ) }

fn wrap_as_phrase (
  word : &str,
) -> String {
  let mut inner : String =
    String::with_capacity ( word . len () + 4 );
  for ch in word . chars () {
    if ch == '\\' || ch == '"' {
      inner . push ( '\\' ); }
    inner . push ( ch ); }
  format! ( "\"{}\"", inner ) }

/// Pre-process `query` for operators=true mode: preserve Tantivy's
/// operator syntax, but backslash-escape any operator character
/// sitting strictly inside a word (alphanumeric or underscore neighbors
/// on both sides within the same whitespace-delimited token).
///
/// Effect: =AND= / =OR= / =NOT= / =+foo= / =-bar= / =(..)= / phrase
/// quotes keep their operator meaning. =foo:bar= becomes =foo\:bar=
/// so the literal text is findable. =C++= stays =C++= because the
/// trailing =+= is not bounded by a word char on the right — it'll
/// be handled by QueryParser's tokenization rather than by escape.
///
/// LIMITATION: the heuristic can't tell a known field name from a
/// user word. =title_or_alias:dog= gets its =:= escaped too, so
/// field-qualified queries need manual =\:= or the user switches
/// back to operators=false.
pub fn escape_tantivy_intra_word (
  query : &str,
) -> String {
  let chars : Vec<char> = query . chars () . collect ();
  let mut out : String =
    String::with_capacity ( query . len () + 8 );
  for (i, &ch) in chars . iter () . enumerate () {
    if is_tantivy_operator_char (ch)
       && has_word_neighbor (&chars, i, false)
       && has_word_neighbor (&chars, i, true) {
      out . push ( '\\' ); }
    out . push (ch); }
  out }

/// True iff, scanning outward from index `i` within the current
/// whitespace-delimited token, the first non-operator non-whitespace
/// character is alphanumeric or underscore.
fn has_word_neighbor (
  chars   : &[char],
  i       : usize,
  forward : bool,
) -> bool {
  let range : Box<dyn Iterator<Item = usize>> =
    if forward { Box::new ( (i + 1) .. chars . len () ) }
    else       { Box::new ( (0 .. i) . rev () ) };
  for j in range {
    let c : char = chars [j];
    if c . is_whitespace () { return false; }
    if is_tantivy_operator_char (c) { continue; }
    return c . is_alphanumeric () || c == '_'; }
  false }

fn is_tantivy_operator_char (
  ch : char,
) -> bool {
  matches! ( ch,
    '+' | '-' | '&' | '|' | '!' | '(' | ')' | '{' | '}' |
    '[' | ']' | '^' | '"' | '~' | '*' | '?' | ':' | '\\' ) }

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

/// Returns ALL matching Tantivy "Documents" (see glossary, above).
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

/// Updates the index with the provided SkgNodes.
///   For existing IDs, updates the title.
///   For new IDs, adds new entries.
/// Returns the number of documents processed.
pub fn update_index_with_nodes (
  nodes: &[SkgNode],
  tantivy_index: &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut writer: IndexWriter =
    tantivy_index . index . writer (
      crate::consts::TANTIVY_WRITER_BUFFER_BYTES)?;
  delete_nodes_from_index(
    // Delete those IDs from the index. (They'll come back.)
    nodes . iter(), &mut writer, tantivy_index)?;
  let processed_count: usize = // Add new associations.
    add_documents_to_tantivy_writer (
      nodes, &mut writer, tantivy_index)?;
  commit_with_status(
    &mut writer, processed_count, "Updated")?;
  Ok (processed_count) }

pub fn delete_nodes_from_index<'a, I>(
  nodes_iter: I,
  writer: &mut IndexWriter,
  tantivy_index: &TantivyIndex,
) -> Result<(), Box<dyn Error>>
where I: Iterator<Item = &'a SkgNode>, {
  for node in nodes_iter {
    { let primary_id : &ID = &node . pid;
      writer . delete_term (
        Term::from_field_text( tantivy_index . id_field,
                               primary_id . as_str() ) ); }}
  Ok (( )) }

pub fn delete_nodes_by_id_from_index<'a, I>(
  ids_iter: I,
  writer: &mut IndexWriter,
  tantivy_index: &TantivyIndex,
) -> Result<(), Box<dyn Error>>
where I: Iterator<Item = &'a ID>, {
  for id in ids_iter {
    writer . delete_term (
      Term::from_field_text( tantivy_index . id_field,
                             id . as_str() ) ); }
  Ok (( )) }

pub fn add_documents_to_tantivy_writer<'a, I> (
  nodes         : I,
  writer        : &mut IndexWriter,
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>>
where I: IntoIterator<Item = &'a SkgNode>, {

  let mut indexed_count: usize = 0;
  for node in nodes {
    let documents: Vec<TantivyDocument> =
      create_documents_from_node(
        node, tantivy_index )?;
    for document in documents {
      writer . add_document (document)?;
      indexed_count += 1; }}
  Ok (indexed_count) }

/// Look up the canonical title and source for a node by its exact primary ID.
/// Prefers the document marked is_title="true"; falls back to the
/// first title_or_alias found if no title document exists.
pub fn title_and_source_by_id (
  tantivy_index : &TantivyIndex,
  id            : &ID,
) -> Option < (String, SourceName) > {
  let reader : IndexReader =
    tantivy_index . index . reader () . ok () ?;
  let searcher : Searcher =
    reader . searcher ();
  let query : Box < dyn Query > =
    Box::new ( tantivy::query::TermQuery::new (
      Term::from_field_text (
        tantivy_index . id_field, id . as_str () ),
      schema::IndexRecordOption::Basic ));
  let results : Vec < (f32, tantivy::DocAddress) > =
    searcher . search (
      &query, &TopDocs::with_limit (
        crate::consts::TANTIVY_PER_ID_LOOKUP_LIMIT )
        . order_by_score () ) . ok () ?;
  let mut fallback : Option < (String, SourceName) > = None;
  for (_score, doc_address) in &results {
    let retrieved_doc : TantivyDocument =
      searcher . doc (*doc_address) . ok () ?;
    let is_title : bool =
      retrieved_doc
        . get_first ( tantivy_index . is_title_field )
        . and_then ( |v| v . as_str () )
        . map ( |s| s == "true" )
        . unwrap_or (false);
    let title_or_alias : Option < String > =
      retrieved_doc
        . get_first ( tantivy_index . title_or_alias_field )
        . and_then ( |v| v . as_str () )
        . map ( |s| s . to_string () );
    let source : SourceName =
      SourceName::from (
        retrieved_doc
          . get_first ( tantivy_index . source_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("") );
    if is_title {
      return title_or_alias . map (
        |t| (t, source) ); }
    if fallback . is_none () {
      fallback = title_or_alias . map (
        |t| (t, source) ); } }
  tracing::warn! (
    "title_and_source_by_id: no is_title=\"true\" document \
     found for ID {}. Falling back to first title_or_alias.",
    id );
  fallback }

/// Look up canonical titles for multiple IDs in a single searcher session.
/// IDs not found in Tantivy are absent from the result.
pub fn titles_by_ids (
  tantivy_index : &TantivyIndex,
  ids           : &[ID],
) -> HashMap<ID, String> {
  let mut result : HashMap<ID, String> = HashMap::new ();
  let reader : IndexReader =
    match tantivy_index . index . reader () {
      Ok (r) => r,
      Err (_) => return result };
  let searcher : Searcher = reader . searcher ();
  for id in ids {
    let query : Box < dyn Query > =
      Box::new ( tantivy::query::TermQuery::new (
        Term::from_field_text (
          tantivy_index . id_field, id . as_str () ),
        schema::IndexRecordOption::Basic ));
    let results : Vec < (f32, tantivy::DocAddress) > =
      match searcher . search (
        &query, &TopDocs::with_limit (
          crate::consts::TANTIVY_PER_ID_LOOKUP_LIMIT )
          . order_by_score () )
      { Ok (r) => r,
        Err (_) => continue };
    let mut fallback : Option<String> = None;
    for (_score, doc_address) in &results {
      let retrieved_doc : TantivyDocument =
        match searcher . doc (*doc_address) {
          Ok (d) => d,
          Err (_) => continue };
      let is_title : bool =
        retrieved_doc
          . get_first ( tantivy_index . is_title_field )
          . and_then ( |v| v . as_str () )
          . map ( |s| s == "true" )
          . unwrap_or (false);
      let title_or_alias : Option<String> =
        retrieved_doc
          . get_first ( tantivy_index . title_or_alias_field )
          . and_then ( |v| v . as_str () )
          . map ( |s| s . to_string () );
      if is_title {
        if let Some (t) = title_or_alias {
          result . insert ( id . clone (), t ); }
        fallback = None; // signal: found title, skip fallback
        break; }
      if fallback . is_none () {
        fallback = title_or_alias; } }
    if let Some (fb) = fallback {
      tracing::debug! (
        "titles_by_ids: no is_title=\"true\" document \
         found for ID {}. Falling back to first title_or_alias.",
        id );
      result . insert ( id . clone (), fb ); } }
  result }

/// Return the subset of the input for which 'had_id' is true.
pub fn subset_with_hadid (
  tantivy_index : &TantivyIndex,
  ids           : &HashSet<ID>,
) -> HashSet<ID> {
  let reader : IndexReader
    = match tantivy_index . index . reader () {
      Ok (r) => r,
      Err (_) => return HashSet::new () };
  let searcher : Searcher = reader . searcher ();
  let mut result : HashSet<ID> = HashSet::new ();
  for id in ids {
    let query : Box < dyn Query > =
      Box::new ( tantivy::query::TermQuery::new (
        Term::from_field_text (
          tantivy_index . id_field, id . as_str () ),
        schema::IndexRecordOption::Basic ));
    let hits : Vec < (f32, tantivy::DocAddress) > =
      match searcher . search (
        &query, &TopDocs::with_limit (1)
          . order_by_score () )
      { Ok (h) => h,
        Err (_) => continue };
    if let Some ( (_score, doc_address) ) = hits . first () {
      if let Ok (doc) = searcher . doc::<TantivyDocument> (*doc_address) {
        let had_id : bool =
          doc . get_first ( tantivy_index . had_id_field )
          . and_then ( |v| v . as_str () )
          . map ( |s| s == "true" )
          . unwrap_or (false);
        if had_id {
          result . insert (id . clone () ); }} }}
  result }

/* -------------------- Private helpers -------------------- */

fn create_documents_from_node (
  node: &SkgNode,
  tantivy_index: &TantivyIndex,
) -> Result < Vec < TantivyDocument >,
              Box < dyn Error >> {
  let primary_id : &ID = &node . pid;
  let had_id : &str =
    if node . misc . contains (
      &FileProperty::Had_ID_Before_Import )
    { "true" } else { "false" };
  // Only the primary-title doc carries the body.
  // Alias docs share the id so body search still returns
  // this node, but we don't duplicate the body text across
  // aliases.
  let body_text : String =
    node . body . as_deref () . map_or ( String::new (),
      |b| replace_each_link_with_its_label (b) );
  let mut documents: Vec<TantivyDocument> =
    Vec::new();
  let mut titles_and_aliases: Vec<String> =
    vec![node . title . clone()];
  titles_and_aliases . extend_from_slice (
    node . aliases . or_default () );
  for (i, title_or_alias) in
    titles_and_aliases . iter() . enumerate()
  { let is_title : bool = i == 0;
    let is_title_str : &str =
      if is_title { "true" } else { "false" };
    let body_for_this_doc : &str =
      if is_title { body_text . as_str () } else { "" };
    documents . push (
      doc!(
        tantivy_index . id_field =>
          primary_id . as_str (),
        tantivy_index . title_or_alias_field =>
          replace_each_link_with_its_label (
            title_or_alias ),
        tantivy_index . source_field =>
          node . source . as_str(),
        tantivy_index . context_origin_type_field =>
          "",
        tantivy_index . is_title_field =>
          is_title_str,
        tantivy_index . had_id_field =>
          had_id,
        tantivy_index . body_field =>
          body_for_this_doc ) ); }
  Ok (documents) }

/// Updates context_origin_type for all documents matching each ID.
/// Deletes and re-adds each document with the new context_origin_type.
pub fn update_context_origin_types (
  tantivy_index       : &TantivyIndex,
  context_types_by_id : &HashMap<ID, String>,
) -> Result<usize, Box<dyn Error>> {
  let reader : IndexReader =
    tantivy_index . index . reader () ?;
  let searcher : Searcher =
    reader . searcher ();
  let mut writer : IndexWriter =
    tantivy_index . index . writer (
      crate::consts::TANTIVY_WRITER_BUFFER_BYTES) ?;
  let mut updated_count : usize = 0;
  for (pid, context_type) in context_types_by_id {
    let query : Box < dyn Query > =
      // Find all documents with this ID.
      Box::new ( tantivy::query::TermQuery::new (
        Term::from_field_text (
          tantivy_index . id_field, pid . as_str () ),
        schema::IndexRecordOption::Basic ));
    let results : Vec < (f32, tantivy::DocAddress) > =
      searcher . search (
        &query, &TopDocs::with_limit (
          crate::consts::TANTIVY_PER_ID_LOOKUP_LIMIT )
          . order_by_score () ) ?;
    if results . is_empty () { continue; }
    writer . delete_term ( // Delete all documents for this ID.
      Term::from_field_text (
        tantivy_index . id_field, pid . as_str () ));
    for (_score, doc_address) in &results {
      // Re-add with the new context_origin_type.
      let retrieved_doc : TantivyDocument =
        searcher . doc (*doc_address) ?;
      let title_or_alias : String =
        retrieved_doc
          . get_first ( tantivy_index . title_or_alias_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("") . to_string ();
      let source : String =
        retrieved_doc
          . get_first ( tantivy_index . source_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("") . to_string ();
      let is_title : String =
        retrieved_doc
          . get_first ( tantivy_index . is_title_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("false") . to_string ();
      let had_id : String =
        retrieved_doc
          . get_first ( tantivy_index . had_id_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("false") . to_string ();
      let body : String =
        retrieved_doc
          . get_first ( tantivy_index . body_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("") . to_string ();
      writer . add_document ( doc! (
        tantivy_index . id_field =>
          pid . as_str (),
        tantivy_index . title_or_alias_field =>
          title_or_alias . as_str (),
        tantivy_index . source_field =>
          source . as_str (),
        tantivy_index . context_origin_type_field =>
          context_type . as_str (),
        tantivy_index . is_title_field =>
          is_title . as_str (),
        tantivy_index . had_id_field =>
          had_id . as_str (),
        tantivy_index . body_field =>
          body . as_str () )) ?;
      updated_count += 1; } }
  commit_with_status (
    &mut writer, updated_count, "Context-updated") ?;
  Ok (updated_count) }

pub fn commit_with_status (
  writer: &mut IndexWriter,
  indexed_count: usize,
  operation: &str,
) -> Result<(), Box<dyn Error>> {
  if indexed_count > 0 {
    tracing::info!( "{} {} documents. Committing changes...",
              operation, indexed_count );
    writer . commit () ?;
  } else {
    tracing::debug!("No documents to process found."); }
  Ok (( )) }
