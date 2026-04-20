// PURPOSE: Tantivy integration. This file holds schema + index
// opening, plus exact-ID document lookups. The other phases live
// in submodules:
//   - escape:         query preprocessing (pure strings).
//   - search:         the text-search API (QueryParser / RegexQuery).
//   - write:          update/delete/add documents + commit helper.
//   - context_update: refresh `context_origin_type` via
//                     delete-and-readd.

// GLOSSARY:
// See the Tantivy section in glossary.md.

pub mod context_update;
pub mod escape;
pub mod search;
pub mod write;

use crate::types::misc::{ID, SourceName, TantivyIndex};

use tantivy::{Index, Term, IndexReader, Searcher, TantivyDocument};
use tantivy::schema::document::Value;
use tantivy::query::Query;
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

