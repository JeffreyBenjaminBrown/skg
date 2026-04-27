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
  let raw_title_field : schema::Field =
    schema . get_field ("raw_title") ?;
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
    raw_title_field,
    source_field,
    context_origin_type_field,
    is_title_field,
    had_id_field,
    body_field, } ) }

/// The Tantivy schema.
/// Fields:
/// - "id":                  STRING | STORED — the node's primary ID.
/// - "title_or_alias":      TEXT   | STORED — searchable titles and aliases,
///                          with textlinks reduced to their labels.
/// - "raw_title":           STRING | STORED — the un-reduced title, stored
///                          only on is_title="true" docs. Preserves the
///                          textlink syntax that 'title_or_alias' strips.
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
    "raw_title", schema::STRING | schema::STORED);
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
///
/// Returns the raw_title (with textlink syntax intact) when available,
/// so the user sees `[[id:X][label]]` rather than just `label` -- this
/// preserves the visual distinction between a node titled "science"
/// and one whose title links to a different node also labelled
/// "science". Alias-doc fallback paths use 'title_or_alias' because
/// only is_title="true" docs carry a 'raw_title'.
pub fn title_and_source_by_id (
  tantivy_index : &TantivyIndex,
  id            : &ID,
) -> Option < (String, SourceName) > {
  let reader : IndexReader =
    tantivy_index . index . reader () . ok () ?;
  let searcher : Searcher = reader . searcher ();
  let doc_addresses : Vec<tantivy::DocAddress> =
    doc_addresses_for_id (
      tantivy_index, &searcher, id,
      crate::consts::TANTIVY_PER_ID_LOOKUP_LIMIT ) ?;
  let (doc, was_fallback) : (TantivyDocument, bool) =
    pick_title_doc ( tantivy_index, &searcher, id, &doc_addresses ) ?;
  if was_fallback {
    tracing::warn! (
      "title_and_source_by_id: no is_title=\"true\" document \
       found for ID {}. Falling back to first title_or_alias.",
      id ); }
  let title : String =
    string_field ( &doc, tantivy_index . raw_title_field )
      . filter ( |s| ! s . is_empty () )
      . or_else ( || string_field (
        &doc, tantivy_index . title_or_alias_field )) ?;
  let source : SourceName = SourceName::from (
    string_field ( &doc, tantivy_index . source_field )
      . unwrap_or_default () . as_str () );
  Some ( (title, source) ) }

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
    let doc_addresses : Vec<tantivy::DocAddress> =
      match doc_addresses_for_id (
        tantivy_index, &searcher, id,
        crate::consts::TANTIVY_PER_ID_LOOKUP_LIMIT )
      { Some (a) => a,
        None     => continue };
    let (doc, was_fallback) : (TantivyDocument, bool) =
      match pick_title_doc (
        tantivy_index, &searcher, id, &doc_addresses )
      { Some (d) => d,
        None     => continue };
    if was_fallback {
      tracing::debug! (
        "titles_by_ids: no is_title=\"true\" document \
         found for ID {}. Falling back to first title_or_alias.",
        id ); }
    // Prefer the un-reduced raw title (only on is_title="true" docs);
    // fall back to 'title_or_alias' for the alias-only fallback case,
    // which has no raw_title stored.
    let title : Option<String> =
      string_field ( &doc, tantivy_index . raw_title_field )
        . filter ( |s| !s . is_empty () )
        . or_else ( || string_field (
          &doc, tantivy_index . title_or_alias_field ));
    if let Some (t) = title
    { result . insert ( id . clone (), t ); } }
  result }

/// Return the subset of the input for which 'had_id' is true.
pub fn subset_with_hadid (
  tantivy_index : &TantivyIndex,
  ids           : &HashSet<ID>,
) -> HashSet<ID> {
  let reader : IndexReader =
    match tantivy_index . index . reader () {
      Ok (r) => r,
      Err (_) => return HashSet::new () };
  let searcher : Searcher = reader . searcher ();
  let mut result : HashSet<ID> = HashSet::new ();
  for id in ids {
    let doc_addresses : Vec<tantivy::DocAddress> =
      match doc_addresses_for_id (
        tantivy_index, &searcher, id, 1 )
      { Some (a) => a,
        None     => continue };
    let Some (addr) = doc_addresses . first ()
      else { continue };
    let doc : TantivyDocument =
      match load_doc_or_warn ( &searcher, *addr, id ) {
        Some (d) => d,
        None     => continue };
    if bool_field_eq_true ( &doc, tantivy_index . had_id_field ) {
      result . insert ( id . clone () ); } }
  result }

/// Run an exact-match TermQuery against the id_field, returning
/// the matching doc addresses (scores discarded). None on any
/// index/search error; an empty vec means "no hits" (distinct from
/// error).
fn doc_addresses_for_id (
  tantivy_index : &TantivyIndex,
  searcher      : &Searcher,
  id            : &ID,
  limit         : usize,
) -> Option < Vec<tantivy::DocAddress> > {
  let query : Box<dyn Query> =
    Box::new ( tantivy::query::TermQuery::new (
      Term::from_field_text (
        tantivy_index . id_field, id . as_str () ),
      schema::IndexRecordOption::Basic ));
  searcher . search (
    &query, &TopDocs::with_limit (limit) . order_by_score () )
    . ok ()
    . map ( |hits| hits . into_iter ()
              . map ( |(_, a)| a ) . collect () ) }

/// Walk the doc addresses for a single ID and pick the one marked
/// is_title="true". If none is, fall back to the first doc (an
/// alias). Returns (chosen doc, was_fallback). Broken docs log a
/// warning and are skipped.
fn pick_title_doc (
  tantivy_index : &TantivyIndex,
  searcher      : &Searcher,
  id            : &ID,
  doc_addresses : &[tantivy::DocAddress],
) -> Option < (TantivyDocument, bool) > {
  let mut fallback : Option<TantivyDocument> = None;
  for addr in doc_addresses {
    let doc : TantivyDocument =
      match load_doc_or_warn ( searcher, *addr, id ) {
        Some (d) => d,
        None     => continue };
    if bool_field_eq_true ( &doc, tantivy_index . is_title_field ) {
      return Some ( (doc, false) ); }
    if fallback . is_none () {
      fallback = Some (doc); } }
  fallback . map ( |d| (d, true) ) }

/// Fetch a stored doc by address. Logs a warning on failure so
/// operators can diagnose corrupt indexes; returns None so callers
/// can skip this address and continue.
fn load_doc_or_warn (
  searcher : &Searcher,
  addr     : tantivy::DocAddress,
  id       : &ID,
) -> Option < TantivyDocument > {
  match searcher . doc (addr) {
    Ok (doc) => Some (doc),
    Err (e)  => {
      tracing::warn! (
        "Failed to load Tantivy doc for ID {} at {:?}: {}",
        id, addr, e );
      None }} }

fn bool_field_eq_true (
  doc   : &TantivyDocument,
  field : schema::Field,
) -> bool {
  doc . get_first (field)
    . and_then ( |v| v . as_str () )
    . map ( |s| s == "true" )
    . unwrap_or (false) }

fn string_field (
  doc   : &TantivyDocument,
  field : schema::Field,
) -> Option<String> {
  doc . get_first (field)
    . and_then ( |v| v . as_str () )
    . map ( |s| s . to_string () ) }

