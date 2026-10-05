// PURPOSE: Tantivy integration. This file holds schema + search-index
// opening, plus exact-ID document lookups. The other phases live
// in submodules:
//   - escape:         query preprocessing (pure strings).
//   - search:         the text-search API (QueryParser / RegexQuery).
//   - write:          update/delete/add documents + commit helper.
//   - prominence_update: refresh `prominence_source` via
//                     delete-and-readd.

// GLOSSARY:
// See the Tantivy section in docs/glossary.org.

pub mod background_writer;
pub mod prominence_update;
pub mod escape;
pub mod search;
pub mod write;

use crate::consts::TANTIVY_PER_ID_LOOKUP_LIMIT;
use crate::types::misc::{ID, SkgRepoName, TantivyIndex};

use tantivy::{Index, Term, Searcher, TantivyDocument};
use tantivy::schema::document::Value;
use tantivy::query::Query;
use tantivy::collector::TopDocs;
use tantivy::schema;
use std::collections::HashMap;
use std::error::Error;
use std::sync::Arc;

/// Build a TantivyIndex from an already-constructed Index: derive its schema,
/// look up the fields, and open a reader. Shared by the open-existing,
/// create-in-dir, and create-in-RAM constructors, which differ only in how the
/// Index itself is obtained.
pub(crate) fn tantivy_index_from_index (
  index : Index,
) -> Result<TantivyIndex, Box<dyn Error>> {
  let reader : tantivy::IndexReader =
    index . reader () ?;
  let schema : schema::Schema =
    index . schema();
  let id_field : schema::Field =
    schema . get_field ("id") ?;
  let title_or_alias_field : schema::Field =
    schema . get_field ("title_or_alias") ?;
  let raw_title_field : schema::Field =
    schema . get_field ("raw_title") ?;
  let overPrivateText_telescope_field : schema::Field =
    schema . get_field ("overPrivateText_telescope") ?;
  let no_search_matching_field : schema::Field =
    schema . get_field ("no_search_matching") ?;
  let skgrepo_field : schema::Field =
    schema . get_field ("repo") ?;
  let prominence_source_field : schema::Field =
    schema . get_field ("prominence_source") ?;
  let is_title_field : schema::Field =
    schema . get_field ("is_title") ?;
  let had_id_field : schema::Field =
    schema . get_field ("had_id") ?;
  let body_field : schema::Field =
    schema . get_field ("body") ?;
  Ok ( TantivyIndex {
    index              : Arc::new (index),
    reader,
    id_field,
    title_or_alias_field,
    raw_title_field,
    overPrivateText_telescope_field,
    no_search_matching_field,
    skgrepo_field,
    prominence_source_field,
    is_title_field,
    had_id_field,
    body_field, } ) }

/// The Tantivy schema.
/// Fields:
/// - "id":                  STRING | STORED — the node's primary ID.
/// - "title_or_alias":      TEXT   | STORED — searchable titles and aliases,
///                          with links reduced to their labels.
/// - "raw_title":           STRING | STORED — the un-reduced title, stored
///                          only on is_title="true" docs. Preserves the
///                          link syntax that 'title_or_alias' strips.
/// - "overPrivateText_telescope":      STRING | STORED — "true" when title or body
///                          was selected below the node's home.
/// - "no_search_matching":  STRING | STORED — "true" when this
///                          document must not directly match text search.
/// - "repo":              STRING | STORED — the skgrepo name.
/// - "prominence_source": STRING | STORED — Root/CycleMember/Target/…
/// - "is_title":            STRING | STORED — "true" for the primary title,
///                          "false" for alias docs.
/// - "had_id":              STRING | STORED — "true" if the node had an
///                          org-roam ID before import.
/// - "body":                TEXT   | STORED — searchable body text. STORED
///                          so that `update_prominence_sources` can
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
    "overPrivateText_telescope", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "no_search_matching", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "repo", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "prominence_source", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "is_title", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "had_id", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "body", schema::TEXT | schema::STORED);
  schema_builder . build() }

/// Look up the canonical title and skgrepo for a node by its exact primary ID.
/// Prefers the document marked is_title="true"; falls back to the
/// first title_or_alias found if no title document exists.
///
/// Returns the raw_title (with link syntax intact) when available,
/// so the user sees `[[id:X][label]]` rather than just `label` -- this
/// preserves the visual distinction between a node titled "science"
/// and one whose title links to a different node also labelled
/// "science". Alias-doc fallback paths use 'title_or_alias' because
/// only is_title="true" docs carry a 'raw_title'.
pub fn title_and_skgrepo_by_skgid (
  tantivy_index : &TantivyIndex,
  skgid            : &ID,
) -> Option < (String, SkgRepoName) > {
  let searcher : Searcher = tantivy_index . reader . searcher ();
  let doc_addresses : Vec<tantivy::DocAddress> =
    doc_addresses_for_skgid (
      tantivy_index, &searcher, skgid,
      TANTIVY_PER_ID_LOOKUP_LIMIT ) ?;
  let (doc, was_fallback) : (TantivyDocument, bool) =
    pick_title_doc ( tantivy_index, &searcher, skgid, &doc_addresses ) ?;
  if was_fallback {
    tracing::warn! (
      "title_and_repo_by_id: no is_title=\"true\" document \
       found for ID {}. Falling back to first title_or_alias.",
      skgid ); }
  let title : String =
    string_field ( &doc, tantivy_index . raw_title_field )
      . filter ( |s| ! s . is_empty () )
      . or_else ( || string_field (
        &doc, tantivy_index . title_or_alias_field )) ?;
  let skgrepo : SkgRepoName = SkgRepoName::from (
    string_field ( &doc, tantivy_index . skgrepo_field )
      . unwrap_or_default () . as_str () );
  Some ( (title, skgrepo) ) }

/// Look up canonical titles for multiple IDs in a single searcher session.
/// IDs not found in Tantivy are absent from the result.
pub fn titles_by_skgids (
  tantivy_index : &TantivyIndex,
  skgids           : &[ID],
) -> HashMap<ID, String> {
  let mut result : HashMap<ID, String> = HashMap::new ();
  let searcher : Searcher = tantivy_index . reader . searcher ();
  for skgid in skgids {
    let doc_addresses : Vec<tantivy::DocAddress> =
      match doc_addresses_for_skgid (
        tantivy_index, &searcher, skgid,
        TANTIVY_PER_ID_LOOKUP_LIMIT )
      { Some (a) => a,
        None     => continue };
    let (doc, was_fallback) : (TantivyDocument, bool) =
      match pick_title_doc (
        tantivy_index, &searcher, skgid, &doc_addresses )
      { Some (d) => d,
        None     => continue };
    if was_fallback {
      tracing::debug! (
        "titles_by_ids: no is_title=\"true\" document \
         found for ID {}. Falling back to first title_or_alias.",
        skgid ); }
    // Prefer the un-reduced raw title (only on is_title="true" docs);
    // fall back to 'title_or_alias' for the alias-only fallback case,
    // which has no raw_title stored.
    let title : Option<String> =
      string_field ( &doc, tantivy_index . raw_title_field )
        . filter ( |s| !s . is_empty () )
        . or_else ( || string_field (
          &doc, tantivy_index . title_or_alias_field ));
    if let Some (t) = title
    { result . insert ( skgid . clone (), t ); } }
  result }


/// Run an exact-match TermQuery against the id_field, returning
/// the matching doc addresses (scores discarded). None on any
/// index/search error; an empty vec means "no hits" (distinct from
/// error).
fn doc_addresses_for_skgid (
  tantivy_index : &TantivyIndex,
  searcher      : &Searcher,
  skgid         : &ID,
  limit         : usize,
) -> Option < Vec<tantivy::DocAddress> > {
  let query : Box<dyn Query> =
    Box::new ( tantivy::query::TermQuery::new (
      Term::from_field_text (
        tantivy_index . id_field, skgid . as_str () ),
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
  skgid         : &ID,
  doc_addresses : &[tantivy::DocAddress],
) -> Option < (TantivyDocument, bool) > {
  let mut fallback : Option<TantivyDocument> = None;
  for addr in doc_addresses {
    let doc : TantivyDocument =
      match load_doc_or_warn ( searcher, *addr, skgid ) {
        Some (d) => d,
        None     => continue };
    if bool_field_eq_true ( &doc, tantivy_index . is_title_field ) {
      return Some ( (doc, false) ); }
    if fallback . is_none () {
      fallback = Some (doc); } }
  fallback . map ( |d| (d, true) ) }

/// Fetch a stored doc by address. Logs a warning on failure so
/// operators can diagnose corrupt search indexes; returns None so callers
/// can skip this address and continue.
fn load_doc_or_warn (
  searcher : &Searcher,
  addr     : tantivy::DocAddress,
  skgid    : &ID,
) -> Option < TantivyDocument > {
  match searcher . doc (addr) {
    Ok (doc) => Some (doc),
    Err (e)  => {
      tracing::warn! (
        "Failed to load Tantivy doc for ID {} at {:?}: {}",
        skgid, addr, e );
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
