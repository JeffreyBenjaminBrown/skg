//! Rewriting the 'prominence_source' field of every document of given
//! IDs ('update_prominence_sources'). Tantivy cannot change a field in
//! place, so each document's stored fields are read, it is deleted,
//! and it is added back with the new value. What a node's prominence
//! is, is 'server/prominence.rs'.

use crate::consts::{TANTIVY_PER_ID_LOOKUP_LIMIT, TANTIVY_WRITER_BUFFER_BYTES};
use crate::dbs::tantivy::background_writer::lock_tantivy_writes;
use crate::dbs::tantivy::string_field;
use crate::dbs::tantivy::write::tantivy_commit_with_status;
use crate::types::misc::{ID, SkgrepoName, TantivyIndex};

use tantivy::{IndexWriter, Searcher, Term, TantivyDocument, doc};
use tantivy::collector::TopDocs;
use tantivy::query::Query;
use tantivy::schema;
use std::collections::HashMap;
use std::error::Error;

/// Updates prominence_source for all documents matching each ID.
/// Deletes and re-adds each document with the new prominence_source.
pub fn update_prominence_sources (
  tantivy_index          : &TantivyIndex,
  prominence_sources_by_gnid : &HashMap<ID, String>,
) -> Result<usize, Box<dyn Error>> {
  let searcher : Searcher =
    tantivy_index . reader . searcher ();
  let _wlock = // serialize with the background search-index worker & other writers
    lock_tantivy_writes ();
  let mut writer : IndexWriter =
    tantivy_index . index . writer (
      TANTIVY_WRITER_BUFFER_BYTES) ?;
  let mut updated_count : usize = 0;
  for (pid, prominence_source) in prominence_sources_by_gnid {
    let query : Box < dyn Query > =
      // Find all documents with this ID.
      Box::new ( tantivy::query::TermQuery::new (
        Term::from_field_text (
          tantivy_index . id_field, pid . as_str () ),
        schema::IndexRecordOption::Basic ));
    let results : Vec < (f32, tantivy::DocAddress) > =
      searcher . search (
        &query, &TopDocs::with_limit (
          TANTIVY_PER_ID_LOOKUP_LIMIT )
          . order_by_score () ) ?;
    if results . is_empty () { continue; }
    writer . delete_term ( // Delete all documents for this ID.
      Term::from_field_text (
        tantivy_index . id_field, pid . as_str () ));
    for (_score, doc_address) in &results {
      // Re-add with the new prominence_source.
      let retrieved_doc : TantivyDocument =
        searcher . doc (*doc_address) ?;
      writer . add_document (
        document_with_prominence_source (
          tantivy_index, &retrieved_doc, pid, prominence_source )) ?;
      updated_count += 1; } }
  tantivy_commit_with_status (
    &mut writer, tantivy_index, updated_count, "Context-updated") ?;
  Ok (updated_count) }

/// The document 'retrieved_doc' rebuilt with 'pid' as its ID and
/// 'prominence_source' as its prominence source. Each other field is
/// read back from what 'retrieved_doc' stores; an absent one becomes
/// its default ("" or "false").
fn document_with_prominence_source (
  tantivy_index     : &TantivyIndex,
  retrieved_doc     : &TantivyDocument,
  pid               : &ID,
  prominence_source : &str,
) -> TantivyDocument {
  let stored = |field : schema::Field, absent : &str| -> String {
    string_field (retrieved_doc, field)
      . unwrap_or_else ( || absent . to_string () ) };
  let title_or_alias : String =
    stored ( tantivy_index . title_or_alias_field, "" );
  let raw_title : String =
    stored ( tantivy_index . raw_title_field, "" );
  let skgrepo : SkgrepoName =
    stored ( tantivy_index . skgrepo_field, "" ) . into ();
  let overPrivateText_telescope : String =
    stored ( tantivy_index . overPrivateText_telescope_field, "false" );
  let no_search_matching : String =
    stored ( tantivy_index . no_search_matching_field, "false" );
  let is_title : String =
    stored ( tantivy_index . is_title_field, "false" );
  let had_id : String =
    stored ( tantivy_index . had_id_field, "false" );
  let body : String =
    stored ( tantivy_index . body_field, "" );
  doc! (
    tantivy_index . id_field =>
      pid . as_str (),
    tantivy_index . title_or_alias_field =>
      title_or_alias . as_str (),
    tantivy_index . raw_title_field =>
      raw_title . as_str (),
    tantivy_index . overPrivateText_telescope_field =>
      overPrivateText_telescope . as_str (),
    tantivy_index . no_search_matching_field =>
      no_search_matching . as_str (),
    tantivy_index . skgrepo_field =>
      skgrepo . as_str (),
    tantivy_index . prominence_source_field =>
      prominence_source,
    tantivy_index . is_title_field =>
      is_title . as_str (),
    tantivy_index . had_id_field =>
      had_id . as_str (),
    tantivy_index . body_field =>
      body . as_str () ) }
