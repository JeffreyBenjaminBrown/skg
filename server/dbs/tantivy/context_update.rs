// PURPOSE: Refresh the `context_origin_type` field on every
// document matching a given ID. Because Tantivy has no in-place
// field mutation, this is a read-all-stored-fields / delete /
// add-back dance. The all-fields copying is the ugliest part of
// the Tantivy integration — isolating it here makes the cost
// visible and clearly motivates the "compute origin types before
// indexing" TODO in the schema doc-comment.

use crate::dbs::tantivy::write::commit_with_status;
use crate::types::misc::{ID, TantivyIndex};

use tantivy::{IndexReader, IndexWriter, Searcher, Term, TantivyDocument, doc};
use tantivy::collector::TopDocs;
use tantivy::query::Query;
use tantivy::schema;
use tantivy::schema::document::Value;
use std::collections::HashMap;
use std::error::Error;

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
