// PURPOSE: Refresh the `prominence_source` field on every
// document matching a given ID. Because Tantivy has no in-place
// field mutation, this is a read-all-stored-fields / delete /
// add-back dance. The all-fields copying is the ugliest part of
// the Tantivy integration — isolating it here makes the cost
// visible and clearly motivates the "compute origin types before
// indexing" TODO in the schema doc-comment.

use crate::consts::{TANTIVY_PER_ID_LOOKUP_LIMIT, TANTIVY_WRITER_BUFFER_BYTES};
use crate::dbs::tantivy::background_writer::lock_tantivy_writes;
use crate::dbs::tantivy::write::tantivy_commit_with_status;
use crate::types::misc::{ID, SkgRepoName, TantivyIndex};

use tantivy::{IndexWriter, Searcher, Term, TantivyDocument, doc};
use tantivy::collector::TopDocs;
use tantivy::query::Query;
use tantivy::schema;
use tantivy::schema::document::Value;
use std::collections::HashMap;
use std::error::Error;

/// Updates prominence_source for all documents matching each ID.
/// Deletes and re-adds each document with the new prominence_source.
pub fn update_prominence_sources (
  tantivy_index          : &TantivyIndex,
  prominence_sources_by_skgid : &HashMap<ID, String>,
) -> Result<usize, Box<dyn Error>> {
  let searcher : Searcher =
    tantivy_index . reader . searcher ();
  let _wlock = // serialize with the background search-index worker & other writers
    lock_tantivy_writes ();
  let mut writer : IndexWriter =
    tantivy_index . index . writer (
      TANTIVY_WRITER_BUFFER_BYTES) ?;
  let mut updated_count : usize = 0;
  for (pid, prominence_source) in prominence_sources_by_skgid {
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
      let title_or_alias : String =
        retrieved_doc
          . get_first ( tantivy_index . title_or_alias_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("") . to_string ();
      let raw_title : String =
        retrieved_doc
          . get_first ( tantivy_index . raw_title_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("") . to_string ();
      let skgrepo : SkgRepoName =
        retrieved_doc
          . get_first ( tantivy_index . skgrepo_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("") . into ();
      let overPrivateText_telescope : String =
        retrieved_doc
          . get_first ( tantivy_index . overPrivateText_telescope_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("false") . to_string ();
      let no_search_matching : String =
        retrieved_doc
          . get_first ( tantivy_index . no_search_matching_field )
          . and_then ( |v| v . as_str () )
          . unwrap_or ("false") . to_string ();
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
        tantivy_index . raw_title_field =>
          raw_title . as_str (),
        tantivy_index . overPrivateText_telescope_field =>
          overPrivateText_telescope . as_str (),
        tantivy_index . no_search_matching_field =>
          no_search_matching . as_str (),
        tantivy_index . skgrepo_field =>
          skgrepo . as_str (),
        tantivy_index . prominence_source_field =>
          prominence_source . as_str (),
        tantivy_index . is_title_field =>
          is_title . as_str (),
        tantivy_index . had_id_field =>
          had_id . as_str (),
        tantivy_index . body_field =>
          body . as_str () )) ?;
      updated_count += 1; } }
  tantivy_commit_with_status (
    &mut writer, tantivy_index, updated_count, "Context-updated") ?;
  Ok (updated_count) }
