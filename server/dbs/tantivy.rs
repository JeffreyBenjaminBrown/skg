// PURPOSE: Creates a Tantivy index from SkgNodes,
// associating each node's primary ID with its processed title and aliases.

// GLOSSARY:
// See the Tantivy section in glossary.md.

use crate::types::textlinks::replace_each_link_with_its_label;
use crate::types::misc::{ID, TantivyIndex};
use crate::types::skgnode::SkgNode;

use tantivy::{Index, IndexWriter, doc, Term, IndexReader, Searcher, Document};
use tantivy::query::{QueryParser, Query};
use tantivy::collector::TopDocs;
use tantivy::schema;
use std::collections::HashMap;
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
    schema . get_field ("id")
    . ok_or ("Schema missing 'id' field") ?;
  let title_or_alias_field : schema::Field =
    schema . get_field ("title_or_alias")
    . ok_or ("Schema missing 'title_or_alias' field") ?;
  let source_field : schema::Field =
    schema . get_field ("source")
    . ok_or ("Schema missing 'source' field") ?;
  let context_origin_type_field : schema::Field =
    schema . get_field ("context_origin_type")
    . ok_or ("Schema missing 'context_origin_type' field") ?;
  Ok ( TantivyIndex {
    index              : Arc::new (index),
    id_field,
    title_or_alias_field,
    source_field,
    context_origin_type_field, } ) }

/// The only Tantivy schema used so far.
/// It includes three fields:
/// - "id": STRING | STORED - the node's primary ID
/// - "title_or_alias": TEXT | STORED - searchable titles and aliases
/// - "source": STRING | STORED - the source nickname
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
  schema_builder . build() }

/// Returns ALL matching Tantivy "Documents" (see glossary, above).
/// No limit: context-based reranking (multipliers up to 100x)
/// can reorder results arbitrarily, so the caller must see
/// every match. The caller truncates after reranking.
pub fn search_index (
  tantivy_index : &TantivyIndex,
  query_text    : &str
) -> Result <
    ( Vec< ( f32, // relevance score
             tantivy::DocAddress )>, // ID for a tantivy Document. (Not a filepath.)
      Searcher ),
    Box <dyn std::error::Error> > {

  println! (
    "\nFinding files with titles or aliases matching \"{}\".",
    query_text);
  let searcher : Searcher = {
    let reader : IndexReader =
      tantivy_index . index . reader () ?;
    reader } . searcher();
  let query : Box < dyn Query > = {
    let query_parser : QueryParser =
      QueryParser::for_index (
        &tantivy_index . index,
        vec! [ tantivy_index . title_or_alias_field ] );
    query_parser } . parse_query (query_text) ?;
  Ok (( {
    let best_matches : Vec < ( f32, tantivy::DocAddress ) > =
      searcher . search (
        &query, &TopDocs::with_limit (
          crate::consts::TANTIVY_SEARCH_LIMIT ) )?;
    best_matches },
       searcher )) }

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
    if !node . ids . is_empty() {
      let primary_id : &ID = node . primary_id()?;
      writer . delete_term (
        Term::from_field_text( tantivy_index . id_field,
                               primary_id . as_str() ) ); }}
  Ok (( )) }

pub fn delete_nodes_by_id_from_index<'a, I>(
  ids_iter: I,
  writer: &mut IndexWriter,
  tantivy_index: &TantivyIndex,
) -> Result<(), Box<dyn Error>>
where I: Iterator<Item = Result<&'a ID, String>>, {
  for id_result in ids_iter {
    let id : &ID = id_result?;
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
    let documents: Vec<Document> =
      create_documents_from_node(
        node, tantivy_index )?;
    for document in documents {
      writer . add_document (document)?;
      indexed_count += 1; }}
  Ok (indexed_count) }


/* -------------------- Private helpers -------------------- */

fn create_documents_from_node (
  node: &SkgNode,
  tantivy_index: &TantivyIndex,
) -> Result < Vec < Document >,
              Box < dyn Error >> {

  let primary_id : &ID = node . primary_id()?;
  let mut documents: Vec<Document> =
    Vec::new();
  let mut titles_and_aliases: Vec<String> =
    vec![node . title . clone()];
  if let Some (aliases) = &node . aliases {
    titles_and_aliases . extend(
      aliases . clone () ); }
  for title_or_alias in titles_and_aliases {
    documents . push (
      doc!(
        tantivy_index . id_field =>
          primary_id . as_str (),
        tantivy_index . title_or_alias_field =>
          replace_each_link_with_its_label (
            & title_or_alias ),
        tantivy_index . source_field =>
          node . source . as_str(),
        tantivy_index . context_origin_type_field =>
          "" ) ); }
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
          crate::consts::TANTIVY_PER_ID_LOOKUP_LIMIT )) ?;
    if results . is_empty () { continue; }
    writer . delete_term ( // Delete all documents for this ID.
      Term::from_field_text (
        tantivy_index . id_field, pid . as_str () ));
    for (_score, doc_address) in &results {
      // Re-add with the new context_origin_type.
      let retrieved_doc : Document =
        searcher . doc (*doc_address) ?;
      let title_or_alias : String =
        retrieved_doc
          . get_first ( tantivy_index . title_or_alias_field )
          . and_then ( |v| v . as_text () )
          . unwrap_or ("") . to_string ();
      let source : String =
        retrieved_doc
          . get_first ( tantivy_index . source_field )
          . and_then ( |v| v . as_text () )
          . unwrap_or ("") . to_string ();
      writer . add_document ( doc! (
        tantivy_index . id_field =>
          pid . as_str (),
        tantivy_index . title_or_alias_field =>
          title_or_alias . as_str (),
        tantivy_index . source_field =>
          source . as_str (),
        tantivy_index . context_origin_type_field =>
          context_type . as_str () )) ?;
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
    println!( "{} {} documents. Committing changes...",
              operation, indexed_count );
    writer . commit () ?;
  } else {
    println!("No documents to process found."); }
  Ok (( )) }
