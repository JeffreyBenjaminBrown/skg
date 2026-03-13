// PURPOSE: Creates a Tantivy index from SkgNodes,
// associating each node's primary ID with its processed title and aliases.

// GLOSSARY:
// See the Tantivy section in glossary.md.

use crate::types::textlinks::replace_each_link_with_its_label;
use crate::types::misc::{ID, SourceName, TantivyIndex};
use crate::types::skgnode::{FileProperty, SkgNode};

use tantivy::{Index, IndexWriter, doc, Term, IndexReader, Searcher, Document};
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
  let is_title_field : schema::Field =
    schema . get_field ("is_title")
    . ok_or ("Schema missing 'is_title' field") ?;
  let had_id_field : schema::Field =
    schema . get_field ("had_id")
    . ok_or ("Schema missing 'had_id' field") ?;
  Ok ( TantivyIndex {
    index              : Arc::new (index),
    id_field,
    title_or_alias_field,
    source_field,
    context_origin_type_field,
    is_title_field,
    had_id_field, } ) }

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
  schema_builder . add_text_field(
    "is_title", schema::STRING | schema::STORED);
  schema_builder . add_text_field(
    "had_id", schema::STRING | schema::STORED);
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

  tracing::info! (
    "Finding files with titles or aliases matching \"{}\".",
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
    let documents: Vec<Document> =
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
        crate::consts::TANTIVY_PER_ID_LOOKUP_LIMIT ) ) . ok () ?;
  let mut fallback : Option < (String, SourceName) > = None;
  for (_score, doc_address) in &results {
    let retrieved_doc : Document =
      searcher . doc (*doc_address) . ok () ?;
    let is_title : bool =
      retrieved_doc
        . get_first ( tantivy_index . is_title_field )
        . and_then ( |v| v . as_text () )
        . map ( |s| s == "true" )
        . unwrap_or (false);
    let title_or_alias : Option < String > =
      retrieved_doc
        . get_first ( tantivy_index . title_or_alias_field )
        . and_then ( |v| v . as_text () )
        . map ( |s| s . to_string () );
    let source : SourceName =
      SourceName::from (
        retrieved_doc
          . get_first ( tantivy_index . source_field )
          . and_then ( |v| v . as_text () )
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
        &query, &TopDocs::with_limit (1) )
      { Ok (h) => h,
        Err (_) => continue };
    if let Some ( (_score, doc_address) ) = hits . first () {
      if let Ok (doc) = searcher . doc (*doc_address) {
        let had_id : bool =
          doc . get_first ( tantivy_index . had_id_field )
          . and_then ( |v| v . as_text () )
          . map ( |s| s == "true" )
          . unwrap_or (false);
        if had_id {
          result . insert (id . clone () ); }} }}
  result }

/* -------------------- Private helpers -------------------- */

fn create_documents_from_node (
  node: &SkgNode,
  tantivy_index: &TantivyIndex,
) -> Result < Vec < Document >,
              Box < dyn Error >> {
  let primary_id : &ID = &node . pid;
  let had_id : &str =
    if node . misc . contains (
      &FileProperty::Had_ID_Before_Import )
    { "true" } else { "false" };
  let mut documents: Vec<Document> =
    Vec::new();
  let mut titles_and_aliases: Vec<String> =
    vec![node . title . clone()];
  titles_and_aliases . extend_from_slice (
    node . aliases . or_default () );
  for (i, title_or_alias) in
    titles_and_aliases . iter() . enumerate()
  { let is_title : &str =
      if i == 0 { "true" } else { "false" };
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
          is_title,
        tantivy_index . had_id_field =>
          had_id ) ); }
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
      let is_title : String =
        retrieved_doc
          . get_first ( tantivy_index . is_title_field )
          . and_then ( |v| v . as_text () )
          . unwrap_or ("false") . to_string ();
      let had_id : String =
        retrieved_doc
          . get_first ( tantivy_index . had_id_field )
          . and_then ( |v| v . as_text () )
          . unwrap_or ("false") . to_string ();
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
          had_id . as_str () )) ?;
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
