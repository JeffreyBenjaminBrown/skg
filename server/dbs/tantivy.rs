// PURPOSE: Creates a Tantivy index from SkgNodes,
// associating each node's primary ID with its processed title and aliases.

// GLOSSARY:
// See the Tantivy section in glossary.md.

use crate::types::textlinks::replace_each_link_with_its_label;
use crate::types::misc::{ID, TantivyIndex};
use crate::types::skgnode::SkgNode;

use tantivy::{IndexWriter, doc, Term, IndexReader, Searcher, Document};
use tantivy::query::{QueryParser, Query};
use tantivy::collector::TopDocs;
use tantivy::schema;
use std::error::Error;


/// The only Tantivy schema used so far.
/// It includes three fields:
/// - "id": STRING | STORED - the node's primary ID
/// - "title_or_alias": TEXT | STORED - searchable titles and aliases
/// - "source": STRING | STORED - the source nickname
pub(super) fn mk_tantivy_schema() -> schema::Schema {
  let mut schema_builder : schema::SchemaBuilder =
    schema::Schema::builder();
  schema_builder.add_text_field(
    "id", schema::STRING | schema::STORED);
  schema_builder.add_text_field(
    "title_or_alias", schema::TEXT | schema::STORED);
  schema_builder.add_text_field(
    "source", schema::STRING | schema::STORED);
  schema_builder.build() }

/// Returns the top 10 matching Tantivy "Documents"
/// (see glossary, above).
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
      tantivy_index.index.reader () ?;
    reader } .searcher();
  let query : Box < dyn Query > = {
    let query_parser : QueryParser =
      QueryParser::for_index (
        &tantivy_index.index,
        vec! [ tantivy_index.title_or_alias_field ] );
    query_parser } .parse_query ( query_text ) ?;
  Ok (( {
    let best_matches : Vec < ( f32, tantivy::DocAddress ) > =
      searcher.search (
        &query, &TopDocs::with_limit (10) )?;
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
    tantivy_index.index.writer(50_000_000)?;
  delete_nodes_from_index(
    // Delete those IDs from the index. (They'll come back.)
    nodes.iter(), &mut writer, tantivy_index)?;
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
    if !node.ids.is_empty() {
      let primary_id : &ID = node.primary_id()?;
      writer . delete_term (
        Term::from_field_text( tantivy_index.id_field,
                               primary_id.as_str() ) ); }}
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
      writer.add_document (document)?;
      indexed_count += 1; }}
  Ok (indexed_count) }


/* -------------------- Private helpers -------------------- */

fn create_documents_from_node (
  node: &SkgNode,
  tantivy_index: &TantivyIndex,
) -> Result < Vec < Document >,
              Box < dyn Error >> {

  let primary_id : &ID = node.primary_id()?;
  let mut documents: Vec<Document> =
    Vec::new();
  let mut titles_and_aliases: Vec<String> =
    vec![node.title.clone()];
  if let Some(aliases) = &node.aliases {
    titles_and_aliases . extend(
      aliases.clone () ); }
  for title_or_alias in titles_and_aliases {
    documents.push (
      doc!(
        tantivy_index . id_field =>
          primary_id.as_str (),
        tantivy_index . title_or_alias_field =>
          replace_each_link_with_its_label (
            & title_or_alias ),
        tantivy_index . source_field =>
          node.source.as_str() ) ); }
  Ok ( documents ) }

pub fn commit_with_status (
  writer: &mut IndexWriter,
  indexed_count: usize,
  operation: &str,
) -> Result<(), Box<dyn Error>> {
  if indexed_count > 0 {
    println!( "{} {} documents. Committing changes...",
              operation, indexed_count );
    writer.commit () ?;
  } else {
    println!("No documents to process found."); }
  Ok (( )) }
