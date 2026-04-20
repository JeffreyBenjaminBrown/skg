// PURPOSE: The index-write path: update/delete/add document
// helpers, plus `create_documents_from_node` (one title doc + one
// doc per alias), and the shared `commit_with_status` used by both
// this module and `context_update`.

use crate::types::misc::{ID, TantivyIndex};
use crate::types::skgnode::{FileProperty, SkgNode};
use crate::types::textlinks::replace_each_link_with_its_label;

use tantivy::{IndexWriter, Term, TantivyDocument, doc};
use std::error::Error;

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
