// PURPOSE: The index-write path: update/delete/add document
// helpers, plus `create_documents_from_node` (one title doc + one
// doc per alias), and the shared `commit_with_status` used by both
// this module and `context_update`.

use crate::consts::TANTIVY_WRITER_BUFFER_BYTES;
use crate::dbs::tantivy::background_writer::lock_tantivy_writes;
use crate::types::misc::{ID, SourceName, TantivyIndex};
use crate::types::nodes::complete::FileProperty;
use crate::types::nodes::tantivy::NodeTantivy;
use crate::types::textlinks::replace_each_link_with_its_label;

use tantivy::{IndexWriter, Term, TantivyDocument, doc};
use std::collections::HashMap;
use std::error::Error;

/// Updates the index with the provided NodeTantivys.
///   For existing IDs, updates the title.
///   For new IDs, adds new entries.
/// Returns the number of documents processed.
pub fn update_index_with_nodes (
  nodes: &[NodeTantivy],
  tantivy_index: &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let _wlock = // serialize with the background save-index worker & other writers
    lock_tantivy_writes ();
  let mut writer: IndexWriter =
    tantivy_index . index . writer (
      TANTIVY_WRITER_BUFFER_BYTES)?;
  delete_nodes_from_index(
    // Delete those IDs from the index. (They'll come back.)
    nodes . iter(), &mut writer, tantivy_index)?;
  let processed_count: usize = // Add new associations.
    add_documents_to_tantivy_writer (
      nodes, &mut writer, tantivy_index, &HashMap::new ())?;
  commit_with_status(
    &mut writer, tantivy_index, processed_count, "Updated")?;
  Ok (processed_count) }

pub fn delete_nodes_from_index<'a, I>(
  nodes_iter: I,
  writer: &mut IndexWriter,
  tantivy_index: &TantivyIndex,
) -> Result<(), Box<dyn Error>>
where I: Iterator<Item = &'a NodeTantivy>, {
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
  context_types : &HashMap<ID, String>, // pid -> context_origin_type label; pids absent here index with "" (filled at init/rebuild).
) -> Result<usize, Box<dyn Error>>
where I: IntoIterator<Item = &'a NodeTantivy>, {

  let mut indexed_count: usize = 0;
  for node in nodes {
    let documents: Vec<TantivyDocument> =
      create_documents_from_node(
        node, tantivy_index, context_types )?;
    for document in documents {
      writer . add_document (document)?;
      indexed_count += 1; }}
  Ok (indexed_count) }

fn create_documents_from_node (
  node: &NodeTantivy,
  tantivy_index: &TantivyIndex,
  context_types : &HashMap<ID, String>,
) -> Result < Vec < TantivyDocument >,
              Box < dyn Error >> {
  let primary_id : &ID = &node . pid;
  let context_origin_type : &str =
    context_types . get (primary_id)
    . map ( |s| s . as_str () ) . unwrap_or ("");
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
  let mut titles_and_aliases: Vec<(String, SourceName)> =
    // Each entry with the SOURCE its document will carry: the home
    // for the title, the alias's own privacy LEVEL for an alias --
    // so restricted search filtering excludes private aliases of
    // public nodes (dbs-and-search, 5_plan.org).
    vec![ ( node . title . clone(),
            node . source . clone() ) ];
  titles_and_aliases . extend (
    node . aliases . or_default () . iter ()
    . map ( |a| ( a . member . clone (),
                  a . level . clone () )));
  for (i, (title_or_alias, doc_source)) in
    titles_and_aliases . iter() . enumerate()
  { let is_title : bool = i == 0;
    let is_title_str : &str =
      if is_title { "true" } else { "false" };
    let body_for_this_doc : &str =
      if is_title { body_text . as_str () } else { "" };
    // Un-reduced title preserved only on the primary-title doc;
    // alias docs get an empty string.
    let raw_title_for_this_doc : &str =
      if is_title { title_or_alias . as_str () } else { "" };
    documents . push (
      doc!(
        tantivy_index . id_field =>
          primary_id . as_str (),
        tantivy_index . title_or_alias_field =>
          replace_each_link_with_its_label (
            title_or_alias ),
        tantivy_index . raw_title_field =>
          raw_title_for_this_doc,
        tantivy_index . source_field =>
          doc_source . as_str(),
        tantivy_index . context_origin_type_field =>
          context_origin_type,
        tantivy_index . is_title_field =>
          is_title_str,
        tantivy_index . had_id_field =>
          had_id,
        tantivy_index . body_field =>
          body_for_this_doc ) ); }
  Ok (documents) }

pub fn commit_with_status (
  writer        : &mut IndexWriter,
  tantivy_index : &TantivyIndex,
  indexed_count : usize,
  operation     : &str,
) -> Result<(), Box<dyn Error>> {
  if indexed_count > 0 {
    tracing::info!( "{} {} documents. Committing changes...",
              operation, indexed_count );
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "tantivy_writer_commit" ). entered();
      writer . commit () ? ; }
    // Force the shared reader to see the new commit. With the
    // default 'ReloadPolicy::OnCommitWithDelay', the reader
    // refreshes asynchronously after a small delay, which races
    // with code (notably tests) that searches immediately after a
    // commit. Manual reload makes the post-commit state visible
    // synchronously.
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "tantivy_reader_reload" ). entered();
      tantivy_index . reader . reload () ? ; }
  } else {
    tracing::debug!("No documents to process found."); }
  Ok (( )) }
