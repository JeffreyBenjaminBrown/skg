// PURPOSE: Update Tantivy index from SaveInstructions.

use crate::textlinks::replace_each_link_with_its_label;
use crate::types::{ID, SkgNode, TantivyIndex, SaveInstruction};

use tantivy::{IndexWriter, doc, Term, Document};
use std::error::Error;


/// Updates the index with the provided SaveInstructions.
/// Deletes IDs from the index for every instruction,
/// but only adds documents for instructions where toDelete is false.
/// Returns the number of documents processed.
pub fn update_index_from_saveinstructions (
  instructions  : &[SaveInstruction],
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut writer: IndexWriter =
    tantivy_index.index.writer(50_000_000)?;
  { // Delete all IDs in the SaveInstructions from the index --
    // be they toDelete or otherwise.
    // (Those that aren't will come back in the next step.)
    for (node, _action) in instructions {
      if !node.ids.is_empty() {
        let primary_id: &ID = &node.ids[0];
        let term: Term = Term::from_field_text(
          tantivy_index.id_field,
          primary_id.as_str() );
        writer.delete_term(term); } } }
  { // Add documents only for non-deleted instructions.
    let mut processed_count: usize = 0;
    for (node, action) in instructions {
      if ! action . toDelete {
        let documents : Vec < Document > =
          create_documents_from_node (
            node, tantivy_index )?;
        for document in documents {
          writer.add_document (document)?;
          processed_count += 1; }} }
    commit_with_status(
      &mut writer, processed_count, "Updated")?;
    Ok (processed_count) }}


/* -------------------- Private helpers -------------------- */

fn create_documents_from_node (
  node: &SkgNode,
  tantivy_index: &TantivyIndex,
) -> Result < Vec < Document >,
            Box < dyn Error >> {

  if node.ids.is_empty() {
    return Err("SkgNode has no IDs" . into () ); }
  let primary_id: &ID = &node.ids[0];
  let mut documents_acc: Vec<Document> =
    Vec::new();
  let mut titles_and_aliases: Vec<String> = // what to index
    vec![node.title.clone()];
  if let Some(aliases) = &node.aliases {
    titles_and_aliases . extend(
      aliases.clone () ); }
  for title_or_alias in titles_and_aliases { // index them
    let doc : Document = doc!(
      tantivy_index . id_field =>
        primary_id.as_str (),
      tantivy_index . title_or_alias_field =>
        replace_each_link_with_its_label (
          & title_or_alias ));
    documents_acc.push (doc); }
  Ok ( documents_acc ) }

fn commit_with_status (
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
