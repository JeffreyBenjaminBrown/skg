use crate::dbs::filesystem::multiple_nodes::{write_all_nodes_to_fs, delete_all_nodes_from_fs};
use crate::dbs::init::{rebuild_typedb_from_disk, rebuild_tantivy_from_disk};
use crate::dbs::tantivy::{add_documents_to_tantivy_writer, commit_with_status, delete_nodes_by_id_from_index};
use crate::dbs::typedb::nodes::create_only_nodes_with_no_ids_present;
use crate::dbs::typedb::nodes::delete_nodes_from_pids;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::dbs::typedb::relationships::delete_out_links;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use crate::types::skgnode::SkgNode;

use itertools::{Itertools, Either};
use std::error::Error;
use std::io;
use std::time::{Duration, Instant};
use tantivy::IndexWriter;
use typedb_driver::TypeDBDriver;

/// Updates **everything** from the given `DefineNode`s, in order:
///   1) Filesystem (source of truth)
///   2) TypeDB (with recovery: rebuild from disk on failure)
///   3) Tantivy (with recovery: rebuild from disk on failure)
/// Returns `None` when all three stores updated normally.
/// Returns `Some(new_index)` when Tantivy had to be rebuilt.
pub async fn update_graph_minus_merges (
  instructions  : Vec<DefineNode>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  tracing::info!("Updating (1) FS, (2) TypeDB, and (3) Tantivy ...");
  let db_name : &str = &config . db_name;

  { // Step 1: FS (source of truth)
    // TODO: Print per-source write information
    tracing::info!( "1) Writing {} instruction(s) to disk ...",
               { let total_input : usize = instructions . len ();
                 total_input } );
    let (deleted_count, written_count) : (usize, usize) =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "update_fs_from_saveinstructions") . entered ();
        update_fs_from_saveinstructions (
          instructions . clone (), config . clone () ) } ?;
    tracing::info!( "   Deleted {} file(s), wrote {} file(s).",
              deleted_count, written_count ); }

  // Steps 2 & 3: TypeDB (async) and Tantivy (sync) in parallel.
  // Both are independent after the FS update.
  let tantivy_instructions : Vec<DefineNode> = instructions . clone ();
  let tantivy_idx : TantivyIndex = tantivy_index . clone ();
  let tantivy_handle : std::thread::JoinHandle<(Result<usize, String>, Duration)> =
    std::thread::spawn ( move || {
      let t0 : Instant = Instant::now ();
      let result : Result<usize, String> =
        update_tantivy_from_saveinstructions (
          &tantivy_instructions, &tantivy_idx )
        . map_err ( |e| e . to_string () );
      (result, t0 . elapsed ()) });

  if let Err (e) = { // Step 2: TypeDB
    let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "update_typedb_from_saveinstructions") . entered ();
    update_typedb_from_saveinstructions (
      db_name, driver, &instructions ) . await }
    { tracing::error!(
        "TypeDB update failed: {}. Rebuilding from disk...", e);
      rebuild_typedb_from_disk (&config, driver) . await
        . map_err (|e2| -> Box<dyn Error> {
          format!("TypeDB rebuild also failed: {}. Restart the server.", e2)
          . into () }) ?;
      tracing::warn!("Save succeeded, but TypeDB had to be rebuilt from disk.");
    } else {
      tracing::info!("   TypeDB update complete."); }

  // Step 3: collect Tantivy result from its thread
  let (tantivy_result, tantivy_duration)
    : (Result<usize, String>, Duration) =
    tantivy_handle . join ()
    . map_err (|_| -> Box<dyn Error> {
      "Tantivy thread panicked" . into () }) ?;
  tracing::info!("{}: {:.3}s", "update_tantivy_from_saveinstructions",
    tantivy_duration . as_secs_f64 ());
  match tantivy_result
    { Ok (indexed_count) => {
        tracing::info!( "   Tantivy updated for {} document(s).",
                  indexed_count );
        tracing::info!("All updates finished successfully.");
        Ok (None) }
      Err (e) => {
        tracing::error!("Tantivy update failed: {}. Rebuilding from disk...", e);
        let new_index : TantivyIndex =
          rebuild_tantivy_from_disk (&config)
          . map_err (|e2| -> Box<dyn Error> {
            format!("Tantivy rebuild also failed: {}. Restart the server.", e2)
            . into () }) ?;
        tracing::warn!("Save succeeded, but Tantivy had to be rebuilt from disk.");
        Ok (Some (new_index)) } } }

/// Update the DB from a batch of `DefineNode`s:
/// 1) Delete all nodes marked Delete, using delete_nodes_from_pids
/// 2) Remove deleted nodes from further processing
/// 3) Create only nodes whose IDs are not present, via
///      create_only_nodes_with_no_ids_present
/// 4) Delete all outbound `contains` from those nodes, via
///      delete_out_links
///    PITFALL: Only the primary ID from each SkgNode is used.
/// 5) Recreate all relationships for those nodes, via
///      create_all_relationships
pub async fn update_typedb_from_saveinstructions (
  db_name : &str,
  driver  : &TypeDBDriver,
  instructions : &Vec<DefineNode>,
) -> Result<(), Box<dyn Error>> {

  // PITFALL: Below, each get(0) on an 'ids' field
  // is not motivated by separating the PID from the others,
  // because (see add_missing_info_to_forest) there are no others.
  // It is simply to turn the Vec<ID> into a bare ID.

  let ( to_delete, to_save )
    : ( Vec<DeleteNode>, Vec<SaveNode> )
    = instructions . iter() . cloned() . partition_map (
      |instr| match instr {
        DefineNode::Delete (d) => Either::Left (d),
        DefineNode::Save (s)   => Either::Right (s) } );

  { // delete
    let to_delete_pids : Vec<ID> =
      to_delete . iter ()
      . map ( |DeleteNode { id, .. }| id . clone() )
      . collect ();
    if ! to_delete_pids . is_empty () {
      tracing::debug!("Deleting nodes with PIDs: {:?}", to_delete_pids);
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "delete_nodes_from_pids") . entered ();
        // PITFALL: deletions cascade in TypeDB by default,
        // so we are left with no incomplete relationships.
        delete_nodes_from_pids (
          db_name, driver, & to_delete_pids )
        . await } ?; }}

  { // create | update
    let to_write_skgnodes : Vec<SkgNode> =
      to_save . iter ()
      . map ( |SaveNode (node) | node . clone() )
      . collect ();
    let to_write_pids : Vec<ID> =
      to_write_skgnodes . iter ()
      . map ( |n| n . pid . clone() )
      . collect ();
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "create_only_nodes_with_no_ids_present") . entered ();
      create_only_nodes_with_no_ids_present (
        db_name, driver, & to_write_skgnodes )
      . await } ?;
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "delete_out_links") . entered ();
      delete_out_links (
        db_name, driver,
        & to_write_pids, // Will barf if nonempty, which is good.
        "contains",
        "container" )
      . await } ?;
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "create_all_relationships") . entered ();
      create_all_relationships (
        db_name, driver, & to_write_skgnodes )
      . await } ?; }

  Ok (( )) }

pub fn update_fs_from_saveinstructions (
  instructions : Vec<DefineNode>,
  config       : SkgConfig,
) -> io::Result<(usize, usize)> { // (deleted, written)
  let ( to_delete, to_save )
    : ( Vec<DeleteNode>, Vec<SaveNode> )
    = instructions . into_iter() . partition_map(
      |instr| match instr {
        DefineNode::Delete (d) => Either::Left (d),
        DefineNode::Save (s)   => Either::Right (s) } );
  let deleted : usize = {
    let delete_targets : Vec<(ID, SourceName)> =
      to_delete . into_iter ()
      . map ( |DeleteNode { id, source }| (id, source) )
      . collect ();
    if ! delete_targets . is_empty () {
      delete_all_nodes_from_fs (
        delete_targets, config . clone () ) ?
    } else { 0 } };
  let written : usize = {
    let nodes_to_write : Vec<SkgNode> =
      to_save . into_iter ()
      . map ( |SaveNode (node) | node )
      . collect ();
    if ! nodes_to_write . is_empty () {
      write_all_nodes_to_fs (
        nodes_to_write, config ) ?
    } else { 0 } };
  Ok ( (deleted, written) ) }


/// Updates the index with the provided DefineNodes.
/// Deletes IDs from the index for every instruction,
/// but only adds documents for instructions where is_save.
/// Returns the number of documents processed.
pub(super) fn update_tantivy_from_saveinstructions (
  instructions  : &[DefineNode],
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut writer: IndexWriter =
    tantivy_index . index . writer (
      crate::consts::TANTIVY_WRITER_BUFFER_BYTES)?;
  delete_nodes_by_id_from_index(
    // Delete all IDs, be they from Saves or Deletes.
    // (The entry for each Save is then recreated.)
    instructions . iter() . map(|instr| match instr {
      DefineNode::Save(SaveNode (node)) => &node . pid,
      DefineNode::Delete(DeleteNode { id, .. }) => id }),
    &mut writer,
    tantivy_index)?;
  let processed_count: usize =
    add_documents_to_tantivy_writer(
      { // Add documents only for non-deletion instructions.
        let nodes_to_add: Vec<&SkgNode> =
          instructions . iter()
          . filter_map( |instr| match instr {
              DefineNode::Save(SaveNode (node)) => Some (node),
              DefineNode::Delete (_) => None } )
          . collect();
        nodes_to_add },
      &mut writer, tantivy_index )? ;
  commit_with_status(
    &mut writer, processed_count, "Updated")?;
  Ok (processed_count) }
