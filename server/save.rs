use crate::dbs::filesystem::multiple_nodes::{write_all_nodes_to_fs, delete_all_nodes_from_fs};
use crate::dbs::tantivy::{add_documents_to_tantivy_writer, commit_with_status, delete_nodes_by_id_from_index};
use crate::dbs::neo4j::nodes::create_only_nodes_with_no_ids_present;
use crate::dbs::neo4j::nodes::delete_nodes_from_pids;
use crate::dbs::neo4j::relationships::create_all_relationships;
use crate::dbs::neo4j::relationships::delete_out_links;
use crate::serve::timing_log::{timed, timed_async};
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use crate::types::skgnode::SkgNode;

use itertools::{Itertools, Either};
use std::error::Error;
use std::io;
use tantivy::IndexWriter;
use neo4rs::Graph;

/// Updates **everything** from the given `DefineNode`s, in order:
///   1) Neo4j
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn update_graph_minus_merges (
  instructions  : Vec<DefineNode>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  graph         : &Graph,
) -> Result < (), Box<dyn Error> > {
  println!( "Updating (1) Neo4j, (2) FS, and (3) Tantivy ..." );

  { println!( "1) Updating Neo4j ..." );
    timed_async ( &config, "update_neo4j_from_saveinstructions",
                  update_neo4j_from_saveinstructions (
                    graph, &instructions )) . await ?;
    println!( "   Neo4j update complete." ); }

  { // filesystem
    // TODO: Print per-source write information
    println!( "2) Writing {} instruction(s) to disk ...",
               { let total_input : usize = instructions.len ();
                 total_input } );
    let (deleted_count, written_count) : (usize, usize) =
      timed ( &config, "update_fs_from_saveinstructions",
              || update_fs_from_saveinstructions (
                   instructions.clone (), config.clone () )) ?;
    println!( "   Deleted {} file(s), wrote {} file(s).",
              deleted_count, written_count ); }

  { // Tantivy
    println!( "3) Updating Tantivy index ..." );
    let indexed_count : usize =
      timed ( &config, "update_tantivy_from_saveinstructions",
              || update_tantivy_from_saveinstructions (
                   &instructions, tantivy_index )) ?;
    println!( "   Tantivy updated for {} document(s).",
              indexed_count ); }

  println!( "All updates finished successfully." );
  Ok (( )) }

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
pub async fn update_neo4j_from_saveinstructions (
  graph        : &Graph,
  instructions : &Vec<DefineNode>
) -> Result<(), Box<dyn Error>> {

  // PITFALL: Below, each get(0) on an 'ids' field
  // is not motivated by separating the PID from the others,
  // because (see add_missing_info_to_forest) there are no others.
  // It is simply to turn the Vec<ID> into a bare ID.

  let ( to_delete, to_save )
    : ( Vec<DeleteNode>, Vec<SaveNode> )
    = instructions . iter() . cloned() . partition_map (
      |instr| match instr {
        DefineNode::Delete(d) => Either::Left(d),
        DefineNode::Save(s)   => Either::Right(s) } );

  { // delete
    let to_delete_pids : Vec<ID> =
      to_delete . iter ()
      . map ( |DeleteNode { id, .. }| id.clone() )
      . collect ();
    if ! to_delete_pids . is_empty () {
      println!("Deleting nodes with PIDs: {:?}", to_delete_pids);
      delete_nodes_from_pids (
        graph, & to_delete_pids ). await ?; }}

  { // create | update
    let to_write_skgnodes : Vec<SkgNode> =
      to_save . iter ()
      . map ( |SaveNode(node)| node.clone() )
      . collect ();
    let to_write_pids : Vec<ID> =
      to_write_skgnodes . iter ()
      . filter_map ( |n|
                      n . ids
                      . get(0)
                      . cloned() )
      . collect ();
    create_only_nodes_with_no_ids_present (
      graph, & to_write_skgnodes ). await ?;
    delete_out_links (
      graph,
      & to_write_pids, // Will barf if nonempty, which is good.
      "contains" ). await ?;
    create_all_relationships (
      graph, & to_write_skgnodes ). await ?; }

  Ok (( )) }

pub fn update_fs_from_saveinstructions (
  instructions : Vec<DefineNode>,
  config       : SkgConfig,
) -> io::Result<(usize, usize)> { // (deleted, written)
  let ( to_delete, to_save )
    : ( Vec<DeleteNode>, Vec<SaveNode> )
    = instructions.into_iter().partition_map(
      |instr| match instr {
        DefineNode::Delete(d) => Either::Left(d),
        DefineNode::Save(s)   => Either::Right(s) } );
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
      . map ( |SaveNode(node)| node )
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
    tantivy_index.index.writer(50_000_000)?;
  delete_nodes_by_id_from_index(
    // Delete all IDs, be they from Saves or Deletes.
    // (The entry for each Save is then recreated.)
    instructions.iter().map(|instr| match instr {
      DefineNode::Save(SaveNode(node)) => node.primary_id(),
      DefineNode::Delete(DeleteNode { id, .. }) => Ok(id) }),
    &mut writer,
    tantivy_index)?;
  let processed_count: usize =
    add_documents_to_tantivy_writer(
      { // Add documents only for non-deletion instructions.
        let nodes_to_add: Vec<&SkgNode> =
          instructions . iter()
          . filter_map( |instr| match instr {
              DefineNode::Save(SaveNode(node)) => Some(node),
              DefineNode::Delete(_) => None } )
          . collect();
        nodes_to_add },
      &mut writer, tantivy_index )? ;
  commit_with_status(
    &mut writer, processed_count, "Updated")?;
  Ok (processed_count) }
