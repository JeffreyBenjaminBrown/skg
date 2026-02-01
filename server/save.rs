use crate::dbs::filesystem::multiple_nodes::{write_all_nodes_to_fs, delete_all_nodes_from_fs};
use crate::dbs::tantivy::{add_documents_to_tantivy_writer, commit_with_status, delete_nodes_from_index};
use crate::dbs::typedb::nodes::create_only_nodes_with_no_ids_present;
use crate::dbs::typedb::nodes::delete_nodes_from_pids;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::dbs::typedb::relationships::delete_out_links;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::save::DefineOneNode;
use crate::types::skgnode::SkgNode;

use std::error::Error;
use std::io;
use tantivy::IndexWriter;
use typedb_driver::TypeDBDriver;

/// Updates **everything** from the given `DefineOneNode`s, in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn update_graph_minus_merges (
  instructions  : Vec<DefineOneNode>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  println!( "Updating (1) TypeDB, (2) FS, and (3) Tantivy ..." );

  let db_name : &str = &config.db_name;

  { println!( "1) Updating TypeDB database '{}' ...", db_name );
    update_typedb_from_saveinstructions (
      db_name,
      driver,
      &instructions ). await ?;
    println!( "   TypeDB update complete." ); }

  { // filesystem
    // TODO: Print per-source write information
    println!( "2) Writing {} instruction(s) to disk ...",
               { let total_input : usize = instructions.len ();
                 total_input } );
    let (deleted_count, written_count) : (usize, usize) =
      update_fs_from_saveinstructions (
        instructions.clone (), config.clone ()) ?;
    println!( "   Deleted {} file(s), wrote {} file(s).",
              deleted_count, written_count ); }

  { // Tantivy
    println!( "3) Updating Tantivy index ..." );
    println!( "   Tantivy updated for {} document(s).",
              { let indexed_count : usize =
                  update_tantivy_from_saveinstructions (
                      &instructions, tantivy_index )?;
              indexed_count } ); }

  println!( "All updates finished successfully." );
  Ok (( )) }

/// Update the DB from a batch of `DefineOneNode`s:
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
  instructions : &Vec<DefineOneNode>
) -> Result<(), Box<dyn Error>> {

  // PITFALL: Below, each get(0) on an 'ids' field
  // is not motivated by separating the PID from the others,
  // because (see add_missing_info_to_forest) there are no others.
  // It is simply to turn the Vec<ID> into a bare ID.

  let ( to_delete_instructions, to_write_instructions )
    : ( Vec<DefineOneNode>, Vec<DefineOneNode> ) =
    instructions . iter ()
    . cloned ()
    . partition ( |instr| instr.is_delete() );

  { // delete
    let to_delete_pids : Vec<ID> =
      to_delete_instructions . iter ()
      . filter_map ( |instr|
                      instr . node() . ids
                      . get(0)
                      . cloned() )
      . collect ();
    if ! to_delete_pids . is_empty () {
      println!("Deleting nodes with PIDs: {:?}", to_delete_pids);
      delete_nodes_from_pids (
        // PITFALL: deletions cascade in TypeDB by default,
        // so we are left with no incomplete relationships.
        db_name, driver, & to_delete_pids ). await ?; }}

  { // create | update
    let to_write_skgnodes : Vec<SkgNode> =
      to_write_instructions . iter ()
      . map ( |instr| instr . node() . clone () )
      . collect ();
    let to_write_pids : Vec<ID> =
      to_write_skgnodes . iter ()
      . filter_map ( |n|
                      n . ids
                      . get(0)
                      . cloned() )
      . collect ();
    create_only_nodes_with_no_ids_present (
      db_name, driver, & to_write_skgnodes ). await ?;
    delete_out_links (
      db_name, driver,
      & to_write_pids, // Will barf if nonempty, which is good.
      "contains",
      "container" ). await ?;
    create_all_relationships (
      db_name, driver, & to_write_skgnodes ). await ?; }

  Ok (( )) }

pub fn update_fs_from_saveinstructions (
  instructions : Vec<DefineOneNode>,
  config       : SkgConfig,
) -> io::Result<(usize, usize)> { // (deleted, written)
  let ( to_delete, to_write ) // functional; no IO
    : ( Vec<DefineOneNode>, Vec<DefineOneNode> ) =
    instructions . into_iter ()
    . partition ( |instr| instr . is_delete() );
  let deleted : usize = {
    let nodes_to_delete : Vec<SkgNode> =
      to_delete . into_iter ()
      . map ( |instr| instr . into_node() )
      . collect ();
    if ! nodes_to_delete . is_empty () {
      delete_all_nodes_from_fs (
        nodes_to_delete, config . clone () ) ?
    } else { 0 } };
  let written : usize = {
    let nodes_to_write : Vec<SkgNode> =
      to_write . into_iter ()
      . map ( |instr| instr . into_node() )
      . collect ();
    if ! nodes_to_write . is_empty () {
      write_all_nodes_to_fs (
        nodes_to_write, config ) ?
    } else { 0 } };
  Ok ( (deleted, written) ) }


/// Updates the index with the provided DefineOneNodes.
/// Deletes IDs from the index for every instruction,
/// but only adds documents for instructions where is_save.
/// Returns the number of documents processed.
pub(super) fn update_tantivy_from_saveinstructions (
  instructions  : &[DefineOneNode],
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut writer: IndexWriter =
    tantivy_index.index.writer(50_000_000)?;
  delete_nodes_from_index(
    // Delete all IDs in the DefineOneNodes from the index.
    // (Each DefineOneNode::Save is then recreated.)
    instructions.iter().map(|instr| instr.node()),
    &mut writer,
    tantivy_index)?;
  let processed_count: usize =
    add_documents_to_tantivy_writer(
      { // Add documents only for non-deletion instructions.
        let nodes_to_add: Vec<&SkgNode> =
          instructions . iter()
          . filter_map( |instr|
                         if instr.is_save()
                         { Some (instr.node()) } else { None } )
          . collect();
        nodes_to_add },
      &mut writer, tantivy_index )? ;
  commit_with_status(
    &mut writer, processed_count, "Updated")?;
  Ok (processed_count) }
