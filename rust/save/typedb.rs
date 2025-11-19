use std::error::Error;
use typedb_driver::TypeDBDriver;

use crate::media::typedb::nodes::create_only_nodes_with_no_ids_present;
use crate::media::typedb::nodes::delete_nodes_from_pids;
use crate::media::typedb::relationships::create_all_relationships;
use crate::media::typedb::relationships::delete_out_links;
use crate::types::{SkgNode, ID, SaveInstruction, NonMerge_NodeAction};

/// Update the DB from a batch of `(SkgNode, NonMerge_NodeAction)` pairs:
/// 1) Delete all nodes marked 'toDelete', using delete_nodes_from_pids
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
  instructions : &Vec<SaveInstruction>
) -> Result<(), Box<dyn Error>> {

  // PITFALL: Below, each get(0) on an 'ids' field
  // is not motivated by separating the PID from the others,
  // because (see add_missing_info_to_forest) there are no others.
  // It is simply to turn the Vec<ID> into a bare ID.

  let ( to_delete_instructions, to_write_instructions )
    : ( Vec<SaveInstruction>, Vec<SaveInstruction> ) =
    instructions . iter ()
    . cloned ()
    . partition (
      |(_, action)| matches!(action,
                             NonMerge_NodeAction::Delete));
  let to_write_skgnodes : Vec<SkgNode> =
    to_write_instructions . iter ()
    . map ( |(node, _)| node . clone () )
    . collect ();
  let to_write_pids : Vec<ID> =
    to_write_skgnodes . iter ()
    . filter_map ( |n|
                    n . ids
                    . get(0)
                    . cloned() )
    . collect ();
  let to_delete_pids : Vec<ID> =
    to_delete_instructions . iter ()
    . filter_map ( |(node, _)|
                    node . ids
                    . get(0)
                    . cloned() )
    . collect ();

  if ! to_delete_pids . is_empty () { // delete
    println!("Deleting nodes with PIDs: {:?}", to_delete_pids);
    delete_nodes_from_pids (
      // PITFALL: deletions cascade in TypeDB by default,
      // so we are left with no incomplete relationships.
      db_name, driver, & to_delete_pids ). await ?; }
  { // create | update
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
