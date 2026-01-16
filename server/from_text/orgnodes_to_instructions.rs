pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod none_node_fields_are_noops;

use crate::types::skgnode::SkgNode;
use crate::types::misc::SkgConfig;
use crate::types::save::SaveInstruction;
use crate::types::orgnode::OrgNode;

use to_naive_instructions::naive_saveinstructions_from_forest;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use none_node_fields_are_noops::clobber_none_fields_with_data_from_disk;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;
use std::error::Error;

/// Converts a forest of OrgNodes to SaveInstructions,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and clobbering None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// (That filtering is done by 'naive_saveinstructions_from_forest'.)
pub async fn orgnode_forest_to_nonmerge_save_instructions (
  forest : &Tree<OrgNode>, // "forest" = tree with ForestRoot
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let mut clobbered_instructions : Vec<SaveInstruction> =
    Vec::new(); // they'll be clobbered by the end of this function
  for (node, action) in
    { let instructions_without_dups : Vec<SaveInstruction> =
        reconcile_same_id_instructions (
          config, driver,
          { let instructions : Vec<SaveInstruction> =
              naive_saveinstructions_from_forest (
                forest . clone( )) ?;
            instructions } ) . await ?;
      instructions_without_dups }
  { clobbered_instructions.push (
      { let clobbered_node : SkgNode =
          clobber_none_fields_with_data_from_disk (
              config, driver, node ) . await ?;
        (clobbered_node, action) } ); }
  Ok (clobbered_instructions) }
