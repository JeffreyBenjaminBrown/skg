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
/// (which filters out indefinitive instructions),
/// and clobbering None fields with data from disk.
pub async fn orgnodes_to_reconciled_save_instructions (
  forest  : &Tree<OrgNode>, // "forest" = tree with ForestRoot
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let instructions : Vec<SaveInstruction> =
    naive_saveinstructions_from_forest ( forest . clone () ) ?;
  let instructions_without_dups : Vec<SaveInstruction> =
    reconcile_same_id_instructions (
      config, driver, instructions ) . await ?;
  let mut clobbered_instructions : Vec<SaveInstruction> =
    Vec::new();
  for (node, action) in instructions_without_dups {
    let clobbered_node : SkgNode =
      clobber_none_fields_with_data_from_disk (
        config, driver, node ) . await ?;
    clobbered_instructions.push(
      (clobbered_node, action)); }
  Ok (clobbered_instructions) }
