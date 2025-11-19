pub mod to_dirty_instructions;
pub mod reconcile_dup_instructions;
pub mod none_node_fields_are_noops;

pub use none_node_fields_are_noops::clobber_none_fields_with_data_from_disk;
pub use reconcile_dup_instructions::reconcile_dup_instructions;
pub use to_dirty_instructions::interpret_orgnode_forest;

use crate::types::{SkgNode, SkgConfig, SaveInstruction};
use crate::types::orgnode::OrgNode;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;
use std::error::Error;

/// Converts a forest of OrgNodes to SaveInstructions,
/// reconciling duplicates via 'reconcile_dup_instructions'
/// (which filters out indefinitive instructions),
/// and clobbering None fields with data from disk.
pub async fn orgnodes_to_reconciled_save_instructions (
  forest  : &Vec<Tree<OrgNode>>,
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let instructions : Vec<SaveInstruction> =
    interpret_orgnode_forest ( forest . clone () ) ?;
  let instructions_without_dups : Vec<SaveInstruction> =
    reconcile_dup_instructions (
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
