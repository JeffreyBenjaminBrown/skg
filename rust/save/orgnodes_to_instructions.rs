pub mod from_tree;
pub mod reconcile_dup_instructions;

pub use from_tree::orgnodes_to_dirty_save_instructions;
pub use reconcile_dup_instructions::reconcile_dup_instructions;

use crate::types::{SkgNode, SkgConfig, SaveInstruction};
use crate::types::orgnode::OrgNode;
use crate::save::clobber_none_fields_with_data_from_disk;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;
use std::error::Error;
use std::io;

/// Converts a forest of OrgNode2s to SaveInstructions,
/// reconciling duplicates via 'reconcile_dup_instructions',
/// and clobbering None fields with data from disk.
pub async fn orgnodes_to_reconciled_save_instructions (
  forest  : &Vec<Tree<OrgNode>>,
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let instructions : Vec<SaveInstruction> =
    orgnodes_to_dirty_save_instructions (
      forest . clone () ) ?;
  let instructions_without_dups : Vec<SaveInstruction> =
    reconcile_dup_instructions (
      config, driver, instructions ) . await ?;
  let clobbered_instructions : Vec<SaveInstruction> =
    instructions_without_dups . into_iter ()
    . map ( |(node, action)| {
      let clobbered_node : SkgNode =
        clobber_none_fields_with_data_from_disk (
          config, node ) ?;
      Ok ((clobbered_node, action)) } )
    . collect::<io::Result<Vec<SaveInstruction>>>() ?;
  Ok (clobbered_instructions) }
