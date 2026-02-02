pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod none_node_fields_are_noops;

use crate::types::skgnode::SkgNode;
use crate::types::misc::SkgConfig;
use crate::types::save::{DefineNode, SaveNode};
use crate::types::orgnode::OrgNode;

use to_naive_instructions::naive_saveinstructions_from_forest;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use none_node_fields_are_noops::noneclobber_skgnode;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;
use std::error::Error;

/// Converts a forest of OrgNodes to DefineNodes,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and clobbering None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// (That filtering is done by 'naive_saveinstructions_from_forest'.)
pub async fn orgnode_forest_to_nonmerge_save_instructions (
  forest : &Tree<OrgNode>, // "forest" = tree with BufferRoot
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let mut clobbered_instructions : Vec<DefineNode> =
    Vec::new(); // they'll be clobbered by the end of this function
  for instr in
    { let instructions_without_dups : Vec<DefineNode> =
        reconcile_same_id_instructions (
          config, driver,
          { let instructions : Vec<DefineNode> =
              naive_saveinstructions_from_forest (
                forest . clone( )) ?;
            instructions } ) . await ?;
      instructions_without_dups }
  { let clobbered_instr : DefineNode = match instr {
      DefineNode::Delete(_) =>
        // Deletes need no clobbering; pass through unchanged.
        instr,
      DefineNode::Save(SaveNode(node)) => {
        let noneclobbered_node : SkgNode =
          noneclobber_skgnode ( config, driver, node ). await ?;
        DefineNode::Save(SaveNode(noneclobbered_node)) } };
    clobbered_instructions.push(clobbered_instr); }
  Ok (clobbered_instructions) }
