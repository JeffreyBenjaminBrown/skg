pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod none_node_fields_are_noops;

use crate::types::skgnode::SkgNode;
use crate::types::misc::SkgConfig;
use crate::types::save::DefineOneNode;
use crate::types::orgnode::OrgNode;

use to_naive_instructions::naive_saveinstructions_from_forest;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use none_node_fields_are_noops::noneclobber_skgnode;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;
use std::error::Error;

/// Converts a forest of OrgNodes to DefineOneNodes,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and clobbering None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// (That filtering is done by 'naive_saveinstructions_from_forest'.)
pub async fn orgnode_forest_to_nonmerge_save_instructions (
  forest : &Tree<OrgNode>, // "forest" = tree with BufferRoot
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Vec<DefineOneNode>, Box<dyn Error>> {
  let mut clobbered_instructions : Vec<DefineOneNode> =
    Vec::new(); // they'll be clobbered by the end of this function
  for instr in
    { let instructions_without_dups : Vec<DefineOneNode> =
        reconcile_same_id_instructions (
          config, driver,
          { let instructions : Vec<DefineOneNode> =
              naive_saveinstructions_from_forest (
                forest . clone( )) ?;
            instructions } ) . await ?;
      instructions_without_dups }
  { let clobbered_node : SkgNode =
      // TODO ? It is natural to clobber Saves, but Deletes?
      noneclobber_skgnode (
        config, driver, instr.node().clone() ). await ?;
    let clobbered_instr : DefineOneNode =
      // This is a noisy way to say: Replace instr's node.
      if instr.is_delete()
      { DefineOneNode::Delete(clobbered_node) }
      else { DefineOneNode::Save(clobbered_node) };
    clobbered_instructions.push(clobbered_instr); }
  Ok (clobbered_instructions) }
