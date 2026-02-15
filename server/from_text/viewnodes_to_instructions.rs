pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod none_node_fields_are_noops;

use crate::types::misc::SkgConfig;
use crate::types::save::{DefineNode, SaveNode};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::ViewNode;

use to_naive_instructions::naive_saveinstructions_from_tree;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use none_node_fields_are_noops::noneclobber_skgnode;
use super::supplement_from_disk::supplement_from_disk_if_save;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;
use std::error::Error;

/// Converts a forest of ViewNodes to DefineNodes,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and clobbering None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// (That filtering is done by 'naive_saveinstructions_from_tree'.)
pub async fn viewnode_forest_to_nonmerge_save_instructions (
  forest : &Tree<ViewNode>, // "forest" = tree with BufferRoot
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let naive_instructions : Vec<DefineNode> =
    naive_saveinstructions_from_tree (
      forest . clone( )) ?;
  let instructions_without_dups : Vec<DefineNode> =
    reconcile_same_id_instructions ( naive_instructions ) ?;
  let mut result : Vec<DefineNode> =
    Vec::new();
  for instr in instructions_without_dups {
    let supplemented : DefineNode =
      supplement_from_disk_if_save (
        config, driver, instr ). await ?;
    let clobbered : DefineNode = match supplemented {
      DefineNode::Delete(_) =>
        supplemented,
      DefineNode::Save(SaveNode(node)) => {
        let noneclobbered_node : SkgNode =
          noneclobber_skgnode ( config, driver, node ). await ?;
        DefineNode::Save(SaveNode(noneclobbered_node)) }};
    result . push (clobbered); }
  Ok (result) }
