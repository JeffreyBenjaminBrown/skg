pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;

use crate::types::misc::SkgConfig;
use crate::types::save::DefineNode;
use crate::types::viewnode::ViewNode;

use to_naive_instructions::naive_saveinstructions_from_tree;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use super::supplement_from_disk::supplement_none_fields_from_disk_if_save;
use ego_tree::Tree;
use neo4rs::Graph;
use std::error::Error;

/// Converts a forest of ViewNodes to DefineNodes,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and supplementing None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// (That filtering is done by 'naive_saveinstructions_from_tree'.)
pub async fn viewnode_forest_to_nonmerge_save_instructions (
  forest : &Tree<ViewNode>, // "forest" = tree with BufferRoot
  config : &SkgConfig,
  graph  : &Graph
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let naive_instructions : Vec<DefineNode> =
    naive_saveinstructions_from_tree (
      forest . clone( )) ?;
  let instructions_without_dups : Vec<DefineNode> =
    reconcile_same_id_instructions ( naive_instructions ) ?;
  let mut result : Vec<DefineNode> = // Mapping supplement_none_fields_from_disk_if_save over instructions_without_dups would be simpler, but async functions can't be mapped.
    Vec::with_capacity ( instructions_without_dups.len() );
  for instr in instructions_without_dups {
    result . push (
      supplement_none_fields_from_disk_if_save (
        config, graph, instr
      ). await ? ); }
  Ok (result) }
