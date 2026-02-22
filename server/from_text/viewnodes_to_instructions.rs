pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;

use crate::dbs::neo4j::search::pids_and_sources_from_ids;
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::skgnodemap::IdToPidAndSource;
use crate::types::viewnode::ViewNode;

use to_naive_instructions::naive_saveinstructions_from_tree;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use super::supplement_from_disk::supplement_none_fields_from_disk_with_pid_source_map;
use ego_tree::Tree;
use neo4rs::Graph;
use std::error::Error;

/// Converts a forest of ViewNodes to DefineNodes,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and supplementing None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// (That filtering is done by 'naive_saveinstructions_from_tree'.)
/// Batch-resolves all PIDs via a single Neo4j query before supplementing.
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
  let all_pids : Vec<ID> =
    instructions_without_dups . iter ()
    . filter_map ( |instr| match instr {
      DefineNode::Save(SaveNode(sn)) =>
        sn . ids . first () . cloned (),
      DefineNode::Delete(_) => None } )
    . collect ();
  let pid_source_map : IdToPidAndSource =
    pids_and_sources_from_ids ( graph, &all_pids ) . await ?;
  let mut result : Vec<DefineNode> =
    Vec::with_capacity ( instructions_without_dups.len() );
  for instr in instructions_without_dups {
    result . push (
      supplement_none_fields_from_disk_with_pid_source_map (
        config, graph, instr, &pid_source_map
      ). await ? ); }
  Ok (result) }
