use crate::save::update_fs_from_saveinstructions;
use crate::types::misc::SkgConfig;
use crate::types::save::{DefineNode, Merge};
use std::error::Error;

/// PURPOSE: For each Merge:
///   - write the updated acquirer
///   - write the new acquiree text preserver
///   - delete the acquiree
///
/// Flattens the Merges to DefineNodes via 'Merge::to_vec', then
/// pipes them through the ordinary fs-save path. This mirrors what
/// 'merge_nodes_in_tantivy' does for Tantivy — a single uniform
/// sink-application from Vec<DefineNode>, no merge-specific fs
/// code.
pub(super) fn merge_nodes_in_fs (
  config             : SkgConfig,
  merge_instructions : &[Merge],
) -> Result < (), Box<dyn Error> > {
  if merge_instructions . is_empty() {
    return Ok (( )); }
  let flat_instructions : Vec<DefineNode> =
    merge_instructions . iter ()
    . flat_map ( |m| m . to_vec () )
    . collect ();
  update_fs_from_saveinstructions (
    flat_instructions,
    &[], // No source-moves during a merge.
    config ) ?;
  Ok (( )) }
