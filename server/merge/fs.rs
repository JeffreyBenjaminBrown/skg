use crate::dbs::filesystem::one_node::write_skgnode_to_source;
use crate::types::save::Merge;
use crate::types::misc::{SkgConfig, ID, SourceName};
use crate::types::skgnode::SkgNode;
use crate::util::path_from_pid_and_source;
use std::error::Error;

/// PURPOSE: For each Merge:
///   - write the updated acquirer
///   - write the new acquiree text preserver
///   - delete the acquiree
pub(super) fn merge_nodes_in_fs (
  config             : SkgConfig,
  merge_instructions : &[Merge],
) -> Result < (), Box<dyn Error> > {
  if merge_instructions.is_empty() {
    return Ok (( )); }
  for merge in merge_instructions {
    let ( acquiree_text_preserver,
          updated_acquirer,
          (acquiree_id, acquiree_source) )
      : (&SkgNode, &SkgNode, (&ID, &SourceName))
      = merge.targets_from_merge();
    write_skgnode_to_source(
      acquiree_text_preserver,
      &config)?;
    write_skgnode_to_source(
      updated_acquirer,
      &config)?;
    { // Delete acquiree from disk
      let acquiree_path : String =
        path_from_pid_and_source (&config,
                       acquiree_source,
                       acquiree_id . clone() );
      std::fs::remove_file(&acquiree_path)
        .map_err(|e| format!(
          "Failed to delete acquiree file '{}': {}",
          acquiree_path, e))?; }}
  Ok (( )) }
