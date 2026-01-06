use crate::dbs::filesystem::one_node::write_skgnode_to_source;
use crate::types::save::MergeInstructionTriple;
use crate::types::misc::SkgConfig;
use crate::types::skgnode::SkgNode;
use crate::util::path_from_pid_and_source;
use std::error::Error;

/// PURPOSE: For each MergeInstructionTriple:
///   - write the updated acquirer
///   - write the new acquiree text preserver
///   - delete the acquiree
pub(super) fn merge_nodes_in_fs (
  config             : SkgConfig,
  merge_instructions : &[MergeInstructionTriple],
) -> Result < (), Box<dyn Error> > {
  if merge_instructions.is_empty() {
    return Ok (( )); }
  for merge in merge_instructions {
    let acquiree : &SkgNode =
      &merge.acquiree_to_delete.0;

    write_skgnode_to_source(
      { let acquiree_text_preserver : &SkgNode =
          &merge.acquiree_text_preserver.0;
        acquiree_text_preserver },
      &config)?;
    write_skgnode_to_source(
      { let updated_acquirer : &SkgNode =
          &merge.updated_acquirer.0;
        updated_acquirer },
      &config)?;
    { // Delete acquiree from disk
      let acquiree_path : String =
        path_from_pid_and_source (&config,
                       &acquiree.source,
                       acquiree . primary_id()? . clone() );
      std::fs::remove_file(&acquiree_path)
        .map_err(|e| format!(
          "Failed to delete acquiree file '{}': {}",
          acquiree_path, e))?; }}
  Ok (( )) }
