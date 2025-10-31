use crate::file_io::write_node;
use crate::types::{MergeInstructionTriple, SkgConfig, SkgNode};
use crate::util::path_from_pid;
use std::error::Error;

/// Merges nodes in filesystem by applying MergeInstructionTriple.
pub(super) fn merge_nodes_in_fs (
  config             : SkgConfig,
  merge_instructions : &[MergeInstructionTriple],
) -> Result < (), Box<dyn Error> > {
  if merge_instructions.is_empty() {
    return Ok (( )); }
  for merge in merge_instructions {
    let acquiree_text_preserver : &SkgNode =
      &merge.acquiree_text_preserver.0;
    let updated_acquirer : &SkgNode =
      &merge.updated_acquirer.0;
    let acquiree : &SkgNode =
      &merge.deleted_acquiree.0;

    // Write acquiree_text_preserver to disk
    let acquiree_text_preserver_path : String =
      path_from_pid(
        &config, acquiree_text_preserver.ids[0].clone());
    write_node(
      acquiree_text_preserver, &acquiree_text_preserver_path)?;

    // Write updated acquirer to disk
    let acquirer_path : String =
      path_from_pid(&config, updated_acquirer.ids[0].clone());
    write_node(updated_acquirer, &acquirer_path)?;

    // Delete acquiree from disk
    let acquiree_path : String =
      path_from_pid(&config, acquiree.ids[0].clone());
    std::fs::remove_file(&acquiree_path)
      .map_err(|e| format!(
        "Failed to delete acquiree file '{}': {}",
        acquiree_path, e))?; }
  Ok (( )) }
