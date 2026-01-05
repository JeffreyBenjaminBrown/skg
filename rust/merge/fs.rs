use crate::dbs::filesystem::write_skgnode;
use crate::types::{MergeInstructionTriple, SkgConfig, SkgNode};
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
    let acquiree_text_preserver : &SkgNode =
      &merge.acquiree_text_preserver.0;
    let updated_acquirer : &SkgNode =
      &merge.updated_acquirer.0;
    let acquiree : &SkgNode =
      &merge.acquiree_to_delete.0;

    { // Write acquiree_text_preserver to disk
      let acquiree_text_preserver_path : String =
        path_from_pid_and_source (&config,
                       &acquiree_text_preserver.source,
                       acquiree_text_preserver.ids[0].clone() );
      write_skgnode(
        acquiree_text_preserver, &acquiree_text_preserver_path)?; }
    { // Write updated acquirer to disk
      let acquirer_path : String =
        path_from_pid_and_source (&config,
                       &updated_acquirer.source,
                       updated_acquirer.ids[0].clone() );
      write_skgnode(
        updated_acquirer, &acquirer_path)?; }
    { // Delete acquiree from disk
      let acquiree_path : String =
        path_from_pid_and_source (&config,
                       &acquiree.source,
                       acquiree.ids[0].clone());
      std::fs::remove_file(&acquiree_path)
        .map_err(|e| format!(
          "Failed to delete acquiree file '{}': {}",
          acquiree_path, e))?; }}
  Ok (( )) }
