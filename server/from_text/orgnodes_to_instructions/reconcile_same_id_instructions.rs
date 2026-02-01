/// PITFALL: ASSUMES IDs have been replaced by PIDs
/// in the 'id' field of every OrgNode.
/// (Currently, 'add_missing_info_to_forest' does this.)
/// .
/// THE IDEA
/// After validation, we have:
/// - At most ONE Save per ID (validation ensures this)
/// - Possibly multiple Delete instructions (all equivalent)
/// .
/// This module does two things:
/// 1. Deduplicates Delete instructions (all are equivalent, keep one)
/// 2. Supplements each Save with data from disk:
///    - Use instruction's value if present (Some)
///    - Otherwise use disk's value as fallback
///    - Append any extra IDs from disk
/// .
/// Validation (find_buffer_errors_for_saving) runs BEFORE this module
/// and blocks saves if:
/// - Multiple definitive nodes have the same ID
/// - Nodes with same ID have inconsistent toDelete values
/// .
/// So this module only handles the simple case: supplement one instruction with disk.

use crate::dbs::filesystem::one_node::optskgnode_from_id;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::save::{SaveInstruction, NonMerge_NodeAction};
use crate::types::skgnode::SkgNode;
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;


/// Runs 'collect_dup_instructions' to group same-ID instructions.
/// Then, on each group of SaveInstructions,
/// runs 'reconcile_same_id_instructions_for_one_id'
/// to get a single SaveInstruction.
pub async fn reconcile_same_id_instructions(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  instructions: Vec<SaveInstruction>
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let grouped_instructions: HashMap<ID, Vec<SaveInstruction>> =
    collect_dup_instructions (instructions);
  let mut result: Vec<SaveInstruction> =
    Vec::new();
  for (_id, instruction_group) in grouped_instructions {
    let reduced_instruction: SaveInstruction =
      reconcile_same_id_instructions_for_one_id(
        config, driver, instruction_group ). await ?;
    result.push (reduced_instruction); }
  Ok(result) }

/// Group SaveInstructions with the same ID.
pub fn collect_dup_instructions(
  instructions: Vec<SaveInstruction>
) -> HashMap<ID, Vec<SaveInstruction>> {
  let mut grouped: HashMap<ID, Vec<SaveInstruction>> =
    HashMap::new();
  for instruction in instructions {
    let (skg_node, save_action) // just for the type signature
      : (SkgNode, NonMerge_NodeAction)
      = instruction;
    let primary_id_opt : Option<&ID> =
      skg_node.ids.first();
    if let Some(primary_id) = primary_id_opt {
      grouped
        . entry (primary_id.clone())
        . or_insert_with (Vec::new)
        . push ((skg_node, save_action)); }}
  grouped }

/// Processes a group of SaveInstructions with the same ID.
/// After validation, each group contains:
/// - Exactly 1 Save (validation ensures no duplicates), OR
/// - 1+ Delete instructions (all equivalent)
/// This function either supplements the single Save
/// with disk data -- replacing None fields in the OrgNode
/// with the same field from disk, and including all the extra IDs
/// -- or else returns the Delete instruction.
pub async fn reconcile_same_id_instructions_for_one_id(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  instructions: Vec<SaveInstruction>
) -> Result<SaveInstruction, Box<dyn Error>> {
  if instructions.is_empty() {
    return Err("Cannot process empty instruction list".into()); }
  let mut save_opt: Option<SaveInstruction> = None;
  let mut delete_opt: Option<SaveInstruction> = None;
  for instruction in instructions {
    let instr: SaveInstruction = instruction;
    match instr.1 {
      NonMerge_NodeAction::Save => {
        if save_opt.is_some() {
          return Err("Multiple save instructions for same ID (should be caught by validation)".into( )); }
        save_opt = Some(instr); }
      NonMerge_NodeAction::Delete => {
        // Multiple deletes are harmless; keep either one.
        if delete_opt.is_none() {
          delete_opt = Some(instr); }} }}
  if let Some(delete_instr) = delete_opt { // Return a Delete.
    if save_opt.is_some() {
      return Err("Cannot have both Delete and Save for same ID"
                 . into() ); }
    return Ok(delete_instr); }
  build_supplemented_save (
    // Return a Save. Replace None fields from the skgnode implied by the buffer with whatever was already on disk. (The buffer node can delete the data in such a field by sending Some([]) rather than None.)
    config, driver, save_opt ). await }

/// Build and return a Save instruction supplemented with disk data.
/// Replaces None fields in the instruction with values from disk,
/// and validates that sources match.
async fn build_supplemented_save(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  save: Option<SaveInstruction>
) -> Result<SaveInstruction, Box<dyn Error>> {
  let (from_buffer, action)
    : (SkgNode, NonMerge_NodeAction)
    = save.ok_or("No delete and no save instruction found. This should not be possible.")?;
  let pid: ID =
    from_buffer.ids.first()
    .ok_or("No primary ID found")?.clone();
  let source : SourceName = from_buffer.source.clone();
  let from_disk : Option<SkgNode> =
    optskgnode_from_id(config, driver, &pid).await?;
  if let Some(ref disk_node) = from_disk {
    if source != disk_node.source { // sources don't match
      return Err(Box::new(
        BufferValidationError::DiskSourceBufferSourceConflict(
          pid.clone(),
          disk_node.source.clone(),
          source.clone() )) ); }}
  let supplemented_node : SkgNode = SkgNode {
    title: from_buffer.title.clone(),
    aliases: (
      from_buffer.aliases.clone().or(
        from_disk.as_ref().and_then(
          |node| node.aliases.clone()))),
    source,
    ids: supplement_ids( &(from_buffer.clone(),
                           action),
                         &from_disk),
    body: from_buffer.body.clone(),
    contains: from_buffer.contains.clone(),
    subscribes_to: (
      from_buffer.subscribes_to.clone().or(
        from_disk.as_ref().and_then(
          |node| node.subscribes_to.clone()))),
    hides_from_its_subscriptions: (
      from_buffer.hides_from_its_subscriptions.clone().or(
        from_disk.as_ref().and_then(
          |node| node.hides_from_its_subscriptions.clone()))),
    overrides_view_of: (
      from_buffer.overrides_view_of.clone().or(
        from_disk.as_ref().and_then(
          |node| node.overrides_view_of.clone()))), };
  Ok((supplemented_node,
      NonMerge_NodeAction::Save)) }

/// Supplements instruction's IDs with any extra IDs from disk.
/// MOTIVATION: An OrgNode uses only one ID,
/// while a SkgNode can have many.
fn supplement_ids(
  definer: &SaveInstruction,
  optskgnode_from_disk: &Option<SkgNode>
) -> Vec<ID> {
  let mut return_val: Vec<ID> = definer.0.ids.clone();
  let disk_node_opt: Option<&SkgNode> =
    optskgnode_from_disk.as_ref();
  if let Some(disk_node) = disk_node_opt {
    for disk_id in &disk_node.ids {
      if !return_val.contains(disk_id) {
        return_val.push(disk_id.clone()); }} }
  return_val }
