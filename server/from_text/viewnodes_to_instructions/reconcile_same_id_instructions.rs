/// PITFALL: ASSUMES IDs have been replaced by PIDs
/// in the 'id' field of every ViewNode.
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
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use crate::types::skgnode::SkgNode;
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Runs 'collect_dup_instructions' to group same-ID instructions.
/// Then, on each group of DefineNodes,
/// runs 'reconcile_same_id_instructions_for_one_id'
/// to get a single DefineNode.
pub async fn reconcile_same_id_instructions(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  instructions: Vec<DefineNode>
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let grouped_instructions: HashMap<ID, Vec<DefineNode>> =
    collect_dup_instructions (instructions)?;
  let mut result: Vec<DefineNode> =
    Vec::new();
  for (_id, instruction_group) in grouped_instructions {
    let reduced_instruction: DefineNode =
      reconcile_same_id_instructions_for_one_id (
        instruction_group )?;
    result . push (
      build_disksupplemented_save (
        config, driver, reduced_instruction
      ). await ?); }
  Ok (result) }

/// Group DefineNodes with the same ID.
pub fn collect_dup_instructions(
  instructions: Vec<DefineNode>
) -> Result<HashMap<ID, Vec<DefineNode>>, Box<dyn Error>> {
  let mut grouped: HashMap<ID, Vec<DefineNode>> =
    HashMap::new();
  for instr in instructions {
    let primary_id : ID = match &instr {
      DefineNode::Save(SaveNode(node)) =>
        node . ids . first()
        . ok_or("DefineNode::Save has no ID")? . clone(),
      DefineNode::Delete(DeleteNode { id, .. }) =>
        id . clone() };
    grouped
      . entry (primary_id)
      . or_insert_with (Vec::new)
      . push (instr); }
  Ok(grouped) }

/// Processes a group of DefineNodes with the same ID.
/// Thanks to validation, the 'instructions' input contains either:
/// - Exactly 1 Save (validation ensures no duplicates)
/// - 1 or more Deletes (all equivalent)
/// Returns the single Save or Delete instruction.
/// (Disk supplementation is done by the caller.)
pub fn reconcile_same_id_instructions_for_one_id(
  instructions: Vec<DefineNode>
) -> Result<DefineNode, Box<dyn Error>> {
  if instructions.is_empty() {
    return Err("Cannot process empty instruction list".into()); }
  let mut save_opt: Option<SaveNode> = None;
  let mut delete_opt: Option<DeleteNode> = None;
  for instr in instructions {
    match instr {
      DefineNode::Save(save) => {
        if save_opt.is_some() {
          return Err("Multiple save instructions for same ID (should be caught by validation)".into( )); }
        save_opt = Some(save); }
      DefineNode::Delete(del) => {
        // Multiple deletes are harmless; keep either one.
        if delete_opt.is_none() {
          delete_opt = Some(del); }} }}
  if let Some(delete_instr) = delete_opt { // Return a Delete.
    if save_opt.is_some() {
      return Err("Cannot have both Delete and Save for same ID"
                 . into() ); }
    return Ok(DefineNode::Delete(delete_instr)); }
  let save : SaveNode =
    save_opt . ok_or("No delete and no save instruction found. This should not be possible.")?;
  Ok(DefineNode::Save(save)) }

/// Has no effect on Delete instructions.
/// Supplements Save instructions with disk data:
/// replaces None fields with values from disk,
/// and validates that sources match. (To delete such a field,
/// the SaveNode should use Some ( [] ) rather than None.)
async fn build_disksupplemented_save(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  instruction: DefineNode,
) -> Result<DefineNode, Box<dyn Error>> {
  let SaveNode(from_buffer) = match instruction {
    DefineNode::Delete(_) => return Ok(instruction),
    DefineNode::Save(save) => save };
  let pid: ID =
    from_buffer . ids . first()
    . ok_or("No primary ID found")? . clone();
  let source : SourceName = from_buffer . source . clone();
  let from_disk : Option<SkgNode> =
    optskgnode_from_id(config, driver, &pid).await?;
  if let Some(ref disk_node) = from_disk {
    if source != disk_node . source { // sources don't match
      return Err(Box::new(
        BufferValidationError::DiskSourceBufferSourceConflict(
          pid . clone(),
          disk_node . source . clone(),
          source . clone() )) ); }}
  let supplemented_node : SkgNode = SkgNode {
    title: from_buffer . title . clone(),
    aliases: ( from_buffer . aliases . clone() . or(
                 from_disk . as_ref() . and_then(
                   |node| node . aliases . clone() )) ),
    source,
    ids: supplement_ids( &from_buffer,
                         &from_disk),
    body: from_buffer . body . clone(),
    contains: from_buffer . contains . clone(),
    subscribes_to: (
      from_buffer . subscribes_to . clone() . or(
        from_disk . as_ref() . and_then(
          |node| node . subscribes_to . clone() )) ),
    hides_from_its_subscriptions: (
      from_buffer . hides_from_its_subscriptions . clone() . or(
        from_disk . as_ref() . and_then(
          |node| node . hides_from_its_subscriptions . clone() )) ),
    overrides_view_of: (
      from_buffer . overrides_view_of . clone() . or(
        from_disk . as_ref() . and_then(
          |node| node . overrides_view_of . clone() )) ), };
  Ok(DefineNode::Save(SaveNode(supplemented_node))) }

/// Supplements instruction's IDs with any extra IDs from disk.
/// MOTIVATION: A ViewNode uses only one ID; a SkgNode can have many.
fn supplement_ids(
  from_buffer: &SkgNode,
  optskgnode_from_disk: &Option<SkgNode>
) -> Vec<ID> {
  let mut return_val: Vec<ID> = from_buffer.ids.clone();
  let disk_node_opt: Option<&SkgNode> =
    optskgnode_from_disk.as_ref();
  if let Some(disk_node) = disk_node_opt {
    for disk_id in &disk_node.ids {
      if !return_val.contains(disk_id) {
        return_val.push(disk_id.clone()); }} }
  return_val }
