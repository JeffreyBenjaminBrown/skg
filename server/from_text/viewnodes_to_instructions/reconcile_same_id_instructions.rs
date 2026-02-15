/// PITFALL: ASSUMES IDs have been replaced by PIDs
/// in the 'id' field of every ViewNode.
/// (Currently, 'add_missing_info_to_forest' does this.)
/// .
/// THE IDEA
/// After validation, we have:
/// - At most ONE Save per ID (validation ensures this)
/// - Possibly multiple Delete instructions (all equivalent)
/// .
/// This module deduplicates: Keeps one Save or one Delete per ID.
/// .
/// Validation (find_buffer_errors_for_saving) runs BEFORE this module
/// and blocks saves if:
/// - Multiple definitive nodes have the same ID
/// - Nodes with same ID have inconsistent toDelete values

use crate::types::misc::ID;
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use std::collections::HashMap;
use std::error::Error;

/// Runs 'collect_dup_instructions' to group same-ID instructions.
/// Then, on each group of DefineNodes,
/// runs 'reconcile_same_id_instructions_for_one_id'
/// to get a single DefineNode.
pub fn reconcile_same_id_instructions(
  instructions: Vec<DefineNode>
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let grouped_instructions: HashMap<ID, Vec<DefineNode>> =
    collect_dup_instructions (instructions)?;
  let mut result: Vec<DefineNode> =
    Vec::new();
  for (_id, instruction_group) in grouped_instructions {
    result . push (
      reconcile_same_id_instructions_for_one_id (
        instruction_group )? ); }
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
