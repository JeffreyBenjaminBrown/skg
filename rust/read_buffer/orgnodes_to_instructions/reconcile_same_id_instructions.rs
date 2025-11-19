/* PITFALL: ASSUMES IDs have been replaced by PIDs
in the 'id' field of every OrgNode.
(Currently, 'add_missing_info_to_forest' does this.)
.
THE IDEA
After filtering out indefinitives and validation, we have:
- At most ONE SaveDefinitive per ID (validation ensures this)
- Possibly multiple Delete instructions (all equivalent)
.
This module does two things:
1. Deduplicates Delete instructions (all are equivalent, keep one)
2. Supplements each SaveDefinitive with data from disk:
   - Use instruction's value if present (Some)
   - Otherwise use disk's value as fallback
   - Append any extra IDs from disk
.
IMPORTANT: SaveIndefinitive instructions are filtered out at the
start of reconcile_same_id_instructions. They contribute nothing to saves.
.
Validation (find_buffer_errors_for_saving) runs BEFORE this module
and blocks saves if:
- Multiple definitive nodes have the same ID
- Nodes with same ID have inconsistent toDelete values
.
So this module only handles the simple case: supplement one instruction with disk.
*/

use crate::types::{
  ID, SkgNode, SaveInstruction, NonMerge_NodeAction, SkgConfig, BufferValidationError, SourceNickname};
use crate::media::file_io::read_node_from_id_optional;
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;


/// Runs 'collect_dup_instructions' to group same-ID instructions.
/// Then, on each group of SaveInstructions,
/// runs 'reconcile_same_id_instructions_for_one_id'
/// to get a single SaveInstruction.
///
/// NOTE: Disregards SaveIndefinitive instructions,
/// because they do not contribute any data to saves.
pub async fn reconcile_same_id_instructions(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  instructions: Vec<SaveInstruction>
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let definitive_instructions : Vec<SaveInstruction> =
    instructions . into_iter ()
    . filter (| (_node, action) | // discard SaveIndefinitives
              ! matches! ( action,
                           NonMerge_NodeAction::SaveIndefinitive ))
    . collect ();
  let grouped_instructions: HashMap<ID, Vec<SaveInstruction>> =
    collect_dup_instructions (definitive_instructions);
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
    let primary_id_opt : Option<&ID> = skg_node.ids.first();
    if let Some(primary_id) = primary_id_opt {
      grouped
        . entry (primary_id.clone())
        . or_insert_with (Vec::new)
        . push ((skg_node, save_action)); }}
  grouped }

/// Processes a group of SaveInstructions with the same ID.
/// After filtering indefinitives and validation, each group contains:
/// - Exactly 1 SaveDefinitive (validation ensures no duplicates), OR
/// - 1+ Delete instructions (all equivalent)
/// This function either supplements the single SaveDefinitive
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

  // Separate Deletes from Definitives
  // (indefinitives were already filtered).
  let mut definitive: Option<SaveInstruction> = None;
  let mut delete: Option<SaveInstruction> = None;
  for instruction in instructions {
    let instr: SaveInstruction = instruction;
    match instr.1 {
      NonMerge_NodeAction::SaveDefinitive => {
        // Validation ensures at most one definitive per ID
        if definitive.is_some() {
          return Err("Multiple definitive instructions for same ID (should be caught by validation)".into()); }
        definitive = Some(instr); }
      NonMerge_NodeAction::Delete => {
        // Multiple deletes are equivalent, just keep one
        if delete.is_none() {
          delete = Some(instr); }}
      NonMerge_NodeAction::SaveIndefinitive => {
        return Err("SaveIndefinitive should have been filtered out"
                   . into() ); }} }
  let delete_opt: Option<SaveInstruction> = delete;
  if let Some(delete_instr) = delete_opt { // Return a Delete.
    if definitive.is_some() {
      return Err("Cannot have both Delete and SaveDefinitive for same ID".into()); }
    return Ok(delete_instr); }
  { // Build and return a SaveDefinitive.
    let definer: SaveInstruction = definitive . ok_or( "No delete and no definitive instruction found. This should not be possible." )?;
    let pid: ID =
      definer.0.ids.first()
      . ok_or("No primary ID found")?.clone();
    let source : String = definer.0.source.clone();
    let disk_read_result: Option<(SkgNode, SourceNickname)> =
      read_node_from_id_optional(config, driver, &pid).await?;
    let from_disk: Option<SkgNode> = {
      match disk_read_result
      { Some(tuple) =>
        { let (disk_node, disk_source)
          : (SkgNode, SourceNickname) = tuple;
          if source != disk_source.as_str() {
            return Err(Box::new( // sources don't match
              BufferValidationError::DiskSourceBufferSourceConflict(
                pid.clone(),
                disk_source,
                SourceNickname::from(source.clone() )) )); }
          Some(disk_node) }
        None => None }};

    let supplemented_node: SkgNode = SkgNode {
      title         : definer.0.title.clone(),
      aliases       : (
        definer.0.aliases.clone() . or(
          from_disk.as_ref().and_then(
            |node| node.aliases.clone() )) ),
      source        : source,
      ids           : supplement_ids( &definer, &from_disk),
      body          : definer.0.body.clone(),
      contains      : definer.0.contains.clone(),
      subscribes_to : (
        definer.0.subscribes_to.clone() . or (
          from_disk.as_ref().and_then(
            |node| node.subscribes_to.clone() )) ),
      hides_from_its_subscriptions : (
        definer.0.hides_from_its_subscriptions.clone() . or (
          from_disk.as_ref().and_then(
            |node| node.hides_from_its_subscriptions.clone() )) ),
      overrides_view_of : (
        definer.0.overrides_view_of.clone() . or (
          from_disk.as_ref().and_then(
            |node| node.overrides_view_of.clone() )) ), };
    Ok((supplemented_node, NonMerge_NodeAction::SaveDefinitive)) }}

/// Supplements instruction's IDs with any extra IDs from disk.
fn supplement_ids(
  definer: &SaveInstruction,
  from_disk: &Option<SkgNode>
) -> Vec<ID> {
  let mut return_val: Vec<ID> = definer.0.ids.clone();
  let disk_node_opt: Option<&SkgNode> = from_disk.as_ref();
  if let Some(disk_node) = disk_node_opt {
    for disk_id in &disk_node.ids {
      if !return_val.contains(disk_id) {
        return_val.push(disk_id.clone()); }} }
  return_val }
