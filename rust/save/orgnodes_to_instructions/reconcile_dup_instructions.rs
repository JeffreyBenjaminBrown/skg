/* PITFALL: ASSUMES IDs have been replaced by PIDs
in the 'id' field of every OrgNode.
(Currently, 'add_missing_info_to_trees' does this.)
.
THE IDEA
Different SaveInstruction can have the same ID.
And there might be information on disk
that conflicts or supplements information in
the buffer text that a user is trying to save.
Those conflicts must be resolved.
.
If two OrgNodes referring to the same node
have distinct toDelete values, the save is not permitted.
For the rest of this comment, then,
assume all SaveInstructions have toDelete = false.
.
Some SaveInstructions are indefinitive and some aren't.
If an SaveInstruction is not indefinitive,
call it a 'defining' instruction.
There can be at most one such instruction.
If there is, it defines the title, body and first contents.
(If it has no body, neither will the result.)
.
If there is no defining instruction for a node in the tree,
its initial contents are read from disk.
But every 'indefinitive' instruction can append novel contents,
where 'novel' = 'not already in its contents'.
.
All of them, whether indefinitive or not, can contribute aliases.
If none of them mentions aliases, aliases come from disk.
.
Extra IDs are appended from disk.
*/

use crate::types::{
  ID, SkgNode, SaveInstruction, NonMerge_NodeAction, SkgConfig};
use crate::media::file_io::read_node_from_id_optional;
use crate::util::dedup_vector;
use std::collections::HashMap;
use std::error::Error;
use std::iter::once;
use typedb_driver::TypeDBDriver;


/// Runs collect_dup_instructions to group pairs with the same ID.
/// Then, on each group of SaveInstructions, runs reconcile_dup_instructions_for_one_id
/// to get a single SaveInstruction.
pub async fn reconcile_dup_instructions(
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
      reconcile_dup_instructions_for_one_id(
        config, driver, instruction_group ). await ?;
    result.push (reduced_instruction); }
  Ok(result) }

/// Group SaveInstructions with the same ID.
pub fn collect_dup_instructions(
  instructions: Vec<SaveInstruction>
) -> HashMap<ID, Vec<SaveInstruction>> {
  let mut grouped: HashMap<ID, Vec<SaveInstruction>> =
    HashMap::new();
  for (skg_node, save_action) in instructions {
    if let Some(primary_id) = skg_node.ids.first() {
      grouped
        . entry(primary_id.clone())
        . or_insert_with(Vec::new)
        . push((skg_node, save_action)); }}
  grouped }

/// Reduces SaveInstructions with the same ID into a single SaveInstruction.
pub async fn reconcile_dup_instructions_for_one_id(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  instructions: Vec<SaveInstruction>
) -> Result<SaveInstruction, Box<dyn Error>> {
  if instructions.is_empty() {
    return Err("Cannot reduce empty instruction list".into()); }
  let (definer, to_delete, indefinitives)
    : (Option<SaveInstruction>,
       Option<SaveInstruction>,
       Vec<SaveInstruction>)
    = classify_instructions_by_actions(instructions)?;
  if to_delete.is_some() {
    if (definer.is_some() || !indefinitives.is_empty() )
    { return Err(
      "Inconsistent actions for same ID: at least one to delete and one to do something else."
        . into() );
    } else { // Ignore the other SkgNode fields; we only need an ID.
      return Ok(to_delete.unwrap()); }}
  let from_disk: Option<SkgNode> = {
    let primary_id: ID =
      if let Some((ref node, _)) = definer {
        node.ids.first()
      } else { indefinitives[0].0.ids.first()
      } . ok_or("No primary ID found")?.clone();
    read_node_from_id_optional(
      config, driver, &primary_id).await? };

  let reconciled_node: SkgNode = SkgNode {
    title                        : reconciled_title(
      definer.as_ref(), &from_disk)?,
    aliases                      : reconciled_aliases(
      &indefinitives, definer.as_ref(), &from_disk),
    ids                          : reconciled_ids(
      &indefinitives, definer.as_ref(), &from_disk),
    body                         : reconciled_body(
      definer.as_ref(), &from_disk),
    contains                     : reconciled_contains(
      &indefinitives, definer.as_ref(), &from_disk),
    subscribes_to                : reconciled_subscribes_to(
      &indefinitives, definer.as_ref(), &from_disk),
    hides_from_its_subscriptions : reconciled_hides(
      &indefinitives, definer.as_ref(), &from_disk),
    overrides_view_of            : reconciled_overrides(
      &indefinitives, definer.as_ref(), &from_disk), };
  let reconciled_action: NonMerge_NodeAction =
    NonMerge_NodeAction::SaveDefinitive;
  Ok((reconciled_node, reconciled_action)) }

/// Classifies instructions by their action type.
/// Returns (definer, to_delete, indefinitives):
/// - definer: SaveDefinitive instruction, if exactly one exists
/// - to_delete: One Delete instruction (any one, we only need the ID)
/// - indefinitives: SaveIndefinitive instructions
/// Returns Err if multiple definers found.
fn classify_instructions_by_actions(
  instructions: Vec<SaveInstruction>
) -> Result<(Option<SaveInstruction>, // definer. There can be at most one.
             Option<SaveInstruction>, // to_delete. (There might be multiple, but they are all equivalent.)
             Vec<SaveInstruction>), // indefinitives
            Box<dyn Error>> {
  let mut definer: Option<SaveInstruction> = None;
  let mut to_delete: Option<SaveInstruction> = None;
  let mut indefinitives: Vec<SaveInstruction> = Vec::new();
  for instr in instructions {
    match instr.1 {
      NonMerge_NodeAction::SaveDefinitive => {
        if definer.is_some() {
          return Err(
            "Multiple definitive content definitions for same ID"
              . into()); }
        definer = Some(instr); }
      NonMerge_NodeAction::Delete => {
        if to_delete.is_none() {
          to_delete = Some(instr); }}
      NonMerge_NodeAction::SaveIndefinitive => {
        indefinitives.push(instr); }} }
  Ok ((definer, to_delete, indefinitives)) }

/// Reconciles the ids field.
/// Starts with definer's or first indefinitive's IDs,
/// then appends any extra IDs from disk.
fn reconciled_ids(
  indefinitives: &[SaveInstruction],
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>
) -> Vec<ID> {
  let mut final_ids: Vec<ID> = if let Some((node, _)) = definer {
    node.ids.clone()
  } else {
    indefinitives[0].0.ids.clone() };
  if let Some(disk_node) = from_disk {
    for disk_id in &disk_node.ids {
      if !final_ids.contains(disk_id) {
        final_ids.push(disk_id.clone()); }} }
  final_ids }

/// Reconciles the title field.
/// Definer's title takes precedence, then title from file on disk.
/// If neither exists, that's an error.
fn reconciled_title(
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>
) -> Result<String, Box<dyn Error>> {
  if let Some((node, _)) = definer {
    return Ok(node.title.clone()); }
  if let Some(disk_node) = from_disk {
    return Ok(disk_node.title.clone()); }
  Err("No title found for node (neither from definer nor disk)"
      . into()) }

/// Reconciles the body field.
/// Definer's body takes precedence, then disk's body, then None.
fn reconciled_body(
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>
) -> Option<String> {
  if let Some((node, _)) = definer {
    return node.body.clone(); }
  from_disk . as_ref() . and_then (|node|
                                   node.body.clone()) }

/// Reconciles the aliases field.
/// Collects all aliases from definer and indefinitives.
/// If none found, uses disk's aliases.
fn reconciled_aliases(
  indefinitives: &[SaveInstruction],
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>
) -> Option<Vec<String>> {
  reconcile_collected_field(
    indefinitives,
    definer,
    from_disk,
    |node| &node.aliases,
    |disk_node| disk_node.aliases.clone() ) }

/// Reconciles the contains field.
/// Initial contents come from definer, or barring that, from disk.
/// Indefinitive contents are appended.
/// Last, everything is deduplicated.
fn reconciled_contains(
  indefinitives: &[SaveInstruction],
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>
) -> Option<Vec<ID>> {
  let initial_contents: Vec<ID> = if let Some((node, _)) = definer {
    node . contains . clone() . unwrap_or_default()
  } else { from_disk . as_ref()
           . and_then (|node|
                       node.contains.clone())
           . unwrap_or_default() };
  let mut final_contents: Vec<ID> = initial_contents;
  for (node, _) in indefinitives {
    if let Some(contents) = &node.contains {
      final_contents.extend(contents.iter().cloned()); }}
  Some(dedup_vector(final_contents)) }

/// Reconciles the subscribes_to field.
/// Collects all subscribes_to IDs from definer and indefinitives, then deduplicates.
/// If none found, uses disk's subscribes_to.
fn reconciled_subscribes_to(
  indefinitives: &[SaveInstruction],
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>
) -> Option<Vec<ID>> {
  reconcile_collected_field(
    indefinitives,
    definer,
    from_disk,
    |node| &node.subscribes_to,
    |disk_node| disk_node.subscribes_to.clone() ) }

/// Reconciles the hides_from_its_subscriptions field.
/// Collects all hides_from_its_subscriptions IDs from definer and indefinitives, then deduplicates.
/// If none found, uses disk's hides_from_its_subscriptions.
fn reconciled_hides(
  indefinitives: &[SaveInstruction],
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>
) -> Option<Vec<ID>> {
  reconcile_collected_field(
    indefinitives,
    definer,
    from_disk,
    |node| &node.hides_from_its_subscriptions,
    |disk_node| disk_node.hides_from_its_subscriptions.clone() ) }

/// Reconciles the overrides_view_of field.
/// Collects all overrides_view_of IDs from definer and indefinitives, then deduplicates.
/// If none found, uses disk's overrides_view_of.
fn reconciled_overrides(
  indefinitives: &[SaveInstruction],
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>
) -> Option<Vec<ID>> {
  reconcile_collected_field(
    indefinitives,
    definer,
    from_disk,
    |node| &node.overrides_view_of,
    |disk_node| disk_node.overrides_view_of.clone() ) }

/// Generic reconciliation for fields that collect items from all instructions.
/// Collects items from both definer and indefinitives.
/// If any instruction had Some(_) for the field, returns the deduplicated collection.
/// Only falls back to disk if no instruction specified the field (all were None).
fn reconcile_collected_field<T, F>(
  indefinitives: &[SaveInstruction],
  definer: Option<&SaveInstruction>,
  from_disk: &Option<SkgNode>,
  extract_from_node: F,
  extract_from_disk: fn(&SkgNode) -> Option<Vec<T>>
) -> Option<Vec<T>>
where
  T: Clone + Eq + std::hash::Hash,
  F: Fn(&SkgNode) -> &Option<Vec<T>>
{
  let mut collected: Vec<T> = Vec::new();
  let mut any_specified = false;
  for (node, _) in
    once(definer).flatten().chain(indefinitives.iter()) {
      if let Some(items) = extract_from_node(node) {
        any_specified = true;
        collected.extend(items.iter().cloned()); }}
  if any_specified { Some(dedup_vector(collected)) }
  else { // No instruction specified it. Use disk value.
    from_disk . as_ref() . and_then (extract_from_disk) }}
