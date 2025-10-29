/* THE IDEA
Different SaveInstruction can have the same ID.
And there might be information on disk
that conflicts or supplements information in
the buffer text that a user is trying to save.
Those conflicts must be resolved.
.
The treatment of deletion is omitted from this comment,
because it's simple. For the rest of this comment
assume all SaveInstructions have toDelete = false.
.
Some SaveInstructions are indefinitive and some aren't.
If an SaveInstruction is not indefinitive,
call it a 'defining' instruction.
There can be at most one such instruction.
If there is, it defines the title and body.
(If it has no body, neither will the result.)
.
If there is no defining instruction,
then the last instruction defines the title.
The last with a body (if any) defines the body.
If none of them has a body either, that is read from disk.
.
Similarly, the defining node defines the *initial* contents.
If there isn't one, the initial contents are read from disk.
But every 'indefinitive' instruction can append to that,
as long as it's not repeating something in the initial contents.
.
All of them, whether indefinitive or not, can contribute aliases.
If none of them mentions aliases,
aliases are defined by what's on disk.
.
Extra IDs are appended from disk.
*/

use crate::types::{
  ID, SkgNode, SaveInstruction, NodeSaveAction, SkgConfig};
use crate::file_io::read_node_from_id;
use std::collections::{HashMap, HashSet};
use std::error::Error;
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
  // todo ? Break into smaller functions.
  if instructions.is_empty() {
    return Err("Cannot reduce empty instruction list".into()); }
  let to_delete: bool =
    to_delete_if_consistent (&instructions)?;
  let primary_id: ID =
    instructions[0] . 0 . ids . first()
    . ok_or("No primary ID found") ?. clone();

  let mut aliases           : Vec<String>     = Vec::new();
  let mut append_to_content : Vec<ID>         = Vec::new();
  let mut maybe_title       : Option<String>  = None;
  let mut maybe_body        : Option<String>  = None;
  let mut definer_content   : Option<Vec<ID>> = None;
  let mut definer_title     : Option<String>  = None;
  let mut definer_body      : Option<String>  = None;

  for (skg_node, save_action) in &instructions {
    if let Some(node_aliases) = &skg_node.aliases {
      aliases.extend(
        node_aliases . iter() . cloned() ); }
    if save_action.indefinitive {
      // tentative title and body, appendable contents after dedup
      append_to_content.extend(
        skg_node . contains . iter() . cloned() );
      // TODO ? The treatment of title and body here is inefficient.
      // Once we find one we can stop looking, but this implementation
      // keeps overwriting when it finds a new one.
      maybe_title = Some (skg_node . title.clone() );
      if skg_node . body . is_some() {
        maybe_body = skg_node . body . clone(); }
    } else { // definitive title, body; initial content without dedup
      if definer_content.is_some() {
        return Err("Multiple definitive content definitions for same ID" . into()); }
      definer_content = Some(skg_node.contains.clone());
      definer_title = Some(skg_node.title.clone());
      definer_body = skg_node.body.clone(); }}
  aliases.sort(); aliases.dedup(); // dedup aliases
  let from_disk: Result<SkgNode, Box<dyn Error>> =
    read_node_from_id(
      config, driver, &primary_id).await;
  if aliases.is_empty() {
    if let Ok(disk_node) = &from_disk {
      if let Some(disk_aliases) = &disk_node.aliases {
        aliases = disk_aliases.clone(); }} }
  if definer_content.is_none() {
    if let Ok(disk_node) = &from_disk {
      definer_content = Some(disk_node.contains.clone() ); }}

  // Remove from appendToContent anything that's in definesContent
  let definer_content: Vec<ID> =
    definer_content.unwrap_or_default();
  let definer_content_set: HashSet<ID> =
    definer_content.iter().cloned().collect();
  { // 'append_to_content' should not repeat itself,
    // and it should not overlap 'definer_content_set'.
    append_to_content.sort();
    append_to_content.dedup();
    append_to_content.retain(
      |id| !definer_content_set.contains(id)); }

  // Build final contents
  let mut final_contents: Vec<ID> =
    definer_content;
  final_contents.extend(append_to_content);

  // Get all IDs (original + from disk)
  let mut final_ids: Vec<ID> =
    instructions[0] . 0 . ids . clone();
  if let Ok(disk_node) = &from_disk {
    // TODO ? inefficient
    for disk_id in &disk_node.ids {
      if !final_ids.contains(disk_id) {
        final_ids.push(disk_id.clone() ); }} }

  let has_definer_instruction = definer_title.is_some();
  Ok (( SkgNode {
    title: definer_title . or(maybe_title) . unwrap_or_default(),
    aliases: Some(aliases),
    ids: final_ids,
    body: ( if has_definer_instruction { definer_body }
            else { maybe_body.or_else(
              || from_disk . as_ref() . ok() . and_then(
                |node| node.body.clone() )) } ),
    contains: final_contents,
    subscribes_to: None,
    hides_from_its_subscriptions: None,
    overrides_view_of: None,
  }, NodeSaveAction {
    indefinitive: false,
    toDelete: to_delete,
  } )) }

/// Extracts consistent toDelete value from instructions,
/// verifying they all match.
/// (This check is redundant given validate_tree.)
fn to_delete_if_consistent(
  instructions: &[SaveInstruction]
) -> Result<bool, Box<dyn Error>> {
  let to_delete_values: HashSet<bool> =
    instructions . iter ()
    . map ( |(_, action)| action . toDelete)
    . collect ();
  if to_delete_values.len() > 1 {
    return Err(
      "Inconsistent toDelete values for same ID".into()); }
  let to_delete: bool =
    *to_delete_values.iter().next().unwrap();
  Ok(to_delete)
}
