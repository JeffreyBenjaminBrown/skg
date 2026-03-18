/// PURPOSE:
/// When a SkgNode is created from user input,
/// it might not mention every SkgNode field.
/// If it contains Some([]) for that field,
/// then the user is asking to empty the field.
/// But if it has None for that field,
/// then the field should not be changed --
/// which means it must be read from disk
/// and inserted into the SkgNode.

use crate::dbs::filesystem::one_node::optskgnode_from_id;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use crate::types::skgnode::SkgNode;
use crate::types::memory::SkgNodeMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Has no effect on Delete instructions.
/// Supplements Save instructions with disk data.
/// Result Includes a SourceMove if the buffer source
/// differs from the disk source and both sources are owned by the user.
pub async fn supplement_none_fields_from_disk_if_save (
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  pool        : &SkgNodeMap,
  instruction : DefineNode
) -> Result<(DefineNode, Option<SourceMove>), Box<dyn Error>> {
  let mut from_buffer : SkgNode = match instruction {
    DefineNode::Delete (_) => return Ok ((instruction, None)),
    DefineNode::Save(SaveNode (sn)) => sn };
  let pid: ID =
    from_buffer . pid . clone();
  let from_disk : Option<SkgNode> =
    if let Some (from_pool) = pool . get (&pid)
      { Some ( from_pool . clone () ) }
      else { optskgnode_from_id( config, driver, &pid
                               ) . await? };
  let mut source_move : Option<SourceMove> = None;
  if let Some (disk_node) = from_disk {
    { // Replace buffer's (singleton) ids
      // with disk's (possibly multiple) ids.
      for buffer_id in from_buffer . all_ids() {
        if ! disk_node . all_ids() . any ( |id| id == buffer_id ) {
          return Err(format!(
            "ID '{}' from buffer not found in IDs form disk.",
            buffer_id ) . into() ); }}
      from_buffer . pid = disk_node . pid;
      from_buffer . extra_ids = disk_node . extra_ids; }
    if from_buffer . source != disk_node . source {
      if config . user_owns_source (&disk_node . source)
      && config . user_owns_source (&from_buffer . source) {
        source_move = Some (SourceMove {
          pid        : pid . clone(),
          old_source : disk_node . source . clone(),
          new_source : from_buffer . source . clone() });
      } else {
        return Err(Box::new(
          BufferValidationError::CannotMoveToOrFromForeignSource(
            pid . clone(),
            disk_node . source . clone(),
            from_buffer . source . clone() )) ); }}
    if from_buffer . aliases . is_unspecified() {
      from_buffer . aliases = disk_node . aliases; }
    if from_buffer . subscribes_to . is_unspecified() {
      from_buffer . subscribes_to = disk_node . subscribes_to; }
    if from_buffer . hides_from_its_subscriptions . is_unspecified() {
      from_buffer . hides_from_its_subscriptions =
        disk_node . hides_from_its_subscriptions; }
    if from_buffer . overrides_view_of . is_unspecified() {
      from_buffer . overrides_view_of =
        disk_node . overrides_view_of; }
    if from_buffer . misc . is_empty() {
      from_buffer . misc = disk_node . misc; }}
  Ok((DefineNode::Save(SaveNode (from_buffer) ), source_move)) }
