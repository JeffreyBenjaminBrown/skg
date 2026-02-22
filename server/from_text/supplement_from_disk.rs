/// PURPOSE:
/// When a SkgNode is created from user input,
/// it might not mention every SkgNode field.
/// If it contains Some([]) for that field,
/// then the user is asking to empty the field.
/// But if it has None for that field,
/// then the field should not be changed --
/// which means it must be read from disk
/// and inserted into the SkgNode.

use crate::dbs::filesystem::one_node::{optskgnode_from_id, skgnode_from_pid_and_source};
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::IdToPidAndSource;
use std::error::Error;
use neo4rs::Graph;

/// Has no effect on Delete instructions.
/// Supplements Save instructions with disk data.
pub async fn supplement_none_fields_from_disk_if_save (
  config      : &SkgConfig,
  graph       : &Graph,
  instruction : DefineNode
) -> Result<DefineNode, Box<dyn Error>> {
  let mut from_buffer : SkgNode = match instruction {
    DefineNode::Delete(_) => return Ok(instruction),
    DefineNode::Save(SaveNode(sn)) => sn };
  let pid: ID =
    from_buffer . ids . first()
    . ok_or("No primary ID found")? . clone();
  let from_disk : Option<SkgNode> =
    optskgnode_from_id( config, graph, &pid
                      ). await?;
  if let Some(disk_node) = from_disk {
    { // Replace buffer's (singleton) ids
      // with disk's (possibly multiple) ids.
      for buffer_id in &from_buffer.ids {
        if ! disk_node . ids . contains(buffer_id) {
          return Err(format!(
            "ID '{}' from buffer not found in IDs form disk.",
            buffer_id ). into() ); }}
      from_buffer . ids = disk_node . ids; }
    if from_buffer.source != disk_node.source {
      return Err(Box::new(
        BufferValidationError::DiskSourceBufferSourceConflict(
          pid . clone(),
          disk_node . source . clone(),
          from_buffer . source . clone() )) ); }
    if from_buffer . aliases . is_none() {
      from_buffer . aliases = disk_node . aliases; }
    if from_buffer . subscribes_to . is_none() {
      from_buffer . subscribes_to = disk_node . subscribes_to; }
    if from_buffer . hides_from_its_subscriptions . is_none() {
      from_buffer . hides_from_its_subscriptions =
        disk_node . hides_from_its_subscriptions; }
    if from_buffer . overrides_view_of . is_none() {
      from_buffer . overrides_view_of =
        disk_node . overrides_view_of; }}
  Ok(DefineNode::Save(SaveNode(from_buffer) )) }

/// Like supplement_none_fields_from_disk_if_save, but uses a
/// pre-fetched map of ID→(PID, SourceName) instead of querying Neo4j.
/// Falls back to the original function for IDs not in the map.
pub async fn supplement_none_fields_from_disk_with_pid_source_map (
  config         : &SkgConfig,
  graph          : &Graph,
  instruction    : DefineNode,
  pid_source_map : &IdToPidAndSource,
) -> Result<DefineNode, Box<dyn Error>> {
  let mut from_buffer : SkgNode = match instruction {
    DefineNode::Delete(_) => return Ok(instruction),
    DefineNode::Save(SaveNode(sn)) => sn };
  let pid : ID =
    from_buffer . ids . first()
    . ok_or("No primary ID found")? . clone();
  let from_disk : Option<SkgNode> =
    match pid_source_map . get ( &pid ) {
      Some ( (fetched_pid, source) ) =>
        match skgnode_from_pid_and_source (
          config, fetched_pid . clone (), source ) {
            Ok ( node ) => Some ( node ),
            Err ( e ) => {
              let error_msg : String = e.to_string();
              if error_msg.contains("not found")
                || error_msg.contains("No such file")
                || error_msg.contains("does not exist")
              { None }
              else { return Err ( e . into () ) } } },
      None =>
        optskgnode_from_id( config, graph, &pid ). await? };
  if let Some(disk_node) = from_disk {
    { // Replace buffer's (singleton) ids
      // with disk's (possibly multiple) ids.
      for buffer_id in &from_buffer.ids {
        if ! disk_node . ids . contains(buffer_id) {
          return Err(format!(
            "ID '{}' from buffer not found in IDs form disk.",
            buffer_id ). into() ); }}
      from_buffer . ids = disk_node . ids; }
    if from_buffer.source != disk_node.source {
      return Err(Box::new(
        BufferValidationError::DiskSourceBufferSourceConflict(
          pid . clone(),
          disk_node . source . clone(),
          from_buffer . source . clone() )) ); }
    if from_buffer . aliases . is_none() {
      from_buffer . aliases = disk_node . aliases; }
    if from_buffer . subscribes_to . is_none() {
      from_buffer . subscribes_to = disk_node . subscribes_to; }
    if from_buffer . hides_from_its_subscriptions . is_none() {
      from_buffer . hides_from_its_subscriptions =
        disk_node . hides_from_its_subscriptions; }
    if from_buffer . overrides_view_of . is_none() {
      from_buffer . overrides_view_of =
        disk_node . overrides_view_of; }}
  Ok(DefineNode::Save(SaveNode(from_buffer) )) }
