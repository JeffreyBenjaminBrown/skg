use crate::dbs::filesystem::one_node::optskgnode_from_id;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::skgnode::SkgNode;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Has no effect on Delete instructions.
/// Supplements Save instructions with disk data:
/// replaces None fields with values from disk,
/// and validates that sources match. (To delete such a field,
/// the SaveNode should use Some ( [] ) rather than None.)
pub(crate) async fn supplement_from_disk_if_save(
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
