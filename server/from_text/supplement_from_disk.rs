/// PURPOSE:
/// When a NodeComplete is created from user input,
/// it might not mention every NodeComplete field.
/// If it contains Some([]) for that field,
/// then the user is asking to empty the field.
/// But if it has None for that field,
/// then the field should not be changed --
/// which means it must be read from disk
/// and inserted into the NodeComplete.

use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::SourceMove;
use std::error::Error;

/// Replace buffer's (singleton) ids with disk's (possibly multiple) ids.
pub fn canonicalize_ids_from_disk (
  mut from_buffer : NodeComplete,
  disk_node       : &NodeComplete,
) -> Result<NodeComplete, Box<dyn Error>> {
  for buffer_id in from_buffer . all_ids() {
    let buffer_id : &ID = buffer_id;
    if ! disk_node . all_ids() . any ( |id| id == buffer_id ) {
      return Err(format!(
        "ID '{}' from buffer not found in IDs form disk.",
        buffer_id ) . into() ); }}
  from_buffer . pid = disk_node . pid . clone();
  from_buffer . extra_ids = disk_node . extra_ids . clone();
  Ok (from_buffer) }

/// Return a SourceMove when the source changes
/// between two owned sources.
pub fn detect_source_move (
  config        : &SkgConfig,
  pid           : &ID,
  buffer_source : &SourceName,
  disk_source   : &SourceName,
) -> Result<Option<SourceMove>, Box<dyn Error>> {
  if buffer_source == disk_source {
    return Ok (None); }
  if config . user_owns_source (disk_source)
  && config . user_owns_source (buffer_source) {
    Ok (Some (SourceMove {
      pid        : pid . clone(),
      old_source : disk_source . clone(),
      new_source : buffer_source . clone() }))
  } else {
    Err(Box::new(
      BufferValidationError::CannotMoveToOrFromForeignSource(
        pid . clone(),
        disk_source . clone(),
        buffer_source . clone() )) ) }}

/// Fill buffer fields that the buffer left unspecified.
pub fn supplement_unspecified_fields_from_disk (
  mut from_buffer : NodeComplete,
  disk_node       : &NodeComplete,
) -> NodeComplete {
  if from_buffer . aliases . is_unspecified() {
    from_buffer . aliases = disk_node . aliases . clone(); }
  if from_buffer . subscribes_to . is_unspecified() {
    from_buffer . subscribes_to =
      disk_node . subscribes_to . clone(); }
  if from_buffer . hides_from_its_subscriptions . is_unspecified() {
    from_buffer . hides_from_its_subscriptions =
      disk_node . hides_from_its_subscriptions . clone(); }
  if from_buffer . overrides_view_of . is_unspecified() {
    from_buffer . overrides_view_of =
      disk_node . overrides_view_of . clone(); }
  if from_buffer . misc . is_empty() {
    from_buffer . misc = disk_node . misc . clone(); }
  from_buffer }
