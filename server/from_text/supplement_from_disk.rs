/// PURPOSE:
/// When a NodeComplete is created from user input,
/// it might not mention every NodeComplete field.
/// If it contains Some([]) for that field,
/// then the user is asking to empty the field.
/// But if it has None for that field,
/// then the field should not be changed --
/// which means it must be read from disk
/// and inserted into the NodeComplete.

use crate::dbs::node_lookup::optNodeComplete_rustFIrst_by_id;
use crate::from_text::local_instruction_collection::lower::{
  NodeIntent, NodeSaveIntent };
use crate::source_sets::ActiveSourceSet;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub struct Definenodes_with_Sourcemoves {
  pub instructions : Vec<DefineNode>,
  pub source_moves : Vec<SourceMove>,
}

struct Definenode_with_Opt_Sourcemove {
  instruction : DefineNode,
  source_move : Option<SourceMove>,
}

impl Definenodes_with_Sourcemoves {
  fn with_capacity (
    capacity : usize,
  ) -> Definenodes_with_Sourcemoves {
    Definenodes_with_Sourcemoves {
      instructions : Vec::with_capacity (capacity),
      source_moves : Vec::new(),
    }}

  fn push (
    &mut self,
    node : Definenode_with_Opt_Sourcemove,
  ) {
    self . instructions . push (node . instruction);
    if let Some (sm) = node . source_move {
      let sm : SourceMove = sm;
      self . source_moves . push (sm); }}
}

pub async fn build_diskSupplemented_defineNodes (
  intents : Vec<NodeIntent>,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>, // None means no restriction; callers normalize 'all' to None.
) -> Result<Definenodes_with_Sourcemoves, Box<dyn Error>> {
  let mut result : Definenodes_with_Sourcemoves =
    Definenodes_with_Sourcemoves::with_capacity (intents . len());
  for intent in intents {
    let supplemented : Definenode_with_Opt_Sourcemove =
      supplement_nodeeditintent_from_disk (
        intent, config, driver, restricted_source_set ) . await ?;
    result . push (supplemented); }
  Ok (result) }

async fn supplement_nodeeditintent_from_disk (
  intent : NodeIntent,
  config : &SkgConfig,
  driver : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>,
) -> Result<Definenode_with_Opt_Sourcemove, Box<dyn Error>> {
  match intent {
    NodeIntent::Delete (_) =>
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : intent . into_define_node()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        source_move : None,
      }),
    _ => supplement_saveintent_from_disk (
      intent . save_intent()
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      config, driver, restricted_source_set ) . await,
  }}

async fn supplement_saveintent_from_disk (
  from_buffer : NodeSaveIntent,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  restricted_source_set : Option<&ActiveSourceSet>,
) -> Result<Definenode_with_Opt_Sourcemove, Box<dyn Error>> {
  let _ = restricted_source_set; // consumed by the weave/merge wiring in the next commit
  let pid : ID =
    from_buffer . pid . clone();
  let from_disk : Option<NodeComplete> =
    optNodeComplete_rustFIrst_by_id (
      config, driver, &pid) . await ?;
  match from_disk {
    None =>
      Ok (Definenode_with_Opt_Sourcemove {
        instruction :
          NodeIntent::Save (from_buffer) . into_define_node()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        source_move : None, } ),
    Some (disk_node) => {
      let disk_node : NodeComplete = disk_node;
      let mut from_buffer : NodeSaveIntent = from_buffer;
      from_buffer . fill_unspecified_contains (
        &disk_node . contains);
      let from_buffer : NodeComplete =
        from_buffer . into_nodecomplete();
      let canonicalized : NodeComplete =
        canonicalize_ids_from_disk (from_buffer, &disk_node) ?;
      let maybe_move : Option<SourceMove> =
        detect_source_move ( config,  &pid,
                             &canonicalized . source,
                             &disk_node . source) ?;
      let supplemented : NodeComplete =
        supplement_unspecified_fields_from_disk (
          canonicalized, &disk_node);
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : DefineNode::Save (SaveNode (supplemented)),
        source_move : maybe_move,
      }) }}}

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
