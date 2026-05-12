pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod classify;

use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::types::views_state::nodecomplete_from_in_rust_graph;
use classify::{classify_save_roles, SaveRole, SaveRoleMap};
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use super::supplement_from_disk::{ canonicalize_ids_from_disk, detect_source_move, supplement_unspecified_fields_from_disk, };
use super::validate::buffernode_differs_from_disknode;
use to_naive_instructions::naive_saveinstructions_from_tree;

use ego_tree::Tree;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Converts a viewforest of ViewNodes to DefineNodes,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and supplementing None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// The initial extraction is called "naive" because its output
/// is preliminary: it has not yet gone through same-ID reconciliation,
/// disk supplementation, or unchanged filtering.
pub async fn viewforest_to_nonmerge_save_instructions (
  viewforest : &Tree<ViewNode>, // "viewforest" = tree with BufferRoot
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<(Vec<DefineNode>, Vec<SourceMove>), Box<dyn Error>> {
  let direct_as_subscribee_save_pids : HashSet<ID> =
    direct_as_subscribee_save_pids (viewforest) ?;
  let naive_instructions : Vec<DefineNode> =
    naive_saveinstructions_from_tree (
      viewforest . clone( )) ?;
  let instructions_without_dups : Vec<DefineNode> =
    reconcile_same_id_instructions (naive_instructions) ?;
  let mut result : Vec<DefineNode> =
    Vec::with_capacity ( instructions_without_dups . len() );
  let mut source_moves : Vec<SourceMove> = Vec::new();
  for instr in instructions_without_dups {
    let instr : DefineNode = instr;
    match instr {
      DefineNode::Delete (_) =>
        result . push (instr),
      DefineNode::Save (SaveNode (from_buffer)) => {
        let from_buffer : NodeComplete = from_buffer;
        let pid : ID =
          from_buffer . pid . clone();
        let from_disk : Option<NodeComplete> =
          optnodecomplete_from_id (config, driver, &pid) . await ?;
        match from_disk {
          None => result . push (
            DefineNode::Save (SaveNode (from_buffer))),
          Some (disk_node) => {
            let disk_node : NodeComplete = disk_node;
            let canonicalized : NodeComplete =
              canonicalize_ids_from_disk (from_buffer, &disk_node) ?;
            let maybe_move : Option<SourceMove> =
              detect_source_move ( config,  &pid,
                                   &canonicalized . source,
                                   &disk_node . source) ?;
            let supplemented : NodeComplete =
              supplement_unspecified_fields_from_disk (
                canonicalized, &disk_node);
            let supplemented : NodeComplete =
              restore_direct_as_subscribee_contains_from_disk (
                // PITFALL | TODO: This is just a temporary guardrail, to prevent bogus edits to (usually foreign) subscribees being shown as subscribees.
                supplemented,
                &disk_node,
                &direct_as_subscribee_save_pids);
            result . push (DefineNode::Save (SaveNode (supplemented)));
            if let Some (sm) = maybe_move {
              let sm : SourceMove = sm;
              source_moves . push (sm); }}} }}}
  let changed_instructions : Vec<DefineNode> =
    filter_unchanged_save_instructions (result);
  Ok ((changed_instructions, source_moves)) }

/// PIDs for TrueNodes shown directly as subscribees in a SubscribeeCol
/// branch. Their buffer children are meaningful, but not as graph
/// `contains` edits for the subscribee; later they should become
/// subscriber hide/unhide intent. Until that pass exists, these PIDs
/// are used only as a guardrail to block bogus subscribee `contains`
/// saves. A subscribee's descendants are not treated specially
/// -- just the subscribee itself.
pub fn direct_as_subscribee_save_pids (
  viewforest : &Tree<ViewNode>,
) -> Result<HashSet<ID>, Box<dyn Error>> {
  let roles : SaveRoleMap =
    classify_save_roles (viewforest) ?;
  let mut result : HashSet<ID> = HashSet::new();
  for node_ref in viewforest . nodes() {
    if ! matches!(
      roles . get (&node_ref . id()),
      Some (SaveRole::AsSubscribee { .. }))
    { continue; }
    if let ViewNodeKind::True (t) = &node_ref . value() . kind {
      result . insert (t . id . clone()); }}
  Ok (result) }

fn restore_direct_as_subscribee_contains_from_disk (
  mut from_buffer                  : NodeComplete,
  disk_node                        : &NodeComplete,
  direct_as_subscribee_save_pids   : &HashSet<ID>,
) -> NodeComplete {
  // Transitional guardrail: direct AsSubscribee child lists should
  // not define graph `contains`, but future hide/unhide inference
  // will need to read them before this value is restored.
  if direct_as_subscribee_save_pids . contains (&from_buffer . pid) {
    from_buffer . contains = disk_node . contains . clone(); }
  from_buffer }

/// Filters out Save instructions that would be no-ops,
/// because they match the pre-save in-Rust graph entry
/// (nothing changed). Delete instructions and new nodes (not yet
/// in the in-Rust graph) are kept. This runs after disk
/// supplementation so unspecified fields have already been restored
/// to their disk values before comparison.
fn filter_unchanged_save_instructions (
  instructions : Vec<DefineNode>,
) -> Vec<DefineNode> {
  let initial_count : usize = instructions . len();
  let filtered : Vec<DefineNode> = instructions
    . into_iter()
    . filter(|instr| match instr {
      DefineNode::Save(SaveNode (node)) => {
        match nodecomplete_from_in_rust_graph (&node . pid) {
          Some (pre_save) =>
            buffernode_differs_from_disknode (node, &pre_save),
          None => true, }}
      DefineNode::Delete (_) => true, })
    . collect();
  let removed_count : usize = initial_count - filtered . len();
  tracing::debug!("filter_unchanged_save_instructions: \
             kept {} of {} instructions ({} unchanged filtered out)",
            filtered . len(), initial_count, removed_count);
  filtered }
