pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod classify;
pub mod subscribee_visibility_intents;

use classify::{classify_save_roles, SaveRole, SaveRoleMap};
use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::types::misc::{ID, MSV, SkgConfig};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::types::views_state::nodecomplete_from_in_rust_graph;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use subscribee_visibility_intents::{ SubscribeeVisibilityIntent, subscribee_visibility_intents_from_tree, };
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
  let roles : SaveRoleMap =
    classify_save_roles (viewforest) ?;
  let direct_as_subscribee_save_pids : HashSet<ID> =
    direct_as_subscribee_save_pids_from_roles (viewforest, &roles) ?;
  let visibility_intents : Vec<SubscribeeVisibilityIntent> =
    subscribee_visibility_intents_from_tree (
      viewforest, &roles )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
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
  let result : Vec<DefineNode> =
    add_hiderels_from_subscribees (
      result, &visibility_intents, config, driver ) . await ?;
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
  direct_as_subscribee_save_pids_from_roles (viewforest, &roles) }

fn direct_as_subscribee_save_pids_from_roles (
  viewforest : &Tree<ViewNode>,
  roles      : &SaveRoleMap,
) -> Result<HashSet<ID>, Box<dyn Error>> {
  let mut result : HashSet<ID> = HashSet::new();
  for node_ref in viewforest . nodes() {
    if ! matches!(
      roles . get (&node_ref . id()),
      Some (SaveRole::AsSubscribee { .. }))
    { continue; }
    if let ViewNodeKind::True (t) = &node_ref . value() . kind {
      result . insert (t . id . clone()); }}
  Ok (result) }

/// Interpret direct child-list edits under definitive AsSubscribee
/// branches as subscriber visibility edits.
///
/// At this point ordinary graph SaveNodes have already been built,
/// reconciled, and supplemented from disk, but filtering
/// unchanged (no-op) instructions has not run yet.
/// That lets this pass add a real (does-op)
/// `hides_from_its_subscriptions` change to the owned subscriber
/// before the subscriber would otherwise be filtered out as a no-op.
///
/// For each `(subscriber, subscribee, visible_content)` intent, any
/// pre-save direct content of the subscribee that is absent from
/// `visible_content` becomes hidden from that subscriber, unless the
/// same ID was moved into the subscriber's ordinary direct content.
///
/// This is hide-only for now. Unhide inference will need the inverse
/// comparison against existing subscriber hides.
async fn add_hiderels_from_subscribees (
  mut instructions : Vec<DefineNode>,
  intents          : &[SubscribeeVisibilityIntent],
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  for intent in intents {
    let Some (subscribee_from_disk) =
      optnodecomplete_from_id (
        config, driver, &intent . subscribee ) . await ?
    else { continue; };
    let Some (subscriber_from_disk) =
      optnodecomplete_from_id (
        config, driver, &intent . subscriber ) . await ?
    else { continue; };
    if ! source_is_owned (config, &subscriber_from_disk . source) {
      continue; }

    let subscriber_contains : HashSet<ID> =
      subscriber_will_contain_after_save (
        &instructions, &subscriber_from_disk );
    let visible_content : HashSet<ID> =
      intent . visible_content . iter() . cloned() . collect();
    let inferred_hides : Vec<ID> =
      subscribee_from_disk . contains . iter()
      . filter ( |id| ! visible_content . contains (*id) )
      . filter ( |id| ! subscriber_contains . contains (*id) )
      . cloned()
      . collect();
    if inferred_hides . is_empty() { continue; }

    let subscriber_node : &mut NodeComplete =
      ensure_subscriber_save_instruction (
        &mut instructions, subscriber_from_disk );
    append_hides_to_subscriber (
      subscriber_node, &inferred_hides ); }
  Ok (instructions) }

fn source_is_owned (
  config : &SkgConfig,
  source : &crate::types::misc::SourceName,
) -> bool {
  config . sources . get (source)
    . map ( |s| s . user_owns_it )
    . unwrap_or (false) }

fn subscriber_will_contain_after_save (
  instructions          : &[DefineNode],
  subscriber_from_disk  : &NodeComplete,
) -> HashSet<ID> {
  instructions . iter()
    . find_map ( |instr| match instr {
      DefineNode::Save (SaveNode (node))
        if node . pid == subscriber_from_disk . pid =>
          Some (node . contains . iter() . cloned() . collect()),
      _ => None })
    . unwrap_or_else ( ||
      subscriber_from_disk . contains . iter() . cloned() . collect()) }

/// Return the subscriber SaveNode that should receive inferred
/// `hides_from_its_subscriptions` edits.
///
/// If the save already includes an instruction for the subscriber, reuse
/// that mutable node so inferred hides compose with same-save edits.
/// Otherwise, create a SaveNode from the current disk value; the added
/// hide relation is what makes that otherwise unchanged subscriber
/// instruction worth keeping.
fn ensure_subscriber_save_instruction (
  instructions         : &mut Vec<DefineNode>,
  subscriber_from_disk : NodeComplete,
) -> &mut NodeComplete {
  if let Some (index) =
    instructions . iter() . position (
      |instr| match instr {
        DefineNode::Save (SaveNode (node)) =>
          node . pid == subscriber_from_disk . pid,
        _ => false } )
  { match instructions . get_mut (index) . unwrap() {
      DefineNode::Save (SaveNode (node)) => node,
      _ => unreachable!(), }
  } else {
    instructions . push (
      DefineNode::Save (SaveNode (subscriber_from_disk)));
    match instructions . last_mut() . unwrap() {
      DefineNode::Save (SaveNode (node)) => node,
      _ => unreachable!(), }}}

fn append_hides_to_subscriber (
  subscriber     : &mut NodeComplete,
  inferred_hides : &[ID],
) {
  let mut hides : Vec<ID> =
    subscriber . hides_from_its_subscriptions
    . or_default() . to_vec();
  for id in inferred_hides {
    if ! hides . contains (id) {
      hides . push (id . clone()); }}
  subscriber . hides_from_its_subscriptions =
    MSV::Specified (hides); }

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
