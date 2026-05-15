pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod classify;
pub mod subscribee_visibility_intents;

use classify::{ viewforest_with_save_roles, ViewNode_in_Role };
use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, SkgConfig};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use crate::types::viewnode::ViewNode;
use crate::types::views_state::nodecomplete_from_in_rust_graph;
use subscribee_visibility_intents::{ SubscribeeVisibilityIntent, subscribee_visibility_intents_from_tree, };
use super::supplement_from_disk::{ canonicalize_ids_from_disk, detect_source_move, supplement_unspecified_fields_from_disk, };
use super::validate::buffernode_differs_from_disknode;
use to_naive_instructions::{ naive_node_edit_intents_from_viewforest, reconcile_nodeEditIntents, NodeEditIntent, };

use ego_tree::Tree;
use std::collections::{HashMap, HashSet};
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
  let role_viewforest : Tree<ViewNode_in_Role> =
    viewforest_with_save_roles (viewforest) ?;
  let visibility_intents : Vec<SubscribeeVisibilityIntent> =
    subscribee_visibility_intents_from_tree (&role_viewforest)
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  let naive_intents : Vec<NodeEditIntent> =
    naive_node_edit_intents_from_viewforest (viewforest)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  let intents_without_dups : Vec<NodeEditIntent> =
    reconcile_nodeEditIntents (naive_intents)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  let mut result : Vec<DefineNode> =
    Vec::with_capacity ( intents_without_dups . len() );
  let mut source_moves : Vec<SourceMove> = Vec::new();
  for intent in intents_without_dups {
    let children_affect_only_hiding : bool =
      intent . children_affect_only_hiding();
    let instr_dn : DefineNode =
      intent . into_define_node()
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    match instr_dn {
      DefineNode::Delete (_) =>
        result . push (instr_dn),
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
            let mut supplemented : NodeComplete =
              supplement_unspecified_fields_from_disk (
                canonicalized, &disk_node);
            if children_affect_only_hiding { // Clobber content. For why, see the comment about children_affect_only_hiding in the NodeSaveIntent typedef.
              supplemented . contains = disk_node . contains . clone(); }
            result . push (DefineNode::Save (SaveNode (supplemented)));
            if let Some (sm) = maybe_move {
              let sm : SourceMove = sm;
              source_moves . push (sm); }}} }}}
  let result : Vec<DefineNode> =
    apply_hiderels_from_subscribee_visibility (
      result, &visibility_intents, config, driver ) . await ?;
  let changed_instructions : Vec<DefineNode> =
    filter_unchanged_save_instructions (result);
  Ok ((changed_instructions, source_moves)) }

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
/// For each `(subscriber, subscribee, visible_content)` intent:
///
/// - any pre-save direct content of the subscribee that is absent from
///   `visible_content` becomes hidden from that subscriber, unless the
///   same ID was moved into the subscriber's ordinary direct content;
/// - any pre-save direct content of the subscribee that is present in
///   `visible_content` is removed from that subscriber's hides.
async fn apply_hiderels_from_subscribee_visibility (
  mut instructions : Vec<DefineNode>,
  intents          : &[SubscribeeVisibilityIntent],
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  validate_no_overlapping_subscribee_visibility_conflicts (
    intents, config, driver ) . await ?;
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
    let inferred_unhides : Vec<ID> =
      subscribee_from_disk . contains . iter()
      . filter ( |id| visible_content . contains (*id) )
      . filter ( |id| subscriber_from_disk . hides_from_its_subscriptions
                      . or_default() . contains (*id) )
      . cloned() . collect();
    if inferred_hides . is_empty() && inferred_unhides . is_empty()
      { continue; }
    let subscriber_node : &mut NodeComplete =
      ensure_subscriber_save_instruction (
        &mut instructions, subscriber_from_disk );
    apply_hiderel_delta_to_subscriber (
      subscriber_node, &inferred_hides, &inferred_unhides ); }
  Ok (instructions) }

async fn validate_no_overlapping_subscribee_visibility_conflicts (
  intents : &[SubscribeeVisibilityIntent],
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let mut seen : HashMap<(ID, ID), bool> = HashMap::new();
  for intent in intents {
    let intent : &SubscribeeVisibilityIntent = intent;
    let subscribee_from_disk : NodeComplete =
      match optnodecomplete_from_id (
        config, driver, &intent . subscribee ) . await ?
      { Some (subscribee_from_disk) => subscribee_from_disk,
        None                        => continue, };
    let visible_content : HashSet<ID> =
      intent . visible_content . iter() . cloned() . collect();
    for content_id in subscribee_from_disk . contains {
      let content_id : ID = content_id;
      let key : (ID, ID) =
        (intent . subscriber . clone(), content_id . clone());
      let visible : bool = visible_content . contains (&content_id);
      match seen . get (&key) {
        Some (previous_visible) => {
          let previous_visible : &bool = previous_visible;
          if *previous_visible != visible {
            return Err (Box::new (BufferValidationError::Other (
              format!( "Conflicting subscribee visibility edits for subscriber {} and content {}",
                        intent . subscriber, content_id )) )); }},
        None => {}, }
      seen . insert (key, visible); }}
  Ok (( )) }

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

fn apply_hiderel_delta_to_subscriber (
  subscriber     : &mut NodeComplete,
  inferred_hides : &[ID],
  inferred_unhides : &[ID],
) {
  let mut hides : Vec<ID> =
    subscriber . hides_from_its_subscriptions
    . or_default() . to_vec();
  hides . retain ( |id| ! inferred_unhides . contains (id) );
  for id in inferred_hides {
    if ! hides . contains (id) {
      hides . push (id . clone()); }}
  subscriber . hides_from_its_subscriptions =
    MSV::Specified (hides); }

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
