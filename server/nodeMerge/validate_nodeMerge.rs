/// Validation rules:
///   - Both merge partners must be UnrestrictedVognodes with IDs.
///     - Those two IDs must represent distinct nodes.
///     - Those two IDs must already be in the DB.
///   - Neither merge partner can be marked for deletion.
///   - Monogamy:
///     - No node can be an acquirer and an acquiree.
///     - No node can be involved in more than one merge.

use crate::types::viewnode::NodeEditRequest;
use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind, MpUnrestrictedVognode};
use crate::types::maybe_placed_viewnode::MpVognode;
use crate::types::misc::ID;
use crate::dbs::in_rust_graph::InRustGraph;
use ego_tree::Tree;
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct NodeMergeValidationData<'a> {
  acquirer_viewnodes    : Vec<&'a MpViewnode>,
  acquirer_to_acquirees : HashMap<ID, HashSet<ID>>,
  acquiree_to_acquirers : HashMap<ID, HashSet<ID>>,
  to_delete_skgids      : HashSet<ID>, }

/// Validates merge requests in a viewnode viewforest.
/// Returns a vector of validation error messages,
/// which is empty if all are valid.
pub fn validate_nodeMerge_requests(
  viewforest: &Tree<MpViewnode>,
  graph: &InRustGraph,
) -> Result<Vec<String>, Box<dyn Error>> {
  let mut errors: Vec<String> = Vec::new();
  let nodeMerge_validation_data : NodeMergeValidationData =
    collect_nodeMerge_validation_data (viewforest);
  for node in nodeMerge_validation_data . acquirer_viewnodes {
    let t : &MpUnrestrictedVognode = match &node . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) => t,
      _ => { errors . push(format!( "Acquirer must be a vognode that exists: {:?}",
                                     node . kind));
             continue; }};
    let acquirer_skgid : &ID = match &t . skgid {
      Some (skgid) => skgid,
      None => { errors . push(format!(
                  "Acquirer node '{}' must have an ID", t . title));
                continue; }};
    if let Some(NodeEditRequest::NodeMerge (acquiree_skgid))
      = t . edit_request ()
    { let pair_errors : Vec<String> = validate_nodeMerge_pair(
        graph, acquirer_skgid, acquiree_skgid,
        &nodeMerge_validation_data . to_delete_skgids)?;
      errors . extend (pair_errors); }}
  errors . extend( {
    let monogamy_errors : Vec<String> =
      validate_monogamy_for_all_nodeMerges(
        &nodeMerge_validation_data . acquirer_to_acquirees,
        &nodeMerge_validation_data . acquiree_to_acquirers );
    monogamy_errors } );
  Ok (errors) }

/// To understand what this function does,
/// it's easiest to read the definition of its return type.
fn collect_nodeMerge_validation_data<'a>(
  viewforest: &'a Tree<MpViewnode>,
) -> NodeMergeValidationData<'a> {
  let mut acquirer_viewnodes : Vec<&MpViewnode> = Vec::new();
  let mut acquirer_to_acquirees : HashMap<ID, HashSet<ID>> = HashMap::new();
  let mut acquiree_to_acquirers : HashMap<ID, HashSet<ID>> = HashMap::new();
  let mut to_delete_skgids : HashSet<ID> = HashSet::new();
  for edge in viewforest . root() . traverse() {
    if let ego_tree::iter::Edge::Open (node_ref) = edge {
      let viewnode : &MpViewnode = node_ref . value();
      if let MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
        = &viewnode . kind
      { if let Some (skgid) = &t . skgid {
          if matches!(t . edit_request (),
                      Some (NodeEditRequest::Delete)) {
            to_delete_skgids . insert(skgid . clone()); } // mutate!
          if let Some(NodeEditRequest::NodeMerge (acquiree_skgid))
          = t . edit_request ()
          { acquirer_viewnodes . push (viewnode); // mutate!
            acquirer_to_acquirees // mutate!
              . entry(skgid . clone())
              . or_insert_with (HashSet::new)
              . insert(acquiree_skgid . clone());
            acquiree_to_acquirers // mutate!
              . entry(acquiree_skgid . clone())
              . or_insert_with (HashSet::new)
              . insert(skgid . clone()); }} }} }
  NodeMergeValidationData { acquirer_viewnodes,
                        acquirer_to_acquirees,
                        acquiree_to_acquirers,
                        to_delete_skgids, }}

/// Validates a single merge pair (acquirer + acquiree).
/// Returns a vector of validation errors for this pair.
/// The error messages explain what each passage does.
fn validate_nodeMerge_pair(
  graph: &InRustGraph,
  acquirer_skgid: &ID,
  acquiree_skgid: &ID,
  to_delete_skgids: &HashSet<ID>,
) -> Result<Vec<String>, Box<dyn Error>> {
  let mut errors: Vec<String> = Vec::new();
  let acquirer_pid : ID = (
    match graph . pid_and_skgrepo (acquirer_skgid)
    { Some((pid, _skgrepo)) => pid,
      None      => {
        errors . push(format!(
          "Acquirer ID '{}' not found in database",
          acquirer_skgid . as_str() ));
        return Ok (errors); }} );
  let acquiree_pid : ID = (
    match graph . pid_and_skgrepo (acquiree_skgid)
    { Some((pid, _skgrepo)) => pid,
      None => {
        errors . push(format!(
          "Acquiree ID '{}' (requested by '{}') not found in database",
          acquiree_skgid . as_str(),
          acquirer_skgid . as_str() ));
        return Ok (errors); }} );
  if acquirer_pid == acquiree_pid {
    errors . push(format!(
      "Self-merge detected: acquirer '{}' and acquiree '{}' resolve to the same node (PID: '{}')",
      acquirer_skgid . as_str(),
      acquiree_skgid . as_str(),
      acquirer_pid . as_str() )); }
  if to_delete_skgids . contains (acquirer_skgid) {
    errors . push(format!(
      "Acquirer '{}' cannot be marked for deletion",
      acquirer_skgid . as_str() )); }
  if to_delete_skgids . contains (acquiree_skgid) {
    errors . push(format!(
      "Acquiree '{}' (requested by '{}') cannot be marked for deletion",
      acquiree_skgid . as_str(),
      acquirer_skgid . as_str() )); }
  Ok (errors) }

/// Validates monogamy rules for all merges.
/// Returns a vector of validation errors.
/// The error text explains what each passage does.
fn validate_monogamy_for_all_nodeMerges(
  acquirer_to_acquirees: &HashMap<ID, HashSet<ID>>,
  acquiree_to_acquirers: &HashMap<ID, HashSet<ID>>,
) -> Vec<String> {
  let mut errors: Vec<String> = Vec::new();
  for (acquirer_skgid, acquirees) in acquirer_to_acquirees {
    if acquirees . len() > 1 {
      errors . push(format!(
        "Monogamy violation: acquirer '{}' would merge with multiple nodes: {:?}",
        acquirer_skgid . as_str(),
        acquirees . iter() . map(
          |skgid| skgid . as_str()
        ) . collect::<Vec<_>>() )); }}
  for (acquiree_skgid, acquirers) in acquiree_to_acquirers {
    if acquirers . len() > 1 {
      errors . push(format!(
        "Monogamy violation: acquiree '{}' would merge with multiple nodes: {:?}",
        acquiree_skgid . as_str(),
        acquirers . iter() . map(
          |skgid| skgid . as_str()
        ) . collect::<Vec<_>>() )); }}
  let overlap: Vec<&ID> = {
    let acquirer_skgids: HashSet<&ID> =
      acquirer_to_acquirees . keys() . collect();
    let acquiree_skgids: HashSet<&ID> =
      acquiree_to_acquirers . keys() . collect();
    acquirer_skgids . intersection (&acquiree_skgids)
      . copied() . collect() };
  if !overlap . is_empty() {
    errors . push(format!(
      "Monogamy violation: nodes cannot be both acquirer and acquiree: {:?}",
      overlap . iter() . map(
        |skgid| skgid . as_str()
      ) . collect::<Vec<_>>() )); }
  errors }
