/// Validation rules:
///   - Both merge partners must be Occurrences with IDs.
///     - Those two IDs must represent distinct nodes.
///     - Those two IDs must already be in the DB.
///   - Neither merge partner can be marked for deletion.
///   - Monogamy:
///     - No node can be an acquirer and an acquiree.
///     - No node can be involved in more than one merge.

use crate::types::viewnode::NodeEditRequest;
use crate::types::viewnode::{Viewnode, ViewnodeKind, Occurrence};
use crate::types::viewnode::Vognode;
use crate::types::misc::ID;
use crate::dbs::in_rust_graph::InRustGraph;
use ego_tree::Tree;
use std::collections::{HashMap, HashSet};
use std::error::Error;

struct NodeMergeValidationData<'a> {
  acquirer_viewnodes    : Vec<&'a Viewnode>,
  acquirer_to_acquirees : HashMap<ID, HashSet<ID>>,
  acquiree_to_acquirers : HashMap<ID, HashSet<ID>>,
  to_delete_gnids      : HashSet<ID>, }

/// Validates merge requests in a viewnode viewforest.
/// Returns a vector of validation error messages,
/// which is empty if all are valid.
pub fn validate_nodeMerge_requests(
  viewforest: &Tree<Viewnode>,
  graph: &InRustGraph,
) -> Result<Vec<String>, Box<dyn Error>> {
  let mut errors: Vec<String> = Vec::new();
  let nodeMerge_validation_data : NodeMergeValidationData =
    collect_nodeMerge_validation_data (viewforest);
  for node in nodeMerge_validation_data . acquirer_viewnodes {
    let t : &Occurrence = match &node . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) => t,
      _ => { errors . push(format!( "Acquirer must be a vognode that exists: {:?}",
                                     node . kind));
             continue; }};
    let acquirer_gnid : &ID = &t . gnid;
    if let Some(NodeEditRequest::NodeMerge (acquiree_gnid))
      = t . edit_request ()
    { let pair_errors : Vec<String> = validate_nodeMerge_pair(
        graph, acquirer_gnid, acquiree_gnid,
        &nodeMerge_validation_data . to_delete_gnids)?;
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
  viewforest: &'a Tree<Viewnode>,
) -> NodeMergeValidationData<'a> {
  let mut acquirer_viewnodes : Vec<&Viewnode> = Vec::new();
  let mut acquirer_to_acquirees : HashMap<ID, HashSet<ID>> = HashMap::new();
  let mut acquiree_to_acquirers : HashMap<ID, HashSet<ID>> = HashMap::new();
  let mut to_delete_gnids : HashSet<ID> = HashSet::new();
  for edge in viewforest . root() . traverse() {
    if let ego_tree::iter::Edge::Open (node_ref) = edge {
      let viewnode : &Viewnode = node_ref . value();
      if let ViewnodeKind::Vognode (Vognode::Occurrence (t))
        = &viewnode . kind
      { { let gnid : &ID = &t . gnid;
          if matches!(t . edit_request (),
                      Some (NodeEditRequest::Delete)) {
            to_delete_gnids . insert(gnid . clone()); } // mutate!
          if let Some(NodeEditRequest::NodeMerge (acquiree_gnid))
          = t . edit_request ()
          { acquirer_viewnodes . push (viewnode); // mutate!
            acquirer_to_acquirees // mutate!
              . entry(gnid . clone())
              . or_insert_with (HashSet::new)
              . insert(acquiree_gnid . clone());
            acquiree_to_acquirers // mutate!
              . entry(acquiree_gnid . clone())
              . or_insert_with (HashSet::new)
              . insert(gnid . clone()); }} }} }
  NodeMergeValidationData { acquirer_viewnodes,
                        acquirer_to_acquirees,
                        acquiree_to_acquirers,
                        to_delete_gnids, }}

/// Validates a single merge pair (acquirer + acquiree).
/// Returns a vector of validation errors for this pair.
/// The error messages explain what each passage does.
fn validate_nodeMerge_pair(
  graph: &InRustGraph,
  acquirer_gnid: &ID,
  acquiree_gnid: &ID,
  to_delete_gnids: &HashSet<ID>,
) -> Result<Vec<String>, Box<dyn Error>> {
  let mut errors: Vec<String> = Vec::new();
  let acquirer_pid : ID = (
    match graph . pid_and_skgrepo (acquirer_gnid)
    { Some((pid, _skgrepo)) => pid,
      None      => {
        errors . push(format!(
          "Acquirer ID '{}' not found in database",
          acquirer_gnid . as_str() ));
        return Ok (errors); }} );
  let acquiree_pid : ID = (
    match graph . pid_and_skgrepo (acquiree_gnid)
    { Some((pid, _skgrepo)) => pid,
      None => {
        errors . push(format!(
          "Acquiree ID '{}' (requested by '{}') not found in database",
          acquiree_gnid . as_str(),
          acquirer_gnid . as_str() ));
        return Ok (errors); }} );
  if acquirer_pid == acquiree_pid {
    errors . push(format!(
      "Self-merge detected: acquirer '{}' and acquiree '{}' resolve to the same node (PID: '{}')",
      acquirer_gnid . as_str(),
      acquiree_gnid . as_str(),
      acquirer_pid . as_str() )); }
  if to_delete_gnids . contains (acquirer_gnid) {
    errors . push(format!(
      "Acquirer '{}' cannot be marked for deletion",
      acquirer_gnid . as_str() )); }
  if to_delete_gnids . contains (acquiree_gnid) {
    errors . push(format!(
      "Acquiree '{}' (requested by '{}') cannot be marked for deletion",
      acquiree_gnid . as_str(),
      acquirer_gnid . as_str() )); }
  Ok (errors) }

/// Validates monogamy rules for all merges.
/// Returns a vector of validation errors.
/// The error text explains what each passage does.
fn validate_monogamy_for_all_nodeMerges(
  acquirer_to_acquirees: &HashMap<ID, HashSet<ID>>,
  acquiree_to_acquirers: &HashMap<ID, HashSet<ID>>,
) -> Vec<String> {
  let mut errors: Vec<String> = Vec::new();
  for (acquirer_gnid, acquirees) in acquirer_to_acquirees {
    if acquirees . len() > 1 {
      errors . push(format!(
        "Monogamy violation: acquirer '{}' would merge with multiple nodes: {:?}",
        acquirer_gnid . as_str(),
        acquirees . iter() . map(
          |gnid| gnid . as_str()
        ) . collect::<Vec<_>>() )); }}
  for (acquiree_gnid, acquirers) in acquiree_to_acquirers {
    if acquirers . len() > 1 {
      errors . push(format!(
        "Monogamy violation: acquiree '{}' would merge with multiple nodes: {:?}",
        acquiree_gnid . as_str(),
        acquirers . iter() . map(
          |gnid| gnid . as_str()
        ) . collect::<Vec<_>>() )); }}
  let overlap: Vec<&ID> = {
    let acquirer_gnids: HashSet<&ID> =
      acquirer_to_acquirees . keys() . collect();
    let acquiree_gnids: HashSet<&ID> =
      acquiree_to_acquirers . keys() . collect();
    acquirer_gnids . intersection (&acquiree_gnids)
      . copied() . collect() };
  if !overlap . is_empty() {
    errors . push(format!(
      "Monogamy violation: nodes cannot be both acquirer and acquiree: {:?}",
      overlap . iter() . map(
        |gnid| gnid . as_str()
      ) . collect::<Vec<_>>() )); }
  errors }
