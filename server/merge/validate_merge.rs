/// Validation rules:
///   - Both merge partners must be TrueNodes with IDs.
///     - Those two IDs must represent distinct nodes.
///     - Those two IDs must already be in the DB.
///   - Neither merge partner can be marked for deletion.
///   - Monogamy:
///     - No node can be an acquirer and an acquiree.
///     - No node can be involved in more than one merge.

use crate::types::orgnode::EditRequest;
use crate::types::orgnode::{OrgNode, OrgNodeKind, TrueNode};
use crate::types::misc::{ID, SkgConfig};
use crate::dbs::typedb::search::pid_and_source_from_id;
use ego_tree::Tree;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

struct MergeValidationData<'a> {
  acquirer_orgnodes     : Vec<&'a OrgNode>,
  acquirer_to_acquirees : HashMap<ID, HashSet<ID>>,
  acquiree_to_acquirers : HashMap<ID, HashSet<ID>>,
  to_delete_ids         : HashSet<ID>, }

/// Validates merge requests in an orgnode forest.
/// Returns a vector of validation error messages,
/// which is empty if all are valid.
pub async fn validate_merge_requests(
  forest: &Tree<OrgNode>,
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<String>, Box<dyn Error>> {
  let mut errors: Vec<String> = Vec::new();
  let merge_validation_data : MergeValidationData =
    collect_merge_validation_data ( forest );
  for node in merge_validation_data.acquirer_orgnodes {
    let t : &TrueNode = match &node.kind {
      OrgNodeKind::True(t) => t,
      OrgNodeKind::Scaff(s) => {
        errors.push(format!(
          "Acquirer node cannot be a Scaffold: {:?}", s));
        continue; }};
    let acquirer_id : &ID = match &t.id_opt {
      Some(id) => id,
      None => { errors.push(format!(
                  "Acquirer node '{}' must have an ID", t.title));
                continue; }};
    if let Some(EditRequest::Merge(acquiree_id)) = &t.edit_request
    { let pair_errors : Vec<String> = validate_merge_pair(
        config, driver, acquirer_id, acquiree_id,
        &merge_validation_data.to_delete_ids).await?;
      errors.extend( pair_errors ); }}
  errors.extend( {
    let monogamy_errors : Vec<String> =
      validate_monogamy_for_all_merges(
        &merge_validation_data.acquirer_to_acquirees,
        &merge_validation_data.acquiree_to_acquirers );
    monogamy_errors } );
  Ok(errors) }

/// To understand what this function does,
/// it's easiest to read the definition of its return type.
fn collect_merge_validation_data<'a>(
  forest: &'a Tree<OrgNode>,
) -> MergeValidationData<'a> {
  let mut acquirer_orgnodes : Vec<&OrgNode> = Vec::new();
  let mut acquirer_to_acquirees : HashMap<ID, HashSet<ID>> = HashMap::new();
  let mut acquiree_to_acquirers : HashMap<ID, HashSet<ID>> = HashMap::new();
  let mut to_delete_ids : HashSet<ID> = HashSet::new();
  for edge in forest.root().traverse() {
    if let ego_tree::iter::Edge::Open(node_ref) = edge {
      let orgnode : &OrgNode = node_ref.value();
      if let OrgNodeKind::True(t) = &orgnode.kind {
        if let Some(id) = &t.id_opt {
          if matches!(&t.edit_request, Some(EditRequest::Delete)) {
            to_delete_ids.insert(id.clone()); } // mutate!
          if let Some(EditRequest::Merge(acquiree_id))
          = &t.edit_request
          { acquirer_orgnodes.push(orgnode); // mutate!
            acquirer_to_acquirees // mutate!
              .entry(id.clone())
              .or_insert_with(HashSet::new)
              .insert(acquiree_id.clone());
            acquiree_to_acquirers // mutate!
              .entry(acquiree_id.clone())
              .or_insert_with(HashSet::new)
              .insert(id.clone()); }} }} }
  MergeValidationData { acquirer_orgnodes,
                        acquirer_to_acquirees,
                        acquiree_to_acquirers,
                        to_delete_ids, }}

/// Validates a single merge pair (acquirer + acquiree).
/// Returns a vector of validation errors for this pair.
/// The error messages explain what each passage does.
async fn validate_merge_pair(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  acquirer_id: &ID,
  acquiree_id: &ID,
  to_delete_ids: &HashSet<ID>,
) -> Result<Vec<String>, Box<dyn Error>> {
  let mut errors: Vec<String> = Vec::new();
  let acquirer_pid : ID = (
    match pid_and_source_from_id (
      &config.db_name, driver, acquirer_id).await?
    { Some((pid, _source)) => pid,
      None      => {
        errors.push(format!(
          "Acquirer ID '{}' not found in database",
          acquirer_id.as_str() ));
        return Ok(errors); }} );
  let acquiree_pid : ID = (
    match pid_and_source_from_id(
      &config.db_name, driver, acquiree_id).await?
    { Some((pid, _source)) => pid,
      None => {
        errors.push(format!(
          "Acquiree ID '{}' (requested by '{}') not found in database",
          acquiree_id.as_str(),
          acquirer_id.as_str() ));
        return Ok(errors); }} );
  if acquirer_pid == acquiree_pid {
    errors.push(format!(
      "Self-merge detected: acquirer '{}' and acquiree '{}' resolve to the same node (PID: '{}')",
      acquirer_id.as_str(),
      acquiree_id.as_str(),
      acquirer_pid.as_str() )); }
  if to_delete_ids.contains(acquirer_id) {
    errors.push(format!(
      "Acquirer '{}' cannot be marked for deletion",
      acquirer_id.as_str() )); }
  if to_delete_ids.contains(acquiree_id) {
    errors.push(format!(
      "Acquiree '{}' (requested by '{}') cannot be marked for deletion",
      acquiree_id.as_str(),
      acquirer_id.as_str() )); }
  Ok (errors) }

/// Validates monogamy rules for all merges.
/// Returns a vector of validation errors.
/// The error text explains what each passage does.
fn validate_monogamy_for_all_merges(
  acquirer_to_acquirees: &HashMap<ID, HashSet<ID>>,
  acquiree_to_acquirers: &HashMap<ID, HashSet<ID>>,
) -> Vec<String> {
  let mut errors: Vec<String> = Vec::new();
  for (acquirer_id, acquirees) in acquirer_to_acquirees {
    if acquirees.len() > 1 {
      errors.push(format!(
        "Monogamy violation: acquirer '{}' would merge with multiple nodes: {:?}",
        acquirer_id.as_str(),
        acquirees.iter().map(
          |id| id.as_str()
        ).collect::<Vec<_>>() )); }}
  for (acquiree_id, acquirers) in acquiree_to_acquirers {
    if acquirers.len() > 1 {
      errors.push(format!(
        "Monogamy violation: acquiree '{}' would merge with multiple nodes: {:?}",
        acquiree_id.as_str(),
        acquirers.iter().map(
          |id| id.as_str()
        ).collect::<Vec<_>>() )); }}
  let overlap: Vec<&ID> = {
    let acquirer_ids: HashSet<&ID> =
      acquirer_to_acquirees.keys().collect();
    let acquiree_ids: HashSet<&ID> =
      acquiree_to_acquirers.keys().collect();
    acquirer_ids . intersection(&acquiree_ids)
      . copied() . collect() };
  if !overlap.is_empty() {
    errors.push(format!(
      "Monogamy violation: nodes cannot be both acquirer and acquiree: {:?}",
      overlap.iter().map(
        |id| id.as_str()
      ).collect::<Vec<_>>() )); }
  errors }
