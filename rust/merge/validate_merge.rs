use crate::types::{OrgNode, RelToParent, ID, NodeRequest, SkgConfig};
use crate::typedb::util::pid_from_id;
use ego_tree::Tree;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

struct MergeValidationData<'a> {
  acquirer_nodes: Vec<&'a OrgNode>,
  acquirer_to_acquirees: HashMap<ID, HashSet<ID>>,
  acquiree_to_acquirers: HashMap<ID, HashSet<ID>>,
  to_delete_ids: HashSet<ID>, }

/// Validates merge requests in an orgnode forest.
/// Returns Ok(()) if all merge requests are valid,
/// or Err with a list of validation errors.
pub async fn validate_merge_requests(
  forest: &[Tree<OrgNode>],
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let mut errors: Vec<String> = Vec::new();
  let collections = collect_merge_validation_data(forest);
  for node in collections.acquirer_nodes {
    let acquirer_id = node.metadata.id.as_ref()
      .ok_or("Acquirer node must have an ID")?;
    if node.metadata.code.relToParent == RelToParent::Alias
      || node.metadata.code.relToParent == RelToParent::AliasCol {
        errors.push(
          format!(
            "Acquirer node '{}' cannot be an Alias or AliasCol",
            acquirer_id.as_str() ));
        continue; }
    for request in &node.metadata.code.nodeRequests {
      if let NodeRequest::Merge(acquiree_id) = request {
        let pair_errors = validate_merge_pair(
          config,
          driver,
          acquirer_id,
          acquiree_id,
          &collections.to_delete_ids ). await?;
        errors.extend(pair_errors); } }}
  let monogamy_errors = validate_monogamy(
    &collections.acquirer_to_acquirees,
    &collections.acquiree_to_acquirers );
  errors.extend(monogamy_errors);
  if errors.is_empty() { Ok(()) }
  else { Err(
    errors . join("\n") .into()) }}

fn collect_merge_validation_data<'a>(
  forest: &'a [Tree<OrgNode>],
) -> MergeValidationData<'a> {
  let mut acquirer_nodes: Vec<&OrgNode> =
    Vec::new();
  let mut acquirer_to_acquirees: HashMap<ID, HashSet<ID>> =
    HashMap::new();
  let mut acquiree_to_acquirers: HashMap<ID, HashSet<ID>> =
    HashMap::new();
  let mut to_delete_ids: HashSet<ID> =
    HashSet::new();
  for tree in forest {
    for edge in tree.root().traverse() {
      if let ego_tree::iter::Edge::Open(node_ref) = edge {
        let orgnode: &OrgNode = node_ref.value();
        if orgnode.metadata.code.toDelete {
          if let Some(ref id) = orgnode.metadata.id {
            to_delete_ids.insert( // Mutate!
              id.clone()); }}
        if !orgnode.metadata.code.nodeRequests.is_empty() {
          for request in &orgnode.metadata.code.nodeRequests {
            if let NodeRequest::Merge(acquiree_id) = request {
              if let Some(ref acquirer_id) = orgnode.metadata.id {
                acquirer_nodes.push( // Mutate!
                  orgnode);
                acquirer_to_acquirees // Mutate!
                  .entry(acquirer_id.clone())
                  .or_insert_with(HashSet::new)
                  .insert(acquiree_id.clone());
                acquiree_to_acquirers // Mutate!
                  .entry(acquiree_id.clone())
                  .or_insert_with(HashSet::new)
                  .insert(acquirer_id.clone()); }} }} }} }
  MergeValidationData {
    acquirer_nodes,
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
  let acquirer_pid = match pid_from_id(
    &config.db_name, driver, acquirer_id).await?
  { Some(pid) => pid,
    None => {
      errors.push(format!(
        "Acquirer ID '{}' not found in database",
        acquirer_id.as_str() ));
      return Ok(errors);
    }};
  let acquiree_pid = match pid_from_id(
    &config.db_name, driver, acquiree_id).await?
  { Some(pid) => pid,
    None => {
      errors.push(format!(
        "Acquiree ID '{}' (requested by '{}') not found in database",
        acquiree_id.as_str(),
        acquirer_id.as_str() ));
      return Ok(errors); }};
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
fn validate_monogamy(
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
  let acquirer_ids: HashSet<&ID> =
    acquirer_to_acquirees.keys().collect();
  let acquiree_ids: HashSet<&ID> =
    acquiree_to_acquirers.keys().collect();
  let overlap: Vec<&ID> =
    acquirer_ids.intersection(&acquiree_ids).copied().collect();
  if !overlap.is_empty() {
    errors.push(format!(
      "Monogamy violation: nodes cannot be both acquirer and acquiree: {:?}",
      overlap.iter().map(
        |id| id.as_str()
      ).collect::<Vec<_>>() )); }
  errors }
