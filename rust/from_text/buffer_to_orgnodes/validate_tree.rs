pub mod contradictory_instructions;

use crate::types::{ID, OrgNode, RelToParent, BufferValidationError, SkgConfig, SourceNickname, ViewRequest};
use crate::merge::validate_merge_requests;
use ego_tree::Tree;
use ego_tree::iter::Edge;
use std::collections::HashSet;
use typedb_driver::TypeDBDriver;

// Re-export the main function
pub use contradictory_instructions::find_inconsistent_instructions;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// ASSUMES that in the forest:
/// - IDs have been replaced with PIDs. Otherwise two org nodes
///   might refer to the same skg node, yet appear not to.
/// - Where missing, source has been inherited from an ancestor.
pub async fn find_buffer_errors_for_saving (
  trees: &[Tree<OrgNode>],
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<BufferValidationError>,
            Box<dyn std::error::Error>> {
  let mut errors: Vec<BufferValidationError> = Vec::new();
  { // inconsistent instructions (deletion, defining containers, and sources)
    let (ambiguous_deletion_ids, problematic_defining_ids, inconsistent_source_ids) =
      find_inconsistent_instructions(trees);
    { // transfer the relevant IDs, in the appropriate constructors.
      for id in ambiguous_deletion_ids {
        errors.push (
          BufferValidationError::AmbiguousDeletion(id)); }
      for id in problematic_defining_ids {
        errors.push(
          BufferValidationError::Multiple_Defining_Orgnodes(id)); }
      for (id, sources) in inconsistent_source_ids {
        errors.push(
          BufferValidationError::InconsistentSources(id, sources));
      }} }
  { // merge validation
    let merge_errors: Vec<String> =
      validate_merge_requests(trees, config, driver).await?;
    for error_msg in merge_errors {
      errors.push(
        BufferValidationError::Other(error_msg)); }}
  validate_definitive_view_requests(
    trees, &mut errors);
  validate_roots_have_sources(
    trees, &mut errors);
  for tree in trees { // other kinds of error
    validate_node_and_children (
      tree.root(),
      None, // because a root has no parent
      config,
      &mut errors); }
  Ok(errors) }

/// Validate that all root nodes (top-level in forest) have sources.
/// After Phase 6 source inheritance, only roots can be without sources.
fn validate_roots_have_sources(
  trees: &[Tree<OrgNode>],
  errors: &mut Vec<BufferValidationError>
) {
  for tree in trees {
    let root: &OrgNode = tree.root().value();
    if root.metadata.source.is_none() {
      errors.push(
        BufferValidationError::RootWithoutSource(
          root.clone() )); }} }

/// Recursively validate a node and its children for saving errors
fn validate_node_and_children (
  node_ref: ego_tree::NodeRef<OrgNode>,
  parent_treatment : Option<RelToParent>, // that is, the treatment of the parent of what node_ref points to
  config: &SkgConfig,
  errors: &mut Vec<BufferValidationError>
) {

  let node: &OrgNode = node_ref.value();
  match node.metadata.code.relToParent {
    RelToParent::AliasCol => {
      if node.body.is_some() {
        errors.push(
          BufferValidationError::Body_of_AliasCol(
            node.clone() )); }},

    RelToParent::Alias => {
      if node.body.is_some() {
        errors.push(
          BufferValidationError::Body_of_Alias(
            node.clone() )); }
      if let Some(ref parent_rel) = parent_treatment {
        if *parent_rel != RelToParent::AliasCol {
          errors.push(
            BufferValidationError::Alias_with_no_AliasCol_Parent(
              node.clone() )); }
      } else {
        // Root level Alias is also invalid
        errors.push(
          BufferValidationError::Alias_with_no_AliasCol_Parent(
            node.clone() )); }},
    _ => {} }

  if let Some(parent_rel) = parent_treatment {
    match parent_rel {
      RelToParent::AliasCol => {
        // Children of AliasCol should not have IDs
        if node.metadata.id.is_some() {
          errors.push(
            BufferValidationError::Child_of_AliasCol_with_ID(
              node.clone() )); }},
      RelToParent::Alias => {
        // Children of Alias should not exist at all
        errors.push(
          BufferValidationError::Child_of_Alias(
            node.clone() )); },
        _ => {} }}

  { // At most one child should have treatment=AliasCol
    let aliasCol_children_count: usize =
      node_ref . children()
      . filter ( |child|
                  child.value() . metadata.code.relToParent
                  == RelToParent::AliasCol )
      . count();
    if aliasCol_children_count > 1 {
      errors.push (
        BufferValidationError::Multiple_AliasCols_in_Children (
          node.clone() )); }}

  { // If a node is definitive, it should have
    // no two treatment=Content children with the same ID.
    if ! node . metadata . code.indefinitive {
      let mut seen_content_ids : HashSet < ID > =
        HashSet::new ();
      for child in node_ref . children () {
        let child_node : &OrgNode =
          child . value ();
        if child_node . metadata . code.relToParent == RelToParent::Content {
          if let Some ( ref child_id ) = child_node . metadata . id {
            if ! seen_content_ids . insert ( child_id . clone () ) {
              errors . push (
                BufferValidationError::DuplicatedContent (
                  child_id . clone () )); }} }} }}

  { // Validate that the source exists in config
    // todo ? For speed, we could restrict this check to those nodes that have a source specified in the original buffer-text, excluding nodes for which source is inherited from an ancestor.
    if let Some(ref source_str) = node.metadata.source {
      if ! config.sources.contains_key(source_str) {
        let source_nickname: SourceNickname =
          SourceNickname::from ( source_str.as_str() );
        let node_id: ID =
          node.metadata.id.clone()
          .unwrap_or_else(|| ID::from("<no ID>"));
        errors.push(
          BufferValidationError::SourceNotInConfig(
            node_id,
            source_nickname )); }} }

  if node.metadata.code.indefinitive { // indef + edit = error
    if node.metadata.code.editRequest.is_some() {
      errors.push(
        BufferValidationError::IndefinitiveWithEditRequest(
          node.clone() )); }}

  for child in node_ref.children() { // recurse
    let cloned_rel: RelToParent =
      node.metadata.code.relToParent.clone();
    validate_node_and_children(
      child, Some(cloned_rel), config, errors);
  }}

/// Validate definitive view requests:
/// - Must be on an indefinitive orgnode
/// - Must be on a childless orgnode
/// - At most one request of this kind per ID
fn validate_definitive_view_requests (
  trees  : &[Tree<OrgNode>],
  errors : &mut Vec<BufferValidationError>,
) {
  let mut ids_with_requests : HashSet<ID> =
    HashSet::new();
  for tree in trees {
    for edge in tree.root().traverse() {
      if let Edge::Open(node_ref) = edge {
        let node : &OrgNode =
          node_ref.value();
        if node.metadata.code.viewRequests.contains(
          &ViewRequest::Definitive ) {
          if let Some(ref id) = node.metadata.id {
            { // Error: must be indefinitive
              if ! node.metadata.code.indefinitive {
                errors.push(
                  BufferValidationError::DefinitiveRequestOnDefinitiveNode(
                    id.clone() )); }}
            { // Error: must be childless
              if node_ref.children().next().is_some() {
                errors.push(
                  BufferValidationError::DefinitiveRequestOnNodeWithChildren(
                    id.clone() )); }}
            { // Error: at most one request per ID
              if ! ids_with_requests.insert(id.clone()) {
                errors.push(
                  BufferValidationError::MultipleDefinitiveRequestsForSameId(
                    id.clone() )); }} }} }} }}
