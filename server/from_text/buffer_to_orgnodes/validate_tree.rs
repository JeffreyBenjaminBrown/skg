pub mod contradictory_instructions;

use crate::types::misc::{ID, SkgConfig, SourceNickname};
use crate::types::orgnode::{OrgNode, Interp, ViewRequest};
use crate::types::errors::BufferValidationError;
use crate::types::tree::accessors::unique_orgnode_child_with_interp_from_ref;
use crate::merge::validate_merge::validate_merge_requests;
use contradictory_instructions::find_inconsistent_instructions;
use ego_tree::Tree;
use ego_tree::iter::Edge;
use std::collections::HashSet;
use typedb_driver::TypeDBDriver;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// ASSUMES that in the "forest" (tree with ForestRoot):
/// - IDs have been replaced with PIDs. Otherwise two org nodes
///   might refer to the same skg node, yet appear not to.
/// - Where missing, source has been inherited from an ancestor.
pub async fn find_buffer_errors_for_saving (
  forest: &Tree<OrgNode>,
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<BufferValidationError>,
            Box<dyn std::error::Error>> {
  let mut errors: Vec<BufferValidationError> = Vec::new();
  { // inconsistent instructions (deletion, defining containers, and sources)
    let (ambiguous_deletion_ids, problematic_defining_ids, inconsistent_source_ids) =
      find_inconsistent_instructions(forest);
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
    for error_msg in {
      let merge_errors: Vec<String> =
        validate_merge_requests(forest, config, driver).await?;
      merge_errors }
    { errors.push(
        BufferValidationError::Other(error_msg)); }}
  validate_definitive_view_requests(
    forest, &mut errors);
  validate_roots_have_sources(
    forest, &mut errors);
  for tree_root in forest.root().children() {
    validate_node_and_children (
      tree_root,
      None, // because a tree root has no meaningful parent
      config,
      &mut errors); }
  Ok(errors) }

/// Validate that all roots of the "forest" (children of ForestRoot)
/// have sources. After source inheritance,
/// only tree roots can be without sources.
fn validate_roots_have_sources(
  forest: &Tree<OrgNode>,
  errors: &mut Vec<BufferValidationError>
) {
  for tree_root in forest.root().children() {
    let root: &OrgNode = tree_root.value();
    if root.metadata.source.is_none() {
      errors.push(
        BufferValidationError::RootWithoutSource(
          root.clone() )); }} }

/// Recursively validate a node and its children for saving errors
fn validate_node_and_children (
  node_ref: ego_tree::NodeRef<OrgNode>,
  parent_interp : Option<Interp>, // interp of parent of node_ref
  config: &SkgConfig,
  errors: &mut Vec<BufferValidationError>
) {

  let orgnode: &OrgNode = node_ref.value();
  match orgnode.metadata.code.interp {
    Interp::AliasCol => {
      if orgnode.body.is_some() {
        errors.push(
          BufferValidationError::Body_of_AliasCol(
            orgnode.clone() )); }},

    Interp::Alias => {
      if orgnode.body.is_some() {
        errors.push(
          BufferValidationError::Body_of_Alias(
            orgnode.clone() )); }
      if let Some(ref parent_rel) = parent_interp {
        if *parent_rel != Interp::AliasCol {
          errors.push(
            BufferValidationError::Alias_with_no_AliasCol_Parent(
              orgnode.clone() )); }
      } else {
        // Root level Alias is also invalid
        errors.push(
          BufferValidationError::Alias_with_no_AliasCol_Parent(
            orgnode.clone() )); }},
    _ => {} }

  if let Some(parent_rel) = parent_interp {
    match parent_rel {
      Interp::AliasCol => {
        // Children of AliasCol should not have IDs
        if orgnode.metadata.id.is_some() {
          errors.push(
            BufferValidationError::Child_of_AliasCol_with_ID(
              orgnode.clone() )); }},
      Interp::Alias => {
        // Children of Alias should not exist at all
        errors.push(
          BufferValidationError::Child_of_Alias(
            orgnode.clone() )); },
        _ => {} }}

  if unique_orgnode_child_with_interp_from_ref (
    &node_ref, Interp::AliasCol ) . is_err ()
  { errors.push (
      BufferValidationError::Multiple_AliasCols_in_Children (
        orgnode.clone() )); }

  { // If a node is definitive, it should have
    // no two treatment=Content children with the same ID.
    if ! orgnode . metadata . code.indefinitive {
      let mut seen_content_ids : HashSet < ID > =
        HashSet::new ();
      for child in node_ref . children () {
        let child_orgnode : &OrgNode =
          child . value ();
        if child_orgnode . metadata . code.interp == Interp::Content {
          if let Some ( ref child_skgid )
            = child_orgnode . metadata . id
          { if ! seen_content_ids . insert ( child_skgid . clone () ) {
            errors . push (
              BufferValidationError::DuplicatedContent (
                child_skgid . clone () )); }} }} }}

  { // Validate that the source exists in config
    // todo ? For speed, we could restrict this check to those nodes that have a source specified in the original buffer-text, excluding nodes for which source is inherited from an ancestor.
    if let Some(ref source_str) = orgnode.metadata.source {
      if ! config.sources.contains_key(source_str) {
        let source_nickname: SourceNickname =
          SourceNickname::from ( source_str.as_str() );
        let skgid: ID =
          orgnode.metadata.id.clone()
          .unwrap_or_else(|| ID::from("<no ID>"));
        errors.push(
          BufferValidationError::SourceNotInConfig(
            skgid,
            source_nickname )); }} }

  if orgnode.metadata.code.indefinitive { // indef + edit = error
    if orgnode.metadata.code.editRequest.is_some() {
      errors.push(
        BufferValidationError::IndefinitiveWithEditRequest(
          orgnode.clone() )); }}

  for child in node_ref.children() { // recurse
    validate_node_and_children(
      child, Some( { let cloned_rel: Interp =
                       orgnode.metadata.code.interp.clone();
                     cloned_rel } ),
      config, errors); }}

/// Validate definitive view requests:
/// - Must be on an indefinitive orgnode
/// - Must be on a childless orgnode
/// - At most one request of this kind per ID
fn validate_definitive_view_requests (
  forest : &Tree<OrgNode>, // "forest" = tree with ForestRoot
  errors : &mut Vec<BufferValidationError>,
) {
  let mut ids_with_requests : HashSet<ID> =
    HashSet::new();
  for edge in forest.root().traverse() {
    if let Edge::Open(node_ref) = edge {
        let orgnode : &OrgNode =
          node_ref.value();
        if orgnode.metadata.code.viewRequests.contains(
          &ViewRequest::Definitive ) {
          if let Some(ref id) = orgnode.metadata.id {
            { // Error: must be indefinitive
              if ! orgnode.metadata.code.indefinitive {
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
                    id.clone() )); }} }} }} }
