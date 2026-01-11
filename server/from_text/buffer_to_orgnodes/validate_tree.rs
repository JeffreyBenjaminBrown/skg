pub mod contradictory_instructions;

use crate::types::misc::{ID, SkgConfig, SourceNickname};
use crate::types::orgnode::ViewRequest;
use crate::types::orgnode::{OrgNode, OrgNodeKind, TrueNode, EffectOnParent, Scaffold};
use crate::types::errors::BufferValidationError;
use crate::merge::validate_merge::validate_merge_requests;
use contradictory_instructions::find_inconsistent_instructions;
use ego_tree::Tree;
use ego_tree::iter::Edge;
use std::collections::HashSet;
use typedb_driver::TypeDBDriver;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// SHARES RESPONSIBILITY for error detection
/// with 'org_to_uninterpreted_nodes',
/// which runs earlier and detects a few errors that this one can't,
/// because this one acts on a tree of OrgNodes rather than raw text.
/// (Namely, Alias and AliasCol should not have body text.)
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
    let (ambiguous_deletion_ids,
         problematic_defining_ids,
         inconsistent_source_ids) =
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
      tree_root, config, &mut errors); }
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
    let has_source : bool = match &root.kind {
      OrgNodeKind::True(t) => t.source_opt.is_some(),
      OrgNodeKind::Scaff(_) => false };
    if !has_source {
      errors.push(
        BufferValidationError::RootWithoutSource(
          root.clone() )); }} }

fn validate_node_and_children (
  node_ref: ego_tree::NodeRef<OrgNode>,
  config: &SkgConfig,
  errors: &mut Vec<BufferValidationError>
) {
  let orgnode: &OrgNode = node_ref.value();
  let parent_kind: Option<&OrgNodeKind> =
    node_ref.parent().map(
      |p| &p.value().kind);
  if matches!(parent_kind,
              Some(OrgNodeKind::Scaff(Scaffold::Alias(_)) )) {
    errors.push(
      BufferValidationError::Child_of_Alias(
        orgnode.clone() )); }

  match &orgnode.kind {
    OrgNodeKind::Scaff ( s ) =>
      validate_scaffold ( s, orgnode, parent_kind, errors ),
    OrgNodeKind::True ( t ) =>
      validate_truenode ( t, orgnode, node_ref, parent_kind, config, errors ),
  };

  for child in node_ref.children()
  { validate_node_and_children ( // recurse
      child, config, errors ); }}

fn validate_scaffold (
  scaffold    : &Scaffold,
  orgnode     : &OrgNode,
  parent_kind : Option<&OrgNodeKind>,
  errors      : &mut Vec<BufferValidationError>
) {
  // Note: Body_of_AliasCol and Body_of_Alias are detected during parsing
  // (in from_parsed), not here, because Scaffold nodes don't store body data.
  match scaffold {
    Scaffold::Alias ( _ ) => {
      if ! matches!(parent_kind,
                    Some(OrgNodeKind::Scaff(Scaffold::AliasCol))) {
        errors.push(
          BufferValidationError::Alias_with_no_AliasCol_Parent(
            orgnode.clone() )); }}
    _ => {} }}

fn validate_truenode (
  t           : &TrueNode,
  orgnode     : &OrgNode,
  node_ref    : ego_tree::NodeRef<OrgNode>,
  parent_kind : Option<&OrgNodeKind>,
  config      : &SkgConfig,
  errors      : &mut Vec<BufferValidationError>
) {
  if matches!(parent_kind,
              Some(OrgNodeKind::Scaff(Scaffold::AliasCol)))
     && t.id_opt.is_some()
  { errors.push(
      BufferValidationError::Child_of_AliasCol_with_ID(
        orgnode.clone() )); }
  { let aliascol_count : usize =
      node_ref.children() .filter(
        |c| matches!( &c.value().kind,
                      OrgNodeKind::Scaff(Scaffold::AliasCol)) )
      .count();
    if aliascol_count > 1
    { errors.push(
        BufferValidationError::Multiple_AliasCols_in_Children(
          orgnode.clone() )); }}
  if ! t.indefinitive { // Check for duplicate content children
    let mut seen_content_ids : HashSet < ID > =
      HashSet::new ();
    for child in node_ref.children() {
      let child_orgnode : &OrgNode = child.value();
      if let OrgNodeKind::True ( ct ) = &child_orgnode.kind {
        if ct.effect_on_parent == EffectOnParent::Content {
          if let Some ( child_skgid ) = &ct.id_opt {
            if ! seen_content_ids.insert( child_skgid.clone() ) {
              errors.push(
                BufferValidationError::DuplicatedContent(
                  child_skgid.clone() )); }} }} }}
  if let Some ( source_str ) = &t.source_opt {
    if ! config.sources.contains_key( source_str ) {
      let source_nickname : SourceNickname =
        SourceNickname::from( source_str.as_str() );
      let skgid : ID = t.id_opt.clone()
        .unwrap_or_else(|| ID::from("<no ID>"));
      errors.push(
        BufferValidationError::SourceNotInConfig(
          skgid, source_nickname )); }}
  if t.indefinitive && t.edit_request.is_some() {
    errors.push(
      BufferValidationError::IndefinitiveWithEditRequest(
        orgnode.clone() )); }}

/// For each node in the forest, if it has a definitive view request,
/// verify that:
/// - The node is indefinitive and childless.
/// - No other node with the same ID has a definitive view request,
///   because there can only be one definitive view.
fn validate_definitive_view_requests (
  forest : &Tree<OrgNode>, // "forest" = tree with ForestRoot
  errors : &mut Vec<BufferValidationError>,
) {
  let mut ids_with_requests : HashSet<ID> =
    HashSet::new();
  for edge in forest.root().traverse()
  { if let Edge::Open(node_ref) = edge
    { let orgnode : &OrgNode =
        node_ref.value();
      if let OrgNodeKind::True(t) = &orgnode.kind
      { if t.view_requests.contains( &ViewRequest::Definitive )
        { if let Some(id) = &t.id_opt {
          { // Error: must be indefinitive
            if ! t.indefinitive
            { errors.push( BufferValidationError::DefinitiveRequestOnDefinitiveNode( id.clone() )); }}
          { // Error: must be childless
            if node_ref.children().next().is_some()
            { errors.push( BufferValidationError::DefinitiveRequestOnNodeWithChildren( id.clone() )); }}
          { // Error: at most one request per ID
            if ! ids_with_requests.insert(id.clone())
            { errors.push( BufferValidationError::MultipleDefinitiveRequestsForSameId( id.clone() )); }} }}} }} }
