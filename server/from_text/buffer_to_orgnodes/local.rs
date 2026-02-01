/// Local validation functions for OrgNode trees.
/// These check structural properties of individual nodes
/// without requiring global context.

use crate::types::unchecked_orgnode::{UncheckedOrgNode, UncheckedOrgNodeKind, UncheckedTrueNode};
use crate::types::orgnode::Scaffold;
use crate::types::misc::{ID, SkgConfig};
use crate::types::tree::orgnode_skgnode::{
  generation_includes_only,
  generation_exists_and_includes,
  generation_does_not_exist,
  siblings_cannot_include,
  id_from_self_or_nearest_ancestor,
};
use ego_tree::{Tree, NodeId};
use std::collections::HashSet;

/// Error from local structure validation.
/// Contains the error message and the ID of the nearest TrueNode ancestor.
#[derive(Debug, Clone, PartialEq)]
pub struct LocalStructureError {
  pub message : String,
  pub id      : ID,
}

/// Validate the local structure of a node.
/// Returns Ok(()) if valid, or Err with the error details.
pub fn validate_local_structure (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result<(), LocalStructureError> {
  let Some(node_ref) = tree.get(node_id)
    else { return Err(LocalStructureError {
      message: "node not found".to_string(),
      id: ID::from("<unknown>"),
    }); };

  let errors : Vec<String> =
    match &node_ref.value().kind {
    UncheckedOrgNodeKind::True(t) =>
      validate_truenode(tree, node_id, t, config),
    UncheckedOrgNodeKind::Scaff(s) => match s {
      Scaffold::BufferRoot =>
        validate_buffer_root(tree, node_id),
      Scaffold::Alias { .. } =>
        validate_alias(tree, node_id),
      Scaffold::AliasCol =>
        validate_aliascol(tree, node_id),
      Scaffold::HiddenInSubscribeeCol =>
        validate_hidden_in_subscribee_col(tree, node_id),
      Scaffold::HiddenOutsideOfSubscribeeCol =>
        validate_hidden_outside_of_subscribee_col(tree, node_id),
      Scaffold::SubscribeeCol =>
        validate_subscribeecol(tree, node_id),
      Scaffold::TextChanged =>
        validate_text_changed(tree, node_id),
      Scaffold::IDCol =>
        validate_idcol(tree, node_id),
      Scaffold::ID { .. } =>
        validate_idscaffold(tree, node_id),
    }};

  if errors.is_empty() {
    Ok (( ))
  } else {
    let id : ID =
      id_from_self_or_nearest_ancestor(tree, node_id)
      . unwrap_or_else(|_| ID::from("<no ancestor ID>"));
    Err(LocalStructureError {
      message: errors.join("; "),
      id,
    } ) }}

fn validate_buffer_root (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(
    tree, node_id, -1, false)
  { errors.push("BufferRoot must have no parent.".to_string() ); }
  if !generation_includes_only(
    tree, node_id, 1, false,
    |node| matches!(&node.kind,
                    UncheckedOrgNodeKind::True(_) ))
 { errors.push("BufferRoot's children must be TrueNodes."
                . to_string() ); }
  errors }

fn validate_alias (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors.push("Alias must have no (non-ignored) children.".to_string()); }
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::AliasCol))) {
    errors.push("Alias must have an AliasCol parent.".to_string()); }
  errors }

fn validate_aliascol (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::Alias { .. }))) {
    errors.push("AliasCol's (non-ignored) children must include only Aliases."
                . to_string()); }
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::True(_))) {
    errors.push("AliasCol must have a TrueNode parent.".to_string()); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::AliasCol))) {
    errors.push("AliasCol must be unique among its siblings.".to_string()); }
  errors }

fn validate_hidden_in_subscribee_col (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::True(_))) {
    errors.push("HiddenInSubscribeeCol must have a TrueNode parent (the subscribee)".to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::True(_))) {
    errors.push("HiddenInSubscribeeCol's (non-ignored) children must include only TrueNodes (they are what's hidden).".to_string()); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::HiddenInSubscribeeCol))) {
    errors.push("HiddenInSubscribeeCol must be unique among its siblings.".to_string()); }
  errors }

fn validate_hidden_outside_of_subscribee_col (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::SubscribeeCol))) {
    errors.push("HiddenOutsideOfSubscribeeCol must have a SubscribeeCol parent.".to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::True(_))) {
    errors.push("HiddenOutsideOfSubscribeeCol's (non-ignored) children must include only TrueNodes.".to_string()); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::HiddenOutsideOfSubscribeeCol))) {
    errors.push("HiddenOutsideOfSubscribeeCol must be unique among its siblings.".to_string()); }
  errors }

fn validate_subscribeecol (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::True(_))) {
    errors.push("SubscribeeCol must have a TrueNode parent.".to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node.kind,
         UncheckedOrgNodeKind::True(_) |
         UncheckedOrgNodeKind::Scaff(Scaffold::HiddenOutsideOfSubscribeeCol))) {
    errors.push("SubscribeeCol's children must include only TrueNodes or HiddenOutsideOfSubscribeeCol.".to_string()); }
  if !nonignored_children_have_distinct_ids(tree, node_id) {
    errors.push("SubscribeeCol must not have duplicate TrueNode children.".to_string()); }
  errors }

fn validate_text_changed (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::True(_))) {
    errors.push("TextChanged must have a TrueNode parent.".to_string()); }
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors.push("TextChanged must have no (non-ignroed) children.".to_string()); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::TextChanged))) {
    errors.push("TextChanged must be unique among its siblings.".to_string()); }
  errors }

fn validate_idcol (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::True(_))) {
    errors.push("IDCol must have a TrueNode parent.".to_string()); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::ID { .. } )) ) {
    errors.push("IDCol's (non-ignored) children can only be ID scaffolds."
                . to_string() ); }
  if !siblings_cannot_include(tree, node_id, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::IDCol))) {
    errors.push("IDCol must be unique among its siblings.".to_string()); }
  errors }

fn validate_idscaffold (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !generation_does_not_exist(tree, node_id, 1, true) {
    errors.push("ID scaffold must have no (non-ignored) children.".to_string()); }
  if !generation_exists_and_includes(tree, node_id, -1, false, |node|
       matches!(&node.kind, UncheckedOrgNodeKind::Scaff(Scaffold::IDCol))) {
    errors.push("ID scaffold must have an IDCol parent.".to_string()); }
  errors }

fn validate_truenode (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
  t       : &UncheckedTrueNode,
  config  : &SkgConfig,
) -> Vec<String> {
  let mut errors : Vec<String> = Vec::new();
  if !has_id(t) {
    errors.push("TrueNode must have an ID.".to_string()); }
  if !has_valid_source(t, config) {
    errors.push("TrueNode must have a source that exists in the config."
                . to_string() ); }
  if !generation_includes_only(tree, node_id, 1, true, |node|
       matches!(&node.kind,
         UncheckedOrgNodeKind::True(_)                        |
         UncheckedOrgNodeKind::Scaff(Scaffold::AliasCol)      |
         UncheckedOrgNodeKind::Scaff(Scaffold::IDCol)         |
         UncheckedOrgNodeKind::Scaff(Scaffold::SubscribeeCol) |
         UncheckedOrgNodeKind::Scaff(Scaffold::TextChanged))) {
    errors.push("TrueNode's children must include only TrueNode, AliasCol, IDCol, SubscribeeCol, or TextChanged".to_string()); }
  if !nonignored_children_have_distinct_ids(tree, node_id) {
    errors.push("TrueNode's non-ignored TrueNode children must be unique (no two sharing the same ID).".to_string()); }
  if t.indefinitive && t.edit_request.is_some() {
    errors.push("Indefinitive node must not have an edit request.".to_string()); }
  errors }

/// Check if an UncheckedTrueNode has an ID.
pub fn has_id ( t : &UncheckedTrueNode ) -> bool {
  t.id_opt.is_some() }

/// Check if an UncheckedTrueNode has a source and it exists in the config.
pub fn has_valid_source (
  t      : &UncheckedTrueNode,
  config : &SkgConfig,
) -> bool {
  t.source_opt.as_ref()
    .is_some_and( |s| config.sources.contains_key(s) ) }

/// Check that all non-ignored TrueNode children have distinct IDs.
/// "Non-ignored" means parent_ignores == false.
/// Returns true if all such children have distinct IDs,
/// or if there are no such children.
pub fn nonignored_children_have_distinct_ids (
  tree    : &Tree<UncheckedOrgNode>,
  node_id : NodeId,
) -> bool {
  let Some(node_ref) = tree.get(node_id)
    else { return true; };
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref.children() {
    if let UncheckedOrgNodeKind::True(t) = &child.value().kind {
      if !t.parent_ignores {
        if let Some(id) = &t.id_opt {
          if !seen.insert(id.clone()) {
            return false; }} }} }
  true }
