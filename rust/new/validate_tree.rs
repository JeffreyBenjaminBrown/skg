pub mod contradictory_delete;

use crate::types::{OrgNode2, RelToOrgParent2, ID};
use ego_tree::Tree;

// Re-export the main function
pub use contradictory_delete::find_inconsistent_toDelete_instructions;

/// If the user attempts to save a buffer
/// with any of these properties,, the server should refuse.
#[derive(Debug, Clone, PartialEq)]
#[allow(non_camel_case_types)]
pub enum BufferInvalidForSaving {
  Body_of_AliasCol              (OrgNode2),
  Child_of_AliasCol_with_ID     (OrgNode2),
  Body_of_Alias                 (OrgNode2),
  Child_of_Alias                (OrgNode2),
  Alias_with_no_AliasCol_Parent (OrgNode2),
  MultipleAliasCols_in_Children (OrgNode2),
  AmbiguousDeletion             (ID),
}

/// Look for invalid structure in the org buffer
/// when a user asks to save it.
pub fn find_buffer_errors_for_saving (
  trees: &[Tree<OrgNode2>]
) -> Vec<BufferInvalidForSaving> {
  let mut errors: Vec<BufferInvalidForSaving> = Vec::new();
  { // ambiguous deletion instructions
    let ambiguous_deletion_ids: Vec<ID> =
      find_inconsistent_toDelete_instructions(trees);
    for id in ambiguous_deletion_ids {
      errors.push (
        BufferInvalidForSaving::AmbiguousDeletion(id)); }}
  { // other kinds of error
    for tree in trees {
      validate_node_and_children(
        tree.root(),
        None, // because a root has no parent
        &mut errors); }}
  errors }

/// Recursively validate a node and its children for saving errors
fn validate_node_and_children(
  node_ref: ego_tree::NodeRef<OrgNode2>,
  parent_relToOrgParent : Option<RelToOrgParent2>, // that is, the reltoorgparent2 of the parent of what node_ref points to
  errors: &mut Vec<BufferInvalidForSaving>
) {

  let node: &OrgNode2 = node_ref.value();
  match node.metadata.relToOrgParent {
    RelToOrgParent2::AliasCol => {
      if node.body.is_some() {
        errors.push(
          BufferInvalidForSaving::Body_of_AliasCol(
            node.clone() )); }},
    RelToOrgParent2::Alias => {
      if node.body.is_some() {
        errors.push(
          BufferInvalidForSaving::Body_of_Alias(
            node.clone() )); }
      if let Some(ref parent_rel) = parent_relToOrgParent {
        if *parent_rel != RelToOrgParent2::AliasCol {
          errors.push(
            BufferInvalidForSaving::Alias_with_no_AliasCol_Parent(
              node.clone() )); }
      } else {
        // Root level Alias is also invalid
        errors.push(
          BufferInvalidForSaving::Alias_with_no_AliasCol_Parent(
            node.clone() )); }},
    _ => {} }

  if let Some(parent_rel) = parent_relToOrgParent {
    match parent_rel {
      RelToOrgParent2::AliasCol => {
        // Children of AliasCol should not have IDs
        if node.metadata.id.is_some() {
          errors.push(
            BufferInvalidForSaving::Child_of_AliasCol_with_ID(
              node.clone() )); }},
      RelToOrgParent2::Alias => {
        // Children of Alias should not exist at all
        errors.push(
          BufferInvalidForSaving::Child_of_Alias(
            node.clone() )); },
        _ => {} }}

  { // At most one child should have relToOrgParent=AliasCol
    let aliasCol_children_count: usize =
      node_ref . children()
      . filter ( |child|
                  child.value() . metadata.relToOrgParent
                  == RelToOrgParent2::AliasCol )
      . count();
    if aliasCol_children_count > 1 {
      errors.push (
        BufferInvalidForSaving::MultipleAliasCols_in_Children (
          node.clone() )); }}

  for child in node_ref.children() { // recurse
    let cloned_rel: RelToOrgParent2 =
      node.metadata.relToOrgParent.clone();
    validate_node_and_children(
      child, Some(cloned_rel), errors);
  }}
