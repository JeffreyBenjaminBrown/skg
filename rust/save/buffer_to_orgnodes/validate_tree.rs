pub mod contradictory_instructions;

use crate::types::{ID, OrgNode, RelToParent, Buffer_Cannot_Be_Saved, SkgConfig};
use crate::merge::validate_merge_requests;
use ego_tree::Tree;
use std::collections::HashSet;
use typedb_driver::TypeDBDriver;

// Re-export the main function
pub use contradictory_instructions::find_inconsistent_instructions;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// ASSUMES: IDs have been replaced with PIDs.
/// Otherwise two org nodes might refer to the same skg node,
/// yet appear not to.
pub async fn find_buffer_errors_for_saving (
  trees: &[Tree<OrgNode>],
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<Buffer_Cannot_Be_Saved>, Box<dyn std::error::Error>> {
  let mut errors: Vec<Buffer_Cannot_Be_Saved> = Vec::new();
  { // inconsistent instructions (deletion and defining containers)
    let (ambiguous_deletion_ids, problematic_defining_ids) =
      find_inconsistent_instructions(trees);
    { // transfer the relevant IDs, in the appropriate constructors.
      for id in ambiguous_deletion_ids {
        errors.push (
          Buffer_Cannot_Be_Saved::AmbiguousDeletion(id)); }
      for id in problematic_defining_ids {
        errors.push(
          Buffer_Cannot_Be_Saved::Multiple_DefiningContainers(id));
      }} }
  { // merge validation
    let merge_errors: Vec<String> =
      validate_merge_requests(trees, config, driver).await?;
    for error_msg in merge_errors {
      errors.push(
        Buffer_Cannot_Be_Saved::Other(error_msg)); }}
  { // other kinds of error
    for tree in trees {
      validate_node_and_children (
        tree.root(),
        None, // because a root has no parent
        &mut errors); }}
  Ok(errors) }

/// Recursively validate a node and its children for saving errors
fn validate_node_and_children (
  node_ref: ego_tree::NodeRef<OrgNode>,
  parent_treatment : Option<RelToParent>, // that is, the treatment of the parent of what node_ref points to
  errors: &mut Vec<Buffer_Cannot_Be_Saved>
) {

  let node: &OrgNode = node_ref.value();
  match node.metadata.code.relToParent {
    RelToParent::AliasCol => {
      if node.body.is_some() {
        errors.push(
          Buffer_Cannot_Be_Saved::Body_of_AliasCol(
            node.clone() )); }},

    RelToParent::Alias => {
      if node.body.is_some() {
        errors.push(
          Buffer_Cannot_Be_Saved::Body_of_Alias(
            node.clone() )); }
      if let Some(ref parent_rel) = parent_treatment {
        if *parent_rel != RelToParent::AliasCol {
          errors.push(
            Buffer_Cannot_Be_Saved::Alias_with_no_AliasCol_Parent(
              node.clone() )); }
      } else {
        // Root level Alias is also invalid
        errors.push(
          Buffer_Cannot_Be_Saved::Alias_with_no_AliasCol_Parent(
            node.clone() )); }},
    _ => {} }

  if let Some(parent_rel) = parent_treatment {
    match parent_rel {
      RelToParent::AliasCol => {
        // Children of AliasCol should not have IDs
        if node.metadata.id.is_some() {
          errors.push(
            Buffer_Cannot_Be_Saved::Child_of_AliasCol_with_ID(
              node.clone() )); }},
      RelToParent::Alias => {
        // Children of Alias should not exist at all
        errors.push(
          Buffer_Cannot_Be_Saved::Child_of_Alias(
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
        Buffer_Cannot_Be_Saved::Multiple_AliasCols_in_Children (
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
                Buffer_Cannot_Be_Saved::DuplicatedContent (
                  child_id . clone () )); }} }} }}

  for child in node_ref.children() { // recurse
    let cloned_rel: RelToParent =
      node.metadata.code.relToParent.clone();
    validate_node_and_children(
      child, Some(cloned_rel), errors);
  }}
