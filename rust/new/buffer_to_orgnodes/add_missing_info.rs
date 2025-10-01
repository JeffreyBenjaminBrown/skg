/* PURPOSE:
Add missing information to nodes in the forest.
Namely, make it so when relToOrgParent should be Alias,
and add missing IDs where relToOrgParent is Content.
*/

use crate::types::{OrgNode2, RelToOrgParent2, ID};
use ego_tree::Tree;
use uuid::Uuid;

/// Main entry point.
pub fn add_missing_info_to_trees(
  trees: &mut [Tree<OrgNode2>]
) {
  for tree in trees {
    add_missing_info_dfs ( tree.root_mut(),
                           None); }}

fn add_missing_info_dfs(
  mut node_ref: ego_tree::NodeMut<OrgNode2>,
  parent_relToOrgParent: Option<RelToOrgParent2>
) {
  { // Process current node
    assign_alias_relation_if_needed(
      node_ref.value(), parent_relToOrgParent);
    assign_id_if_needed(
      node_ref.value() ); }
  let node_rel: RelToOrgParent2 =
    ( // Used to process each of its children.
      node_ref.value() . metadata.relToOrgParent . clone() );
  { // Process children, DFS.
    // First collect child NodeIDs,
    // by reading from the immutable node reference.
    // PITFALL: don't confused egoTree NodeIDs, which are all distinct,
    // from Skg IDs (which might not be).
    let node_id: ego_tree::NodeId = node_ref.id();
    let child_ids: Vec<ego_tree::NodeId> = {
      let tree = node_ref.tree();
      tree . get (node_id) . unwrap()
        . children() . map( |child|
                             child.id() )
        . collect() };
    for child_id in child_ids {
      if let Some(child_mut) =
        node_ref . tree() . get_mut(child_id)
      { add_missing_info_dfs(
        child_mut,
        Some (node_rel.clone() )); }} }}

/// Assign relToOrgParent=Alias
/// to nodes whose parent has relToOrgParent=AliasCol
fn assign_alias_relation_if_needed(
  node: &mut OrgNode2,
  parent_relToOrgParent: Option<RelToOrgParent2>
) {
  if let Some(parent_rel) = parent_relToOrgParent {
    if parent_rel == RelToOrgParent2::AliasCol {
      node.metadata.relToOrgParent = RelToOrgParent2::Alias; }} }

/// Assign a UUID v4 to Content nodes that don't have an ID
fn assign_id_if_needed(
  node: &mut OrgNode2
) {
  if ( node.metadata.relToOrgParent == RelToOrgParent2::Content
       && node.metadata.id . is_none() ) {
    let new_id: String = Uuid::new_v4().to_string();
    node.metadata.id = Some(ID(new_id)); }}

