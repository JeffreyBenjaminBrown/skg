/* PURPOSE:
Add missing information to nodes in the forest.
Namely, make it so when relToOrgParent should be Alias,
and add missing IDs where relToOrgParent is Content.
*/

use crate::types::{OrgNode, RelToOrgParent, ID};
use crate::typedb::search::util::pid_from_id;
use ego_tree::Tree;
use std::boxed::Box;
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;
use uuid::Uuid;

/* Runs the following on each node:
- assign_alias_relation_if_needed(
- assign_id_if_needed(
- assign_pid_if_possible(
.
PITFALL:
Does not add *all* missing info.
'clobber_none_fields_with_data_from_disk' does some of that, too,
but it operates on SaveInstructions, downstream. */
pub async fn add_missing_info_to_trees(
  trees: &mut [Tree<OrgNode>],
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  for tree in trees {
    add_missing_info_dfs ( tree.root_mut(),
                           None,
                           db_name,
                           driver ).await ?; }
  Ok(()) }

fn add_missing_info_dfs<'a>(
  mut node_ref: ego_tree::NodeMut<'a, OrgNode>,
  parent_relToOrgParent: Option<RelToOrgParent>,
  db_name: &'a str,
  driver: &'a TypeDBDriver
) -> Pin<
    Box<
        dyn Future<
            Output = Result<
                (),
              Box<dyn Error>>>
        + 'a>>

{ Box::pin(async move {
  { // Process current node
    assign_alias_relation_if_needed(
      node_ref.value(), parent_relToOrgParent);
    assign_id_if_needed(
      node_ref.value() );
    assign_pid_if_possible(
      node_ref.value(), db_name, driver ).await ?; }
  let node_rel: RelToOrgParent =
    ( // Used to process each of its children.
      node_ref.value() . metadata.relToOrgParent . clone() );
  { // Process children, DFS.
    // First collect child NodeIDs,
    // by reading from the immutable node reference.
    // PITFALL: don't confuse egoTree NodeIDs, which are all distinct,
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
        Some (node_rel.clone() ),
        db_name,
        driver ).await ?; }} }
  Ok(())
  }) }

/// Assign relToOrgParent=Alias
/// to nodes whose parent has relToOrgParent=AliasCol
fn assign_alias_relation_if_needed(
  node: &mut OrgNode,
  parent_relToOrgParent: Option<RelToOrgParent>
) {
  if let Some(parent_rel) = parent_relToOrgParent {
    if parent_rel == RelToOrgParent::AliasCol {
      node.metadata.relToOrgParent = RelToOrgParent::Alias; }} }

/// Assign a UUID v4 to Content nodes that don't have an ID
fn assign_id_if_needed(
  node: &mut OrgNode
) {
  if ( node.metadata.relToOrgParent == RelToOrgParent::Content
       && node.metadata.id . is_none() ) {
    let new_id: String = Uuid::new_v4().to_string();
    node.metadata.id = Some(ID(new_id)); }}

/// Look up the PID for the node's current ID,
/// and replace it if found.
async fn assign_pid_if_possible (
  node: &mut OrgNode,
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  if let Some(current_id) = &node.metadata.id {
    if let Some(pid) = pid_from_id(
      db_name, driver, current_id ). await ? {
      node . metadata . id = Some(pid); }}
  Ok (( )) }
