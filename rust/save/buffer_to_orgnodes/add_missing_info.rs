/* PURPOSE:
Add missing information to nodes in the forest.
Namely, make it so when treatment should be Alias,
and add missing IDs where treatment is Content.
*/

use crate::types::{OrgNode, RelToParent, ID};
use crate::typedb::util::{pids_from_ids, collect_ids_for_pid_lookup, assign_pids_from_map};
use ego_tree::Tree;
use std::boxed::Box;
use std::collections::HashMap;
use std::error::Error;
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
  for tree in trees . iter_mut () {
    add_missing_info_dfs (
      tree . root_mut (),
      None ); }
  let mut ids_to_lookup : Vec < ID > = Vec::new ();
  for tree in trees . iter () {
    collect_ids_for_pid_lookup (
      tree . root (),
      & mut ids_to_lookup ); }
  let pid_map : HashMap < ID, Option < ID > > =
    pids_from_ids (
      db_name, driver, & ids_to_lookup ). await ?;
  for tree in trees . iter_mut () {
    assign_pids_from_map (
      tree . root_mut (),
      & pid_map ); }
  Ok (( )) }

fn add_missing_info_dfs (
  mut node_ref: ego_tree::NodeMut < OrgNode >,
  parent_treatment: Option < RelToParent >
) {
  // Process current node
  assign_alias_relation_if_needed (
    node_ref . value (), parent_treatment );
  assign_id_if_needed (
    node_ref . value () );

  let node_rel: RelToParent =
    ( // Used to process each child.
      node_ref . value () . metadata . code.relToParent . clone () );
  { // Process children, DFS.
    // First collect child NodeIDs,
    // by reading from the immutable node reference.
    // PITFALL: don't confuse egoTree NodeIDs, which are all distinct,
    // from Skg IDs (which might not be).
    let node_id: ego_tree::NodeId = node_ref . id ();
    let child_ids: Vec < ego_tree::NodeId > = {
      let tree = node_ref . tree ();
      tree . get ( node_id ) . unwrap ()
        . children () . map ( | child |
                             child . id () )
        . collect () };
    for child_id in child_ids {
      if let Some ( child_mut ) =
        node_ref . tree () . get_mut ( child_id )
      { add_missing_info_dfs (
        child_mut,
        Some ( node_rel . clone () ) ); } } } }

/// Assign treatment=Alias
/// to nodes whose parent has treatment=AliasCol
fn assign_alias_relation_if_needed(
  node: &mut OrgNode,
  parent_treatment: Option<RelToParent>
) {
  if let Some(parent_rel) = parent_treatment {
    if parent_rel == RelToParent::AliasCol {
      node.metadata.code.relToParent = RelToParent::Alias; }} }

/// Assign a UUID v4 to Content nodes that don't have an ID
fn assign_id_if_needed(
  node: &mut OrgNode
) {
  if ( node.metadata.code.relToParent == RelToParent::Content
       && node.metadata.id . is_none() ) {
    let new_id: String = Uuid::new_v4().to_string();
    node.metadata.id = Some(ID(new_id)); }}
