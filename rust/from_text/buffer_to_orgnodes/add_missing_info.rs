/// PURPOSE:
/// Add missing information to nodes in the forest. Namely:
/// - when treatment should be Alias, make it so
/// - add missing IDs where treatment is Content

use crate::types::{OrgNode, Interp, ID};
use crate::dbs::typedb::util::{pids_from_ids, collect_ids_in_tree, assign_pids_throughout_tree_from_map};
use ego_tree::{Tree, NodeId, NodeMut};
use std::boxed::Box;
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;
use uuid::Uuid;

/// Runs the following on each node:
/// - assign_alias_relation_if_needed(
/// - assign_new_id_if_needed(
/// - assign_pid_if_possible(
/// .
/// PITFALL:
/// Does not add *all* missing info.
/// 'clobber_none_fields_with_data_from_disk' does some of that, too,
/// but it operates on SaveInstructions, downstream.
///
/// Input forest has ForestRoot at root; tree roots are its children.
pub async fn add_missing_info_to_forest(
  forest: &mut Tree<OrgNode>,
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let tree_root_ids: Vec<NodeId> =
    forest.root().children().map(|c| c.id()).collect();
  for tree_root_id in &tree_root_ids {
    if let Some(tree_root_mut) = forest.get_mut(*tree_root_id) {
      add_missing_info_dfs(
        tree_root_mut,
        None,     // its parent's Interp
        None ); } } // its parent's source

  // collect IDs
  let mut ids_to_lookup: Vec<ID> = Vec::new();
  for tree_root_id in &tree_root_ids {
    if let Some(tree_root_ref) = forest.get(*tree_root_id) {
      collect_ids_in_tree(tree_root_ref,
                          &mut ids_to_lookup); }}

  // assign PIDs
  let pid_map: HashMap<ID, Option<ID>> =
    pids_from_ids(db_name, driver, &ids_to_lookup).await?;
  for tree_root_id in &tree_root_ids {
    if let Some(tree_root_mut) = forest.get_mut(*tree_root_id) {
      assign_pids_throughout_tree_from_map(
        tree_root_mut, &pid_map); } }
  Ok(()) }

/// - assign alias Interp, if missing and appropriate
/// - assign source if knowable
/// - assign new ID,       if missing and appropriate
fn add_missing_info_dfs (
  mut node_ref: NodeMut < OrgNode >,
  parent_reltoparent: Option < Interp >, // Thanks to AliasCol, if A(lias) descends from P(arent), which descends from G(randparent), then to process A, we need to know P's relationship to G.
  parent_source: Option < String >,
) {
  { // Process current node
    assign_alias_relation_if_needed (
      node_ref . value (), parent_reltoparent );
    if ! node_ref . value () . metadata . code . interp . should_be_sourceless () {
      inherit_source_if_needed (
        node_ref . value (), parent_source ); }
    assign_new_id_if_needed (
      node_ref . value () ); }

  { // Recurse : Process children, DFS.
    // First collect child NodeIDs,
    // by reading from the immutable node reference.
    // PITFALL: don't confuse egoTree NodeIDs, which are all distinct,
    // from Skg IDs (which might not be).
    let its_interp: Interp =
      node_ref . value ()
      . metadata . code . interp . clone ();
    let its_source: Option < String > = (
      // Sourceless nodes don't propagate source to children.
      if its_interp . should_be_sourceless () { None }
      else { node_ref . value () . metadata . source . clone () } );
    let treeid: ego_tree::NodeId = node_ref . id ();
    let child_treeids: Vec < ego_tree::NodeId > = {
      let tree = node_ref . tree ();
      tree . get ( treeid ) . unwrap ()
        . children () . map ( | child |
                             child . id () )
        . collect () };
    for child_treeid in child_treeids {
      if let Some ( child_mut )
        = node_ref . tree () . get_mut ( child_treeid ) {
          add_missing_info_dfs (
            child_mut,
            Some ( its_interp . clone () ),
            its_source . clone () ); }} }}

/// Assign treatment=Alias
/// to nodes whose parent has treatment=AliasCol
fn assign_alias_relation_if_needed(
  node: &mut OrgNode,
  parent_treatment: Option<Interp>
) {
  if let Some(parent_rel) = parent_treatment {
    if parent_rel == Interp::AliasCol {
      node.metadata.code.interp = Interp::Alias; }} }

/// Assign a UUID v4 to Content nodes that don't have an ID
fn assign_new_id_if_needed(
  node: &mut OrgNode
) {
  if ( node.metadata.code.interp == Interp::Content
       && node.metadata.id . is_none() ) {
    let new_id: String = Uuid::new_v4().to_string();
    node.metadata.id = Some(ID(new_id)); }}

/// Inherit source from parent if node doesn't have one.
/// (The caller is responsible for recognizing, if it's true,
/// that the parent should have no associated source.)
fn inherit_source_if_needed(
  node: &mut OrgNode,
  parent_source: Option<String>
) {
  if node.metadata.source.is_none() {
    node.metadata.source = parent_source; }}
