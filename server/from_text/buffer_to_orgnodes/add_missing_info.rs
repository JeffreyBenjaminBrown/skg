/// PURPOSE:
/// Add missing information to nodes in the forest. Namely:
/// - when treatment should be Alias, make it so
/// - add missing IDs where treatment is Content

use crate::types::orgnode::{OrgNode, OrgNodeKind, EffectOnParent, Scaffold};
use crate::types::misc::ID;
use crate::types::tree::generic::read_at_ancestor_in_tree;
use crate::dbs::typedb::util::pids_from_ids::{pids_from_ids, collect_ids_in_orgnode_tree, assign_pids_throughout_orgnode_tree_from_map};
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
    if let Some(tree_root_mut) =
      forest.get_mut(*tree_root_id) {
        add_missing_info_dfs( tree_root_mut ); } }
  assign_pids_throughout_forest (
    forest, &tree_root_ids, db_name, driver ). await }

/// Collect IDs from forest, look up PIDs in TypeDB, assign them.
async fn assign_pids_throughout_forest (
  forest        : &mut Tree<OrgNode>,
  tree_root_ids : &[NodeId],
  db_name       : &str,
  driver        : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let mut ids_to_lookup: Vec<ID> = Vec::new();
  for tree_root_id in tree_root_ids {
    if let Some(tree_root_ref) = forest.get(*tree_root_id) {
      collect_ids_in_orgnode_tree(tree_root_ref,
                                  &mut ids_to_lookup); }}
  let pid_map: HashMap<ID, Option<ID>> =
    pids_from_ids( db_name, driver, &ids_to_lookup
    ). await?;
  for tree_root_id in tree_root_ids {
    if let Some(tree_root_mut) =
      forest.get_mut(*tree_root_id) {
        assign_pids_throughout_orgnode_tree_from_map(
          tree_root_mut, &pid_map); }}
  Ok(( )) }

/// - assign alias Interp, if missing and appropriate
/// - assign source if knowable
/// - assign new ID,       if missing and appropriate
fn add_missing_info_dfs (
  mut node_ref: NodeMut < OrgNode >,
) {
  if let OrgNodeKind::True ( _ ) =
    &node_ref . value () . kind
  { let ( parent_is_aliascol, parent_source )
      : ( bool, Option < String > ) =
    { let treeid = node_ref . id ();
      match read_at_ancestor_in_tree (
        node_ref . tree (), treeid, 1,
        |orgnode| {
          let is_aliascol : bool =
            orgnode . is_scaffold ( &Scaffold::AliasCol );
          let parent_source : Option < String > =
            match &orgnode . kind
            { OrgNodeKind::True ( t ) => t . source_opt . clone (),
              OrgNodeKind::Scaff ( _ ) => None };
          ( is_aliascol, parent_source ) } )
      { Ok ( ( is_aliascol, source ) ) => ( is_aliascol, source ),
        Err ( _ ) => ( false, None ) } };
    if parent_is_aliascol
    { node_ref . value () . convert_to_alias ();
    } else {
      inherit_source_if_needed ( node_ref . value (), parent_source );
      assign_new_id_if_needed ( node_ref . value () ); } }
  // For Scaffolds, leave the node unchanged, but still recurse.
  { // Recurse into children DFS.
    for child_treeid in {
      let child_treeids: Vec < ego_tree::NodeId > = {
        let treeid: ego_tree::NodeId = node_ref . id ();
        let tree = node_ref . tree ();
        tree . get ( treeid ) . unwrap ()
          . children () . map ( | child | child . id () )
          . collect () };
      child_treeids }
    { if let Some ( child_mut )
        = node_ref . tree () . get_mut ( child_treeid )
      { add_missing_info_dfs ( child_mut ); }} }}

/// Assign a UUID v4 to Content nodes that don't have an ID
fn assign_new_id_if_needed(
  node: &mut OrgNode
) {
  let has_no_id : bool = match &node . kind {
    OrgNodeKind::True ( t ) => t . id_opt . is_none (),
    OrgNodeKind::Scaff ( _ ) => true };
  if node . has_effect ( EffectOnParent::Content )
     && has_no_id
  { let new_id: String = Uuid::new_v4().to_string();
    node . set_id ( Some( ID (new_id)) ); }}

/// Inherit source from parent if node doesn't have one.
/// (The caller is responsible for recognizing, if it's true,
/// that the parent should have no associated source.)
fn inherit_source_if_needed(
  node: &mut OrgNode,
  parent_source: Option<String>
) {
  if ! node . has_source () {
    if let Some ( source ) = parent_source {
      node . set_source ( source ); } } }
