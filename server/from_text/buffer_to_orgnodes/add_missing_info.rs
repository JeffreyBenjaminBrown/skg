/// PURPOSE:
/// Add missing information to nodes in the forest. Namely:
/// - when treatment should be Alias, make it so
/// - add missing IDs where treatment is Content

use crate::types::orgnode::{OrgNode, OrgNodeKind, TrueNode, Scaffold};
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
/// - transforms TrueNode to Alias if needed
/// - modifies ID
///   - make a new one if it doesn't exist
///   - replace with PID if it already exists
/// - assigns source
/// .
/// PITFALL:
/// Does not add *all* missing info.
/// 'clobber_none_fields_with_data_from_disk' does some of that, too,
/// but it operates on SaveInstructions, downstream.
pub async fn add_missing_info_to_forest(
  forest: &mut Tree<OrgNode>, // has ForestRoot at root
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

/// - make it a n Scaffold::Alias if appropriate
/// - inherit parent source if knowable
/// - assign new ID, if missing and appropriate
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
            matches! ( &orgnode . kind,
              OrgNodeKind::Scaff( Scaffold::AliasCol ));
          let source : Option < String > =
            match &orgnode . kind
            { OrgNodeKind::True ( t ) => t . source_opt . clone (),
              OrgNodeKind::Scaff ( _ ) => None };
          ( is_aliascol, source ) } )
      { Ok (( ia, s )) => (ia, s),
        Err ( _ ) => ( false, None ) } };
    if parent_is_aliascol { // convert it to an alias
      let org = node_ref . value ();
      let OrgNodeKind::True ( t ) = &org . kind
        else { unreachable! () };
      org . kind = OrgNodeKind::Scaff (
        Scaffold::Alias ( t . title . clone() ));
    } else {
      let OrgNodeKind::True ( t ) = &mut node_ref . value () . kind // the borrow-checker forces this otherwise redundant pattern match
        else { unreachable! () };
      inherit_source_if_needed ( t, parent_source );
      assign_new_id_if_needed ( t ); } }
  // For Scaffolds, leave the node unchanged, but still recurse.
  { for child_treeid in { // Recurse into children, DFS.
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

/// Assign a UUID v4 to TrueNodes that don't have an ID.
fn assign_new_id_if_needed(
  t: &mut TrueNode
) {
  if t . id_opt . is_none ()
  { let new_id: String = Uuid::new_v4().to_string();
    t . id_opt = Some ( ID (new_id) ); }}

/// Inherit source from parent if node doesn't have one.
/// (The caller is responsible for recognizing, if it's true,
/// that the parent should have no associated source.)
fn inherit_source_if_needed(
  t: &mut TrueNode,
  parent_source: Option<String>
) {
  if t . source_opt . is_none () {
    if let Some ( source ) = parent_source {
      t . source_opt = Some ( source ); } } }
