/// PURPOSE:
/// Add missing information to nodes in the forest. Namely:
/// - when treatment should be Alias, make it so
/// - add missing IDs where treatment is Content

use crate::types::unchecked_orgnode::{UncheckedOrgNode, UncheckedOrgNodeKind};
use crate::types::orgnode::Scaffold;
use crate::types::misc::ID;
use crate::types::tree::generic::do_everywhere_in_tree_dfs;
use crate::dbs::typedb::util::pids_from_ids::{pids_from_ids, collect_ids_in_tree, assign_pids_throughout_tree_from_map};
use ego_tree::{Tree, NodeId, NodeMut};
use std::boxed::Box;
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;
use uuid::Uuid;

/// PURPOSE:
/// The code is clearer than a restatement in English would be.
/// .
/// PITFALL:
/// Does not add *all* missing info.
/// 'clobber_none_fields_with_data_from_disk' does some of that, too,
/// although it operates on SaveInstructions, downstream.
pub async fn add_missing_info_to_forest(
  forest: &mut Tree<UncheckedOrgNode>, // has BufferRoot at root
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let tree_root_ids: Vec<NodeId> =
    forest.root().children().map(|c| c.id()).collect();
  for tree_root_id in &tree_root_ids {
    do_everywhere_in_tree_dfs(
      forest, *tree_root_id,
      &mut |mut node| {
        make_alias_if_appropriate(&mut node)?;
        inherit_parent_source_if_possible(&mut node)?;
        assign_new_id_if_absent(&mut node)?;
        Ok (( )) } )?; }
  assign_pids_throughout_forest (
    forest, &tree_root_ids, db_name, driver ). await }

/// Make this a Scaffold::Alias
/// if this is a TrueNode
///    and its parent is an AliasCol,
fn make_alias_if_appropriate(
  node: &mut NodeMut<UncheckedOrgNode>
) -> Result<(), String> {
  if let UncheckedOrgNodeKind::True(_) = &node.value().kind {
    // It's a TrueNode.
    let parent_is_aliascol : bool =
      node.parent()
      .map(|mut p| matches!(&p.value().kind,
                            UncheckedOrgNodeKind::Scaff(Scaffold::AliasCol)))
      .unwrap_or(false);
    if parent_is_aliascol { // Make it an Alias.
      let org : &mut UncheckedOrgNode = node.value();
      let UncheckedOrgNodeKind::True(t) : &UncheckedOrgNodeKind = &org.kind
        else { unreachable!() };
      org.kind = UncheckedOrgNodeKind::Scaff(
        Scaffold::Alias { text: t.title.clone(),
                          diff: None } ); }}
  Ok (( )) }

/// Inherit parent's source
/// if this node is a sourceless TrueNode
///    and the parent is a TrueNode with a source.
fn inherit_parent_source_if_possible(
  node: &mut NodeMut<UncheckedOrgNode>
) -> Result<(), String> {
  let needs_source : bool =
    match &node.value().kind {
      UncheckedOrgNodeKind::True(t) => t.source_opt.is_none(),
      UncheckedOrgNodeKind::Scaff(_) => false, };
  if needs_source {
    let parent_source : Option<String> =
      node.parent().and_then(|mut p| {
        match &p.value().kind {
          UncheckedOrgNodeKind::True(pt) => pt.source_opt.clone(),
          UncheckedOrgNodeKind::Scaff(_) => None, }} );
    if let Some(source) = parent_source {
      if let UncheckedOrgNodeKind::True(t) = &mut node.value().kind {
        t.source_opt = Some(source); }} }
  Ok (( )) }

/// Assign a new UUID to a TrueNode if it doesn't have an ID.
fn assign_new_id_if_absent(
  node: &mut NodeMut<UncheckedOrgNode>
) -> Result<(), String> {
  if let UncheckedOrgNodeKind::True(t) = &mut node.value().kind {
    if t.id_opt.is_none() {
      let new_id : String = Uuid::new_v4().to_string();
      t.id_opt = Some(ID(new_id)); }}
  Ok (( )) }

/// PURPOSE: Replace each ID with, if it exists, the corresponding PID.
/// METHOD: Collects all IDs, then performs a batch lookup in TypeDB.
async fn assign_pids_throughout_forest (
  forest        : &mut Tree<UncheckedOrgNode>,
  tree_root_ids : &[NodeId],
  db_name       : &str,
  driver        : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let mut ids_to_lookup: Vec<ID> = Vec::new();
  for tree_root_id in tree_root_ids {
    if let Some(tree_root_ref) = forest.get(*tree_root_id) {
      collect_ids_in_tree(tree_root_ref,
                                    &mut ids_to_lookup); }}
  let pid_map: HashMap<ID, Option<ID>> =
    pids_from_ids( db_name, driver, &ids_to_lookup
    ). await?;
  for tree_root_id in tree_root_ids {
    if let Some(tree_root_mut) =
      forest.get_mut(*tree_root_id) {
        assign_pids_throughout_tree_from_map(
          tree_root_mut, &pid_map); }}
  Ok(( )) }
