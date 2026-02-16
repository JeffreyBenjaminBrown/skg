/// PURPOSE:
/// Add missing information to nodes in the forest. Namely:
/// - when treatment should be Alias, make it so
/// - add missing IDs where treatment is Content

use crate::types::unchecked_viewnode::{UncheckedViewNode, UncheckedViewNodeKind};
use crate::types::viewnode::Scaffold;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::do_everywhere_in_tree_dfs;
use crate::dbs::typedb::util::pids_from_ids::replace_ids_with_pids;
use ego_tree::{Tree, NodeId, NodeMut};
use std::boxed::Box;
use std::error::Error;
use typedb_driver::TypeDBDriver;
use uuid::Uuid;

/// PURPOSE:
/// Just read the code; it's clearer than a restatement in English.
/// .
/// PITFALL:
/// Does not add *all* missing info.
/// 'supplement_none_fields_from_disk_if_save' does some of that, too,
/// although it operates on DefineNodes, downstream.
pub async fn add_missing_info_to_forest(
  forest: &mut Tree<UncheckedViewNode>, // has BufferRoot at root
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  do_everywhere_in_tree_dfs(
    forest,
    forest . root() . id(),
    &mut |mut node| {
      make_alias_if_appropriate(&mut node)?;
      inherit_parent_source_if_possible(&mut node)?;
      assign_new_id_if_absent(&mut node)?;
      Ok (( )) } )?;
  let root_id: NodeId = forest.root().id();
  replace_ids_with_pids(
    forest, root_id, db_name, driver ). await }

/// Make this a Scaffold::Alias
/// if this is a TrueNode
///    and its parent is an AliasCol,
fn make_alias_if_appropriate(
  node: &mut NodeMut<UncheckedViewNode>
) -> Result<(), String> {
  if let UncheckedViewNodeKind::True(_) = &node.value().kind {
    // It's a TrueNode.
    let parent_is_aliascol : bool =
      node.parent()
      .map(|mut p| matches!(&p.value().kind,
                            UncheckedViewNodeKind::Scaff(Scaffold::AliasCol)))
      .unwrap_or(false);
    if parent_is_aliascol { // Make it an Alias.
      let org : &mut UncheckedViewNode = node.value();
      let UncheckedViewNodeKind::True(t) : &UncheckedViewNodeKind = &org.kind
        else { unreachable!() };
      org.kind = UncheckedViewNodeKind::Scaff(
        Scaffold::Alias { text: t.title.clone(),
                          diff: None } ); }}
  Ok (( )) }

/// Inherit parent's source
/// if this node is a sourceless TrueNode
///    and the parent is a TrueNode with a source.
fn inherit_parent_source_if_possible(
  node: &mut NodeMut<UncheckedViewNode>
) -> Result<(), String> {
  let needs_source : bool =
    match &node.value().kind {
      UncheckedViewNodeKind::True(t) => t.source_opt.is_none(),
      UncheckedViewNodeKind::Scaff(_) => false, };
  if needs_source {
    let parent_source : Option<SourceName> =
      node.parent().and_then(|mut p| {
        match &p.value().kind {
          UncheckedViewNodeKind::True(pt) => pt.source_opt.clone(),
          UncheckedViewNodeKind::Scaff(_) => None, }} );
    if let Some(source) = parent_source {
      if let UncheckedViewNodeKind::True(t) = &mut node.value().kind {
        t.source_opt = Some(source); }} }
  Ok (( )) }

/// Assign a new UUID to a TrueNode if it doesn't have an ID.
fn assign_new_id_if_absent(
  node: &mut NodeMut<UncheckedViewNode>
) -> Result<(), String> {
  if let UncheckedViewNodeKind::True(t) = &mut node.value().kind {
    if t.id_opt.is_none() {
      let new_id : String = Uuid::new_v4().to_string();
      t.id_opt = Some(ID(new_id)); }}
  Ok (( )) }
