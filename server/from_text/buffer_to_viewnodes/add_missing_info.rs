/// PURPOSE:
/// Add missing information to nodes in the viewforest. Namely:
/// - when treatment should be Alias, make it so
/// - add missing IDs where treatment is Content

use crate::types::git::MembershipAxes;
use crate::types::maybe_placed_viewnode::{MaybePlacedViewnode, MaybePlacedViewnodeKind};
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
/// Just read this function definition;
/// it's clearer than a restatement in English.
/// .
/// PITFALL:
/// Does not add *all* missing info.
/// 'supplement_unspecified_fields_from_disk' does some of that, too,
/// although it operates on DefineNodes, downstream.
pub async fn add_missing_info_to_viewforest(
  viewforest  : &mut Tree<MaybePlacedViewnode>, // has BufferRoot at root
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let root_id: NodeId = viewforest . root() . id();
  replace_ids_with_pids(
    viewforest, root_id, db_name, driver ) . await ?;
  do_everywhere_in_tree_dfs(
    viewforest,
    root_id,
    true,
    &mut |mut node| {
      make_alias_if_appropriate (&mut node)?;
      inherit_parent_source_if_possible (&mut node)?;
      assign_new_id_if_absent (&mut node)?; // Do this *after* PID replacement, so that fresh UUIDs don't trigger a pointless TypeDB lookup.
      Ok (( )) } )?;
  Ok (( )) }

/// Make it a Scaffold::Alias if both:
/// - it is a TrueNode
/// - its parent is an AliasCol
fn make_alias_if_appropriate(
  node: &mut NodeMut<MaybePlacedViewnode>
) -> Result<(), String> {
  if let MaybePlacedViewnodeKind::True (_) = &node . value() . kind {
    // It is a TrueNode.
    let parent_is_aliascol : bool =
      node . parent()
      . map(|mut p|
            matches!(&p . value() . kind,
                     MaybePlacedViewnodeKind::Scaff (Scaffold::AliasCol)))
      . unwrap_or (false);
    if parent_is_aliascol { // Make it an Alias.
      let org : &mut MaybePlacedViewnode = node . value();
      let MaybePlacedViewnodeKind::True (t) : &MaybePlacedViewnodeKind = &org . kind
        else { unreachable!() };
      org . kind = MaybePlacedViewnodeKind::Scaff(
        Scaffold::Alias { text: t . title . clone(),
                          membership: MembershipAxes::default () } ); }}
  Ok (( )) }

/// Inherit parent's source if both:
/// - this is a sourceless TrueNode
/// - its parent is a TrueNode with a source
fn inherit_parent_source_if_possible(
  node: &mut NodeMut<MaybePlacedViewnode>
) -> Result<(), String> {
  let needs_source : bool =
    match &node . value() . kind {
      MaybePlacedViewnodeKind::True (t) => t . source . is_none(),
      _ => false, };
  if needs_source {
    let parent_source : Option<SourceName> =
      node . parent() . and_then(|mut p| {
        match &p . value() . kind {
          MaybePlacedViewnodeKind::True (pt) => pt . source . clone(),
          _ => None, }} );
    if let Some (source) = parent_source {
      if let MaybePlacedViewnodeKind::True (t) = &mut node . value() . kind {
        t . source = Some (source); }} }
  Ok (( )) }

/// Assign a new UUID to a TrueNode if it doesn't have an ID.
fn assign_new_id_if_absent(
  node: &mut NodeMut<MaybePlacedViewnode>
) -> Result<(), String> {
  if let MaybePlacedViewnodeKind::True (t) = &mut node . value() . kind {
    if t . id . is_none() {
      let new_id : String = Uuid::new_v4() . to_string();
      t . id = Some(ID (new_id)); }}
  Ok (( )) }
