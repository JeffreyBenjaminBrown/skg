use crate::dbs::filesystem::skgnode_from_id;
use crate::to_org::util::orgnode_from_title_and_rel;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, Interp};
use crate::types::tree::{NodePair, PairTree};
use crate::types::tree::accessors::{read_at_ancestor_in_tree, read_at_node_in_tree};
use ego_tree::{NodeId, NodeMut, NodeRef};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Complete an AliasCol node by reconciling its Alias children
/// with the aliases found on disk for its parent node.
///
/// PITFALL: This function should be called AFTER saving,
/// so the disk state is the source of truth.
///
/// This function assumes the parent node P has been normalized
/// so that its 'id' field is the PID.
///
/// Given an AliasCol node C:
/// - Fetches the parent P's SkgNode S from disk
///   TODO ? This is inefficient.
///   Climbing the tree, to where it already is, would be smarter.
/// - Deduplicates C's Alias children (preserving order)
/// - Removes invalid Alias children (those not in S.aliases)
/// - Adds missing Alias children (those in S.aliases but not in C's children)
/// - Transfers focus to C if any focused Alias is removed
/// - Errors if C's parent has no ID or if non-Alias children are found
pub async fn completeAliasCol (
  tree             : &mut PairTree,
  aliascol_node_id : NodeId,
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { // Validate this is an AliasCol
    let interp : Interp =
      read_at_node_in_tree(
        tree, aliascol_node_id,
        |np| np . orgnode . metadata . code . interp . clone () )
      . map_err( |e| -> Box<dyn Error> { e . into () })?;
    if interp != Interp::AliasCol {
      return Err( format!(
        "Node is not an AliasCol: {:?}", interp ) . into () ); }}
  let parent_skgnode : SkgNode =
    ancestor_skgnode_from_disk(
      tree, aliascol_node_id, 1, config, driver ) . await?;
  let aliases_from_disk : HashSet < String > = (
    parent_skgnode . aliases // source of truth
      . unwrap_or_default () . into_iter () . collect ( ));
  let aliases_from_branch : Vec < String > =
    collect_alias_titles ( tree, aliascol_node_id ) ?;
  let good_aliases_in_branch : HashSet < String > = (
    // aliases in tree that match aliases on disk
    aliases_from_branch . iter ()
      . filter ( |alias|
                  aliases_from_disk . contains ( *alias ))
      . cloned () . collect ( ));
  let missing_aliases_from_disk : HashSet < String > = (
    // aliases on disk but not in tree
    aliases_from_disk
      . difference ( & good_aliases_in_branch )
      . cloned ()
      . collect ( ));
  remove_duplicates_and_false_aliases_handling_focus (
    tree, // PITFALL: Gets modified.
    aliascol_node_id,
    & good_aliases_in_branch ) ?;
  { // Append new Alias nodes for missing aliases
    let mut aliascol_mut : NodeMut < NodePair > =
      tree . get_mut ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    for alias in missing_aliases_from_disk {
      aliascol_mut . append (
        NodePair { mskgnode: None,
                   orgnode: orgnode_from_title_and_rel (
                     Interp::Alias, alias ) }); }}
  Ok (( )) }

/// Reads from disk the SkgNode
/// for a node or for one of its tree-ancestors.
async fn ancestor_skgnode_from_disk (
  tree       : &PairTree,
  treeid     : NodeId,
  generation : usize, // 0 = self, 1 = parent, etc.
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < SkgNode, Box<dyn Error> > {
  let ancestor_skgid : ID =
    read_at_ancestor_in_tree( tree, treeid, generation,
      |np| np . orgnode . metadata . id . clone () )
    . map_err( |e| -> Box<dyn Error> { e . into () })?
    . ok_or_else(
      || -> Box<dyn Error> {
        "Ancestor node has no ID" . into () })?;
  let skgnode : SkgNode =
    skgnode_from_id(
      config, driver, &ancestor_skgid ) . await?;
  Ok ( skgnode ) }

/// Collect titles from Alias children of an AliasCol node.
/// Errors if any non-Alias children are found.
fn collect_alias_titles (
  tree             : &PairTree,
  aliascol_node_id : NodeId,
) -> Result < Vec < String >, Box<dyn Error> > {
  let mut aliases_from_tree : Vec < String > =
    Vec::new ();
  let aliascol_ref : NodeRef < NodePair > =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found" ) ?;
  for child in aliascol_ref . children () {
    let child_orgnode : &OrgNode =
      & child . value () . orgnode;
    if child_orgnode . metadata . code.interp != Interp::Alias {
      return Err (
        format! ( "AliasCol has non-Alias child with interp: {:?}",
                  child_orgnode . metadata . code.interp )
        . into () ); }
    aliases_from_tree . push (
      child_orgnode . title . clone () ); }
  Ok ( aliases_from_tree ) }

/// Removes duplicate and invalid Alias children from an AliasCol,
/// preserving focus information,
/// where 'invalid' means not found on disk.
///
/// If a duplicate node has focus, after it is removed,
/// the remaining node with the same title gets focus.
///
/// If an invalid (non-existent on disk) alias has focus,
/// focus is transferred to the AliasCol itself.
fn remove_duplicates_and_false_aliases_handling_focus (
  tree             : &mut PairTree,
  aliascol_node_id : NodeId,
  good_aliases     : &HashSet < String >,
) -> Result < (), Box<dyn Error> > {
  let mut removed_focused : bool = false;
  let mut focused_title : Option < String > = None;

  let children_to_remove : Vec < NodeId > = {
    let aliascol_ref : NodeRef < NodePair > =
      tree . get ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    let mut children_to_remove_acc : Vec < NodeId > =
      Vec::new ();
    let mut seen : HashSet < String > =
      HashSet::new ();
    for child in aliascol_ref . children () {
      let child_orgnode : &OrgNode =
        & child . value () . orgnode;
      let title : &String =
        & child_orgnode . title;
      let is_duplicate : bool =
        ! seen . insert ( title . clone () );
      let is_invalid : bool =
        ! good_aliases . contains ( title );
      if is_duplicate || is_invalid {
        children_to_remove_acc . push ( child . id () );
        if child_orgnode . metadata . viewData.focused {
          if is_duplicate {
            focused_title = Some ( title . clone () ); }
          else {
            removed_focused = true; }}} }
    children_to_remove_acc };

  for child_treeid in children_to_remove {
    let mut child_mut : NodeMut < NodePair > =
      tree . get_mut ( child_treeid )
      . ok_or ( "Child node not found" ) ?;
    child_mut . detach (); }

  if let Some ( title ) = focused_title {
    let aliascol_ref : NodeRef < NodePair > =
      tree . get ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    for child in aliascol_ref . children () {
      if child . value () . orgnode . title == title {
        let mut child_mut : NodeMut < NodePair > =
          tree . get_mut ( child . id () )
          . ok_or ( "Child node not found" ) ?;
        child_mut . value () . orgnode . metadata . viewData.focused = true;
        break; }}}

  if removed_focused {
    let mut aliascol_mut : NodeMut < NodePair > =
      tree . get_mut ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    aliascol_mut . value () . orgnode . metadata . viewData.focused = true; }

  Ok (( )) }
