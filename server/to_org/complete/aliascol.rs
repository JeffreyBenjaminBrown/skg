use crate::types::orgnode::Interp;
use crate::types::tree::{NodePair, PairTree};
use crate::types::tree::generic::{read_at_node_in_tree, write_at_node_in_tree, with_node_mut};
use crate::types::tree::orgnode_skgnode::{
  ancestor_skgnode_from_disk, collect_child_aliases_at_nodepair_aliascol, insert_sourceless_node };
use crate::types::misc::SkgConfig;
use crate::types::skgnode::SkgNode;
use ego_tree::{NodeId, NodeRef};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Reconciles its Alias children
///   with the aliases found on disk for its parent node.
/// Might add and remove aliases.
/// Might transfer focus.
///
/// ASSUMES the buffer that generated this tree was already saved,
/// so the disk state is the source of truth.
/// ASSUMES the parent node P has been normalized
/// so that its 'id' field is the PID.
pub async fn completeAliasCol (
  tree             : &mut PairTree,
  aliascol_node_id : NodeId,
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { // Validate this is an AliasCol
    let is_aliascol : bool =
      read_at_node_in_tree(
        tree, aliascol_node_id,
        |np| np . orgnode_new () . matches_interp ( &Interp::AliasCol ) )
      . map_err( |e| -> Box<dyn Error> { e . into () })?;
    if ! is_aliascol {
      return Err( "Node is not an AliasCol" . into () ); }}
  let parent_skgnode : SkgNode =
    ancestor_skgnode_from_disk(
      tree, aliascol_node_id, 1, config, driver ) . await?;
  let aliases_from_disk : HashSet < String > = (
    parent_skgnode . aliases // source of truth
      . unwrap_or_default () . into_iter () . collect ( ));
  let aliases_from_branch : Vec < String > =
    collect_child_aliases_at_nodepair_aliascol (
      tree, aliascol_node_id )?;
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
  for alias in missing_aliases_from_disk {
    insert_sourceless_node (
      tree, aliascol_node_id,
      Interp::Alias, & alias, false ) ?; }
  Ok (( )) }

/// Removes duplicate and invalid Alias children from an AliasCol,
/// preserving focus information,
/// where 'invalid' means not found on disk.
fn remove_duplicates_and_false_aliases_handling_focus (
  tree             : &mut PairTree,
  aliascol_node_id : NodeId,
  good_aliases     : &HashSet < String >,
) -> Result < (), Box<dyn Error> > {
  let mut removed_focus : bool = false;
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
      let child_new_orgnode = child . value () . orgnode_new ();
      let title : &str = child_new_orgnode . title ();
      let is_duplicate : bool =
        ! seen . insert ( title . to_string () );
      let is_invalid : bool =
        ! good_aliases . contains ( title );
      if is_duplicate || is_invalid {
        children_to_remove_acc . push ( child . id () );
        if child_new_orgnode . focused {
          // We will delete the focused node.
          removed_focus = true;
          if is_duplicate {
            // Use this to move focus to the earlier duplicate title.
            focused_title = Some ( title . to_string () ); }; }} }
    children_to_remove_acc };

  for child_treeid in children_to_remove {
    with_node_mut ( tree, child_treeid,
                    |mut child_mut| child_mut . detach () ) ?; }

  if removed_focus {
    if let Some ( title ) = focused_title {
      // Move focus to the earlier duplicate title.
      let aliascol_ref : NodeRef < NodePair > =
        tree . get ( aliascol_node_id )
        . ok_or ( "AliasaCol node not found" ) ?;
      for child in aliascol_ref . children () {
        if child . value () . orgnode_new () . title () == title {
          write_at_node_in_tree (
            tree, child . id (),
            |np| {
              np . orgnode_new_mut () . focused = true; } ) ?;
          break; }} }
    else { // Move focus to aliasCol itself.
      write_at_node_in_tree (
        tree, aliascol_node_id,
        |np| {
          np . orgnode_new_mut () . focused = true; } ) ?; }}

  Ok (( )) }
