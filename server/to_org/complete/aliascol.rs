use crate::types::orgnode::{OrgNode, OrgNodeKind, Scaffold};
use crate::types::tree::generic::{read_at_node_in_tree, write_at_node_in_tree, with_node_mut};
use crate::types::tree::orgnode_skgnode::insert_scaffold_as_child;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::{SkgNode, SkgNodeMap};
use crate::to_org::util::get_pid_in_tree;
use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Reconciles Alias children with aliases from the map.
pub async fn completeAliasCol (
  tree             : &mut Tree<OrgNode>,
  map              : &SkgNodeMap,
  aliascol_node_id : NodeId,
  _config          : &SkgConfig,
  _driver          : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { // Validate this is an AliasCol
    let is_aliascol : bool =
      read_at_node_in_tree(
        tree, aliascol_node_id,
        |orgnode| matches!( &orgnode.kind,
                       OrgNodeKind::Scaff(Scaffold::AliasCol)) )
      . map_err( |e| -> Box<dyn Error> { e . into () })?;
    if ! is_aliascol {
      return Err( "Node is not an AliasCol" . into () ); }}

  // Get parent node's ID (AliasCol's parent)
  let parent_id : ID = {
    let aliascol_ref : NodeRef<OrgNode> =
      tree . get ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    let parent_ref : NodeRef<OrgNode> =
      aliascol_ref . parent ()
      . ok_or ( "AliasCol has no parent" ) ?;
    get_pid_in_tree ( tree, parent_ref . id () ) ?
  };

  // Get parent SkgNode from map
  let parent_skgnode : &SkgNode =
    map . get ( &parent_id )
    . ok_or ( "Parent SkgNode not in map" ) ?;

  let aliases_from_disk : HashSet < String > = (
    parent_skgnode . aliases // source of truth
      . clone() . unwrap_or_default () . into_iter () . collect ( ));

  let aliases_from_branch : Vec < String > =
    collect_child_aliases_at_aliascol_in_orgtree (
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
    insert_scaffold_as_child (
      tree, aliascol_node_id,
      Scaffold::Alias ( alias . clone () ), false ) ?; }

  Ok (( )) }

/// Remove duplicates and invalid aliases. Works with Tree<OrgNode>.
fn remove_duplicates_and_false_aliases_handling_focus (
  tree             : &mut Tree<OrgNode>,
  aliascol_node_id : NodeId,
  good_aliases     : &HashSet < String >,
) -> Result < (), Box<dyn Error> > {
  let mut removed_focus : bool = false;
  let mut focused_title : Option < String > = None;

  let children_to_remove : Vec < NodeId > = {
    let aliascol_ref : NodeRef < OrgNode > =
      tree . get ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    let mut children_to_remove_acc : Vec < NodeId > =
      Vec::new ();
    let mut seen : HashSet < String > =
      HashSet::new ();
    for child in aliascol_ref . children () {
      let child_orgnode : &OrgNode = child . value ();
      let title : &str = child_orgnode . title ();
      let is_duplicate : bool =
        ! seen . insert ( title . to_string () );
      let is_invalid : bool =
        ! good_aliases . contains ( title );
      if is_duplicate || is_invalid {
        children_to_remove_acc . push ( child . id () );
        if child_orgnode . focused {
          // We will delete the focused node.
          removed_focus = true;
          if is_duplicate {
            // Use this to move focus to the earlier duplicate title.
            focused_title = Some ( title . to_string () ); }; }} }
    children_to_remove_acc };

  for child_treeid in children_to_remove {
    with_node_mut ( tree, child_treeid,
                    |mut child_mut| child_mut . detach () )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?; }

  if removed_focus {
    if let Some ( title ) = focused_title {
      // Move focus to the earlier duplicate title.
      let aliascol_ref : NodeRef < OrgNode > =
        tree . get ( aliascol_node_id )
        . ok_or ( "AliasCol node not found" ) ?;
      for child in aliascol_ref . children () {
        if child . value () . title () == title {
          write_at_node_in_tree (
            tree, child . id (),
            |orgnode| {
              orgnode . focused = true; } )
            . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
          break; }} }
    else { // Move focus to aliasCol itself.
      write_at_node_in_tree (
        tree, aliascol_node_id,
        |orgnode| {
          orgnode . focused = true; } )
        . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?; }}

  Ok (( )) }
/// Collect aliases from Alias children of an AliasCol node (for Tree<OrgNode>).
fn collect_child_aliases_at_aliascol_in_orgtree (
  tree             : &Tree<OrgNode>,
  aliascol_node_id : NodeId,
) -> Result < Vec < String >, Box<dyn Error> > {
  let mut aliases : Vec < String > =
    Vec::new ();
  let aliascol_ref : NodeRef < OrgNode > =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found" ) ?;
  for child_ref in aliascol_ref . children() {
    let child : &OrgNode = child_ref . value();
    if ! matches! ( &child . kind,
                    OrgNodeKind::Scaff ( Scaffold::Alias(_) )) {
      return Err (
        format! ( "AliasCol has non-Alias child with kind: {:?}",
                  child . kind )
        . into () ); }
    aliases . push (
      child . title () . to_string () ); }
  Ok ( aliases ) }

