pub mod org_to_mskg_org_adaptor;

use crate::media::file_io::one_node::read_node;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, OrgnodeMetadata, RelToParent, default_metadata};
use crate::util::path_from_pid_and_source;
use ego_tree::NodeId;
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
/// so that its 'id' field is the PID (no need to call pid_from_id).
///
/// Given an AliasCol node C:
/// - Fetches the parent P's SkgNode S from disk
/// - Deduplicates C's Alias children (preserving order)
/// - Removes invalid Alias children (those not in S.aliases)
/// - Adds missing Alias children (those in S.aliases but not in C's children)
/// - Transfers focus to C if any focused Alias is removed
/// - Errors if C's parent has no ID or if non-Alias children are found
pub async fn completeAliasCol (
  tree             : &mut ego_tree::Tree < OrgNode >,
  aliascol_node_id : NodeId,
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let parent_id : ID =
    get_aliascol_parent_id (
      tree,
      aliascol_node_id ) ?;
  let (parent_pid, parent_source) : (ID, String) =
    pid_and_source_from_id( // Query TypeDB for them
      &config.db_name, driver, &parent_id).await?
    . ok_or_else( || format!(
      "Parent ID '{}' not found in database", parent_id))?;
  let mut skgnode : SkgNode = {
    let path : String =
      path_from_pid_and_source ( config, &parent_source, parent_pid );
    read_node ( path )? };
  skgnode.source = parent_source;
  let aliases_from_disk : HashSet < String > = (
    // source of truth
    skgnode . aliases
      . unwrap_or_default ()
      . into_iter ()
      . collect ( ));
  let aliases_from_tree : Vec < String > =
    collect_alias_titles ( tree, aliascol_node_id ) ?;
  let good_aliases_in_tree : HashSet < String > = (
    // aliases in tree that match aliases on disk
    aliases_from_tree
      . iter ()
      . filter ( |alias| aliases_from_disk . contains ( *alias ))
      . cloned ()
      . collect ( ));
  let missing_aliases_from_disk : HashSet < String > = (
    // aliases on disk but not in tree
    aliases_from_disk
      . difference ( & good_aliases_in_tree )
      . cloned ()
      . collect ( ));
  remove_duplicates_and_false_aliases_handling_focus (
    tree, // PITFALL: Gets modified.
    aliascol_node_id,
    & good_aliases_in_tree ) ?;
  { // Append new Alias nodes for missing aliases
    let mut aliascol_mut : ego_tree::NodeMut < OrgNode > =
      tree . get_mut ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    for alias in missing_aliases_from_disk {
      let mut md : OrgnodeMetadata =
        default_metadata ();
      md . code.relToParent = RelToParent::Alias;
      aliascol_mut . append ( OrgNode {
        metadata : md,
        title    : alias,
        body     : None, }); }}
  Ok (( )) }

/// Collect titles from Alias children of an AliasCol node.
/// Errors if any non-Alias children are found.
fn collect_alias_titles (
  tree             : &ego_tree::Tree < OrgNode >,
  aliascol_node_id : NodeId,
) -> Result < Vec < String >, Box<dyn Error> > {
  let mut aliases_from_tree : Vec < String > =
    Vec::new ();
  let aliascol_ref : ego_tree::NodeRef < OrgNode > =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found" ) ?;
  for child in aliascol_ref . children () {
    let child_node : &OrgNode =
      child . value ();
    if child_node . metadata . code.relToParent != RelToParent::Alias {
      return Err (
        format! ( "AliasCol has non-Alias child with relToParent: {:?}",
                  child_node . metadata . code.relToParent )
        . into () ); }
    aliases_from_tree . push (
      child_node . title . clone () );
  }
  Ok ( aliases_from_tree ) }

/// Removes duplicate and invalid Alias children from an AliasCol,
/// preserving focus information.
///
/// If a duplicate node has focus, after it is removed,
/// the remaining node with the same title gets focus.
///
/// If an invalid (non-existent on disk) alias has focus,
/// focus is transferred to the AliasCol itself.
fn remove_duplicates_and_false_aliases_handling_focus (
  tree             : &mut ego_tree::Tree < OrgNode >,
  aliascol_node_id : NodeId,
  good_aliases     : &HashSet < String >,
) -> Result < (), Box<dyn Error> > {
  let mut removed_focused : bool = false;
  let mut focused_title : Option < String > = None;

  let children_to_remove : Vec < NodeId > = {
    let aliascol_ref : ego_tree::NodeRef < OrgNode >
      = ( tree . get ( aliascol_node_id )
          . ok_or ( "AliasCol node not found" ) ) ?;
    let mut children_to_remove_acc : Vec < NodeId > =
      Vec::new ();
    let mut seen : HashSet < String > =
      HashSet::new ();
    for child in aliascol_ref . children () {
      let child_node : &OrgNode =
        child . value ();
      let title : &String =
        & child_node . title;
      let is_duplicate : bool =
        ! seen . insert ( title . clone () );
      let is_invalid : bool =
        ! good_aliases . contains ( title );
      if is_duplicate || is_invalid {
        children_to_remove_acc . push ( child . id () );
        if child_node . metadata . viewData.focused {
          if is_duplicate {
            focused_title = Some ( title . clone () ); }
          else {
            removed_focused = true; }}} }
    children_to_remove_acc };

  for child_id in children_to_remove {
    let mut child_mut : ego_tree::NodeMut < OrgNode > =
      tree . get_mut ( child_id )
      . ok_or ( "Child node not found" ) ?;
    child_mut . detach (); }

  if let Some ( title ) = focused_title {
    let aliascol_ref : ego_tree::NodeRef < OrgNode > =
      tree . get ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    for child in aliascol_ref . children () {
      if child . value () . title == title {
        let mut child_mut : ego_tree::NodeMut < OrgNode > =
          tree . get_mut ( child . id () )
          . ok_or ( "Child node not found" ) ?;
        child_mut . value () . metadata . viewData.focused = true;
        break; }}}

  if removed_focused {
    let mut aliascol_mut : ego_tree::NodeMut < OrgNode > =
      tree . get_mut ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    aliascol_mut . value () . metadata . viewData.focused = true; }

  Ok (( )) }

/// Get the parent ID of an AliasCol node.
/// Verifies the node is an AliasCol and its parent has an ID.
fn get_aliascol_parent_id (
  tree             : &ego_tree::Tree < OrgNode >,
  aliascol_node_id : NodeId,
) -> Result < ID, Box<dyn Error> > {
  let aliascol_node : &OrgNode =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found in tree" ) ?
    . value ();
  if aliascol_node . metadata . code.relToParent != RelToParent::AliasCol {
    return Err (
      format! ( "Node is not an AliasCol: {:?}",
                 aliascol_node . metadata . code.relToParent )
        . into () ); }
  let parent_node_id : NodeId =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found" ) ?
    . parent ()
    . ok_or ( "AliasCol node has no parent" ) ?
    . id ();
  let parent_id : &ID =
    tree . get ( parent_node_id )
    . ok_or ( "Parent node not found" ) ?
    . value ()
    . metadata . id . as_ref ()
    . ok_or ( "Parent node has no ID" ) ?;
  Ok ( parent_id . clone () ) }
