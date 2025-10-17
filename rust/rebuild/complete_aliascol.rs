use crate::file_io::read_node;
use crate::types::{ID, OrgNode, OrgnodeMetadata, SkgConfig, SkgNode, Treatment};
use crate::types::orgnode::default_metadata;
use crate::util::path_from_pid;
use ego_tree::NodeId;
use std::collections::HashSet;
use std::error::Error;

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
pub fn completeAliasCol (
  tree             : &mut ego_tree::Tree < OrgNode >,
  aliascol_node_id : NodeId,
  config           : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let parent_id : ID =
    get_aliascol_parent_id (
      tree,
      aliascol_node_id
    ) ?;
  let skgnode : SkgNode = {
    let path : String =
      path_from_pid ( config, parent_id . clone () );
    read_node ( path )? };
  let aliases_from_disk : HashSet < String > = // source of truth
    skgnode . aliases
    . unwrap_or_default ()
    . into_iter ()
    . collect ();

  // Collect aliases from tree (checking for non-Alias children)
  let aliases_from_tree : Vec < String > =
    collect_alias_titles ( tree, aliascol_node_id ) ?;

  // Compute which aliases are valid (on disk)
  let good_aliases : HashSet < String > =
    aliases_from_tree
    . iter ()
    . filter ( |alias| aliases_from_disk . contains ( *alias ))
    . cloned ()
    . collect ();

  // Compute missing aliases (on disk but not in tree)
  let missing_aliases_from_disk : HashSet < String > =
    aliases_from_disk
    . difference ( & good_aliases )
    . cloned ()
    . collect ();

  // Remove duplicates and invalid children in one pass
  let mut removed_focused : bool = false;
  {
    let children_to_remove : Vec < NodeId > = {
      let aliascol_ref : ego_tree::NodeRef < OrgNode > =
        tree . get ( aliascol_node_id )
        . ok_or ( "AliasCol node not found" ) ?;

      let mut result : Vec < NodeId > =
        Vec::new ();
      let mut seen : HashSet < String > =
        HashSet::new ();

      for child in aliascol_ref . children () {
        let child_node : &OrgNode =
          child . value ();
        let title : &String =
          & child_node . title;

        // Remove if: duplicate OR not valid
        let is_duplicate : bool =
          ! seen . insert ( title . clone () );
        let is_invalid : bool =
          ! good_aliases . contains ( title );

        if is_duplicate || is_invalid {
          result . push ( child . id () );
          if child_node . metadata . focused {
            removed_focused = true; }} }
      result };

    for child_id in children_to_remove {
      let mut child_mut : ego_tree::NodeMut < OrgNode > =
        tree . get_mut ( child_id )
        . ok_or ( "Child node not found" ) ?;
      child_mut . detach (); }}

  { // Append new Alias nodes for missing aliases
    let mut aliascol_mut : ego_tree::NodeMut < OrgNode > =
      tree . get_mut ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    for alias in missing_aliases_from_disk {
      let mut md : OrgnodeMetadata =
        default_metadata ();
      md . treatment = Treatment::Alias;
      aliascol_mut . append ( OrgNode {
        metadata : md,
        title    : alias,
        body     : None, }); }}

  if removed_focused {
    // Transfer focus to AliasCol if any focused nodes were removed
    let mut aliascol_mut : ego_tree::NodeMut < OrgNode > =
      tree . get_mut ( aliascol_node_id )
      . ok_or ( "AliasCol node not found" ) ?;
    aliascol_mut . value () . metadata . focused = true; }
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
    if child_node . metadata . treatment != Treatment::Alias {
      return Err (
        format! ( "AliasCol has non-Alias child with treatment: {:?}",
                  child_node . metadata . treatment )
        . into () ); }
    aliases_from_tree . push (
      child_node . title . clone () );
  }
  Ok ( aliases_from_tree ) }

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
  if aliascol_node . metadata . treatment != Treatment::AliasCol {
    return Err (
      format! ( "Node is not an AliasCol: {:?}",
                 aliascol_node . metadata . treatment )
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
