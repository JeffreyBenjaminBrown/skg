/// Wrapper to call 'completeAliasCol'
/// on a Tree<Optional<SkgNode,OrgNode>>.
///
/// Strategy: Build a minimal Tree<OrgNode>
/// containing just a fake parent (to hold the ID)
/// and the AliasCol subtree, call 'completeAliasCol',
/// then sync the changes back to the paired tree.

use crate::to_org::complete_aliascol::completeAliasCol;
use crate::to_org::util::{get_pid_from_pair_using_noderef, collect_child_ids};
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, OrgnodeMetadata, RelToParent, default_metadata};
use crate::types::trees::PairTree;

use ego_tree::{Tree, NodeId, NodeRef};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Complete an AliasCol node in a paired tree.
/// See 'completeAliasCol' for the full documentation.
pub async fn completeAliasCol_in_mskg_org_tree (
  paired_tree      : &mut PairTree,
  aliascol_node_id : NodeId,
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let (parent_id, aliascol_orgnode) : (ID, OrgNode) =
    extract_parent_id_and_aliascol (
      paired_tree, aliascol_node_id ) ?;

  // Build minimal Tree<OrgNode>: fake parent -> AliasCol -> children
  let mut orgnode_tree : Tree < OrgNode > =
    build_minimal_orgnode_tree (
      paired_tree, aliascol_node_id,
      parent_id, aliascol_orgnode ) ?;
  let orgnode_aliascol_id : NodeId =
    orgnode_tree . root () . first_child ()
    . ok_or ( "No AliasCol in orgnode_tree" ) ? . id ();
  completeAliasCol ( // get what we came here for
    &mut orgnode_tree,
    orgnode_aliascol_id,
    config, driver ) . await ?;
  sync_aliascol_changes_to_paired_tree ( // translate back
    paired_tree, aliascol_node_id,
    &orgnode_tree, orgnode_aliascol_id ) ?;
  Ok (( )) }

/// Extract AliasCol OrgNode and its parent ID.
fn extract_parent_id_and_aliascol (
  tree             : &PairTree,
  aliascol_node_id : NodeId,
) -> Result < (ID, OrgNode), Box<dyn Error> > {
  let aliascol_ref : NodeRef < (Option<SkgNode>, OrgNode) > =
    tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol node not found" ) ?;
  let aliascol_orgnode : OrgNode =
    aliascol_ref . value () . 1 . clone ();
  if aliascol_orgnode . metadata . code . relToParent
    != RelToParent::AliasCol {
    return Err ( format! (
      "Node is not an AliasCol: {:?}",
      aliascol_orgnode . metadata . code . relToParent
    ). into () ); }
  let parent_ref : NodeRef < (Option<SkgNode>, OrgNode) > =
    aliascol_ref . parent ()
    . ok_or ( "AliasCol has no parent" ) ?;
  let parent_id : ID =
    get_pid_from_pair_using_noderef ( &parent_ref ) ?;
  Ok (( parent_id, aliascol_orgnode )) }

/// Build a minimal Tree<OrgNode> with structure:
///   fake_parent (has the ID) -> aliascol -> aliases
fn build_minimal_orgnode_tree (
  paired_tree      : &PairTree,
  aliascol_node_id : NodeId,
  parent_id        : ID,
  aliascol_orgnode : OrgNode,
) -> Result < Tree < OrgNode >, Box<dyn Error> > {
  // Create fake parent with the ID
  let mut fake_parent_metadata : OrgnodeMetadata =
    default_metadata ();
  fake_parent_metadata . id = Some ( parent_id );
  let fake_parent = OrgNode {
    metadata : fake_parent_metadata,
    title    : String::new (),
    body     : None, };
  let mut tree : Tree < OrgNode > =
    Tree::new ( fake_parent );
  let root_id : NodeId =
    tree . root () . id ();

  // Add AliasCol as child of fake parent
  let aliascol_id : NodeId = {
    let mut root_mut = tree . get_mut ( root_id ) . unwrap ();
    root_mut . append ( aliascol_orgnode ) . id () };

  // Copy aliases from paired tree
  let paired_aliascol_ref : NodeRef < (Option<SkgNode>, OrgNode) > =
    paired_tree . get ( aliascol_node_id )
    . ok_or ( "AliasCol not found in paired tree" ) ?;
  let child_orgnodes : Vec < OrgNode > =
    paired_aliascol_ref . children ()
    . map ( |c| c . value () . 1 . clone () )
    . collect ();
  { // append each one
    let mut aliascol_mut =
      tree . get_mut ( aliascol_id ) . unwrap ();
    for child in child_orgnodes {
      aliascol_mut . append ( child ); }}
  Ok ( tree ) }

/// Sync changes from the completed orgnode tree
/// back to the paired tree.
/// This updates the AliasCol's metadata and replaces its children.
fn sync_aliascol_changes_to_paired_tree (
  paired_tree          : &mut PairTree,
  pairtree_aliascol_id : NodeId,
  orgnode_tree         : &Tree < OrgNode >,
  orgtree_aliascol_id  : NodeId,
) -> Result < (), Box<dyn Error> > {
  // Get the completed AliasCol and its children from orgnode_tree
  let orgnode_aliascol_ref : NodeRef < OrgNode > =
    orgnode_tree . get ( orgtree_aliascol_id )
    . ok_or ( "AliasCol not found in orgnode_tree" ) ?;
  let new_children : Vec < OrgNode > =
    orgnode_aliascol_ref . children ()
    . map ( |c| c . value () . clone () )
    . collect ();

  { // Update the AliasCol's OrgNode in paired tree.
    let new_aliascol_orgnode : OrgNode =
      orgnode_aliascol_ref . value () . clone ();
    let mut paired_mut =
      paired_tree . get_mut ( pairtree_aliascol_id )
      . ok_or ( "AliasCol not found in paired tree" ) ?;
    paired_mut . value () . 1 = new_aliascol_orgnode; }
  { // Remove old children from paired tree
    let old_child_ids : Vec < NodeId > =
      collect_child_ids ( paired_tree, pairtree_aliascol_id ) ?;
    for child_id in old_child_ids {
      let mut child_mut = paired_tree . get_mut ( child_id )
        . ok_or ( "Child not found" ) ?;
      child_mut . detach (); }}
  { // Add new children
    // (with None for SkgNode, since aliases don't have SkgNodes)
    let mut paired_mut =
      paired_tree . get_mut ( pairtree_aliascol_id )
      . ok_or ( "AliasCol not found in paired tree" ) ?;
    for child in new_children {
      paired_mut . append ( (None, child) ); }}
  Ok (( )) }
