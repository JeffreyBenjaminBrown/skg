use crate::file_io::read_node;
use crate::mk_org_text::content_view::{
  mk_repeated_orgnode_from_id,
  skgnode_and_orgnode_from_pid};
use crate::types::{ID, SkgConfig, SkgNode, OrgNode, RelToParent};
use crate::util::path_from_pid;
use ego_tree::{NodeId, NodeMut, Tree};
use std::collections::{HashSet, HashMap};
use std::error::Error;

/// Completes a Content node's children by reconciling them
/// with the 'contains' relationships found on disk.
///
/// ASSUMES it is called *after* saving,
/// so the disk state is the source of truth.
///
/// ASSUMES the node P has been normalized
/// so that its 'id' field is the PID (no need to call pid_from_id).
///
/// METHOD: Given a node N:
/// - Get N's ID (error if missing)
/// - If N's ID is in 'visited', replace N with a repeated marker and return
/// - Add N's ID to 'visited'.
/// - If N is indefinitive, do nothing and return (but repetition was still checked).
/// - Read N's SkgNode from disk
/// - Categorize N's children into content/non-content
/// - Change invalid content children to treatment=parentIgnores and move to the end of non-content.
/// - Build completed-content list from disk, preserving order.
/// - Reorder children: non-content first, then completed-content
/// Check for repetition and mark node if it's a repeat.
/// Returns true if node was marked as repeated (and should not be processed further).
pub fn check_and_mark_repetition (
  tree      : &mut Tree < OrgNode >,
  node_id   : NodeId,
  config    : &SkgConfig,
  visited   : &mut HashSet < ID >,
) -> Result < bool, Box<dyn Error> > {
  let node_pid : ID = {
    let node_ref : ego_tree::NodeRef < OrgNode > =
      tree . get ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    node_ref . value () . metadata . id . clone ()
      . ok_or ( "Node has no ID" ) ? };
  if visited . contains ( & node_pid ) {
    // If already visited, replace with repeated node and return true.
    let repeated_node : OrgNode =
      mk_repeated_orgnode_from_id (
        config, & node_pid ) ?;
    let has_focused : bool =
      subtree_contains_focused ( tree, node_id );
    replace_subtree_with_node (
      tree, node_id, repeated_node, has_focused ) ?;
    return Ok ( true ); }
  visited . insert ( node_pid );
  Ok ( false ) }

pub fn completeContents (
  tree      : &mut Tree < OrgNode >,
  node_id   : NodeId,
  config    : &SkgConfig,
  visited   : &mut HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  // ASSUMES: check_and_mark_repetition has already been called
  // Get node_pid and check if indefinitive
  let (node_pid, is_indefinitive) : (ID, bool) = {
    let node_ref : ego_tree::NodeRef < OrgNode > =
      tree . get ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    ( node_ref . value () . metadata . id . clone ()
        . ok_or ( "Node has no ID" ) ?,
      node_ref . value () . metadata . code.indefinitive ) };
  // Indefinitive nodes are not completed (though their children might be).
  if is_indefinitive { return Ok (( )); }
  let skgnode : SkgNode = {
    let path : String =
      path_from_pid ( config, node_pid . clone () );
    read_node ( path ) ? };
  let content_from_disk : HashSet < ID > =
    skgnode . contains . clone ()
    . unwrap_or_default ()
    . into_iter ()
    . collect ();

  let ( content_child_ids, mut non_content_child_ids )
    : ( Vec < NodeId >, Vec < NodeId > )
    = categorize_children_by_treatment (
      tree,
      node_id ) ?;

  // Validate content children have IDs
  // and build map from ID to NodeId
  let mut content_id_to_node_id : HashMap < ID, NodeId > =
    HashMap::new ();
  for child_id in & content_child_ids {
    let child_ref : ego_tree::NodeRef < OrgNode > =
      tree . get ( *child_id )
      . ok_or ( "Child node not found" ) ?;
    let child_node : &OrgNode =
      child_ref . value ();
    let child_pid : &ID =
      child_node . metadata . id . as_ref ()
      . ok_or ( "Content child has no ID" ) ?;
    content_id_to_node_id . insert (
      child_pid . clone (), *child_id ); }

  // Mark invalid content as ParentIgnores
  // and move to non-content list
  let mut invalid_content_ids : Vec < NodeId > =
    Vec::new ();
  for ( child_pid, child_node_id ) in & content_id_to_node_id {
    if ! content_from_disk . contains ( child_pid ) {
      invalid_content_ids . push ( *child_node_id ); }}
  for invalid_id in & invalid_content_ids {
    let mut child_mut : NodeMut < OrgNode > =
      tree . get_mut ( *invalid_id )
      . ok_or ( "Invalid content child not found" ) ?;
    child_mut . value () . metadata . code.relToParent =
      RelToParent::ParentIgnores;
    content_id_to_node_id . remove (
      & child_mut . value ()
        . metadata . id . clone () . unwrap () );
    non_content_child_ids . push ( *invalid_id ); }

  // Build completed-content list
  // preserving order from disk
  let mut completed_content_nodes : Vec < NodeId > =
    Vec::new ();
  if let Some(contains) = & skgnode . contains {
    for disk_id in contains {
      if let Some ( existing_node_id ) =
        content_id_to_node_id . get ( disk_id ) {
        // Content already exists in tree
        completed_content_nodes . push ( *existing_node_id );
      } else
      { // PITFALL: A preorder DFS traversal for completeOrgnodeTree
        // lets us add a child without considering grandchildren yet.
        completed_content_nodes . push (
          extend_content (
            tree, node_id, disk_id, config )? ); }}}

  reorder_children (
    tree,
    node_id,
    & non_content_child_ids,
    & completed_content_nodes ) ?;
  Ok (( )) }

/// Categorize a node's children into Content and non-Content.
/// Returns (content_child_ids, non_content_child_ids).
fn categorize_children_by_treatment (
  tree    : &Tree < OrgNode >,
  node_id : NodeId,
) -> Result < ( Vec < NodeId >, Vec < NodeId > ), Box<dyn Error> > {
  let mut content_child_ids : Vec < NodeId > =
    Vec::new ();
  let mut non_content_child_ids : Vec < NodeId > =
    Vec::new ();
  let node_ref : ego_tree::NodeRef < OrgNode > =
    tree . get ( node_id )
    . ok_or ( "Node not found in tree" ) ?;
  for child in node_ref . children () {
    let child_node : &OrgNode =
      child . value ();
    if child_node . metadata . code.relToParent == RelToParent::Content {
      content_child_ids . push ( child . id () );
    } else {
      non_content_child_ids . push ( child . id () ); }}
  Ok (( content_child_ids, non_content_child_ids )) }

/// Create a new Content node from disk and append it to a parent.
fn extend_content (
  tree      : &mut Tree < OrgNode >,
  parent_id : NodeId,
  disk_id   : &ID,
  config    : &SkgConfig,
) -> Result < NodeId, Box<dyn Error> > {
  let ( new_orgnode, _skgnode ) : ( OrgNode, SkgNode ) =
    skgnode_and_orgnode_from_pid (
      config, disk_id ) ?;
  let mut parent_mut : NodeMut < OrgNode > =
    tree . get_mut ( parent_id )
    . ok_or ( "Parent node not found" ) ?;
  let new_child : NodeMut < OrgNode > =
    parent_mut . append ( new_orgnode );
  Ok ( new_child . id ( )) }

/// Check if a subtree contains any focused node.
fn subtree_contains_focused (
  tree    : &Tree < OrgNode >,
  node_id : NodeId,
) -> bool {
  let node_ref : ego_tree::NodeRef < OrgNode > =
    match tree . get ( node_id ) {
      Some ( n ) => n,
      None => return false, };
  if node_ref . value () . metadata . viewData.focused {
    return true; }
  for child in node_ref . children () {
    if subtree_contains_focused ( tree, child . id () ) {
      return true; }}
  false }

/// Replace a subtree with a single node,
/// optionally transferring focus.
fn replace_subtree_with_node (
  tree           : &mut Tree < OrgNode >,
  old_node_id    : NodeId,
  mut new_node   : OrgNode,
  transfer_focus : bool,
) -> Result < (), Box<dyn Error> > {
  if transfer_focus {
    new_node . metadata . viewData.focused = true; }
  // Collect child IDs before mutating
  let child_ids : Vec < NodeId > = {
    let node_ref : ego_tree::NodeRef < OrgNode > =
      tree . get ( old_node_id )
      . ok_or ( "Old node not found" ) ?;
    node_ref . children ()
      . map ( |c| c . id () )
      . collect () };
  // Remove all children
  for child_id in child_ids {
    let mut child_mut : NodeMut < OrgNode > =
      tree . get_mut ( child_id )
      . ok_or ( "Child not found" ) ?;
    child_mut . detach (); }
  // Replace the node's value
  let mut old_node_mut : NodeMut < OrgNode > =
    tree . get_mut ( old_node_id )
    . ok_or ( "Old node not found" ) ?;
  * old_node_mut . value () = new_node;
  Ok (( )) }

/// Reorder a node's children by detaching all
/// and re-appending in desired order.
fn reorder_children (
  tree                      : &mut Tree < OrgNode >,
  parent_id                 : NodeId,
  non_content_child_ids     : &Vec < NodeId >,
  completed_content_node_ids : &Vec < NodeId >,
) -> Result < (), Box<dyn Error> > {
  // Collect all child NodeIds in desired order
  let mut desired_order : Vec < NodeId > =
    Vec::new ();
  desired_order . extend ( non_content_child_ids );
  desired_order . extend ( completed_content_node_ids );

  // Detach all children
  for child_id in & desired_order {
    let mut child_mut : NodeMut < OrgNode > =
      tree . get_mut ( *child_id )
      . ok_or ( "Child not found for detaching" ) ?;
    child_mut . detach (); }

  for child_id in & desired_order {
    // Re-append in desired order,
    // using append_id, which preserves entire subtrees.
    let mut parent_mut : NodeMut < OrgNode > =
      tree . get_mut ( parent_id )
      . ok_or ( "Parent not found" ) ?;
    parent_mut . append_id ( *child_id ); }
  Ok (( )) }
