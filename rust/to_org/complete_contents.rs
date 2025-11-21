use crate::media::file_io::one_node::read_node;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::to_org::util::skgnode_and_orgnode_from_id;
use crate::to_org::aliases::wrapped_build_and_integrate_aliases_view;
use crate::to_org::complete_aliascol::completeAliasCol;
use crate::to_org::integrate_backpath::{
  wrapped_build_and_integrate_containerward_view,
  wrapped_build_and_integrate_sourceward_view, };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, RelToParent, ViewRequest};
use crate::util::path_from_pid_and_source;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// Calls 'complete_node_preorder' on each tree,
/// but threading 'visited' through the entire forest
/// (rather than restarting with an empty 'visited' set in each tree).
pub async fn completeOrgnodeForest (
  forest        : &mut Vec < Tree < OrgNode > >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let mut visited : HashSet < ID > =
    HashSet::new ();
  for tree in forest . iter_mut () {
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut ancestor_path : Vec < ID > =
      Vec::new ();
    complete_node_preorder (
      tree, root_id, config, typedb_driver,
      &mut visited, &mut ancestor_path, errors ) . await ?; }
  Ok (( )) }

/* Check if this node creates a cycle (its ID is in ancestor_path).
If so, mark it with cycle=true; otherwise mark it with cycle=false.
Then push this node's ID to ancestor_path (to be popped later by caller). */
fn detect_cycle_and_mark_if_so (
  tree          : &mut Tree < OrgNode >,
  node_id       : NodeId,
  ancestor_path : &mut Vec < ID >,
) -> Result < (), Box<dyn Error> > {
  let node_pid_opt : Option < ID > = {
    let node_ref : NodeRef < OrgNode > =
      tree . get ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    node_ref . value () . metadata . id . clone () };
  if let Some ( node_pid ) = node_pid_opt {
    let mut node_mut : ego_tree::NodeMut < OrgNode > =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    node_mut . value () . metadata . viewData.cycle =
      ancestor_path . contains ( & node_pid );
    ancestor_path . push ( node_pid ); }
  Ok (( )) }

/// Completes a node, and then its children ('preorder DFS traversal').
/// - For AliasCol nodes, delegate to 'completeAliasCol'
///   (which handles the whole subtree)
/// - For other nodes, these happen (in order):
///   - futz with the node itself
///     - check for repetition via 'make_indefinitive_if_repeated'
///       (if repeated, modify body and metadata, update 'visited')
///     - call detect_cycle_and_mark_if_so (marks cycle before recursing to children)
///   - futz with its descendents
///     - if definitive, call completeDefinitiveOrgnode
///       - else call clobberIndefinitiveOrgnode
///     - recurse via 'map_complete_node_preorder_over_children'
///     - integrate any view requests
/// .
/// NOTE: Processing view requests *after* completing children helps,
/// because if a content child added during completion
/// matches the head of the path, the path will be integrated there
/// (where treatment=Content), instead of creating a duplicate child
/// with treatment=ParentIgnores. */
fn complete_node_preorder<'a> (
  tree          : &'a mut Tree < OrgNode >,
  node_id       : ego_tree::NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  visited       : &'a mut HashSet < ID >,
  ancestor_path : &'a mut Vec < ID >, // path from root to current node (for cycle detection)
  errors        : &'a mut Vec < String >,
) -> Pin<Box<dyn Future<Output =
                        Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let treatment : RelToParent = {
      let node_ref : NodeRef < OrgNode > =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      let node : &OrgNode =
        node_ref . value ();
      node . metadata . code.relToParent . clone () };
    if treatment == RelToParent::AliasCol {
      completeAliasCol (
        tree, node_id, config, typedb_driver ). await ?;
      // Don't recurse; completeAliasCol handles the whole subtree.
    } else {
      { // futz with the OrgNode itself
        make_indefinitive_if_repeated (
          // Maybe mark indefinitive, maybe update 'visited'.
          tree, node_id, visited
        ). await ?;
        detect_cycle_and_mark_if_so (
          tree, node_id, ancestor_path ) ?; }

      { let is_indefinitive : bool = { // 'make_indefinitive_if_repeated' may have just changed this value.
          let node_ref : NodeRef < OrgNode > =
            tree . get ( node_id )
            . ok_or ( "Node not found in tree" ) ?;
          node_ref . value () . metadata . code.indefinitive };
        if is_indefinitive {
          clobberIndefinitiveOrgnode (
            tree, node_id, config, typedb_driver ). await ?;
        } else { // futz with the orgnode and its content children
          completeDefinitiveOrgnode (
            tree, node_id, config, typedb_driver ). await ?; }
        map_complete_node_preorder_over_children (
          // Always recurse to children, even for indefinitive nodes, since they may have children from (for instance) view requests.
          tree, node_id, config, typedb_driver,
          visited, ancestor_path, errors ) . await ?; }

      { // integrate view requests
        let view_requests : Vec < ViewRequest > = {
          let node_ref : NodeRef < OrgNode > =
            tree . get ( node_id )
            . ok_or ( "Node not found in tree" ) ?;
          node_ref . value () . metadata . code . viewRequests
            . iter () . cloned () . collect () };
        for request in view_requests {
          match request {
            ViewRequest::Aliases => {
              wrapped_build_and_integrate_aliases_view (
                tree, node_id, config, typedb_driver, errors )
                . await ?; },
            ViewRequest::Containerward => {
              wrapped_build_and_integrate_containerward_view (
                tree, node_id, config, typedb_driver, errors )
                . await ?; },
            ViewRequest::Sourceward => {
              wrapped_build_and_integrate_sourceward_view (
                tree, node_id, config, typedb_driver, errors )
                . await ?; }, }} }}
    Ok (( )) }) }

fn map_complete_node_preorder_over_children<'a> (
  tree          : &'a mut Tree < OrgNode >,
  node_id       : ego_tree::NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  visited       : &'a mut HashSet < ID >,
  ancestor_path : &'a mut Vec < ID >,
  errors        : &'a mut Vec < String >,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let child_ids : Vec < ego_tree::NodeId > = {
      let node_ref : NodeRef < OrgNode > =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      node_ref . children ()
        . map ( |c| c . id () )
        . collect () };
    for child_id in child_ids {
      let child_has_id : bool = {
        let child_ref : NodeRef < OrgNode > =
          tree . get ( child_id )
          . ok_or ( "Child not found in tree" ) ?;
        child_ref . value () . metadata . id . is_some () };
      complete_node_preorder (
        tree, child_id, config, typedb_driver,
        visited, ancestor_path, errors ) . await ?;
      if child_has_id {
        ancestor_path . pop (); }}
    Ok (( )) }) }

/// If this orgnode is a repeat, then modify its body and metadata
///   (but change no descendents).
/// If it is definitive, update 'visited' to include it.
pub async fn make_indefinitive_if_repeated (
  tree      : &mut Tree < OrgNode >,
  node_id   : NodeId,
  visited   : &mut HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  let (node_pid, is_indefinitive) : (ID, bool) = {
    let node_ref : NodeRef < OrgNode > =
      tree . get ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    ( node_ref . value () . metadata . id . clone ()
        . ok_or ( "Node has no ID" ) ?,
      node_ref . value () . metadata . code . indefinitive ) };
  if visited . contains ( & node_pid ) {
    // It is a repeat. Mark it as indefinitive.
    // Its children are neither removed nor completed,
    // although their children might be completed.
    let mut node_mut : NodeMut < OrgNode > =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found" ) ?;
    node_mut . value () . metadata . code . indefinitive = true;
    return Ok ( () ); }
  if ! is_indefinitive { // Only definitive nodes count as visited.
    visited . insert ( node_pid ); }
  Ok ( () ) }

/// Completes an indefinitive orgnode by fetching its canonical title and source from disk.
///
/// ASSUMES: Node has been normalized so its 'id' field is the PID
/// ASSUMES: make_indefinitive_if_repeated has already been called
/// ASSUMES: The input is indefinitive
///
/// METHOD: Given an indefinitive node N:
/// - Read N's SkgNode from disk
/// - Set orgnode's title to the canonical title from disk
/// - Set orgnode's source (a field of its metadata)
/// - Set orgnode's body to None
///
/// Note: Children are unaffected by this function,
/// and will be recursed into separately by the caller.
pub async fn clobberIndefinitiveOrgnode (
  tree      : &mut Tree < OrgNode >,
  node_id   : NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = {
    let node_ref : NodeRef < OrgNode > =
      tree . get ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    node_ref . value () . metadata . id . clone ()
      . ok_or ( "Node has no ID" ) ? };
  let (node_pid_resolved, node_source) : (ID, String) =
    pid_and_source_from_id(
      &config.db_name, driver, &node_pid).await?
    . ok_or_else( || format!(
      "Node ID '{}' not found in database", node_pid))?;
  let skgnode : SkgNode = {
    let path : String =
      path_from_pid_and_source (
        config, &node_source, node_pid_resolved );
    read_node ( path ) ? };
  { // Update its title and source, and clear its body.
    let mut node_mut : NodeMut < OrgNode > =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found" ) ?;
    node_mut . value () . title = skgnode . title;
    node_mut . value () . metadata . source = Some ( node_source );
    node_mut . value () . body = None; }

  Ok (( )) }

/// Completes a definitive Content node's children by reconciling them
/// with the 'contains' relationships found on disk.
///
/// ASSUMES: Node has been normalized so its 'id' field is the PID
/// ASSUMES: make_indefinitive_if_repeated has already been called
/// ASSUMES: Input is definitive
///
/// METHOD: Given a node N:
/// - Read N's SkgNode from disk
/// - Categorize N's children into content/non-content
/// - Mark 'invalid content' as parentIgnores and move to non-content
///   - 'invalid content' = nodes marked content that are not content.
///     This can happen if a separate buffer edited their container.
/// - Build completed-content list from disk, preserving order
/// - Add missing content children from disk (via extend_content)
/// - Reorder children: non-content first, then completed-content
pub async fn completeDefinitiveOrgnode (
  tree      : &mut Tree < OrgNode >,
  node_id   : NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  // Get node_pid
  let node_pid : ID = {
    let node_ref : NodeRef < OrgNode > =
      tree . get ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    node_ref . value () . metadata . id . clone ()
      . ok_or ( "Node has no ID" ) ? };
  // Query TypeDB for node PID and source
  let (node_pid_resolved, node_source) : (ID, String) =
    pid_and_source_from_id(
      &config.db_name, driver, &node_pid).await?
    . ok_or_else( || format!(
      "Node ID '{}' not found in database", node_pid))?;
  let mut skgnode : SkgNode = {
    let path : String =
      path_from_pid_and_source (
        config, &node_source, node_pid_resolved );
    read_node ( path ) ? };
  skgnode.source = node_source;
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
    let child_ref : NodeRef < OrgNode > =
      tree . get ( *child_id )
      . ok_or ( "Child node not found" ) ?;
    let child_node : &OrgNode =
      child_ref . value ();
    let child_pid : &ID =
      child_node . metadata . id . as_ref ()
      . ok_or ( "Content child has no ID" ) ?;
    content_id_to_node_id . insert (
      child_pid . clone (), *child_id ); }

  // Mark invalidated content as ParentIgnores
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
            tree, node_id, disk_id, config, driver ) . await ? ); }}}

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
  let node_ref : NodeRef < OrgNode > =
    tree . get ( node_id )
    . ok_or ( "Node not found in tree" ) ?;
  for child in node_ref . children () {
    let child_node : &OrgNode =
      child . value ();
    if child_node . metadata . code.relToParent
      == RelToParent::Content
    { content_child_ids . push ( child . id () );
    } else {
      non_content_child_ids . push ( child . id () ); }}
  Ok (( content_child_ids, non_content_child_ids )) }

/// Create a new Content node from disk (using 'disk_id')
/// and append it to the children of 'parent_nid'.
async fn extend_content (
  tree       : &mut Tree < OrgNode >,
  parent_nid : NodeId,
  id         : &ID,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let ( _skgnode, new_orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, id ) . await ?;
  let mut parent_mut : NodeMut < OrgNode > =
    tree . get_mut ( parent_nid )
    . ok_or ( "Parent node not found" ) ?;
  let new_child : NodeMut < OrgNode > =
    parent_mut . append ( new_orgnode );
  Ok ( new_child . id ( )) }

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
