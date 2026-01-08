/// SINGLE ENTRY POINT: 'completeAndRestoreForest_collectingViewRequests'.

use crate::to_org::util::{
  skgnode_and_orgnode_from_id, VisitedMap,
  get_pid_in_pairtree, is_indefinitive, collect_child_treeids,
  mark_if_visited_or_repeat_or_cycle };
use crate::to_org::complete::aliascol::completeAliasCol;
use crate::to_org::complete::sharing::{
  maybe_add_subscribeeCol_branch };
use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, Interp, ViewRequest};
use crate::types::tree::{NodePair, PairTree};
use crate::types::tree::generic::{
  read_at_node_in_tree, write_at_node_in_tree, with_node_mut };

use ego_tree::{NodeId, NodeMut, NodeRef};
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// TRIVIAL: Just a wrapper for
///   'completeAndRestoreNode_collectingViewRequests',
/// which it calls on each "tree root" (child of the ForestRoot),
/// threading 'visited' through the forest.
pub async fn completeAndRestoreForest_collectingViewRequests (
  forest        : &mut PairTree,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
) -> Result < ( VisitedMap,
                Vec < ( NodeId, ViewRequest ) > ),
              Box<dyn Error> > {
  let mut visited : VisitedMap = VisitedMap::new ();
  let mut view_requests : Vec < (NodeId, ViewRequest) > = Vec::new ();
  let tree_root_ids : Vec < NodeId > =
    forest . root () . children ()
    . map ( |c| c . id () )
    . collect ();
  for tree_root_id in tree_root_ids {
    completeAndRestoreNode_collectingViewRequests (
      forest, tree_root_id, config, typedb_driver,
      &mut visited, &mut view_requests ) . await ?; }
  Ok (( visited, view_requests )) }

/// Completes a node, and then its children ('preorder DFS traversal').
/// - For most nodes, these happen (in order):
///   - Futz with the node itself
///     - Fetch a skgnode from disk if needed ('ensure_skgnode')
///     - Check for repetition via 'mark_if_visited_or_repeat_or_cycle'
///       (if repeated, modify body and metadata, update 'visited')
///   - Futz with its descendents
///     - If definitive, call completeDefinitiveOrgnode.
///         LOTS OF WORK happens here.
///       Else, call clobberIndefinitiveOrgnode.
///     - Recurse via 'map_completeAndRestoreNodeCollectingViewRequests_over_children'.
///     - Collect all view requests for later processing.
/// - For AliasCol nodes, this delegates to 'completeAliasCol'.
///   It handles the whole subtree, so don't recurse into children.
/// - For *Col Interps that don't generate instructions themselves,
///   merely recurse into children, otherwise ignoring.
fn completeAndRestoreNode_collectingViewRequests<'a> (
  tree                 : &'a mut PairTree,
  node_id              : NodeId,
  config               : &'a SkgConfig,
  typedb_driver        : &'a TypeDBDriver,
  visited              : &'a mut VisitedMap,
  view_requests    : &'a mut Vec < (NodeId, ViewRequest) >,
) -> Pin<Box<dyn Future<Output =
                        Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let treatment: Interp =
      read_at_node_in_tree(tree, node_id, |node| {
        node.orgnode.metadata.code.interp.clone() })?;
    if treatment == Interp::AliasCol {
      completeAliasCol (
        tree, node_id, config, typedb_driver ). await ?;
      // Don't recurse; completeAliasCol handles the whole subtree.
    } else if treatment == Interp::SubscribeeCol
           || treatment == Interp::HiddenOutsideOfSubscribeeCol
           || treatment == Interp::HiddenInSubscribeeCol {
      // Recurse into children to collect view requests (e.g. from Subscribee nodes) but don't try to complete the SubscribeeCol node itself.
      map_completeAndRestoreNodeCollectingViewRequests_over_children (
        tree, node_id, config, typedb_driver,
        visited, view_requests ) . await ?;
    } else {
      ensure_skgnode (
        tree, node_id, config, typedb_driver ). await ?;
      mark_if_visited_or_repeat_or_cycle (
        tree, node_id, visited ) ?;
      { if is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveOrgnode (
            tree, node_id ) ?;
        } else { // futz with the orgnode and its content children
          completeDefinitiveOrgnode (
            tree, node_id, config, typedb_driver ). await ?; }
        map_completeAndRestoreNodeCollectingViewRequests_over_children (
          // Always recurse to children, even for indefinitive nodes, since they may have children from (for instance) view requests.
          tree, node_id, config, typedb_driver,
          visited, view_requests ) . await ?; }
      view_requests_at_node (
        tree, node_id, view_requests ) ?; }
    Ok (( )) } ) }

fn map_completeAndRestoreNodeCollectingViewRequests_over_children<'a> (
  tree                 : &'a mut PairTree,
  node_id              : NodeId,
  config               : &'a SkgConfig,
  typedb_driver        : &'a TypeDBDriver,
  visited              : &'a mut VisitedMap,
  view_requests_out    : &'a mut Vec < (NodeId, ViewRequest) >,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let child_treeids : Vec < NodeId > =
      collect_child_treeids ( tree, node_id ) ?;
    for child_treeid in child_treeids {
      completeAndRestoreNode_collectingViewRequests (
        tree, child_treeid, config, typedb_driver,
        visited, view_requests_out ) . await ?; }
    Ok (( )) }) }

/// Append its view requests to the output vector.
fn view_requests_at_node (
  tree              : &PairTree,
  node_id           : NodeId,
  view_requests_out : &mut Vec < (NodeId, ViewRequest) >,
) -> Result < (), Box<dyn Error> > {
  let node_view_requests : Vec < ViewRequest > =
    read_at_node_in_tree (
      tree, node_id,
      ( |np| np . orgnode . metadata . code . viewRequests
        . iter () . cloned () . collect () )) ?;
  for request in node_view_requests {
    view_requests_out . push ( (node_id, request) ); }
  Ok (( )) }

/// Completes an indefinitive orgnode using its SkgNode.
/// Affects only the input node, not its tree-children.
///
/// ASSUMES: Node's 'id' field is its PID.
/// ASSUMES: The SkgNode at that tree node is accurate.
/// ASSUMES: The input is indefinitive.
///
/// METHOD: Given an indefinitive node N:
/// - Reset title.
/// - Reset source.
/// - Set body to None.
pub fn clobberIndefinitiveOrgnode (
  tree    : &mut PairTree,
  treeid : NodeId,
) -> Result < (), Box<dyn Error> > {
  write_at_node_in_tree ( tree, treeid, |pair| {
    let skgnode : &SkgNode =
      pair . mskgnode . as_ref ()
        . ok_or ("SkgNode should exist after fetch" . to_string() )?;
    pair.orgnode.title = skgnode.title.clone();
    pair.orgnode.metadata.source = Some(skgnode.source.clone());
    pair.orgnode.body = None;
    Ok::<(), String>(( ))
  } )? // before the '?' it's a nested Result: R<R<(),String>,String>
    . map_err( |e| -> Box<dyn Error> { e.into() } ) }

/// Completes a definitive Content node's children,
/// by reconciling them with the 'contains' relationships
/// found in the SkgNode.
///
/// ASSUMES: Node has been normalized so its "id" field is the PID
/// ASSUMES: the PairTree has a SkgNode here (via "ensure_skgnode")
/// ASSUMES: 'mark_if_visited_or_repeat_or_cycle' was already called
/// ASSUMES: Input is definitive
///
/// METHOD: Given a node N:
/// - Add SubscribeeCol if node subscribes to something and one is not already present
/// - If node is a Subscribee, and its predecessor Subscriber hides any of its content, then add a HiddenInSubscribeeCol
/// - Categorize N's children into content/non-content
/// - Mark 'invalid content' as parentIgnores and move to non-content
///   - 'invalid content' = nodes marked content that are not content.
///     This can happen if a separate buffer edited their container.
/// - Build completed-content list from SkgNode, preserving order
/// - Add missing content children from disk (via extend_content)
/// - Reorder children: non-content first, then completed-content
pub async fn completeDefinitiveOrgnode (
  tree   : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  maybe_add_subscribeeCol_branch (
    tree, node_id, config, driver ) . await ?;
  let (content_from_disk, contains_list)
    : (HashSet<ID>, Option<Vec<ID>>) = {
      let node_ref : NodeRef < NodePair > =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      let skgnode : &SkgNode =
        node_ref . value () . mskgnode . as_ref ()
        . ok_or ( "SkgNode should exist" ) ?;
      let content_from_disk : HashSet<ID> =
        skgnode . contains . clone ()
        . unwrap_or_default ()
        . into_iter ()
        . collect ();
      (content_from_disk, skgnode . contains . clone ()) };

  let ( content_child_treeids, mut non_content_child_treeids )
    : ( Vec < NodeId >, Vec < NodeId > )
    = categorize_children_by_treatment (
      tree,
      node_id ) ?;

  // Validate content children have IDs
  // and build map from skg ID to tree NodeId
  let mut content_skgid_to_treeid : HashMap < ID, NodeId > =
    HashMap::new ();
  for child_treeid in & content_child_treeids {
    let child_ref : NodeRef < NodePair > =
      tree . get ( *child_treeid )
      . ok_or ( "Child node not found" ) ?;
    let child_pid : &ID =
      child_ref . value () . orgnode . metadata . id . as_ref ()
      . ok_or ( "Content child has no ID" ) ?;
    content_skgid_to_treeid . insert (
      child_pid . clone (), *child_treeid ); }

  // Mark invalidated content as ParentIgnores
  // and move to non-content list
  let mut invalid_content_treeids : Vec < NodeId > =
    Vec::new ();
  for ( child_pid, child_treeid ) in & content_skgid_to_treeid {
    if ! content_from_disk . contains ( child_pid ) {
      invalid_content_treeids . push ( *child_treeid ); }}
  for invalid_treeid in & invalid_content_treeids {
    let mut child_mut : NodeMut < NodePair > =
      tree . get_mut ( *invalid_treeid )
      . ok_or ( "Invalid content child not found" ) ?;
    child_mut . value () . orgnode . metadata . code.interp =
      Interp::ParentIgnores;
    content_skgid_to_treeid . remove (
      & child_mut . value () . orgnode
        . metadata . id . clone () . unwrap () );
    non_content_child_treeids . push ( *invalid_treeid ); }

  // Build completed-content list
  // preserving order from SkgNode
  let mut completed_content_treeids : Vec < NodeId > =
    Vec::new ();
  if let Some(contains) = & contains_list {
    for disk_skgid in contains {
      if let Some ( existing_treeid ) =
        content_skgid_to_treeid . get ( disk_skgid ) {
        // Content already exists in tree
        completed_content_treeids . push ( *existing_treeid );
      } else
      { // PITFALL: A preorder DFS traversal for completeOrgnodeTree
        // lets us add a child without considering grandchildren yet.
        completed_content_treeids . push (
          extend_content (
            tree, node_id, disk_skgid, config, driver
          ) . await ? ); }} }

  reorder_children (
    tree,
    node_id,
    & non_content_child_treeids,
    & completed_content_treeids ) ?;
  Ok (( )) }

/// Categorize a node's children into Content and non-Content.
/// Returns (content_child_ids, non_content_child_ids).
fn categorize_children_by_treatment (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ( Vec < NodeId >, Vec < NodeId > ), Box<dyn Error> > {
  let mut content_child_ids : Vec < NodeId > =
    Vec::new ();
  let mut non_content_child_ids : Vec < NodeId > =
    Vec::new ();
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "Node not found in tree" ) ?;
  for child in node_ref . children () {
    if child . value () . orgnode . metadata . code . interp
      == Interp::Content
    { content_child_ids . push ( child . id () );
    } else {
      non_content_child_ids . push ( child . id () ); }}
  Ok (( content_child_ids, non_content_child_ids )) }

/// Create a new Content node from disk (using 'disk_id')
/// and append it to the children of 'parent_nid'.
async fn extend_content (
  tree       : &mut PairTree,
  parent_nid : NodeId,
  skgid     : &ID,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let ( skgnode, new_orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, skgid ) . await ?;
  let new_child_id : NodeId = with_node_mut (
    tree, parent_nid,
    ( |mut parent_mut|
      parent_mut . append ( NodePair { mskgnode    : Some(skgnode),
                                       orgnode     : new_orgnode,
                                       new_orgnode : None } )
      . id () )) ?;
  Ok ( new_child_id ) }

/// Reorder a node's children by detaching all
/// and re-appending in desired order.
fn reorder_children (
  tree                       : &mut PairTree,
  parent_treeid              : NodeId,
  non_content_child_treeids  : &Vec < NodeId >,
  completed_content_treeids  : &Vec < NodeId >,
) -> Result < (), Box<dyn Error> > {
  // Collect all child NodeIds in desired order
  let mut desired_order : Vec < NodeId > =
    Vec::new ();
  desired_order . extend ( non_content_child_treeids );
  desired_order . extend ( completed_content_treeids );

  // Detach all children
  for child_treeid in & desired_order {
    with_node_mut ( tree, *child_treeid,
                    |mut child_mut| child_mut . detach () ) ?; }

  for child_treeid in & desired_order {
    // Re-append in desired order,
    // using append_id, which preserves entire subtrees.
    with_node_mut (
      tree, parent_treeid,
      |mut parent_mut| {
        parent_mut . append_id ( *child_treeid ); } ) ?; }
  Ok (( )) }

/// Ensure a node in a PairTree has a SkgNode.
/// If the node already has Some(skgnode), does nothing.
/// Otherwise fetches from disk and stores it.
pub async fn ensure_skgnode (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  let has_skgnode : bool =
    read_at_node_in_tree (
      tree, node_id,
      |np| np . mskgnode . is_some () ) ?;
  if ! has_skgnode {
    let skgnode : SkgNode =
      skgnode_from_id (
        config, driver, &node_pid ) . await ?;
    write_at_node_in_tree (
      tree, node_id,
      |np| np . mskgnode = Some ( skgnode ) ) ?; }
  Ok (( )) }

/// Ensure a node in a PairTree has a source in its metadata.
/// If the node already has Some(source), does nothing.
/// Otherwise fetches from TypeDB and stores it.
pub async fn ensure_source (
  tree    : &mut PairTree,
  node_id : NodeId,
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let has_source : bool =
    read_at_node_in_tree (
      tree, node_id,
      |np| np . orgnode . metadata . source . is_some () ) ?;
  if ! has_source {
    let node_pid : ID =
      get_pid_in_pairtree ( tree, node_id ) ?;
    let (_pid, source) : (ID, String) =
      pid_and_source_from_id (
        db_name, driver, &node_pid ) . await ?
      . ok_or_else ( || format! (
        "ensure_source: could not find source for ID {:?}",
        node_pid ) ) ?;
    write_at_node_in_tree (
      tree, node_id,
      |np| np . orgnode . metadata . source = Some (source) ) ?; }
  Ok (( )) }
