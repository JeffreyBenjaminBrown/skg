/// SINGLE ENTRY POINT: 'completeAndRestoreForest_collectingViewRequests'.

use crate::to_org::util::{
  skgnode_and_orgnode_from_id, VisitedMap,
  get_pid_in_pairtree, is_indefinitive, collect_child_ids,
  mark_visited_or_repeat_or_cycle };
use crate::to_org::complete::aliascol::completeAliasCol;
use crate::media::file_io::skgnode_and_source_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, RelToParent, ViewRequest, default_metadata};
use crate::types::trees::{NodePair, PairTree};

use ego_tree::{NodeId, NodeMut, NodeRef};
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// CALLS 'completeAndRestoreNode_collectingViewRequests'
///   on each tree, and thus on each node in the forest,
/// threading 'visited' through the forest (rather than
/// restarting with an empty 'visited' set in each tree).
///
/// RETURNS (visited, view_requests).
pub async fn completeAndRestoreForest_collectingViewRequests (
  forest        : &mut Vec < PairTree >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
) -> Result < ( VisitedMap,
                Vec < // view_requests
                    ( usize, // which tree in the forest
                      NodeId,
                      ViewRequest ) > ),
              Box<dyn Error> > {
  let mut visited : VisitedMap = VisitedMap::new ();
  let mut view_requests : Vec < (usize, NodeId, ViewRequest) > = Vec::new ();
  for (tree_idx, tree) in forest . iter_mut () . enumerate () {
    let root_id : NodeId =
      tree . root () . id ();
    completeAndRestoreNode_collectingViewRequests (
      tree, root_id, tree_idx, config, typedb_driver,
      &mut visited, &mut view_requests ) . await ?; }
  Ok (( visited, view_requests )) }

/// Completes a node, and then its children ('preorder DFS traversal').
/// - For AliasCol nodes, delegate to 'completeAliasCol'
///   (which handles the whole subtree)
/// - For other nodes, these happen (in order):
///   - futz with the node itself
///     - check for repetition via 'mark_visited_or_repeat_or_cycle'
///       (if repeated, modify body and metadata, update 'visited')
///   - futz with its descendents
///     - if definitive, call completeDefinitiveOrgnode
///       - else call clobberIndefinitiveOrgnode
///     - recurse via 'map_completeAndRestoreNodeCollectingViewRequests_over_children'.
///     - collect all view requests for later processing
fn completeAndRestoreNode_collectingViewRequests<'a> (
  tree                 : &'a mut PairTree,
  node_id              : NodeId,
  tree_idx             : usize,
  config               : &'a SkgConfig,
  typedb_driver        : &'a TypeDBDriver,
  visited              : &'a mut VisitedMap,
  view_requests_out    : &'a mut Vec < (usize, NodeId, ViewRequest) >,
) -> Pin<Box<dyn Future<Output =
                        Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let treatment : RelToParent = {
      let node_ref : NodeRef < NodePair > =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      node_ref.value () . 1 . metadata.code.relToParent . clone () };
    if treatment == RelToParent::AliasCol {
      completeAliasCol (
        tree, node_id, config, typedb_driver ). await ?;
      // Don't recurse; completeAliasCol handles the whole subtree.
    } else if treatment == RelToParent::SubscribeeCol {
      // For now, SubscribeeCol has no children to process.
      // Future generations will add Subscribee children here.
    } else {
      ensure_skgnode (
        tree, node_id, config, typedb_driver ). await ?;
      mark_visited_or_repeat_or_cycle (
        tree, tree_idx, node_id, visited ) ?;
      { if is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveOrgnode (
            tree, node_id ) ?;
        } else { // futz with the orgnode and its content children
          completeDefinitiveOrgnode (
            tree, node_id, config, typedb_driver ). await ?; }
        map_completeAndRestoreNodeCollectingViewRequests_over_children (
          // Always recurse to children, even for indefinitive nodes, since they may have children from (for instance) view requests.
          tree, node_id, tree_idx, config, typedb_driver,
          visited, view_requests_out ) . await ?; }
      collect_view_requests (
        tree, node_id, tree_idx, view_requests_out ) ?; }
    Ok (( )) } ) }

/// Collect all view requests from a node and append them to the output vector.
fn collect_view_requests (
  tree              : &PairTree,
  node_id           : NodeId,
  tree_idx          : usize,
  view_requests_out : &mut Vec < (usize, NodeId, ViewRequest) >,
) -> Result < (), Box<dyn Error> > {
  let node_view_requests : Vec < ViewRequest > = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "collect_view_requests: node not found" ) ?;
    node_ref . value () . 1 . metadata . code . viewRequests
      . iter () . cloned () . collect () };
  for request in node_view_requests {
    view_requests_out . push ( (tree_idx, node_id, request) ); }
  Ok (( )) }

/// Completes an indefinitive orgnode using its SkgNode.
///
/// ASSUMES: Node has been normalized so its 'id' field is the PID
/// ASSUMES: ensure_skgnode has already been called
/// ASSUMES: mark_visited_or_repeat_or_cycle has already been called
/// ASSUMES: The input is indefinitive
///
/// METHOD: Given an indefinitive node N:
/// - Set orgnode's title to the canonical title from the SkgNode
/// - Set orgnode's source (a field of its metadata)
/// - Set orgnode's body to None
///
/// Note: Children are unaffected by this function,
/// and will be recursed into separately by the caller.
pub fn clobberIndefinitiveOrgnode (
  tree   : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  { // Update title, source, and clear body using the SkgNode
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found" ) ?;
    let (skgnode_opt, orgnode) = node_mut . value ();
    let skgnode : &SkgNode =
      skgnode_opt . as_ref ()
      . ok_or ( "SkgNode should exist after fetch" ) ?;
    orgnode . title = skgnode . title . clone ();
    orgnode . metadata . source =
      Some ( skgnode . source . clone () );
    orgnode . body = None; }
  Ok (( )) }

/// Completes a definitive Content node's children by reconciling them
/// with the 'contains' relationships found in the SkgNode.
///
/// ASSUMES: Node has been normalized so its 'id' field is the PID
/// ASSUMES: ensure_skgnode has already been called
/// ASSUMES: make_indefinitive_if_repeated has already been called
/// ASSUMES: Input is definitive
///
/// METHOD: Given a node N:
/// - Add SubscribeeCol if node subscribes to something and one is not already present
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
  maybe_add_subscribee_col ( tree, node_id ) ?;
  let (content_from_disk, contains_list)
    : (HashSet<ID>, Option<Vec<ID>>) = {
      let node_ref : NodeRef < NodePair > =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      let skgnode : &SkgNode =
        node_ref . value () . 0 . as_ref ()
        . ok_or ( "SkgNode should exist" ) ?;
      let content_from_disk : HashSet<ID> =
        skgnode . contains . clone ()
        . unwrap_or_default ()
        . into_iter ()
        . collect ();
      (content_from_disk, skgnode . contains . clone ()) };

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
    let child_ref : NodeRef < NodePair > =
      tree . get ( *child_id )
      . ok_or ( "Child node not found" ) ?;
    let child_pid : &ID =
      child_ref . value () . 1 . metadata . id . as_ref ()
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
    let mut child_mut : NodeMut < NodePair > =
      tree . get_mut ( *invalid_id )
      . ok_or ( "Invalid content child not found" ) ?;
    child_mut . value () . 1 . metadata . code.relToParent =
      RelToParent::ParentIgnores;
    content_id_to_node_id . remove (
      & child_mut . value () . 1
        . metadata . id . clone () . unwrap () );
    non_content_child_ids . push ( *invalid_id ); }

  // Build completed-content list
  // preserving order from SkgNode
  let mut completed_content_nodes : Vec < NodeId > =
    Vec::new ();
  if let Some(contains) = & contains_list {
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
    if child . value () . 1 . metadata . code.relToParent
      == RelToParent::Content
    { content_child_ids . push ( child . id () );
    } else {
      non_content_child_ids . push ( child . id () ); }}
  Ok (( content_child_ids, non_content_child_ids )) }

/// Create a new Content node from disk (using 'disk_id')
/// and append it to the children of 'parent_nid'.
async fn extend_content (
  tree       : &mut PairTree,
  parent_nid : NodeId,
  id         : &ID,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let ( skgnode, new_orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, id ) . await ?;
  let mut parent_mut : NodeMut < NodePair > =
    tree . get_mut ( parent_nid )
    . ok_or ( "Parent node not found" ) ?;
  let new_child : NodeMut < NodePair > =
    parent_mut . append ( (Some(skgnode), new_orgnode) );
  Ok ( new_child . id ( )) }

/// Reorder a node's children by detaching all
/// and re-appending in desired order.
fn reorder_children (
  tree                      : &mut PairTree,
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
    let mut child_mut : NodeMut < NodePair > =
      tree . get_mut ( *child_id )
      . ok_or ( "Child not found for detaching" ) ?;
    child_mut . detach (); }

  for child_id in & desired_order {
    // Re-append in desired order,
    // using append_id, which preserves entire subtrees.
    let mut parent_mut : NodeMut < NodePair > =
      tree . get_mut ( parent_id )
      . ok_or ( "Parent not found" ) ?;
    parent_mut . append_id ( *child_id ); }
  Ok (( )) }

fn map_completeAndRestoreNodeCollectingViewRequests_over_children<'a> (
  tree                 : &'a mut PairTree,
  node_id              : NodeId,
  tree_idx             : usize,
  config               : &'a SkgConfig,
  typedb_driver        : &'a TypeDBDriver,
  visited              : &'a mut VisitedMap,
  view_requests_out    : &'a mut Vec < (usize, NodeId, ViewRequest) >,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let child_ids : Vec < NodeId > =
      collect_child_ids ( tree, node_id ) ?;
    for child_id in child_ids {
      completeAndRestoreNode_collectingViewRequests (
        tree, child_id, tree_idx, config, typedb_driver,
        visited, view_requests_out ) . await ?; }
    Ok (( )) }) }

/// Ensure a node in a PairTree has an SkgNode.
/// If the node already has Some(skgnode), does nothing.
/// Otherwise fetches from disk and stores it.
pub async fn ensure_skgnode (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  let has_skgnode : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "ensure_skgnode: node not found" ) ?;
    node_ref . value () . 0 . is_some () };
  if ! has_skgnode {
    let (skgnode, _source) =
      skgnode_and_source_from_id (
        config, driver, &node_pid ) . await ?;
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "ensure_skgnode: node not found" ) ?;
    node_mut . value () . 0 = Some ( skgnode ); }
  Ok (( )) }

/// If the node: - subscribes to something
///              - is definitive
///              - doesn't have a SubscribeeCol child
/// then prepend a SubscribeeCol child,
/// with (for now) no children of its own.
pub fn maybe_add_subscribee_col (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  let is_indefinitive : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "maybe_add_subscribee_col: node not found" ) ?;
    node_ref . value () . 1 . metadata . code . indefinitive };
  if is_indefinitive { return Ok (( )); }
  let has_subscriptions : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "maybe_add_subscribee_col: node not found" ) ?;
    let skgnode : &SkgNode =
      node_ref . value () . 0 . as_ref ()
      . ok_or ( "maybe_add_subscribee_col: SkgNode should exist" ) ?;
    skgnode . subscribes_to . as_ref ()
      . map ( | v | ! v . is_empty () )
      . unwrap_or ( false ) };
  if ! has_subscriptions { return Ok (( )); }
  let has_subscribee_col : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "maybe_add_subscribee_col: node not found" ) ?;
    node_ref . children () . any ( | child |
      child . value () . 1 . metadata . code . relToParent
        == RelToParent::SubscribeeCol ) };
  if has_subscribee_col { return Ok (( )); }
  { // Prepend a subscribees collector
    let subscribee_col_orgnode : OrgNode = {
      let mut md = default_metadata ();
      md . code . relToParent = RelToParent::SubscribeeCol;
      OrgNode {
        metadata : md,
        title : "it subscribes to these" . to_string (),
        body : None, } };
    let mut node_mut : NodeMut < NodePair > = (
      tree . get_mut ( node_id )
        . ok_or ( "maybe_add_subscribee_col: node not found" )) ?;
    node_mut . prepend ( (None, subscribee_col_orgnode) ); }
  Ok (( )) }
