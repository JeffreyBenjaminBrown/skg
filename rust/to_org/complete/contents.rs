/// SINGLE ENTRY POINT: 'completeOrgnodeForest_collectingDefinitiveRequests'.

use crate::to_org::util::{
  skgnode_and_orgnode_from_id, VisitedMap, is_ancestor_id,
  get_pid_in_pairtree, is_indefinitive, collect_child_ids };
use crate::to_org::expand::aliases::wrapped_build_and_integrate_aliases_view;
use crate::to_org::complete::aliascol::completeAliasCol;
use crate::to_org::expand::backpath::{
  wrapped_build_and_integrate_containerward_view,
  wrapped_build_and_integrate_sourceward_view, };
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

/// CALLS 'completeNodePreorder_collectingDefinitiveRequests' on each tree,
/// threading 'visited' through the forest (rather than
/// restarting with an empty 'visited' set in each tree).
///
/// RETURNS (visited, definitive_requests).
pub async fn completeOrgnodeForest_collectingDefinitiveRequests (
  forest        : &mut Vec < PairTree >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < ( VisitedMap,
                Vec < // definitive_requests
                    ( usize, // which tree in the forest
                      NodeId ) > ),
              Box<dyn Error> > {
  let mut visited : VisitedMap = VisitedMap::new ();
  let mut definitive_requests : Vec < (usize, NodeId) > = Vec::new ();
  for (tree_idx, tree) in forest . iter_mut () . enumerate () {
    let root_id : NodeId =
      tree . root () . id ();
    completeNodePreorder_collectingDefinitiveRequests (
      tree, root_id, tree_idx, config, typedb_driver,
      &mut visited, &mut definitive_requests,
      errors ) . await ?; }
  Ok (( visited, definitive_requests )) }

/// Completes a node, and then its children ('preorder DFS traversal').
/// - For AliasCol nodes, delegate to 'completeAliasCol'
///   (which handles the whole subtree)
/// - For other nodes, these happen (in order):
///   - futz with the node itself
///     - check for repetition via 'make_indefinitive_if_repeated'
///       (if repeated, modify body and metadata, update 'visited')
///     - detect cycles via 'is_ancestor_id'
///   - futz with its descendents
///     - if definitive, call completeDefinitiveOrgnode
///       - else call clobberIndefinitiveOrgnode
///     - recurse via 'map_completeNodePreorderCollectingDefinitiveRequests_over_children'
///     - integrate any view requests except definitive view ones
///     - collect definitive view requests
fn completeNodePreorder_collectingDefinitiveRequests<'a> (
  tree                 : &'a mut PairTree,
  node_id              : NodeId,
  tree_idx             : usize,
  config               : &'a SkgConfig,
  typedb_driver        : &'a TypeDBDriver,
  visited              : &'a mut VisitedMap,
  definitive_requests  : &'a mut Vec < (usize, NodeId) >, // collected for processing after traversal
  errors               : &'a mut Vec < String >,
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
      { // futz with the OrgNode itself
        make_indefinitive_if_repeated (
          // Maybe mark indefinitive, maybe update 'visited'.
          tree, node_id, tree_idx, visited
        ). await ?;
        detect_cycle_and_mark (
          tree, node_id ) ?; }

      { // 'make_indefinitive_if_repeated' may have just changed this value.
        if is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveOrgnode (
            tree, node_id, config, typedb_driver ). await ?;
        } else { // futz with the orgnode and its content children
          completeDefinitiveOrgnode (
            tree, node_id, config, typedb_driver ). await ?; }
        map_completeNodePreorderCollectingDefinitiveRequests_over_children (
          // Always recurse to children, even for indefinitive nodes, since they may have children from (for instance) view requests.
          tree, node_id, tree_idx, config, typedb_driver,
          visited, definitive_requests, errors ) . await ?; }

      { /* Integrate view requests.
           PITFALL: Do this *after* completing children, because
           if a content child added during completion
           matches the head of the path, then
           the path will be integrated there (where treatment=Content),
           instead of creating a duplicate child
           with treatment=ParentIgnores. */
        let view_requests : Vec < ViewRequest > = {
          let node_ref : NodeRef < NodePair > =
            tree . get ( node_id )
            . ok_or ( "Node not found in tree" ) ?;
          node_ref . value () . 1 . metadata . code . viewRequests
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
                . await ?; },
            ViewRequest::Definitive => { // Collect for processing after all trees are traversed, so that `visited` is fully populated when they are traveresed.
              definitive_requests . push ( (tree_idx, node_id) );
            }, }} }}
    Ok (( )) } ) }

/// If this orgnode is a repeat, then modify its body and metadata
///   (but change no descendents).
/// If it is definitive, update 'visited' to include it.
pub async fn make_indefinitive_if_repeated (
  tree      : &mut PairTree,
  node_id   : NodeId,
  tree_idx  : usize,
  visited   : &mut VisitedMap,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  let is_indefinitive : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    node_ref . value () . 1 . metadata . code . indefinitive };
  if visited . contains_key ( & node_pid ) {
    // It is a repeat. Mark it as indefinitive.
    // Its children are neither removed nor completed,
    // although their children might be completed.
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found" ) ?;
    node_mut . value () . 1 . metadata . code . indefinitive = true;
    return Ok ( () ); }
  if ! is_indefinitive { // Only definitive nodes count as visited.
    visited . insert ( node_pid, (tree_idx, node_id) ); }
  Ok ( () ) }

/// Check if this node creates a cycle (its ID is an ancestor).
/// If so, mark it with cycle=true; otherwise mark it with cycle=false.
fn detect_cycle_and_mark (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID =
    get_pid_in_pairtree ( tree, node_id ) ?;
  let is_cycle : bool = is_ancestor_id (
    tree, node_id, &node_pid ) ?;
  let mut node_mut : NodeMut < NodePair > =
    tree . get_mut ( node_id )
    . ok_or ( "Node not found in tree" ) ?;
  node_mut . value () . 1 . metadata . viewData . cycle =
    is_cycle;
  Ok (( )) }

/// Completes an indefinitive orgnode by fetching its canonical title and source from disk.
///
/// ASSUMES: Node has been normalized so its 'id' field is the PID
/// ASSUMES: make_indefinitive_if_repeated has already been called
/// ASSUMES: The input is indefinitive
///
/// METHOD: Given an indefinitive node N:
/// - If N has no SkgNode, fetch from disk and store it
/// - Set orgnode's title to the canonical title from the SkgNode
/// - Set orgnode's source (a field of its metadata)
/// - Set orgnode's body to None
///
/// Note: Children are unaffected by this function,
/// and will be recursed into separately by the caller.
pub async fn clobberIndefinitiveOrgnode (
  tree   : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  ensure_skgnode ( tree, node_id, config, driver ) . await ?;
  { // Update title, source, and clear body using the SkgNode
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found" ) ?;
    let (skgnode_opt, orgnode) = node_mut . value ();
    let skgnode : &SkgNode =
      skgnode_opt . as_ref ()
      . ok_or ( "SkgNode should exist after fetch" ) ?;
    orgnode . title = skgnode . title . clone ();
    orgnode . metadata . source = Some ( skgnode . source . clone () );
    orgnode . body = None; }
  Ok (( )) }

/// Completes a definitive Content node's children by reconciling them
/// with the 'contains' relationships found in the SkgNode.
///
/// ASSUMES: Node has been normalized so its 'id' field is the PID
/// ASSUMES: make_indefinitive_if_repeated has already been called
/// ASSUMES: Input is definitive
///
/// METHOD: Given a node N:
/// - Get N's SkgNode (fetch from disk if needed)
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
  ensure_skgnode ( tree, node_id, config, driver ) . await ?;
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

fn map_completeNodePreorderCollectingDefinitiveRequests_over_children<'a> (
  tree                 : &'a mut PairTree,
  node_id              : NodeId,
  tree_idx             : usize,
  config               : &'a SkgConfig,
  typedb_driver        : &'a TypeDBDriver,
  visited              : &'a mut VisitedMap,
  definitive_requests  : &'a mut Vec < (usize, NodeId) >,
  errors               : &'a mut Vec < String >,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let child_ids : Vec < NodeId > =
      collect_child_ids ( tree, node_id ) ?;
    for child_id in child_ids {
      completeNodePreorder_collectingDefinitiveRequests (
        tree, child_id, tree_idx, config, typedb_driver,
        visited, definitive_requests, errors ) . await ?; }
    Ok (( )) }) }

/// Ensure a node in a PairTree has an SkgNode.
/// If the node already has Some(skgnode), does nothing.
/// Otherwise fetches from disk and stores it.
async fn ensure_skgnode (
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
    let (skgnode, _orgnode) : (SkgNode, OrgNode) =
      skgnode_and_orgnode_from_id ( config, driver, &node_pid ) . await ?;
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "ensure_skgnode: node not found" ) ?;
    node_mut . value () . 0 = Some ( skgnode ); }
  Ok (( )) }

/// If node subscribes to something
/// *and* doesn't have a SubscribeeCol child,
/// then prepend one. The SubscribeeCol has no children (for now).
fn maybe_add_subscribee_col (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
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
