/// SINGLE ENTRY POINT: 'completeAndRestoreForest_collectingViewRequests'.

use crate::to_org::util::{
  skgnode_and_orgnode_from_id, VisitedMap,
  get_pid_in_pairtree, is_indefinitive, collect_child_tree_ids,
  mark_if_visited_or_repeat_or_cycle };
use crate::to_org::complete::aliascol::completeAliasCol;
use crate::media::file_io::skgnode_and_source_from_id;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::media::typedb::search::{
  partition_subscribee_content_for_subscriber,
  what_node_hides,
  what_nodes_contain };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, Interp, ViewRequest, default_metadata};
use crate::types::trees::{NodePair, PairTree, read_at_node_in_tree};

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
        node.orgnode.metadata.code.interp.clone()
      })?;
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
      collect_view_requests (
        tree, node_id, view_requests ) ?; }
    Ok (( )) } ) }

/// Collect all view requests from a node and append them to the output vector.
fn collect_view_requests (
  tree              : &PairTree,
  node_id           : NodeId,
  view_requests_out : &mut Vec < (NodeId, ViewRequest) >,
) -> Result < (), Box<dyn Error> > {
  let node_view_requests : Vec < ViewRequest > = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "collect_view_requests: node not found" ) ?;
    node_ref . value () . orgnode . metadata . code . viewRequests
      . iter () . cloned () . collect () };
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
  tree   : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  { // Update title, source. Clear body. Get data from the SkgNode.
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found" ) ?;
    let pair = node_mut . value ();
    let skgnode : &SkgNode =
      pair . mskgnode . as_ref ()
      . ok_or ( "SkgNode should exist after fetch" ) ?;
    pair . orgnode . title =
      skgnode . title . clone ();
    pair . orgnode . metadata . source =
      Some ( skgnode . source . clone () );
    pair . orgnode . body = None; }
  Ok (( )) }

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
  maybe_add_subscribee_col (
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

  let ( content_child_tree_ids, mut non_content_child_tree_ids )
    : ( Vec < NodeId >, Vec < NodeId > )
    = categorize_children_by_treatment (
      tree,
      node_id ) ?;

  // Validate content children have IDs
  // and build map from skg ID to tree NodeId
  let mut content_skg_id_to_tree_id : HashMap < ID, NodeId > =
    HashMap::new ();
  for child_tree_id in & content_child_tree_ids {
    let child_ref : NodeRef < NodePair > =
      tree . get ( *child_tree_id )
      . ok_or ( "Child node not found" ) ?;
    let child_pid : &ID =
      child_ref . value () . orgnode . metadata . id . as_ref ()
      . ok_or ( "Content child has no ID" ) ?;
    content_skg_id_to_tree_id . insert (
      child_pid . clone (), *child_tree_id ); }

  // Mark invalidated content as ParentIgnores
  // and move to non-content list
  let mut invalid_content_tree_ids : Vec < NodeId > =
    Vec::new ();
  for ( child_pid, child_tree_id ) in & content_skg_id_to_tree_id {
    if ! content_from_disk . contains ( child_pid ) {
      invalid_content_tree_ids . push ( *child_tree_id ); }}
  for invalid_tree_id in & invalid_content_tree_ids {
    let mut child_mut : NodeMut < NodePair > =
      tree . get_mut ( *invalid_tree_id )
      . ok_or ( "Invalid content child not found" ) ?;
    child_mut . value () . orgnode . metadata . code.interp =
      Interp::ParentIgnores;
    content_skg_id_to_tree_id . remove (
      & child_mut . value () . orgnode
        . metadata . id . clone () . unwrap () );
    non_content_child_tree_ids . push ( *invalid_tree_id ); }

  // Build completed-content list
  // preserving order from SkgNode
  let mut completed_content_tree_ids : Vec < NodeId > =
    Vec::new ();
  if let Some(contains) = & contains_list {
    for disk_skg_id in contains {
      if let Some ( existing_tree_id ) =
        content_skg_id_to_tree_id . get ( disk_skg_id ) {
        // Content already exists in tree
        completed_content_tree_ids . push ( *existing_tree_id );
      } else
      { // PITFALL: A preorder DFS traversal for completeOrgnodeTree
        // lets us add a child without considering grandchildren yet.
        completed_content_tree_ids . push (
          extend_content (
            tree, node_id, disk_skg_id, config, driver
          ) . await ? ); }} }

  reorder_children (
    tree,
    node_id,
    & non_content_child_tree_ids,
    & completed_content_tree_ids ) ?;
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
  skg_id     : &ID,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let ( skgnode, new_orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, skg_id ) . await ?;
  let mut parent_mut : NodeMut < NodePair > =
    tree . get_mut ( parent_nid )
    . ok_or ( "Parent node not found" ) ?;
  let new_child : NodeMut < NodePair > =
    parent_mut . append ( NodePair { mskgnode: Some(skgnode),
                                     orgnode: new_orgnode } );
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
  config               : &'a SkgConfig,
  typedb_driver        : &'a TypeDBDriver,
  visited              : &'a mut VisitedMap,
  view_requests_out    : &'a mut Vec < (NodeId, ViewRequest) >,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let child_tree_ids : Vec < NodeId > =
      collect_child_tree_ids ( tree, node_id ) ?;
    for child_tree_id in child_tree_ids {
      completeAndRestoreNode_collectingViewRequests (
        tree, child_tree_id, config, typedb_driver,
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
    node_ref . value () . mskgnode . is_some () };
  if ! has_skgnode {
    let (skgnode, _source) =
      skgnode_and_source_from_id (
        config, driver, &node_pid ) . await ?;
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "ensure_skgnode: node not found" ) ?;
    node_mut . value () . mskgnode = Some ( skgnode ); }
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
  let has_source : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "ensure_source: node not found" ) ?;
    node_ref . value () . orgnode . metadata . source . is_some () };
  if ! has_source {
    let node_pid : ID =
      get_pid_in_pairtree ( tree, node_id ) ?;
    let (_pid, source) : (ID, String) =
      pid_and_source_from_id (
        db_name, driver, &node_pid ) . await ?
      . ok_or_else ( || format! (
        "ensure_source: could not find source for ID {:?}",
        node_pid ) ) ?;
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "ensure_source: node not found" ) ?;
    node_mut . value () . orgnode . metadata . source =
      Some ( source ); }
  Ok (( )) }

/// If the node:
///   - subscribes to something
///   - is definitive
///   - doesn't have a SubscribeeCol child
/// then prepend a SubscribeeCol child containing:
///   - for each subscribee, an indefinitive Subscribee child
///   - if any hidden nodes are outside subscribee content,
///     a HiddenOutsideOfSubscribeeCol
pub async fn maybe_add_subscribee_col (
  tree    : &mut PairTree,
  node_id : NodeId, // if applicable, this is the subscriber
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let is_indefinitive : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "maybe_add_subscribee_col: node not found" ) ?;
    node_ref . value () . orgnode . metadata . code . indefinitive };
  if is_indefinitive { return Ok (( )); }
  let ( subscriber_pid, subscribee_ids ) : ( ID, Vec < ID > ) = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "maybe_add_subscribee_col: node not found" ) ?;
    let skgnode : &SkgNode =
      node_ref . value () . mskgnode . as_ref ()
      . ok_or ( "maybe_add_subscribee_col: SkgNode should exist" ) ?;
    ( skgnode . ids [ 0 ] . clone (),
      skgnode . subscribes_to . clone () . unwrap_or_default () ) };
  if subscribee_ids . is_empty () { return Ok (( )); }
  let has_subscribee_col : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "maybe_add_subscribee_col: node not found" ) ?;
    node_ref . children () . any ( | child |
      child . value () . orgnode . metadata . code . interp
        == Interp::SubscribeeCol ) };
  if has_subscribee_col { return Ok (( )); }

  let hidden_outside_content : HashSet < ID > = {
    // hidden IDs that are outside all subscribee content
    let r_hides : HashSet < ID > =
      what_node_hides (
        &config.db_name, driver, & subscriber_pid ) . await ?;
    let all_subscribee_content : HashSet < ID > =
      what_nodes_contain (
        &config.db_name, driver, & subscribee_ids ) . await ?;
    r_hides . iter ()
      . filter ( | id | ! all_subscribee_content . contains ( id ) )
      . cloned () . collect () };

  let subscribee_col_nid : NodeId = { // the subscribee collector
    let subscribee_col_orgnode : OrgNode = { // build it
      let mut md = default_metadata ();
      md . code . interp = Interp::SubscribeeCol;
      OrgNode {
        metadata : md,
        title : "it subscribes to these" . to_string (),
        body : None, } };
    { // prepend it among the branches
      let mut node_mut : NodeMut < NodePair > = (
        tree . get_mut ( node_id )
          . ok_or ( "maybe_add_subscribee_col: node not found" )) ?;
      node_mut
        . prepend ( NodePair { mskgnode: None,
                               orgnode: subscribee_col_orgnode } )
        . id () }};

  if ! hidden_outside_content . is_empty () {
    // the HiddenOutsideOfSubscribeeCol
    let hidden_outside_col_nid : NodeId = {
      let hidden_outside_col_orgnode : OrgNode = { // build it
        let mut md = default_metadata ();
        md . code . interp = Interp::HiddenOutsideOfSubscribeeCol;
        OrgNode {
          metadata : md,
          title : "hidden from all subscriptions" . to_string (),
          body : None, }};
      { // append it
        let mut col_mut : NodeMut < NodePair > =
          tree . get_mut ( subscribee_col_nid ) . ok_or (
            "maybe_add_subscribee_col: SubscribeeCol not found" )?;
        col_mut
          . append ( NodePair { mskgnode: None,
                                orgnode: hidden_outside_col_orgnode })
          . id () }};
    { // its children, HiddenFromSubscribees (although since no subscribee contains them, they are not actually hidden anywhere)
      for hidden_id in hidden_outside_content {
        let ( skgnode, mut orgnode ) : ( SkgNode, OrgNode ) =
          skgnode_and_orgnode_from_id (
            config, driver, & hidden_id ) . await ?;
        orgnode . metadata . code . interp =
          Interp::HiddenFromSubscribees;
        orgnode . metadata . code . indefinitive = true;
        orgnode . body = None;
        let mut hidden_col_mut : NodeMut < NodePair > =
          tree . get_mut ( hidden_outside_col_nid )
          . ok_or ( "maybe_add_subscribee_col: HiddenOutsideOfSubscribeeCol not found" ) ?;
        hidden_col_mut . append (
          NodePair { mskgnode: Some(skgnode), orgnode } ); }} }
  { // The subscribees. These are indefinitive leaves (trivial 'branches'). They can be expanded by requesting definitive expansion, the same way one would do for ordinary content.
    let mut col_mut : NodeMut < NodePair > =
      tree . get_mut ( subscribee_col_nid )
      . ok_or (
        "maybe_add_subscribee_col: SubscribeeCol not found" )?;
    for subscribee_id in subscribee_ids {
      let subscribee_orgnode : OrgNode = {
        let mut md = default_metadata ();
        md . id = Some ( subscribee_id . clone () );
        md . code . interp = Interp::Subscribee;
        md . code . indefinitive = true;
        OrgNode {
          metadata : md,
          title : subscribee_id . 0, // Use the ID as the title
          body : None, } };
      col_mut . append ( NodePair { mskgnode: None,
                                    orgnode: subscribee_orgnode } ); }}
  Ok (( )) }

/// If this node is a Subscribee,
/// and the corresponding subscriber hides any of its content,
/// then prepend a HiddenInSubscribeeCol to hold those hidden nodes.
/// The subscriber is the Subscribee's grandparent:
///   subscriber -> SubsribeeCol -> Subscribee
pub async fn maybe_add_hidden_in_subscribee_col (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { // error if not a Subscribee
    let is_subscribee: bool =
      read_at_node_in_tree(tree, node_id, |node| {
        node.orgnode.metadata.code.interp == Interp::Subscribee
      })?;
    if ! is_subscribee { return Err (
      "maybe_add_hidden_in_subscribee_col called on non-subscribee"
        . into () ); }}
  { // Do nothing if it already has a HiddenInSubscribeeCol.
    let has_hidden_col : bool = {
      let node_ref : NodeRef < NodePair > =
        tree . get ( node_id ) . ok_or (
          "maybe_add_hidden_in_subscribee_col: node not found" ) ?;
      node_ref . children () . any (
        | child | child . value () . orgnode . metadata . code . interp
          == Interp::HiddenInSubscribeeCol ) };
    if has_hidden_col { return Ok (( )); }}
  let ( subscribee_pid, subscriber_pid ) : ( ID, ID ) = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id ) . ok_or (
        "maybe_add_hidden_in_subscribee_col: node not found" ) ?;
    let subscribee_pid : ID =
      node_ref . value () . orgnode . metadata . id . clone ()
      . ok_or ( "Subscribee has no ID" ) ?;
    let parent_ref : NodeRef < NodePair > =
      node_ref . parent ()
      . ok_or ( "Subscribee has no parent (SubscribeeCol)" ) ?;
    if parent_ref . value () . orgnode . metadata . code . interp
      != Interp::SubscribeeCol {
        return Err ( "Subscribee's parent is not a SubscribeeCol" .
                      into () ); }
    let grandparent_ref : NodeRef < NodePair > =
      parent_ref . parent ()
      . ok_or ( "SubscribeeCol has no parent (subscriber)" ) ?;
    let skgnode : &SkgNode =
      grandparent_ref . value () . mskgnode . as_ref ()
      . ok_or ( "Subscriber has no SkgNode" ) ?;
    ( subscribee_pid,
      skgnode . ids [ 0 ] . clone () ) };
  let ( _visible, hidden_in_content )
    : ( HashSet < ID >, HashSet < ID > )
    = partition_subscribee_content_for_subscriber (
      &config.db_name, driver,
      & subscriber_pid,
      & subscribee_pid ) . await ?;
  if hidden_in_content . is_empty () {
    return Ok (( )); }
  let hidden_col_nid : NodeId = {
    // Create HiddenInSubscribeeCol and prepend to this subscribee
    let hidden_col_orgnode : OrgNode = {
      let mut md = default_metadata ();
      md . code . interp = Interp::HiddenInSubscribeeCol;
      OrgNode {
        metadata : md,
        title : "hidden from this subscription" . to_string (),
        body : None, } };
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id ) . ok_or (
        "maybe_add_hidden_in_subscribee_col: node not found" ) ?;
    node_mut
      . prepend ( NodePair { mskgnode: None,
                             orgnode: hidden_col_orgnode } )
      . id () };
  { // Add HiddenFromSubscribees children
    for hidden_id in hidden_in_content {
      let ( skgnode, mut orgnode ) : ( SkgNode, OrgNode ) =
        skgnode_and_orgnode_from_id (
          config, driver, & hidden_id ) . await ?;
      orgnode . metadata . code . interp = Interp::HiddenFromSubscribees;
      orgnode . metadata . code . indefinitive = true;
      orgnode . body = None;
      let mut hidden_col_mut : NodeMut < NodePair > =
        tree . get_mut ( hidden_col_nid ) . ok_or (
          "maybe_add_hidden_in_subscribee_col: HiddenInSubscribeeCol not found" ) ?;
      hidden_col_mut . append ( NodePair { mskgnode: Some ( skgnode ),
                                           orgnode } ); }}
  Ok (( )) }
