/// SINGLE ENTRY POINT: 'completeAndRestoreForest'.

use crate::to_org::util::{
  skgnode_and_orgnode_from_id, VisitedMap,
  get_pid_in_pairtree, truenode_in_tree_is_indefinitive, collect_child_treeids,
  mark_if_visited_or_repeat_or_cycle };
use crate::to_org::complete::aliascol::completeAliasCol;
use crate::to_org::complete::sharing::{
  maybe_add_subscribeeCol_branch };
use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNodeKind, OrgNode, Scaffold};
use crate::types::tree::{NodePair, PairTree};
use crate::types::tree::generic::{
  read_at_node_in_tree, write_at_node_in_tree, with_node_mut };

use ego_tree::{NodeId, NodeMut, NodeRef};
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// TRIVIAL: Just wraps 'complete_or_restore_each_node_in_branch',
/// calling it on each "tree root" (child of the ForestRoot),
/// but threading 'visited' through that sequence of calls.
pub async fn completeAndRestoreForest (
  forest        : &mut PairTree,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
) -> Result < VisitedMap, Box<dyn Error> > {
  let mut visited : VisitedMap = VisitedMap::new ();
  let tree_root_ids : Vec < NodeId > =
    forest . root () . children ()
    . map ( |c| c . id () )
    . collect ();
  for tree_root_id in tree_root_ids {
    complete_or_restore_each_node_in_branch (
      forest, tree_root_id, config, typedb_driver,
      &mut visited ) . await ?; }
  Ok ( visited ) }

/// PURPOSE: Complete or restore a node,
/// and then its children (a preorder DFS traversal).
/// - "complete": Because definitive nodes can be missing branches.
/// - "restore": Because indefinitive nodes may have had their titles or bodies edited.
///   TODO ? Maybe look for edits to indefinitive nodes and throw an error, as is done for foreign nodes.
fn complete_or_restore_each_node_in_branch<'a> (
  tree          : &'a mut PairTree,
  node_id       : NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  visited       : &'a mut VisitedMap,
) -> Pin<Box<dyn Future<Output =
                        Result<(), Box<dyn Error>>> + 'a>> {
  fn recurse<'b> (
    tree          : &'b mut PairTree,
    node_id       : NodeId,
    config        : &'b SkgConfig,
    typedb_driver : &'b TypeDBDriver,
    visited       : &'b mut VisitedMap,
  ) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'b>>
  { Box::pin(async move {
      let child_treeids : Vec < NodeId > =
        collect_child_treeids ( tree, node_id ) ?;
      for child_treeid in child_treeids {
        complete_or_restore_each_node_in_branch (
          tree, child_treeid, config, typedb_driver,
          visited ) . await ?; }
      Ok (( )) } ) }
  Box::pin(async move {
    let is_alias_col: bool =
      read_at_node_in_tree(tree, node_id, |node| {
        matches!(&node.orgnode.kind,
                 OrgNodeKind::Scaff(Scaffold::AliasCol)) })?;
    let is_col_scaffold: bool =
      read_at_node_in_tree(tree, node_id, |node| {
        matches!( &node.orgnode.kind,
                  OrgNodeKind::Scaff(
                    Scaffold::SubscribeeCol |
                    Scaffold::HiddenOutsideOfSubscribeeCol |
                    Scaffold::HiddenInSubscribeeCol )) } )?;
    if is_alias_col {
      // Don't recurse; completeAliasCol handles the whole subtree.
      completeAliasCol (
        tree, node_id, config, typedb_driver ). await ?;
    } else if is_col_scaffold {
      // Skip, but recurse into children.
      recurse ( tree, node_id, config, typedb_driver, visited
              ) . await ?;
    } else {
      ensure_skgnode (
        tree, node_id, config, typedb_driver ). await ?;
      mark_if_visited_or_repeat_or_cycle (
        tree, node_id, visited ) ?;
      { if truenode_in_tree_is_indefinitive ( tree, node_id ) ? {
          clobberIndefinitiveOrgnode (
            tree, node_id ) ?;
        } else { // futz with the orgnode and its content children
          maybe_add_subscribeeCol_branch (
            tree, node_id, config, typedb_driver ) . await ?;
          completeAndReorder_childrenOf_definitiveOrgnode (
            tree, node_id, config, typedb_driver ). await ?; }
        recurse ( // Recurse to children even for indefinitive nodes, since they may have children from (for instance) view requests.
          tree, node_id, config, typedb_driver, visited
        ). await ?; }}
    Ok (( )) } ) }

/// PURPOSE: Given an indefinitive node N:
/// - Reset title.
/// - Reset source.
/// - Set body to None.
///
/// ASSUMES: The SkgNode at that tree node is accurate.
/// ASSUMES: The input is indefinitive.
pub fn clobberIndefinitiveOrgnode (
  tree    : &mut PairTree,
  treeid : NodeId,
) -> Result < (), Box<dyn Error> > {
  write_at_node_in_tree ( tree, treeid, |pair| {
    let (title, source) : (String, String) = {
      let skgnode : &SkgNode =
        pair . mskgnode . as_ref ()
          . ok_or ("SkgNode should exist after fetch" . to_string() )?;
      ( skgnode . title . clone (),
        skgnode . source . clone () ) };
    let OrgNodeKind::True ( t ) = &mut pair.orgnode.kind
      else { return Err ( "clobberIndefinitiveOrgnode: expected TrueNode" . into () ) };
    t . title = title;
    t . source_opt = Some ( source );
    t . body = None;
    Ok::<(), String>(( ))
  } )? // before the '?' it's a nested Result: R<R<(),String>,String>
    . map_err( |e| -> Box<dyn Error> { e.into() } ) }

/// Completes a definitive Content node's children,
/// by reconciling them with the 'contains' relationships
/// found in the SkgNode.
///
/// ASSUMES: The node's "id" field is its PID.
/// ASSUMES: The PairTree has a SkgNode here (via "ensure_skgnode").
/// ASSUMES: 'mark_if_visited_or_repeat_or_cycle' was already called.
/// ASSUMES: Input is definitive.
pub async fn completeAndReorder_childrenOf_definitiveOrgnode (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let content_from_disk_as_list : Option<Vec<ID>> =
    read_at_node_in_tree ( tree, node_id, |np|
      np . mskgnode . as_ref ()
      . map ( |s| s . contains . clone () )
    ) ? . ok_or ( "SkgNode should exist" ) ?;
  let content_from_disk_as_set : HashSet<ID> =
    content_from_disk_as_list . as_ref ()
    . map ( |v| v . iter () . cloned () . collect () )
    . unwrap_or_default ();

  let ( content_child_treeids,
        mut non_content_child_treeids )
    : ( Vec < NodeId >, Vec < NodeId > )
    = partition_nonignored_true_content_from_other_children (
      tree,
      node_id ) ?;

  let mut content_skgid_to_treeid : HashMap < ID, NodeId > =
    map_skgid_to_treeid ( tree, &content_child_treeids ) ?;

  // Mark invalidated content as ParentIgnores
  // and move to non-content list.
  // Content can become invalidated, i.e. non-content,
  // if a separate buffer edited the invalidated content's container.
  let mut invalid_content_treeids : Vec < NodeId > =
    Vec::new ();
  for ( child_pid, child_treeid ) in & content_skgid_to_treeid {
    if ! content_from_disk_as_set . contains ( child_pid ) {
      invalid_content_treeids . push ( *child_treeid ); }}
  for invalid_treeid in & invalid_content_treeids {
    let mut child_mut : NodeMut < NodePair > =
      tree . get_mut ( *invalid_treeid )
      . ok_or ( "Invalid content child not found" ) ?;
    let pair : &mut NodePair = child_mut . value ();
    let OrgNodeKind::True(t) = &mut pair . orgnode . kind
      else { return Err ( "Not-really-content child is not a TrueNode" . into () ) };
    t . parent_ignores = true;
    let child_pid : ID = t . id_opt . clone () . unwrap ();
    content_skgid_to_treeid . remove ( & child_pid );
    non_content_child_treeids . push ( *invalid_treeid ); }

  // Build completed-content list
  // preserving order from SkgNode
  let mut completed_content_treeids : Vec < NodeId > =
    Vec::new ();
  if let Some(contains) = & content_from_disk_as_list {
    for disk_skgid in contains {
      if let Some ( existing_treeid ) =
        content_skgid_to_treeid . get ( disk_skgid ) {
        // Content already exists in tree
        completed_content_treeids . push ( *existing_treeid );
      } else
      { // PITFALL: The preorder DFS traversal for complete_or_restore_each_node_in_branch lets us add a child without considering grandchildren yet.
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

/// PURPOSE: Separate a node's non-ignored true content from its other
/// children.
///
/// SOME TERMS:
/// - "False" content would look like content based on
/// the orgnode tree, but without corresponding content relationships
/// in the dbs. Maybe it can't happen, but the types don't prevent it.
/// - 'Non-intentional' content would be nodes that the parent *does*
/// contain, but marked as parentIgnores. We assume such nodes
/// are copies the user wants to keep in view without affecting
/// the graph. Therefore, when populating a definitive node's contents,
/// they will be ignored. A non-ignored duplicate is made if necessary.
///
/// RETURNS (content_child_ids, non_content_child_ids).
fn partition_nonignored_true_content_from_other_children (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ( Vec <NodeId>, // intentional true content
                Vec <NodeId> ), // others
              Box<dyn Error> > {
  let mut content_child_ids : Vec < NodeId > =
    Vec::new ();
  let mut non_content_child_ids : Vec < NodeId > =
    Vec::new ();
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "Node not found in tree" ) ?;
  for child in node_ref . children () {
    if matches! ( &child . value () . orgnode . kind,
                  OrgNodeKind::True(t)
                  if !t.parent_ignores )
    { content_child_ids . push ( child . id () );
    } else {
      non_content_child_ids . push ( child . id () ); }}
  Ok (( content_child_ids, non_content_child_ids )) }

/// Builds a map from each node's Skg ID to its tree ID.
/// Errors if any node lacks an ID.
///
/// PITFALL: If the same ID appeared twice in the list,
/// the resulting map would not be well-defined.
/// But 'find_buffer_errors_for_saving' prevents that.
fn map_skgid_to_treeid (
  tree          : &PairTree,
  child_treeids : &[NodeId],
) -> Result < HashMap < ID, NodeId >, Box<dyn Error> > {
  let mut result : HashMap < ID, NodeId > = HashMap::new ();
  for child_treeid in child_treeids {
    let child_pid : ID =
      read_at_node_in_tree ( tree, *child_treeid, |np|
        match &np . orgnode . kind
        { OrgNodeKind::True(t) => t.id_opt.clone(),
          OrgNodeKind::Scaff(_) => None }
      ) ? . ok_or ( "Content child has no ID" ) ?;
    result . insert ( child_pid, *child_treeid ); }
  Ok ( result ) }

/// Create a new Content node from disk (using 'disk_id')
/// and append it to the children of 'parent_nid'.
async fn extend_content (
  tree       : &mut PairTree,
  parent_nid : NodeId,
  skgid      : &ID,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let ( skgnode, orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, skgid ) . await ?;
  let new_child_id : NodeId = with_node_mut (
    tree, parent_nid,
    ( |mut parent_mut|
      parent_mut . append ( NodePair { mskgnode : Some(skgnode),
                                       orgnode  : orgnode } )
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

/// Noop for Scaffolds. Otherwise, ensures the node has a source.
/// If needed, fetches the data from TypeDB.
pub async fn ensure_source (
  tree    : &mut PairTree,
  node_id : NodeId,
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let needs_source : bool =
    read_at_node_in_tree (
      tree, node_id,
      |np| matches! ( &np . orgnode . kind,
                      OrgNodeKind::True(t)
                      if t . source_opt . is_none () )) ?;
  if needs_source {
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
      |np| {
        let OrgNodeKind::True ( t ) = &mut np.orgnode.kind
          else { panic! ( "ensure_source: expected TrueNode" ) };
        t . source_opt = Some ( source ); } ) ?; }
  Ok (( )) }
