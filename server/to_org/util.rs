use crate::dbs::filesystem::one_node::skgnode_from_pid_and_source;
use crate::types::tree::generations::collect_generation_ids;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::to_org::complete::contents::clobberIndefinitiveOrgnode;
use crate::to_org::complete::sharing::maybe_add_subscribeeCol_branch;
use crate::types::orgnode::ViewRequest;
use crate::types::orgnode::{
    mk_definitive_orgnode, OrgNode, OrgNodeKind, Scaffold, forest_root_orgnode };
use crate::types::tree::{NodePair, PairTree};
use crate::types::tree::generic::{read_at_node_in_tree, read_at_ancestor_in_tree, write_at_node_in_tree, with_node_mut};
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;

use ego_tree::{Tree, NodeId, NodeRef};
use ego_tree::iter::Edge;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;


/// Tracks which IDs have been rendered definitively and where.
/// - Key: the ID that was visited
/// - Value: NodeId within the forest tree
///
/// Uses:
/// - prevent duplicate definitive expansions
/// - locate the conflict when an earlier definitive view
///   conflicts with a new definitive view request
pub type VisitedMap =
  HashMap < ID, NodeId >;


// ======================================================
// ForestRoot utilities
// ======================================================

/// Create a NodePair representing a ForestRoot.
/// It is never rendered; it just makes forests easier to process.
pub fn forest_root_pair () -> NodePair {
  NodePair { mskgnode : None,
             orgnode  : forest_root_orgnode() } }

/// Create a new forest (a tree with a ForestRoot root).
/// The "tree roots" will be children of this root.
fn new_forest () -> PairTree {
  Tree::new ( forest_root_pair () ) }

/// Check if a node is a ForestRoot.
fn is_forest_root (
  tree    : &PairTree,
  node_id : NodeId,
) -> bool {
  tree . get ( node_id )
    . map ( |node_ref|
             matches! ( &node_ref . value () . orgnode . kind,
                        OrgNodeKind::Scaff ( Scaffold::ForestRoot ) ) )
    . unwrap_or ( false ) }


// ======================================================
// Fetching, building and modifying SkgNodes and OrgNodes
// ======================================================

/// Fetch a SkgNode from disk (queries TypeDB for source).
/// Make an OrgNode from it, with validated title.
/// Return both.
pub async fn skgnode_and_orgnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid : &ID,
) -> Result < ( SkgNode, OrgNode ), Box<dyn Error> > {
  let (pid_resolved, source) : (ID, String) =
    pid_and_source_from_id( // Query TypeDB for them
      &config.db_name, driver, skgid).await?
    . ok_or_else( || format!(
      "ID '{}' not found in database", skgid ))?;
  skgnode_and_orgnode_from_pid_and_source (
    config, &pid_resolved, &source ) }

/// Fetch a SkgNode from disk given PID and source.
/// Make an OrgNode from it, with validated title.
/// Return both.
pub(super) fn skgnode_and_orgnode_from_pid_and_source (
  config : &SkgConfig,
  pid    : &ID,
  source : &str,
) -> Result < ( SkgNode, OrgNode ), Box<dyn Error> > {
  let skgnode : SkgNode =
    skgnode_from_pid_and_source( config, pid.clone(), source )?;
  let title : String = skgnode . title . replace ( '\n', " " );
  if title . is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "SkgNode with ID {} has an empty title",
                 pid ), )) ); }
  let orgnode : OrgNode = mk_definitive_orgnode (
    pid . clone (),
    source . to_string (),
    title,
    skgnode . body . clone () );
  Ok (( skgnode, orgnode )) }

/// Set 'indefinitive' to true,
/// reset title and source,
/// and set body to None.
pub(super) fn makeIndefinitiveAndClobber (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  write_at_node_in_tree ( tree, node_id, |np| {
    let org = &mut np.orgnode;
    org . set_indefinitive ( true );
    org . clear_body (); } ) ?;
  Ok (( )) }

/// This function's callers add a pristine, out-of-context
/// (skgnode, orgnode) pair to the tree.
/// Integrating the pair into the tree requires more work
/// (and later will require even more, probably),
/// which this function does:
/// - handle repeats, cycles and the visited map
/// - build a subscribee branch if needed
pub async fn complete_branch_minus_content (
  tree     : &mut PairTree,
  node_id  : NodeId,
  visited  : &mut VisitedMap,
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  mark_if_visited_or_repeat_or_cycle (
    tree, node_id, visited ) ?;
  maybe_add_subscribeeCol_branch (
    tree, node_id, config, driver ) . await ?;
  Ok (( )) }

/// Handles repetitions, cycles, and the VisitedMap.
/// Contains no surprising of complex logic.
/// - Check for cycle and mark viewData.cycle accordingly.
/// - If node is a repeat (already in visited), mark it indefinitive.
/// - If node is indefinitive, rewrite it (clear body, etc.).
///   (repeat should imply indefinitive. The reverse need not hold.)
/// - If node is definitive, add it to visited.
pub fn mark_if_visited_or_repeat_or_cycle (
  tree     : &mut PairTree,
  node_id  : NodeId,
  visited  : &mut VisitedMap,
) -> Result<(), Box<dyn Error>> {
  let pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  detect_and_mark_cycle ( tree, node_id ) ?;
  if visited . contains_key ( &pid ) {
    // Mark as indefinitive (it's a repeat).
    write_at_node_in_tree ( tree, node_id, |np| {
      np . orgnode . set_indefinitive ( true ); } ) ?; }
  let is_indefinitive : bool =
    read_at_node_in_tree ( tree, node_id, |np|
      np . orgnode . is_indefinitive () ) ?;
  if is_indefinitive {
    clobberIndefinitiveOrgnode ( tree, node_id ) ?;
  } else {
    visited . insert ( pid, node_id ); }
  Ok (( )) }

/// Check if the node's PID appears in its ancestors,
/// and if so, mark viewData.cycle = true.
fn detect_and_mark_cycle (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result<(), Box<dyn Error>> {
  let is_cycle : bool = {
    let pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
    is_ancestor_id ( tree, node_id, &pid ) ? };
  write_at_node_in_tree ( tree, node_id, |np| {
    np . orgnode . set_cycle ( is_cycle ); } ) ?;
  Ok (( )) }


// ==============================================
// Reading and manipulating trees, esp. via IDs
// ==============================================

/// Create a forest (single tree with ForestRoot at root)
/// containing just "tree root" nodes (no grandchildren yet),
/// and complete each via complete_branch_minus_content.
pub async fn stub_forest_from_root_ids (
  root_skgids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
  visited  : &mut VisitedMap,
) -> Result < PairTree, Box<dyn Error> > {
  let mut forest : PairTree = new_forest ();
  let forest_root_treeid : NodeId = forest . root () . id ();
  for root_skgid in root_skgids {
    build_node_branch_minus_content (
      Some ( (&mut forest, forest_root_treeid) ),
      root_skgid, config, driver, visited
    ) . await ?; }
  Ok ( forest ) }

pub fn collect_ids_from_pair_tree (
  tree : &PairTree,
) -> Vec < ID > {
  let mut pids : Vec < ID > = Vec::new ();
  for edge in tree . root () . traverse () {
    if let Edge::Open ( node_ref ) = edge {
      let pid_opt : Option<&ID>
      = match &node_ref . value () . orgnode . kind
      { OrgNodeKind::True ( t ) => t . id_opt . as_ref (),
        OrgNodeKind::Scaff ( _ ) => None };
      if let Some ( pid ) = pid_opt {
        pids . push ( pid . clone( )); }} }
  pids }

/// Check if `target_skgid` appears in the ancestor path of `treeid`.
/// Used for cycle detection.
fn is_ancestor_id (
  tree          : &PairTree,
  origin_treeid : NodeId,
  target_skgid  : &ID,
) -> Result<bool, Box<dyn Error>> {
  read_at_node_in_tree(tree, origin_treeid, |_| ())
    .map_err(|_| "is_ancestor_id: NodeId not in tree")?;
  for generation in 1.. {
    match read_at_ancestor_in_tree(
      tree, origin_treeid, generation,
      |np| match &np . orgnode . kind {
        OrgNodeKind::True ( t ) => t . id_opt . clone (),
        OrgNodeKind::Scaff ( _ ) => None } )
    { Ok(Some(id)) if &id == target_skgid
        => return Ok(true),
      Ok(_) => continue,
      Err(_) => return Ok(false), }}
  unreachable!() }

/// Extract the PID from a node in a PairTree.
/// Returns an error if the node is not found or has no ID.
pub(super) fn get_pid_in_pairtree (
  tree    : &PairTree,
  treeid : NodeId,
) -> Result < ID, Box<dyn Error> > {
  read_at_node_in_tree ( tree, treeid, |np|
    match &np . orgnode . kind {
      OrgNodeKind::True ( t ) => t . id_opt . clone (),
      OrgNodeKind::Scaff ( _ ) => None } ) ?
    . ok_or_else ( || "get_pid_in_pairtree: node has no ID"
                       . into () ) }

/// Build a node from disk and
/// append it at 'parent_treeid' as a child.
/// Returns the new node's ego_tree::NodeId.
///
/// Does *not* take its ancestors into account,
/// and does *not* build any of its descendents.
/// Those can be done later via 'complete_branch_minus_content',
/// or they can all be done at once via
/// 'build_node_branch_minus_content.
pub async fn make_and_append_child_pair (
  tree           : &mut PairTree,
  parent_treeid : NodeId, // will parent the new node
  child_skgid   : &ID, // how to find the new node
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < NodeId, // the new node
              Box<dyn Error> > {
  let (child_skgnode, child_orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id (
      config, driver, child_skgid ) . await ?;
  let child_treeid : NodeId =
    with_node_mut (
      tree, parent_treeid,
      ( |mut parent_mut|
        parent_mut . append ( NodePair { mskgnode : Some(child_skgnode),
                                         orgnode  : child_orgnode } )
        . id () )) ?;
  Ok ( child_treeid ) }

/// Builds a pair from disk, place it in a tree,
/// complete the branch it implies except for 'content' descendents,
/// and return the root of the new branch, but in its tree context:
/// - If tree_and_parent is None:
///   creates new tree, returns (Some(tree), branch_root_nodeid)
/// - If tree_and_parent is Some:
///   appends to tree, returns (None, branch_root_nodeid)
pub async fn build_node_branch_minus_content (
  tree_and_parent : Option<(&mut PairTree, NodeId)>, // if modifying an existing tree, attach as a child here
  skgid          : &ID, // what to fetch
  config          : &SkgConfig,
  driver          : &TypeDBDriver,
  visited         : &mut VisitedMap,
) -> Result < (Option<PairTree>, NodeId), Box<dyn Error> > {
  let (skgnode, orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id (
      config, driver, skgid ) . await ?;
  match tree_and_parent {
    Some ( (tree, parent_treeid) ) => {
      let child_treeid : NodeId =
        with_node_mut (
          tree, parent_treeid,
          ( |mut parent_mut|
            parent_mut . append ( NodePair { mskgnode : Some(skgnode),
                                             orgnode  : orgnode } ) . id () )) ?;
      complete_branch_minus_content (
        tree, child_treeid, visited,
        config, driver ) . await ?;
      Ok ( (None, child_treeid) ) },
    None => {
      let mut tree : PairTree =
        Tree::new ( NodePair { mskgnode : Some(skgnode),
                               orgnode  : orgnode } );
      let root_treeid : NodeId = tree . root () . id ();
      complete_branch_minus_content (
        &mut tree, root_treeid, visited,
        config, driver ) . await ?;
      Ok ( (Some(tree), root_treeid) ) }, } }

/// Collect content child IDs from a node in a PairTree.
/// Returns empty vec if the node is indefinitive or has no SkgNode.
pub(super) fn content_ids_if_definitive_else_empty (
  tree    : &PairTree,
  treeid : NodeId,
) -> Result < Vec < ID >, Box<dyn Error> > {
  read_at_node_in_tree ( tree, treeid,
    |nodepair| {
      if nodepair . orgnode . is_indefinitive () {
        return Vec::new (); }
      match & nodepair . mskgnode {
        Some ( skgnode ) =>
          skgnode . contains . clone () . unwrap_or_default (),
        None => Vec::new (),  // No SkgNode yet
      }}
  ). map_err ( |e| e . into () ) }

/// Collect ego_tree::NodeIds after
///   some member of some generation of a tree.
/// 'effective_root' should be some ancestor.
/// It affects both the meaning of generation numbers,
/// and the scope of which nodes are collected
/// (only its descendents are collected).
/// If effective root is None,
/// the true root is used as the effective root.
pub(super) fn nodes_after_in_generation (
  tree                     : &PairTree,
  generation               : usize,
  generation_member_treeid : NodeId,
  effective_root           : Option < NodeId >,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let treeids_in_gen : Vec < NodeId > =
    collect_generation_ids ( tree, generation, effective_root ) ?;
  let mut result : Vec < NodeId > = Vec::new ();
  let mut found_target : bool = false;
  for treeid in treeids_in_gen {
    if found_target {
      result . push ( treeid );
    } else if treeid == generation_member_treeid {
      found_target = true; } }
  Ok ( result ) }


// ==============================================
// Reading from SkgNodes and OrgNodes, esp. in trees
// ==============================================

/// Check if a node in a PairTree is marked indefinitive.
/// Returns an error if the node is not found.
pub(super) fn is_indefinitive (
  tree    : &PairTree,
  treeid : NodeId,
) -> Result < bool, Box<dyn Error> > {
  read_at_node_in_tree ( tree, treeid, |np|
    np . orgnode . is_indefinitive () )
    . map_err ( |e| e . into () ) }

/// Collect all child tree NodeIds from a node in a PairTree.
/// Returns an error if the node is not found.
pub(super) fn collect_child_treeids (
  tree    : &PairTree,
  treeid : NodeId,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( treeid )
    . ok_or ( "collect_child_treeids: NodeId not in tree" ) ?;
  Ok ( node_ref . children () . map ( |c| c . id () ) . collect () ) }


// ==============================================
// 'remove_completed_view_request'
// ==============================================

/// Log any error and remove the request from the node.
/// Does *not* verify that the request was completed;
/// that's just the only situation in which it would be used.
pub(super) fn remove_completed_view_request (
  tree         : &mut PairTree,
  node_id      : NodeId,
  view_request : ViewRequest,
  error_msg    : &str,
  errors       : &mut Vec < String >,
  result       : Result < (), Box<dyn Error> >,
) -> Result < (), Box<dyn Error> > {
  if let Err ( e ) = result {
    errors . push ( format! ( "{}: {}", error_msg, e )); }
  let mut node_mut = tree . get_mut ( node_id )
    . ok_or ( "remove_completed_view_request: node not found" ) ?;
  if let Some ( vr ) =
    node_mut . value () . orgnode . view_requests_mut ()
  { vr . remove ( &view_request ); }
  Ok (()) }
