use crate::dbs::filesystem::one_node::skgnode_from_pid_and_source;
use crate::types::tree::generations::collect_generation_ids;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::to_org::complete::contents::clobberIndefinitiveOrgnode;
use crate::to_org::complete::sharing::maybe_add_subscribeeCol_branch;
use crate::types::orgnode::ViewRequest;
use crate::types::orgnode::{
    mk_definitive_orgnode, OrgNode, OrgNodeKind, forest_root_orgnode };
use crate::types::tree::generic::{read_at_node_in_tree, read_at_ancestor_in_tree, write_at_node_in_tree, with_node_mut};
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::{SkgNode, SkgNodeMap};

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
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
pub type DefinitiveMap =
  HashMap < ID, NodeId >;


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
  tree    : &mut Tree<OrgNode>,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  write_at_node_in_tree (
    tree, node_id,
    |orgnode| {
      let OrgNodeKind::True ( t ) : &mut OrgNodeKind =
        &mut orgnode.kind
        else { panic! ( "makeIndefinitiveAndClobber: expected TrueNode" ) };
      t . indefinitive = true;
      t . body = None; }
    ). map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok (( )) }

/// This function's callers add a pristine, out-of-context
/// (skgnode, orgnode) pair to the tree.
/// Integrating the pair into the tree requires more work
/// (and later will require even more, probably),
/// which this function does:
/// - handle repeats, cycles and the visited map
/// - build a subscribee branch if needed
pub async fn complete_branch_minus_content (
  tree     : &mut Tree<OrgNode>,
  map      : &mut SkgNodeMap,
  node_id  : NodeId,
  visited  : &mut DefinitiveMap,
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  detect_and_mark_cycle ( tree, node_id ) ?;
  make_indef_if_repeat_then_extend_defmap (
    tree, node_id, visited ) ?;
  if truenode_in_tree_is_indefinitive ( tree, node_id )?
  { clobberIndefinitiveOrgnode (
      tree, map, node_id ) ?; }
  maybe_add_subscribeeCol_branch (
    tree, map, node_id, config, driver ) . await ?;
  Ok (( )) }

/// The two jobs in the name of this are inseparable --
/// we have to interleave extending the defmap
/// with marking things indefinitive, because the defmap
/// is how we know whether to mark something indefinitive.
pub fn make_indef_if_repeat_then_extend_defmap (
  tree    : &mut Tree<OrgNode>,
  node_id : NodeId,
  defMap  : &mut DefinitiveMap,
) -> Result<(), Box<dyn Error>> {
  let pid : ID = // Will error if node is a Scaffold.
    get_pid_in_tree ( tree, node_id ) ?;
  let is_indefinitive : bool =
    write_at_node_in_tree (
      tree, node_id,
      |orgnode|
        { let OrgNodeKind::True ( t ) : &mut OrgNodeKind
            = &mut orgnode.kind
            else { unreachable!( "In make_indef_if_repeat_then_extend_defmap, get_pid_in_tree already verified TrueNode"); };
          if defMap . contains_key ( &pid )
            { // It's a repeat, so it should be indefinitive.
              t . indefinitive = true; }
            t . indefinitive } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  if !is_indefinitive {
    defMap . insert ( pid, node_id ); }
  Ok (( )) }

/// Check if the node's PID appears in its ancestors,
/// and if so, mark viewData.cycle = true.
pub fn detect_and_mark_cycle (
  tree    : &mut Tree<OrgNode>,
  node_id : NodeId,
) -> Result<(), Box<dyn Error>> {
  let is_cycle : bool = {
    let pid : ID = get_pid_in_tree ( tree, node_id ) ?;
    is_ancestor_id ( tree, node_id, &pid ) ? };
  write_at_node_in_tree ( tree, node_id, |orgnode| {
    let OrgNodeKind::True ( t ) : &mut OrgNodeKind =
      &mut orgnode.kind
      else { panic! ( "detect_and_mark_cycle: expected TrueNode" ) };
    t . cycle = is_cycle; } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok (( )) }


// ==============================================
// Reading and manipulating trees, esp. via IDs
// ==============================================

/// Create a forest (single tree with ForestRoot at root)
/// containing just "tree root" nodes (no grandchildren yet),
/// and complete each via build_node_branch_minus_content.
pub async fn stub_forest_from_root_ids (
  root_skgids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
  visited  : &mut DefinitiveMap,
) -> Result < (Tree<OrgNode>, SkgNodeMap), Box<dyn Error> > {
  let mut forest : Tree<OrgNode> = Tree::new ( forest_root_orgnode () );
  let mut map : SkgNodeMap = SkgNodeMap::new ();
  let forest_root_treeid : NodeId = forest . root () . id ();
  for root_skgid in root_skgids {
    build_node_branch_minus_content (
      Some ( (&mut forest, forest_root_treeid) ),
      Some ( &mut map ),
      root_skgid, config, driver, visited
    ) . await ?; }
  Ok ( (forest, map) ) }

pub fn collect_ids_from_tree (
  tree : &Tree<OrgNode>,
) -> Vec < ID > {
  let mut pids : Vec < ID > = Vec::new ();
  for edge in tree . root () . traverse () {
    if let Edge::Open ( node_ref ) = edge {
      let pid_opt : Option<&ID>
      = match &node_ref . value () . kind
      { OrgNodeKind::True ( t ) => t . id_opt . as_ref (),
        OrgNodeKind::Scaff ( _ ) => None };
      if let Some ( pid ) = pid_opt {
        pids . push ( pid . clone( )); }} }
  pids }

/// Check if `target_skgid` appears in the ancestor path of `treeid`.
/// Used for cycle detection.
fn is_ancestor_id (
  tree          : &Tree<OrgNode>,
  origin_treeid : NodeId,
  target_skgid  : &ID,
) -> Result<bool, Box<dyn Error>> {
  read_at_node_in_tree(tree, origin_treeid, |_| ())
    .map_err(|_| "is_ancestor_id: NodeId not in tree")?;
  for generation in 1.. {
    match read_at_ancestor_in_tree(
      tree, origin_treeid, generation,
      |orgnode| match &orgnode . kind {
        OrgNodeKind::True ( t ) => t . id_opt . clone (),
        OrgNodeKind::Scaff ( _ ) => None } )
    { Ok(Some(id)) if &id == target_skgid
        => return Ok(true),
      Ok(_) => continue,
      Err(_) => return Ok(false), }}
  unreachable!() }

/// Errors if the node is a Scaffold, not found, or has no ID.
pub fn get_pid_in_tree (
  tree   : &Tree<OrgNode>,
  treeid : NodeId,
) -> Result < ID, Box<dyn Error> > {
  let node_kind: OrgNodeKind =
    read_at_node_in_tree (
      tree, treeid, |orgnode| orgnode.kind.clone() )?;
  match node_kind {
    OrgNodeKind::Scaff ( _ ) => Err ( "get_pid_in_tree: caller should not pass a Scaffold" . into() ),
    OrgNodeKind::True ( t ) =>
      t . id_opt . ok_or_else (
        || "get_pid_in_tree: node has no ID" . into( )) }}

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
  tree          : &mut Tree<OrgNode>,
  map           : &mut SkgNodeMap,
  parent_treeid : NodeId, // will parent the new node
  child_skgid   : &ID, // how to find the new node
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < NodeId, // the new node
              Box<dyn Error> > {
  let (child_skgnode, child_orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id (
      config, driver, child_skgid ) . await ?;
  map . insert ( child_skgid . clone (),
                 child_skgnode );
  let child_treeid : NodeId =
    with_node_mut ( // append child
      tree, parent_treeid,
      ( |mut parent_mut|
        parent_mut . append(child_orgnode) . id() ))
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok ( child_treeid ) }

/// Builds a node from disk, place it in a tree,
/// complete the branch it implies except for 'content' descendents,
/// and return the root of the new branch, but in its tree context:
/// - If tree_and_parent is None and map is None:
///   creates new tree, returns (Some(tree), Some(map), branch_root_nodeid)
/// - If tree_and_parent is Some and map is Some:
///   appends to tree, returns (None, None, branch_root_nodeid)
pub async fn build_node_branch_minus_content (
  tree_and_parent : Option<(&mut Tree<OrgNode>, NodeId)>, // if modifying an existing tree, attach as a child here
  map             : Option<&mut SkgNodeMap>,
  skgid           : &ID, // what to fetch
  config          : &SkgConfig,
  driver          : &TypeDBDriver,
  visited         : &mut DefinitiveMap,
) -> Result < (Option<Tree<OrgNode>>, Option<SkgNodeMap>, NodeId), Box<dyn Error> > {
  let (skgnode, orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id (
      config, driver, skgid ) . await ?;
  let node_id : ID = match &orgnode.kind {
    OrgNodeKind::True(t) => t . id_opt . clone() . ok_or ( "build_node_branch_minus_content: orgnode has no ID" ) ?,
    OrgNodeKind::Scaff(_) =>
      return Err ( "build_node_branch_minus_content: orgnode is Scaffold".into() ), };
  match (tree_and_parent, map) {
    (Some ( (tree, parent_treeid) ), Some(map)) => {
      map . insert ( node_id, skgnode ); // Add SkgNode to map
      let child_treeid : NodeId = // Add OrgNode to tree
        with_node_mut (
          tree, parent_treeid,
          ( |mut parent_mut|
            parent_mut . append ( orgnode ) . id () ))
        . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
      complete_branch_minus_content (
        tree, map, child_treeid, visited,
        config, driver ) . await ?;
      Ok ( (None, None, child_treeid) ) },
    (None, None) => {
      let mut map : SkgNodeMap =
        SkgNodeMap::new ();
      map . insert ( node_id, skgnode );
      let mut tree : Tree<OrgNode> =
        Tree::new ( orgnode );
      let root_treeid : NodeId = tree . root () . id ();
      complete_branch_minus_content (
        &mut tree, &mut map, root_treeid, visited,
        config, driver ) . await ?;
      Ok ( (Some(tree), Some(map), root_treeid) ) },
    _ => Err("build_node_branch_minus_content: tree_and_parent and map must both be Some or both be None".into()),
  } }

/// Collect content child IDs from a node.
/// Returns empty vec if the node is indefinitive or has no SkgNode.
/// Errors if passed a Scaffold.
pub(super) fn content_ids_if_definitive_else_empty (
  tree   : &Tree<OrgNode>,
  map    : &SkgNodeMap,
  treeid : NodeId,
) -> Result < Vec < ID >, Box<dyn Error> > {
  let ( node_kind, node_id_opt ) : ( OrgNodeKind, Option<ID> ) =
    read_at_node_in_tree (
      tree, treeid,
      |orgnode| ( orgnode.kind.clone(),
                  match &orgnode.kind {
                    OrgNodeKind::True(t) => t . id_opt . clone(),
                    OrgNodeKind::Scaff(_) => None,
                  } ) )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  match node_kind {
    OrgNodeKind::Scaff ( _ ) =>
      Err ( "content_ids_if_definitive_else_empty: \
             caller should not pass a Scaffold".into() ),
    OrgNodeKind::True ( t ) => {
      if t.indefinitive {
        return Ok ( Vec::new () ); }
      Ok ( match node_id_opt {
        Some ( node_id ) =>
          map . get (&node_id) . map (
            |skgnode| skgnode . contains . clone ()
              . unwrap_or_default ()
          ). unwrap_or_default (),
        None => Vec::new (),  // No ID yet
      } ) } }}

/// Collect ego_tree::NodeIds after
///   some member of some generation of a tree.
/// 'effective_root' should be some ancestor.
/// It affects both the meaning of generation numbers,
/// and the scope of which nodes are collected
/// (only its descendents are collected).
/// If effective root is None,
/// the true root is used as the effective root.
pub(super) fn nodes_after_in_generation (
  tree                     : &Tree<OrgNode>,
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

/// Check if a TrueNode is indefinitive.
/// Errs if given a Scaffold.
pub fn truenode_in_tree_is_indefinitive (
  tree   : &Tree<OrgNode>,
  treeid : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_kind: OrgNodeKind =
    read_at_node_in_tree ( tree, treeid, |orgnode| orgnode.kind.clone() )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  match node_kind {
    OrgNodeKind::Scaff (_) => Err ( "is_indefinitive: caller should not pass a Scaffold" . into( )),
    OrgNodeKind::True (t)  => Ok (t.indefinitive) }}

/// Collect all child tree NodeIds from a node.
/// Returns an error if the node is not found.
pub fn collect_child_treeids (
  tree    : &Tree<OrgNode>,
  treeid : NodeId,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let node_ref : NodeRef < OrgNode > =
    tree . get ( treeid )
    . ok_or ( "collect_child_treeids: NodeId not in tree" ) ?;
  Ok ( node_ref . children () . map ( |c| c . id () ) . collect () ) }


// ==============================================
// 'remove_completed_view_request'
// ==============================================

/// Log any error and remove the request from the node.
/// Does *not* verify that the request was completed;
/// that's just the only situation in which it would be used.
pub(super) fn remove_completed_view_request<T> (
  tree         : &mut Tree<T>,
  node_id      : NodeId,
  view_request : ViewRequest,
  error_msg    : &str,
  errors       : &mut Vec < String >,
  result       : Result < (), Box<dyn Error> >,
) -> Result < (), Box<dyn Error> >
where T: AsMut<OrgNode>,
{
  if let Err ( e ) = result {
    errors . push ( format! ( "{}: {}", error_msg, e )); }
  let mut node_mut : NodeMut<T> =
    tree . get_mut (node_id) . ok_or ( "remove_completed_view_request: node not found" ) ?;
  if let OrgNodeKind::True ( t )
    = &mut node_mut . value () . as_mut () . kind
    { t . view_requests . remove ( &view_request ); }
  Ok (()) }
