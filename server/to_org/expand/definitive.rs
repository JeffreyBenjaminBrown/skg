use crate::dbs::filesystem::one_node::optskgnode_from_id;
use crate::dbs::typedb::nodes::which_ids_exist;
use crate::git_ops::read_repo::skgnode_from_git_head;
use crate::to_org::complete::sharing::{ maybe_add_hiddenInSubscribeeCol_branch, type_and_parent_type_consistent_with_subscribee };
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{ build_and_integrate_containerward_view_then_drop_request, build_and_integrate_sourceward_view_then_drop_request};
use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_truncate_some_of_previous;
use crate::to_org::util::{ DefinitiveMap, build_node_branch_minus_content, get_id_from_treenode, makeIndefinitiveAndClobber, truenode_in_tree_is_indefinitive, content_ids_if_definitive_else_empty };
use crate::types::git::NodeDiffStatus;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{ ViewNode, ViewNodeKind, ViewRequest, mk_indefinitive_viewnode };
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::types::tree::generic::{read_at_node_in_tree, write_at_node_in_tree};
use crate::types::tree::viewnode_skgnode::pid_and_source_from_treenode;

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
use std::collections::{BTreeSet,HashSet,HashMap};
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  forest        : &mut Tree<ViewNode>,
  map           : &mut SkgNodeMap,
  requests      : Vec < (NodeId, ViewRequest) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut DefinitiveMap,
  errors        : &mut Vec < String >,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result < (), Box<dyn Error> > {
  for (node_id, request) in requests {
    match request {
      ViewRequest::Aliases => {
        build_and_integrate_aliases_view_then_drop_request (
          forest, map, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Containerward => {
        build_and_integrate_containerward_view_then_drop_request (
          forest, map, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Sourceward => {
        build_and_integrate_sourceward_view_then_drop_request (
          forest, map, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Definitive => {
        execute_definitive_view_request (
          forest, map, node_id, config, typedb_driver,
          visited, errors, deleted_id_src_map ) . await ?; }, }}
  Ok (( )) }

/// Expands a definitive view request.
///
/// This function:
/// 1. 'Indefinitizes' any previously definitive ViewNode with that ID
/// 2. Marks this ViewNode definitive
/// 3. Expands (BFS) its content from disk (or from latest git commit for removed nodes), applying the node limit
/// 4. Removes its ViewRequest::Definitive
///
/// For Subscribee nodes: the subscriber's 'hides_from_its_subscriptions'
/// list is used to filter out content that should be hidden.
///
/// For nodes with `diff: Some(Removed)`: loads content from git HEAD
/// instead of disk, and marks children as removed if they don't exist on disk.
async fn execute_definitive_view_request (
  forest        : &mut Tree<ViewNode>, // "forest" = tree with BufferRoot
  map           : &mut SkgNodeMap,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut DefinitiveMap,
  _errors       : &mut Vec < String >,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_id_from_treenode (
    forest, node_id ) ?;
  let is_removed_node : bool = // for git diff view
    read_at_node_in_tree (
      forest, node_id,
      |n| match &n . kind {
        ViewNodeKind::True ( t ) =>
          matches! (t . diff,
                    Some (NodeDiffStatus::Removed)),
        ViewNodeKind::Scaff ( _ ) => false } ) ?;
  let hidden_ids : HashSet < ID > =
    // If node is a subscribee, we may need to hide some content.
    get_hidden_ids_if_subscribee ( forest, map, node_id ) ?;
  if let Some ( &preexisting_node_id ) =
    visited . get ( & node_pid )
  { if preexisting_node_id != node_id {
    indefinitize_content_subtree ( forest, map,
                                   preexisting_node_id,
                                   visited, config ) ?; }}
  { // Replace title/body, remove request, mark definitive, add to visited.
    if is_removed_node
      { from_git_replace_title_body (
          // TODO ? This per-node git lookup might be slow.
          forest, node_id, config ) ?; }
      else { from_disk_replace_title_body_and_skgnode (
               forest, map, node_id, config ) ?; }
    write_at_node_in_tree (
      forest, node_id,
      |viewnode| {
        let ViewNodeKind::True ( t ) : &mut ViewNodeKind =
          &mut viewnode.kind
          else { panic! ( "execute_definitive_view_request: expected TrueNode" ) };
        t . view_requests . remove ( & ViewRequest::Definitive );
        t . indefinitive = false; } )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
    visited . insert ( node_pid.clone(), node_id ); }
  if is_removed_node {
    extendDefinitiveSubtree_fromGit (
      forest, map, node_id, config.initial_node_limit,
      visited, config, &hidden_ids, typedb_driver,
      deleted_id_src_map ). await ?; }
  else {
    extendDefinitiveSubtreeFromLeaf (
      forest, map, node_id, config.initial_node_limit,
      visited, config, typedb_driver, &hidden_ids ). await ?;
    if type_and_parent_type_consistent_with_subscribee (
      forest, node_id ) ?
    { maybe_add_hiddenInSubscribeeCol_branch (
        forest, map, node_id, config, typedb_driver
      ). await ?; } }
  Ok (( )) }

/// If the node is a Subscribee (child of SubscribeeCol, grandchild of Subscriber),
/// return the Subscriber's 'hides_from_its_subscriptions' as a HashSet.
/// Otherwise return an empty set (which might be dangerous --
/// see the PITFALL | TODO comment in the function body.
fn get_hidden_ids_if_subscribee (
  tree    : &Tree<ViewNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
) -> Result < HashSet < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < ViewNode > =
    tree . get ( node_id )
    . ok_or ( "get_hidden_ids_if_subscribee: node not found" ) ?;
  if !type_and_parent_type_consistent_with_subscribee (
       tree, node_id )?
  { // PITFALL \ TODO: Maybe this should return an error rather than the empty set. The empty set says 'the subscriber hides none of its content', which is technically accurate, but only because it is not the kind of node that could have contents which could be so hidden.
    return Ok ( HashSet::new () ); }
  else {
    let subscribee_col : NodeRef < ViewNode > =
      node_ref . parent ()
      . ok_or ( "get_hidden_ids_if_subscribee: Subscribee has no parent (SubscribeeCol)" ) ?;
    let subscriber : NodeRef < ViewNode > =
      subscribee_col . parent ()
      . ok_or ( "get_hidden_ids_if_subscribee: SubscribeeCol has no parent (Subscriber)" ) ?;
    let subscriber_id : ID =
      get_id_from_treenode ( tree, subscriber.id() ) ?;
    let hidden_ids : HashSet < ID > =
      map . get ( &subscriber_id ) . and_then (
        |skgnode| skgnode .hides_from_its_subscriptions .clone ( ))
      . unwrap_or_default () . into_iter () . collect ();
    Ok ( hidden_ids ) }}

/// Does two things:
/// - Mark a node, and its entire content subtree, as indefinitive.
/// - Remove them from `visited`.
/// Only recurses into children into non-ignored TrueNodes;
///   ignored and scaffold children persist unchanged.
/// TODO : This will need complication to properly handle
///   sharing related nodes among the input node's descendents.
fn indefinitize_content_subtree (
  tree    : &mut Tree<ViewNode>,
  map     : &mut SkgNodeMap,
  node_id : NodeId,
  visited : &mut DefinitiveMap,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (node_pid, content_child_treeids)
    : (ID, Vec <NodeId>) =
  { let node_ref : NodeRef < ViewNode > =
      tree . get ( node_id )
      . ok_or ( "indefinitize_content_subtree: NodeId not in tree" ) ?;
    let node_pid : ID =
      get_id_from_treenode ( tree, node_id ) ?;
    let content_child_treeids : Vec < NodeId > =
      node_ref . children ()
      . filter ( |c| matches! ( &c . value() . kind,
                                ViewNodeKind::True(t)
                                if !t.parent_ignores ))
      . map ( |c| c . id () )
      . collect ();
    (node_pid, content_child_treeids) };
  visited . remove ( &node_pid );
  makeIndefinitiveAndClobber ( tree, map, node_id, config ) ?;
  for child_treeid in content_child_treeids { // recurse
    indefinitize_content_subtree (
      tree, map, child_treeid, visited, config ) ?; }
  Ok (( )) }

/// Expands content for a node using BFS with a node limit.
///
/// This is similar to `render_initial_forest_bfs`
/// but operates on an existing PairTree.
///
/// The starting node is already definitive. This function:
/// - Reads content children from the SkgNode in the tree
/// - Adds them as children using BFS
/// - Marks repeats (nodes already in `visited`) as indefinitive
/// - Adds new definitive nodes to `visited`
/// - Truncates when the node limit is exceeded:
///   - Adds children in that last gen up to the limit as indefinitive
///   - Completes the sibling group of the limit-hitting node
///   - After limit-hitting node's parent, rewrite nodes in that second-to-last gen as indefinitive
///
/// Generations are relative to the effective root (generation 0),
/// *not* the tree's true root. This way, truncation only affects
/// nodes within this subtree, not siblings of ancestors.
///
/// `hidden_ids` contains IDs to filter from a subscribee's
/// top-level children (without recursing into grandchildren, etc.)
/// If the view was requested from a non-subscribee,
/// 'hidden_ids' will be empty.
async fn extendDefinitiveSubtreeFromLeaf (
  tree           : &mut Tree<ViewNode>,
  map            : &mut SkgNodeMap,
  effective_root : NodeId, // It contained the request and is already in the tree. it was indefinitive when the request was issued, but was made definitive by 'execute_definitive_view_request'.
  limit          : usize,
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
  hidden_ids     : &HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  let mut gen_with_children : Vec < (NodeId, // effective root
                                     ID) > = // one of its children
    content_ids_if_definitive_else_empty (
      tree, map, effective_root ) ?
    . into_iter ()
    . filter ( |skgid| ! hidden_ids . contains ( skgid ) )
    . map ( |child_skgid| (effective_root, child_skgid) )
    . collect ();
  let mut nodes_rendered : usize = 0;
  let mut generation : usize = 1; // children of effective root
  while ! gen_with_children . is_empty () {
    if nodes_rendered + gen_with_children . len () >= limit {
      // Limit hit. Complete this sibling group
      // and truncate later parents.
      let space_left : usize = limit - nodes_rendered;
      add_last_generation_and_truncate_some_of_previous (
        tree, map, generation, &gen_with_children,
        space_left, effective_root, visited, config, driver ) . await ?;
      return Ok (( )); }
    let mut next_gen : Vec < (NodeId, ID) > = Vec::new ();
    for (parent_treeid, child_skgid) in gen_with_children {
      let new_treeid : NodeId = build_node_branch_minus_content (
        Some((tree, parent_treeid)),
        Some(map),
        &child_skgid, config, driver, visited ). await ?;
      nodes_rendered += 1;
      if ! truenode_in_tree_is_indefinitive ( tree, new_treeid ) ? {
        // No filtering here; 'hidden_ids' only applies to top-level.
        let grandchild_skgids : Vec < ID > =
          content_ids_if_definitive_else_empty (
            tree, map, new_treeid ) ?;
        for grandchild_skgid in grandchild_skgids {
          next_gen . push ( (new_treeid, grandchild_skgid) ); }} }
    gen_with_children = next_gen;
    generation += 1; }
  Ok (( )) }

/// Fetches SkgNode from map or disk.
/// Updates title and body.
/// Preserves all other ViewNode data.
fn from_disk_replace_title_body_and_skgnode (
  tree    : &mut Tree<ViewNode>,
  map     : &mut SkgNodeMap,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src) : (ID, SourceName) =
    pid_and_source_from_treenode ( tree, node_id,
      "from_disk_replace_title_body_and_skgnode" ) ?;
  let skgnode : &SkgNode = skgnode_from_map_or_disk (
    &pid, &src, map, config ) ?;
  let title : String = skgnode . title . clone();
  if title . is_empty () {
    return Err ( format! ( "SkgNode {} has empty title", pid ) . into () ); }
  let body : Option < String > = skgnode . body . clone ();
  write_at_node_in_tree ( tree, node_id, |viewnode| { // Update viewnode
    let ViewNodeKind::True ( t ) : &mut ViewNodeKind =
      &mut viewnode . kind
      else { panic! ( "rebuild_pair_from_disk: expected TrueNode" ) };
    t . title = title;
    t . body = body; } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok (( )) }

/// Expand children for a removed node,
/// by loading content from git HEAD.
/// Children that exist in TypeDB are marked as RemovedHere.
/// Children that don't exist in TypeDB are marked as Removed.
async fn extendDefinitiveSubtree_fromGit (
  tree           : &mut Tree<ViewNode>,
  _map           : &mut SkgNodeMap,
  effective_root : NodeId,
  limit          : usize,
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
  hidden_ids     : &HashSet<ID>,
  typedb_driver  : &TypeDBDriver,
  deleted_id_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  let (pid, src) : (ID, SourceName) =
    pid_and_source_from_treenode ( tree, effective_root,
      "extendDefinitiveSubtree_fromGit" ) ?;
  let (contents, contents_in_worktree)
    : (Vec<ID>, HashSet<String>) =
    { let skgnode : SkgNode =
        skgnode_from_git_head ( &pid, &src, config ) ?;
      let contents : Vec<ID> =
        skgnode . contains . unwrap_or_default();
      let not_hidden : BTreeSet<String> =
        contents . iter()
        . filter ( |id| ! hidden_ids . contains ( id ) )
        . take ( limit )
        . map ( |id| id . 0 . clone() )
        . collect();
      let contents_in_worktree : HashSet<String> =
        which_ids_exist (
          &config . db_name, typedb_driver, &not_hidden
        ). await ?;
      (contents, contents_in_worktree) };
  for child_id in contents . iter() . take ( limit ) {
    if hidden_ids . contains ( child_id ) { continue; }
    let child_viewnode : ViewNode =
      mk_removed_child_viewnode (
        child_id, &src, &contents_in_worktree,
        deleted_id_src_map, config, typedb_driver ) . await ?;
    let mut parent_mut : NodeMut<ViewNode> = // Add child to tree
      tree . get_mut ( effective_root ) . ok_or (
        "Parent not found" ) ?;
    parent_mut . append ( child_viewnode );
    visited . insert ( child_id . clone(), effective_root ); }
  Ok (( )) }

/// Build an ViewNode for a child of
/// a removed (in the git diff sense) node.
/// The child is loaded from the worktree if it exists there,
/// and otherwise from git HEAD.
async fn mk_removed_child_viewnode (
  child_id           : &ID,
  parent_src         : &SourceName,
  contents_in_worktree : &HashSet<String>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
  typedb_driver      : &TypeDBDriver,
) -> Result<ViewNode, Box<dyn Error>> {
  let in_worktree : bool =
    contents_in_worktree . contains ( &child_id . 0 );
  let (child_diff, child_opt_skgnode)
    : (NodeDiffStatus, Option<SkgNode>)
    = if in_worktree
      { ( NodeDiffStatus::RemovedHere,
          optskgnode_from_id (
            config, typedb_driver, child_id ) . await ? ) }
      else
      { ( NodeDiffStatus::Removed,
          skgnode_from_git_head ( child_id, parent_src, config
                                ) . ok() ) };
  let child_skgnode : &SkgNode =
    child_opt_skgnode . as_ref()
    . ok_or_else ( || format! (
      "mk_removed_child_viewnode: no SkgNode for child {}",
      child_id ) ) ?;
  let child_source : Option<SourceName> =
    if in_worktree { Some ( child_skgnode . source . clone() ) }
    else           { deleted_id_src_map . get ( child_id )
                       . cloned() };
  let mut child_viewnode : ViewNode =
    mk_indefinitive_viewnode (
      child_id . clone(),
      child_skgnode . source . clone(),
      child_skgnode . title . clone(),
      false );                // parent_ignores
  if let ViewNodeKind::True ( ref mut t ) = child_viewnode . kind {
    if let Some ( source ) = child_source {
      t . source = source; }
    t . diff = Some ( child_diff ); }
  Ok ( child_viewnode ) }

/// Load title and body from git HEAD for a removed node.
/// This is used when expanding a definitive view
/// for a node that exists in HEAD but not on disk.
fn from_git_replace_title_body (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src) : (ID, SourceName) =
    pid_and_source_from_treenode ( tree, node_id,
      "from_git_replace_title_body" ) ?;
  let skgnode : SkgNode =
    skgnode_from_git_head ( &pid, &src, config ) ?;
  write_at_node_in_tree (
    tree, node_id,
    |viewnode| {
      let ViewNodeKind::True ( t ) : &mut ViewNodeKind =
        &mut viewnode . kind
        else { panic! ( "from_git_replace_title_body: expected TrueNode" ) };
      t . title = skgnode.title;
      t . body = skgnode.body; } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok (( )) }
