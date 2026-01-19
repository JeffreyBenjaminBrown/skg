use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_truncate_some_of_previous;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{
  build_and_integrate_containerward_view_then_drop_request,
  build_and_integrate_sourceward_view_then_drop_request};
use crate::to_org::complete::contents::ensure_source;
use crate::to_org::complete::sharing::{
  maybe_add_hiddenInSubscribeeCol_branch,
  type_and_parent_type_consistent_with_subscribee };
use crate::to_org::util::{
  build_node_branch_minus_content, get_id_from_treenode,
  DefinitiveMap,
  truenode_in_tree_is_indefinitive,
  content_ids_if_definitive_else_empty };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::types::orgnode::{ OrgNode, OrgNodeKind, ViewRequest };
use crate::types::tree::generic::write_at_node_in_tree;

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  forest        : &mut Tree<OrgNode>,
  map           : &mut SkgNodeMap,
  requests      : Vec < (NodeId, ViewRequest) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut DefinitiveMap,
  errors        : &mut Vec < String >,
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
          visited, errors ) . await ?; }, } }
  Ok (( )) }

/// Expands a definitive view request.
///
/// This function:
/// 1. 'Indefinitizes' any previously definitive OrgNode with that ID
/// 2. Marks this OrgNode definitive
/// 3. Expands (BFS) its content from disk, applying the node limit
/// 4. Removes its ViewRequest::Definitive
///
/// For Subscribee nodes: the subscriber's 'hides_from_its_subscriptions'
/// list is used to filter out content that should be hidden.
async fn execute_definitive_view_request (
  forest        : &mut Tree<OrgNode>, // "forest" = tree with ForestRoot
  map           : &mut SkgNodeMap,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut DefinitiveMap,
  _errors       : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_id_from_treenode (
    forest, node_id ) ?;
  let hidden_ids : HashSet < ID > =
    // If node is a subscribee, we may need to hide some content.
    get_hidden_ids_if_subscribee ( forest, map, node_id ) ?;
  if let Some ( &preexisting_node_id ) =
    visited . get ( & node_pid )
  { if preexisting_node_id != node_id {
    indefinitize_content_subtree ( forest, map,
                                   preexisting_node_id,
                                   visited ) ?; }}
  ensure_source ( // Subscribee nodes may not have one yet.
    forest, node_id, &config.db_name, typedb_driver ) . await ?;
  { // Mutate the root of the definitive view request:
    from_disk_replace_title_body_and_skgnode (
      // preserves relevant orgnode fields
      forest, map, node_id, config ) ?;
    write_at_node_in_tree (
      forest, node_id,
      |orgnode| { // remove ViewRequest and mark definitive
        let OrgNodeKind::True ( t ) : &mut OrgNodeKind =
          &mut orgnode.kind
          else { panic! ( "execute_definitive_view_request_v2: expected TrueNode" ) };
        t . view_requests . remove ( & ViewRequest::Definitive );
        t . indefinitive = false; } )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?; }
  visited . insert ( node_pid.clone(), node_id );
  extendDefinitiveSubtreeFromLeaf ( // populate its tree-descendents
    forest, map, node_id,
    config.initial_node_limit, visited, config, typedb_driver,
    &hidden_ids,
  ) . await ?;
  // If this is a subscribee with some content hidden by its subscriber, add a HiddenInSubscribeeCol
  if type_and_parent_type_consistent_with_subscribee (
    forest, node_id )?
  { maybe_add_hiddenInSubscribeeCol_branch (
      forest, map, node_id, config, typedb_driver
    ). await ?; }
  Ok (( )) }

/// If the node is a Subscribee (child of SubscribeeCol, grandchild of Subscriber),
/// return the Subscriber's 'hides_from_its_subscriptions' as a HashSet.
/// Otherwise return an empty set (which might be dangerous --
/// see the PITFALL | TODO comment in the function body.
fn get_hidden_ids_if_subscribee (
  tree    : &Tree<OrgNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
) -> Result < HashSet < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < OrgNode > =
    tree . get ( node_id )
    . ok_or ( "get_hidden_ids_if_subscribee: node not found" ) ?;
  if !type_and_parent_type_consistent_with_subscribee (
       tree, node_id )?
  { // PITFALL \ TODO: Maybe this should return an error rather than the empty set. The empty set says 'the subscriber hides none of its content', which is technically accurate, but only because it is not the kind of node that could have contents which could be so hidden.
    return Ok ( HashSet::new () ); }
  else {
    let subscribee_col : NodeRef < OrgNode > =
      node_ref . parent ()
      . ok_or ( "get_hidden_ids_if_subscribee: Subscribee has no parent (SubscribeeCol)" ) ?;
    let subscriber : NodeRef < OrgNode > =
      subscribee_col . parent ()
      . ok_or ( "get_hidden_ids_if_subscribee: SubscribeeCol has no parent (Subscriber)" ) ?;
    let subscriber_id : Option<ID>
      = match &subscriber . value () . kind
      { OrgNodeKind::True(t) => t . id_opt . clone(),
        OrgNodeKind::Scaff(_) => None, };
    let hidden_ids : HashSet < ID > =
      match subscriber_id {
        Some ( id ) =>
          map . get ( &id ) . and_then (
            |skgnode| skgnode .hides_from_its_subscriptions .clone ( ))
          . unwrap_or_default () . into_iter () . collect (),
        None => HashSet::new (), };
    Ok ( hidden_ids ) }}

/// Does two things:
/// - Mark a node, and its entire content subtree, as indefinitive.
/// - Remove them from `visited`.
/// Only recurses into content children (interp == Content).
///   Non-content children (AliasCol, ParentIgnores, etc.)
///   persist unchanged.
fn indefinitize_content_subtree (
  tree    : &mut Tree<OrgNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
  visited : &mut DefinitiveMap,
) -> Result < (), Box<dyn Error> > {
  let node_ref : NodeRef < OrgNode > =
    tree . get ( node_id )
    . ok_or ( "indefinitize_content_subtree: NodeId not in tree" ) ?;
  let orgnode : &OrgNode =
    node_ref . value ();
  let node_pid_opt : Option < ID > =
    match &orgnode . kind {
      OrgNodeKind::True ( t ) => t . id_opt . clone (),
      OrgNodeKind::Scaff ( _ ) => None };
  let content_child_treeids : Vec < NodeId > =
    node_ref . children ()
    . filter ( |c| matches! ( &c . value() . kind,
                              OrgNodeKind::True(t)
                              if !t.parent_ignores ))
    . map ( |c| c . id () )
    . collect ();
  if let Some(ref pid) = node_pid_opt { // remove from visited
    visited . remove ( pid ); }
  { // mark indefinitive, restore title if SkgNode present, clear body
    // TODO : This ought to call a function.
    // It, or something very like it anyway, happens elsewhere too.
    let canonical_title : Option<String> =
      node_pid_opt . as_ref () . and_then (
        |pid| map . get (pid) )
      . map ( |s| s . title . clone () );
    write_at_node_in_tree (
      tree, node_id,
      |orgnode| {
        let OrgNodeKind::True ( t ) : &mut OrgNodeKind =
          &mut orgnode.kind
          else { panic! ( "indefinitize_content_subtree: expected TrueNode" ) };
        t . indefinitive = true;
        if let Some ( title ) = canonical_title.clone () {
          t . title = title; }
        t . body = None; } )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?; }
  for child_treeid in content_child_treeids { // recurse
    indefinitize_content_subtree (
      tree, map, child_treeid, visited ) ?; }
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
  tree           : &mut Tree<OrgNode>,
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
/// Preserves all other OrgNode data.
fn from_disk_replace_title_body_and_skgnode (
  tree    : &mut Tree<OrgNode>,
  map     : &mut SkgNodeMap,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src) : (ID, String) = {
    let node_ref : NodeRef<OrgNode> = tree . get(node_id) . ok_or(
      "rebuild_pair_from_disk: node not found") ?;
    let OrgNodeKind::True ( t ) : &OrgNodeKind
      = &node_ref . value () . kind
      else { return Err ( "rebuild_pair_from_disk: expected TrueNode" . into () ) };
    ( t .id_opt     .clone() .ok_or ( "rebuild_pair_from_disk: no ID" ) ?,
      t .source_opt .clone() .ok_or ( "rebuild_pair_from_disk: no source" ) ? ) };
  let skgnode : &SkgNode = skgnode_from_map_or_disk (
    &pid, map, config, &src ) ?;
  let title : String = skgnode . title . clone();
  if title . is_empty () {
    return Err ( format! ( "SkgNode {} has empty title", pid ) . into () ); }
  let body : Option < String > = skgnode . body . clone ();
  write_at_node_in_tree ( tree, node_id, |orgnode| { // Update orgnode
    let OrgNodeKind::True ( t ) : &mut OrgNodeKind =
      &mut orgnode . kind
      else { panic! ( "rebuild_pair_from_disk: expected TrueNode" ) };
    t . title = title;
    t . body = body; } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok (( )) }
