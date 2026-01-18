use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_truncate_some_of_previous_v2;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{
  build_and_integrate_containerward_view_then_drop_request,
  build_and_integrate_sourceward_view_then_drop_request};
use crate::dbs::filesystem::one_node::skgnode_from_pid_and_source;
use crate::to_org::complete::contents::ensure_source;
use crate::to_org::complete::sharing::{
  maybe_add_hiddenInSubscribeeCol_branch_v2,
  type_and_parent_type_consistent_with_subscribee_in_orgtree };
use crate::to_org::util::{
  build_node_branch_minus_content_v2, get_pid_in_tree,
  DefinitiveMap,
  truenode_in_orgtree_is_indefinitive,
  content_ids_if_definitive_else_empty_in_orgtree };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::{SkgNode, SkgNodeMap};
use crate::types::orgnode::{ OrgNode, OrgNodeKind, ViewRequest };
use crate::types::tree::generic::write_at_node_in_tree;

use ego_tree::{NodeId, NodeRef};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  forest        : &mut ego_tree::Tree<OrgNode>,
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
  forest        : &mut ego_tree::Tree<OrgNode>,
  map           : &mut SkgNodeMap,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut DefinitiveMap,
  _errors       : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_pid_in_tree (
    forest, node_id ) ?;
  let hidden_ids : HashSet < ID > =
    get_hidden_ids_if_subscribee ( forest, map, node_id ) ?;
  if let Some ( &preexisting_node_id ) =
    visited . get ( & node_pid )
  { if preexisting_node_id != node_id {
    indefinitize_content_subtree ( forest, map,
                                     preexisting_node_id,
                                     visited ) ?; }}
  // Ensure node has a source
  ensure_source (
    forest, node_id, &config.db_name, typedb_driver ) . await ?;
  { // Mutate the root of the definitive view request:
    from_disk_replace_title_body_and_skgnode (
      forest, map, node_id, config ) ?;
    write_at_node_in_tree (
      forest, node_id,
      |orgnode| {
        let OrgNodeKind::True ( t ) = &mut orgnode.kind
          else { panic! ( "execute_definitive_view_request_v2: expected TrueNode" ) };
        t . view_requests . remove ( & ViewRequest::Definitive );
        t . indefinitive = false; } )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?; }
  visited . insert ( node_pid.clone(), node_id );
  extendDefinitiveSubtreeFromLeaf (
    forest, map, node_id,
    config.initial_node_limit, visited, config, typedb_driver,
    &hidden_ids,
  ) . await ?;
  // If this is a subscribee with hidden content, add HiddenInSubscribeeCol
  if type_and_parent_type_consistent_with_subscribee_in_orgtree (
    forest, node_id )?
  { maybe_add_hiddenInSubscribeeCol_branch_v2 (
      forest, map, node_id, config, typedb_driver
    ). await ?; }
  Ok (( )) }

/// If the node is a Subscribee (child of SubscribeeCol, grandchild of Subscriber),
/// return the Subscriber's 'hides_from_its_subscriptions' as a HashSet.
/// Otherwise return an empty set (which might be dangerous --
/// see the PITFALL | TODO comment in the function body.
fn get_hidden_ids_if_subscribee (
  tree    : &ego_tree::Tree<OrgNode>,
  map     : &SkgNodeMap,
  node_id : NodeId,
) -> Result < HashSet < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < OrgNode > =
    tree . get ( node_id )
    . ok_or ( "get_hidden_ids_if_subscribee: node not found" ) ?;
  if !type_and_parent_type_consistent_with_subscribee_in_orgtree (
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
    // Get subscriber ID from the node
    let subscriber_id : Option<ID> = match &subscriber . value () . kind {
      OrgNodeKind::True(t) => t . id_opt . clone(),
      OrgNodeKind::Scaff(_) => None,
    };
    let hidden_ids : HashSet < ID > =
      match subscriber_id {
        Some ( id ) =>
          map . get ( &id )
          . and_then ( |skgnode| skgnode . hides_from_its_subscriptions . clone () )
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
  tree    : &mut ego_tree::Tree<OrgNode>,
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
    let canonical_title : Option<String> =
      node_pid_opt . as_ref ()
      . and_then ( |pid| map . get ( pid ) )
      . map ( |s| s . title . clone () );
    write_at_node_in_tree (
      tree, node_id,
      |orgnode| {
        let OrgNodeKind::True ( t ) = &mut orgnode.kind
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


/// Expands content for a definitive node using BFS with a node limit.
/// Works with Tree<OrgNode> + SkgNodeMap.
async fn extendDefinitiveSubtreeFromLeaf (
  tree           : &mut ego_tree::Tree<OrgNode>,
  map            : &mut SkgNodeMap,
  effective_root : NodeId,
  limit          : usize,
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
  hidden_ids     : &HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  let mut gen_with_children : Vec < (NodeId, ID) > =
    content_ids_if_definitive_else_empty_in_orgtree (
      tree, map, effective_root ) ?
    . into_iter ()
    . filter ( |skgid| ! hidden_ids . contains ( skgid ) )
    . map ( |child_skgid| (effective_root, child_skgid) )
    . collect ();
  let mut nodes_rendered : usize = 0;
  let mut generation : usize = 1;
  while ! gen_with_children . is_empty () {
    if nodes_rendered + gen_with_children . len () >= limit {
      // Limit hit - add final generation as indefinitive and truncate
      let space_left : usize = limit - nodes_rendered;
      add_last_generation_and_truncate_some_of_previous_v2 (
        tree, map, generation, &gen_with_children,
        space_left, effective_root, visited, config, driver ) . await ?;
      return Ok (( )); }
    let mut next_gen : Vec < (NodeId, ID) > = Vec::new ();
    for (parent_treeid, child_skgid) in gen_with_children {
      let (_tree_opt, _map_opt, new_treeid) =
        build_node_branch_minus_content_v2 (
          Some((tree, map, parent_treeid)),
          &child_skgid, config, driver, visited ). await ?;
      nodes_rendered += 1;
      if ! truenode_in_orgtree_is_indefinitive ( tree, new_treeid ) ? {
        let grandchild_skgids : Vec < ID > =
          content_ids_if_definitive_else_empty_in_orgtree (
            tree, map, new_treeid ) ?;
        for grandchild_skgid in grandchild_skgids {
          next_gen . push ( (new_treeid, grandchild_skgid) ); }} }
    gen_with_children = next_gen;
    generation += 1; }
  Ok (( )) }


/// Fetches SkgNode from disk, updates both tree and map.
/// Works with Tree<OrgNode> + SkgNodeMap.
fn from_disk_replace_title_body_and_skgnode (
  tree    : &mut ego_tree::Tree<OrgNode>,
  map     : &mut SkgNodeMap,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src) : (ID, String) = {
    let node_ref : NodeRef<OrgNode> = tree . get(node_id) . ok_or(
      "rebuild_pair_from_disk: node not found") ?;
    let OrgNodeKind::True ( t )
    = &node_ref . value () . kind
    else { return Err ( "rebuild_pair_from_disk: expected TrueNode" . into () ) };
    ( t .id_opt     .clone() .ok_or ( "rebuild_pair_from_disk: no ID" ) ?,
      t .source_opt .clone() .ok_or ( "rebuild_pair_from_disk: no source" ) ? ) };
  let skgnode : SkgNode =
    skgnode_from_pid_and_source (
      config, pid.clone (), &src ) ?;
  let title : String = skgnode . title . clone();
  if title . is_empty () {
    return Err ( format! ( "SkgNode {} has empty title", pid ) . into () ); }
  let body : Option < String > = skgnode . body . clone ();
  // Update map
  map . insert ( pid, skgnode );
  // Update tree
  write_at_node_in_tree ( tree, node_id, |orgnode| {
    let OrgNodeKind::True ( t ) = &mut orgnode . kind
      else { panic! ( "rebuild_pair_from_disk: expected TrueNode" ) };
    t . title = title;
    t . body = body; } )
    . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  Ok (( )) }
