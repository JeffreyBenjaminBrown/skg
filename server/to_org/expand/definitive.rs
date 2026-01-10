/// SINGLE ENTRY POINT: 'execute_view_requests'.

use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_truncate_some_of_previous;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{
  build_and_integrate_containerward_view_then_drop_request,
  build_and_integrate_sourceward_view_then_drop_request };
use crate::to_org::complete::contents::ensure_source;
use crate::to_org::complete::sharing::maybe_add_hiddenInSubscribeeCol_branch;
use crate::to_org::util::{
  skgnode_and_orgnode_from_pid_and_source,
  build_node_branch_minus_content,
  get_pid_in_pairtree,
  VisitedMap, is_indefinitive,
  content_ids_if_definitive_else_empty };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::ViewRequest;
use crate::types::orgnode::{
    OrgNode, OrgNodeKind, TrueNode, EffectOnParent, mk_orgnode };
use crate::types::tree::{NodePair, PairTree};
use crate::types::tree::generic::write_at_node_in_tree;

use ego_tree::{NodeId, NodeRef};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  forest        : &mut PairTree,
  requests      : Vec < (NodeId, ViewRequest) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut VisitedMap,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  for (node_id, request) in requests {
    match request {
      ViewRequest::Aliases => {
        build_and_integrate_aliases_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Containerward => {
        build_and_integrate_containerward_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Sourceward => {
        build_and_integrate_sourceward_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Definitive => {
        execute_definitive_view_request (
          forest, node_id, config, typedb_driver,
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
  forest        : &mut PairTree, // "forest" = tree with ForestRoot
  node_id       : NodeId, // had the request
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut VisitedMap,
  _errors       : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_pid_in_pairtree (
    forest, node_id ) ?;
  let hidden_ids : HashSet < ID > =
    // If node is a subscribee, we may need to hide some content.
    get_hidden_ids_if_subscribee ( forest, node_id ) ?;
  if let Some ( &preexisting_node_id ) =
    visited . get ( & node_pid )
  { if preexisting_node_id != node_id {
    indefinitize_content_subtree ( forest,
                                   preexisting_node_id,
                                   visited ) ?; }}
  // Ensure node has a source (Subscribee nodes may not have one yet)
  ensure_source (
    forest, node_id, &config.db_name, typedb_driver ) . await ?;
  { // Mutate the root of the definitive view request:
    rebuild_pair_from_disk_mostly_clobbering_the_org (
      // preserves relevant orgnode fields
      forest, node_id, config ) ?;
    write_at_node_in_tree (
      forest, node_id,
      |np| { // remove ViewRequest and mark definitive
        let org = &mut np.orgnode;
        if let Some ( vr ) = org . view_requests_mut () {
          vr . remove ( & ViewRequest::Definitive ); }
        org . set_indefinitive ( false ); } ) ?; }
  visited . insert ( node_pid.clone(), node_id );
  extendDefinitiveSubtreeFromLeaf ( // populate its tree-descendents
    forest, node_id,
    config.initial_node_limit, visited, config, typedb_driver,
    &hidden_ids,
  ) . await ?;
  { // If this is a subscribee with some content hidden by its subscriber, add a HiddenInSubscribeeCol
    let is_subscribee : bool = {
      let node_ref : NodeRef < NodePair > =
        forest . get ( node_id ) . ok_or (
          "execute_definitive_view_request: node not found" ) ?;
      matches! ( &node_ref . value() . orgnode . kind,
                 OrgNodeKind::True(t)
                 if t.effect_on_parent == EffectOnParent::Subscribee ) };
    if is_subscribee {
      maybe_add_hiddenInSubscribeeCol_branch (
        forest, node_id, config, typedb_driver ) . await ?; }}
  Ok (( )) }

/// If the node is a Subscribee (child of SubscribeeCol, grandchild of Subscriber),
/// return the Subscriber's 'hides_from_its_subscriptions' as a HashSet.
/// Otherwise return an empty set (which might be dangerous --
/// see the PITFALL | TODO comment in the function body.
fn get_hidden_ids_if_subscribee (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < HashSet < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "get_hidden_ids_if_subscribee: node not found" ) ?;
  if ! matches! ( &node_ref . value () .orgnode . kind,
                  OrgNodeKind::True(t)
                  if t.effect_on_parent == EffectOnParent::Subscribee )
  { // PITFALL \ TODO: Maybe this is dangerous, because it's misleading. We're saying 'the subscriber hides none of its content', and that's technically accurate, but only because it is not the kind of node that could have contents which could be so hidden.
    return Ok ( HashSet::new () ); }
  else {
    let subscribee_col : NodeRef < NodePair > =
      node_ref . parent ()
      . ok_or ( "get_hidden_ids_if_subscribee: Subscribee has no parent (SubscribeeCol)" ) ?;
    let subscriber : NodeRef < NodePair > =
      subscribee_col . parent ()
      . ok_or ( "get_hidden_ids_if_subscribee: SubscribeeCol has no parent (Subscriber)" ) ?;
    let hidden_ids : HashSet < ID > =
      match & subscriber . value () . mskgnode {
        Some ( skgnode ) =>
          skgnode . hides_from_its_subscriptions . clone ()
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
  tree    : &mut PairTree,
  node_id : NodeId,
  visited : &mut VisitedMap,
) -> Result < (), Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "indefinitize_content_subtree: NodeId not in tree" ) ?;
  let pair : &NodePair =
    node_ref . value ();
  let node_pid_opt : Option < ID > =
    match &pair .orgnode . kind {
      OrgNodeKind::True ( t ) => t . id_opt . clone (),
      OrgNodeKind::Scaff ( _ ) => None };
  let content_child_treeids : Vec < NodeId > =
    node_ref . children ()
    . filter ( |c| matches! ( &c . value () .orgnode . kind,
                              OrgNodeKind::True(t)
                              if t.effect_on_parent == EffectOnParent::Content ))
    . map ( |c| c . id () )
    . collect ();
  if let Some(ref pid) = node_pid_opt { // remove from visited
    visited . remove ( pid ); }
  { // mark indefinitive, restore title if SkgNode present, clear body
    // TODO : This ought to call a function.
    // It, or something very like it anyway, happens elsewhere too.
    let canonical_title : Option<String> =
      pair . mskgnode . as_ref() . map ( |s| s . title . clone () );
    write_at_node_in_tree (
      tree, node_id,
      |np| {
        let org = &mut np.orgnode;
        org . set_indefinitive ( true );
        if let Some ( title ) = canonical_title.clone () {
          org . set_title ( title ); }
        org . clear_body (); } ) ?; }
  for child_treeid in content_child_treeids { // recurse
    indefinitize_content_subtree (
      tree, child_treeid, visited ) ?; }
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
  tree           : &mut PairTree,
  effective_root : NodeId, // It contained the request and is already in the tree. it was indefinitive when the request was issued, but was made definitive by 'execute_definitive_view_request'.
  limit          : usize,
  visited        : &mut VisitedMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
  hidden_ids     : &HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  let mut gen_with_children : Vec < (NodeId, // effective root
                                     ID) > = // one of its children
    content_ids_if_definitive_else_empty (
      tree, effective_root ) ?
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
      add_last_generation_and_truncate_some_of_previous (
        tree, generation, &gen_with_children,
        limit - nodes_rendered,
        effective_root,
        visited, config, driver ). await ?;
      return Ok (( )); }
    let mut next_gen : Vec < (NodeId, ID) > = Vec::new ();
    for (parent_treeid, child_skgid) in gen_with_children {
      let (_tree, new_treeid) =
        build_node_branch_minus_content (
          Some((tree, parent_treeid)),
          &child_skgid, config, driver, visited ). await ?;
      nodes_rendered += 1;
      if ! is_indefinitive ( tree, new_treeid ) ? {
        // No filtering here; 'hidden_ids' only applies to top-level.
        let grandchild_skgids : Vec < ID > =
          content_ids_if_definitive_else_empty (
            tree, new_treeid ) ?;
        for grandchild_skgid in grandchild_skgids {
          next_gen . push ( (new_treeid, grandchild_skgid) ); }} }
    gen_with_children = next_gen;
    generation += 1; }
  Ok (( )) }

/// Fetches fresh (SkgNode, OrgNode) from disk
///   and replaces both in the tree.
/// Preserves OrgnodeCode fields.
/// Replaces all other OrgNode data.
fn rebuild_pair_from_disk_mostly_clobbering_the_org (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src, effect, indef, edit_request, view_requests) = {
    // values to preserve from existing orgnode
    let node_ref : NodeRef<NodePair> = tree.get(node_id)
      .ok_or("rebuild_pair_from_disk_mostly_clobbering_the_org: node not found")?;
    let org : &OrgNode = &node_ref.value().orgnode;
    let t : &TrueNode = match &org.kind {
      OrgNodeKind::True(t) => t,
      OrgNodeKind::Scaff(_) => return Err( "rebuild_pair_from_disk_mostly_clobbering_the_org: node is Scaffold, not TrueNode" . into( )), };
    let pid : ID = t . id_opt . clone() . ok_or("rebuild_pair_from_disk_mostly_clobbering_the_org: node has no ID")?;
    let src : String = t . source_opt . clone() . ok_or("rebuild_pair_from_disk_mostly_clobbering_the_org: node has no source")?;
    (pid, src,
     t.effect_on_parent.clone(),
     t.indefinitive,
     t.edit_request.clone(),
     t.view_requests.clone()) };
  let (skgnode, disk_orgnode) : (SkgNode, OrgNode) = // from disk
    skgnode_and_orgnode_from_pid_and_source(config, &pid, &src)?;
  let disk_t : &TrueNode = match &disk_orgnode.kind {
    OrgNodeKind::True(t) => t,
    OrgNodeKind::Scaff(_) => unreachable!(
      "disk orgnode is always a TrueNode"), };
  let orgnode : OrgNode = mk_orgnode(
    pid, src,
    disk_t . title . clone(),
    disk_t . body . clone(),
    effect, indef, edit_request, view_requests);
  write_at_node_in_tree(
    tree, node_id,
    |np| *np = NodePair { mskgnode: Some(skgnode),
                          orgnode })?;
  Ok (( )) }
