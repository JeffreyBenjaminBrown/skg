use crate::types::viewnode::ViewNode;
use crate::types::tree::generic::with_node_mut;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::hash::Hash;

/// Apply treatment to each child that should be treated.
pub fn treat_certain_children<Node, Treated, Treatment> (
  tree          : &mut Tree<Node>,
  parent_id     : NodeId,
  treated       : Treated,
  mut treatment : Treatment,
) -> Result<(), String>
where
  Treated   : Fn(&Node) -> bool,
  Treatment : FnMut(&mut Node),
{
  let child_ids : Vec<NodeId> =
    { let node_ref : NodeRef<Node> =
        tree.get( parent_id )
        .ok_or( "treat_certain_children: node not found" )?;
      node_ref.children().map( |c| c.id() ).collect() };
  for child_id in child_ids {
    with_node_mut( tree, child_id, |mut n| {
      if treated( n.value() ) {
        treatment( n.value() ); }
    } ).map_err( |e| -> String { e.into() } )?; }
  Ok( () ) }

/// Classify children of a node based on a classifier function.
/// Returns a HashMap from classification key to Vec<NodeId>.
/// Order is preserved within each list.
pub fn partition_children<Node, F> (
  tree       : &Tree<Node>,
  treeid     : NodeId,
  classifier : F,
) -> Result<HashMap<i32, Vec<NodeId>>, String>
where F: Fn(&Node) -> i32 {
  let node_ref : NodeRef<Node> =
    tree.get( treeid )
    .ok_or( "partition_children: node not found" )?;
  let mut groups : HashMap<i32, Vec<NodeId>> = HashMap::new();
  for child_ref in node_ref.children() {
    let key = classifier( child_ref.value() );
    groups.entry( key ).or_default().push( child_ref.id() ); }
  Ok( groups ) }

/// Returns true if this node or any descendant satisfies the predicate.
pub fn subtree_satisfies<Node, Predicate> (
  tree      : &Tree<Node>,
  node_id   : NodeId,
  predicate : &Predicate,
) -> Result<bool, String>
where Predicate: Fn(&Node) -> bool {
  let node_ref : NodeRef<Node> =
    tree.get( node_id )
    .ok_or( "subtree_satisfies: node not found" )?;
  if predicate( node_ref.value() ) {
    return Ok( true ); }
  for child_ref in node_ref.children() {
    if subtree_satisfies( tree, child_ref.id(), predicate )? {
      return Ok( true ); } }
  Ok( false ) }

/// Move an existing child to the end of its parent's children list.
/// Uses detach() + append_id() to move without cloning.
pub fn move_child_to_end<Node> (
  tree      : &mut Tree<Node>,
  parent_id : NodeId,
  child_id  : NodeId,
) -> Result<(), Box<dyn Error>> {
  with_node_mut( tree, child_id, |mut n| { n.detach(); } )
    .map_err( |e| -> Box<dyn Error> { e.into() } )?;
  with_node_mut( tree, parent_id, |mut p| { p.append_id( child_id ); } )
    .map_err( |e| -> Box<dyn Error> { e.into() } )?;
  Ok( () ) }

/// ViewNode-specialized version of complete_relevant_children.
/// See that one's description for more info.
/// This one specializes it so that:
/// - problem discards are those that would discard the focused node.
/// - if any discard is problematic, transfer focus to 'treeid'
pub fn complete_relevant_children_in_viewnodetree
<Orderkey, Relevant, View> (
  tree                : &mut Tree<ViewNode>,
  treeid              : NodeId,
  relevant            : Relevant,
  view_child_orderkey : View,
  goal_list           : &[Orderkey],
  create_child        : impl Fn(&Orderkey) -> ViewNode,
) -> Result<(), Box<dyn Error>>
where Relevant : Fn(&ViewNode) -> bool,
      View     : Fn(&ViewNode) -> Orderkey,
      Orderkey : Eq + Hash + Clone,
{
  let problem_discard =
    |tree: &Tree<ViewNode>, node_id: NodeId| -> Result<bool, String> {
      subtree_satisfies( tree, node_id, &|n: &ViewNode| n.focused ) };
  let problem_discard_response =
    |n: &mut ViewNode| { n.focused = true; };
  complete_relevant_children(
    tree,
    treeid,
    relevant,
    view_child_orderkey,
    goal_list,
    create_child,
    problem_discard,
    problem_discard_response ) }

/// TODO ? Marking parentIgnores might be than deletion.
/// (Or, keeping it abstract, replacing deletion with a caller-supplied lambda.)
/// .
/// PURPOSE:
/// Reconciles a parent's "relevant" children against a desired list of 'orderkeys'.
/// The payload of each tree node should be some possibly-improper supertype of 'orderkey' -- that is,
/// there is a way to extract the orderkey from a node, but the node might have other information.
/// .
/// STEPS:
/// - Partitions children into "relevant" (matching a predicate) and "irrelevant".
///   It is not necessary that irrelevant children be able to provide an orderkey.
/// - Among relevant children, identifies duplicates and those whose orderkey is not in goal_list.
///   Runs problem_discard on each such node.
///   If any returns true, will run problem_discard_response on the parent after discarding.
///   Then discards those nodes.
/// - Reorders remaining children: irrelevant first, then relevant in goal_list order.
///   Creates new children for any orderkeys missing from the original children.
pub fn complete_relevant_children
<Node, Orderkey, Relevant, View, ProblemDiscard, ProblemResponse> (
  tree                     : &mut Tree<Node>,
  parent_id                : NodeId,
  relevant                 : Relevant,
  view_child_orderkey      : View,
  goal_list                : &[Orderkey],
  create_child             : impl Fn(&Orderkey) -> Node,
  problem_discard          : ProblemDiscard,
  mut problem_discard_response : ProblemResponse,
) -> Result<(), Box<dyn Error>>
where
  Relevant        : Fn(&Node) -> bool,
  View            : Fn(&Node) -> Orderkey,
  Orderkey        : Eq + Hash + Clone,
  ProblemDiscard  : Fn(&Tree<Node>, NodeId) -> Result<bool, String>,
  ProblemResponse : FnMut(&mut Node),
{
  let groups : HashMap<i32, Vec<NodeId>> =
    partition_children (
      tree, parent_id,
      |n| if relevant(n) { 1 } else { 0 } )
        . map_err( |e| -> Box<dyn Error> { e.into() } )?;
  let relevant_ids   : Vec<NodeId> =
    groups.get( &1 ).cloned().unwrap_or_default();
  let irrelevant_ids : Vec<NodeId> =
    groups.get( &0 ).cloned().unwrap_or_default();
  let goal_list_as_set : HashSet<Orderkey> =
    goal_list.iter().cloned().collect();

  let mut orderkey_to_treeid
    : HashMap<Orderkey, NodeId> = HashMap::new();
  let mut duplicate_ids : Vec<( NodeId, Orderkey )> = Vec::new();
  let mut invalid_ids : Vec<NodeId> = Vec::new();
  for &node_id in &relevant_ids { // populate the above three variables
    let node_ref : NodeRef<Node> =
      tree.get( node_id )
      .ok_or( "complete_relevant_children: node not found" )?;
    let orderkey : Orderkey =
      view_child_orderkey( node_ref.value() );
    if !goal_list_as_set.contains( &orderkey ) {
      invalid_ids.push( node_id ); // hopefully none
    } else if orderkey_to_treeid.contains_key( &orderkey ) {
      duplicate_ids.push( ( node_id, orderkey ) ); // hopefully none
    } else {
      orderkey_to_treeid.insert( orderkey, node_id ); } }
  let discard_has_problem : bool =
    { let mut found : bool = false;
      for &( node_id, _ ) in &duplicate_ids {
        if problem_discard( tree, node_id )? {
          found = true; } }
      for &node_id in &invalid_ids {
        if problem_discard( tree, node_id )? {
          found = true; } }
      found };
  { // Remove invalid and duplicate nodes
    for &( node_id, _ ) in &duplicate_ids {
      with_node_mut( tree, node_id, |mut n| { n.detach(); } )
        .map_err( |e| -> Box<dyn Error> { e.into() } )?; }
    for &node_id in &invalid_ids {
      with_node_mut( tree, node_id, |mut n| { n.detach(); } )
        .map_err( |e| -> Box<dyn Error> { e.into() } )?; } }
  if discard_has_problem {
    // Respond to problem if any discard was problematic
    with_node_mut(
        tree, parent_id,
        |mut n| { problem_discard_response( n.value() ); }
      ). map_err( |e| -> Box<dyn Error> { e.into() } )?; }
  for &child_id in &irrelevant_ids {
    // Move irrelevant children to end. (They will precede the relevant ones.)
    move_child_to_end( tree, parent_id, child_id )?; }
  for orderkey in goal_list {
    // Move/create relevant children in desired order
    match orderkey_to_treeid.get( orderkey ) {
      Some( &child_id ) => {
        move_child_to_end( tree, parent_id, child_id )?; },
      None => {
        let node : Node = create_child( orderkey );
        with_node_mut(
            tree, parent_id,
            |mut p| { p.append( node ); }
          ). map_err( |e| -> Box<dyn Error>
                      { e.into() } )?; }, }}
  Ok (( )) }
