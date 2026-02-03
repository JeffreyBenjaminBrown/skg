use crate::types::orgnode::OrgNode;
use crate::types::tree::generic::with_node_mut;
use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::hash::Hash;

/// Reconciles a parent's "relevant" children against a desired list of 'orderkeys'. The payload of each tree node should be some possibly-improper supertype of 'orderkey' -- that is, there is a way to extract the orderkey from a node, but the node might have other information.
///
/// 1. Partition children into "relevant" (matching some predicate) and "irrelevant". It is not necessary that things not matching the predicate be able to provide an orderkey. (So for instance, Scaffolds can be discarded, and each member of the remaining 'relevant set' can be expected to have an ID, even though Scafflods don't have those.)
/// 2. Among relevant children, keeps only those whose orderkey appears in goal_list, removing the rest of the 'relevant set'. Also discard duplicates.
/// 3. Remove duplicates in the relevant set.
///   TODO: Only removes dups in the relevant set, right?
/// 4. Reorders children: irrelevant first, then relevant in the order of goal_list.
/// 5. Creates new relevant children for any orderkeys missing from the original children.
/// 6. Passes focus to the closest remaining ancestor when removing focused nodes.
///
/// Parameters:
/// - `tree`: The ego_tree containing OrgNodes
/// - `parent_id`: NodeId of the parent whose children we're reconciling
/// - `relevant`: Predicate - returns true for "relevant" children
/// - `view_child_orderkey`: Extracts the orderkey from a relevant child
/// - `goal_list`: The source-of-truth list of orderkeys in desired order
/// - `create_child`: Factory to create a new OrgNode from a orderkey.
pub fn complete_relevant_children<Orderkey, Relevant, View> (
  tree                    : &mut Tree<OrgNode>,
  parent_id               : NodeId,
  relevant                : Relevant,
  view_child_orderkey     : View,
  goal_list : &[Orderkey],
  create_child            : impl Fn(&Orderkey) -> OrgNode,
) -> Result<(), Box<dyn Error>>
where
  Relevant : Fn(&OrgNode) -> bool,
  View     : Fn(&OrgNode) -> Orderkey,
  Orderkey : Eq + Hash + Clone,
{
  let ( relevant_ids, irrelevant_ids )
    : ( Vec<NodeId>, Vec<NodeId> )
    = partition_children( tree, parent_id, &relevant )
        .map_err( |e| -> Box<dyn Error> { e.into() } )?;
  let goal_list_as_set : HashSet<Orderkey> =
    goal_list.iter().cloned().collect();

  let mut orderkey_to_treeid
    : HashMap<Orderkey, NodeId> = HashMap::new();
  let mut duplicate_ids : Vec<( NodeId, Orderkey )> = Vec::new();
  let mut invalid_ids : Vec<NodeId> = Vec::new();
  for &node_id in &relevant_ids { // populate the above three variables
    let node_ref : NodeRef<OrgNode> =
      tree.get( node_id )
      .ok_or( "complete_relevant_children: node not found" )?;
    let orderkey : Orderkey =
      view_child_orderkey( node_ref.value() );
    if !goal_list_as_set.contains( &orderkey ) {
      invalid_ids.push( node_id ); // hopefully none
    } else if orderkey_to_treeid.contains_key( &orderkey ) {
      duplicate_ids.push( ( node_id, orderkey ) ); // hopefully none
    } else {
      orderkey_to_treeid.insert( orderkey, node_id ); }}
  let deleting_focused_subtree : bool =
    { let mut found : bool = false;
      for &( node_id, _ ) in &duplicate_ids {
        if subtree_has_focus( tree, node_id )? {
          found = true; }}
      for &node_id in &invalid_ids {
        if subtree_has_focus( tree, node_id )? {
          found = true; }}
      found };
  { // Remove invalid and duplicate nodes
    for &( node_id, _ ) in &duplicate_ids {
      with_node_mut( tree, node_id, |mut n| { n.detach(); } )
        .map_err( |e| -> Box<dyn Error> { e.into() } )?; }
    for &node_id in &invalid_ids {
      with_node_mut( tree, node_id, |mut n| { n.detach(); } )
        .map_err( |e| -> Box<dyn Error> { e.into() } )?; }}
  { // Transfer focus to parent if we removed a focused subtree
    if deleting_focused_subtree {
      with_node_mut(
          tree, parent_id,
          |mut n| { n.value().focused = true; }
        ). map_err( |e| -> Box<dyn Error> { e.into() } )?; }}
  { // Reorder: move all irrelevant children to end. (They will precede the relevant ones.)
    for &child_id in &irrelevant_ids {
      move_child_to_end( tree, parent_id, child_id )?; }}
  { // Move/create relevant children in desired order
    for orderkey in goal_list {
      match orderkey_to_treeid.get( orderkey ) {
        Some( &child_id ) => {
          move_child_to_end( tree, parent_id, child_id )?; },
        None => {
          let orgnode : OrgNode = create_child( orderkey );
          with_node_mut(
              tree, parent_id,
              |mut p| { p.append( orgnode ); }
            ). map_err( |e| -> Box<dyn Error>
                        { e.into() } )?; }, }} }
  Ok (( )) }

/// Partition children of a node based on a predicate.
/// Returns (true_children, false_children) where:
/// - true_children: NodeIds of children for which predicate returns true
/// - false_children: NodeIds of children for which predicate returns false
/// Order is preserved within each list.
pub fn partition_children<F> (
  tree    : &Tree<OrgNode>,
  treeid  : NodeId,
  predicate : F,
) -> Result<( Vec<NodeId>, Vec<NodeId> ), String>
where F: Fn(&OrgNode) -> bool {
  let node_ref : NodeRef<OrgNode> =
    tree.get( treeid )
    .ok_or( "partition_children: node not found" )?;
  let mut true_children  : Vec<NodeId> = Vec::new();
  let mut false_children : Vec<NodeId> = Vec::new();
  for child_ref in node_ref.children() {
    if predicate( child_ref.value() ) {
      true_children.push( child_ref.id() );
    } else {
      false_children.push( child_ref.id() ); }}
  Ok( ( true_children, false_children ) ) }

/// Returns true if this node or any of its descendants has `focused = true`.
pub fn subtree_has_focus (
  tree    : &Tree<OrgNode>,
  node_id : NodeId,
) -> Result<bool, String> {
  let node_ref : NodeRef<OrgNode> =
    tree.get( node_id )
    .ok_or( "subtree_has_focus: node not found" )?;
  if node_ref.value().focused {
    return Ok( true ); }
  for child_ref in node_ref.children() {
    if subtree_has_focus( tree, child_ref.id() )? {
      return Ok( true ); }}
  Ok( false ) }

/// Move an existing child to the end of its parent's children list.
/// Uses detach() + append_id() to move without cloning.
pub fn move_child_to_end (
  tree      : &mut Tree<OrgNode>,
  parent_id : NodeId,
  child_id  : NodeId,
) -> Result<(), Box<dyn Error>> {
  with_node_mut( tree, child_id, |mut n| { n.detach(); } )
    .map_err( |e| -> Box<dyn Error> { e.into() } )?;
  with_node_mut( tree, parent_id, |mut p| { p.append_id( child_id ); } )
    .map_err( |e| -> Box<dyn Error> { e.into() } )?;
  Ok( () ) }
