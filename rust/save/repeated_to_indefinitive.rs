use crate::types::OrgNode;
use ego_tree::Tree;

/// Convert repeat markers to indefinitive markers in the forest.
/// For each node in the forest:
/// - If repeat=true, set indefinitive=true and repeat=false
/// - Otherwise leave unchanged
/// This is because repeated nodes should be treated as indefinitive
/// (as should anything that was marked definitive),
/// but 'repeated' must be recalculated.
pub fn change_repeated_to_indefinitive (
  forest : &mut Vec < Tree < OrgNode > >
) {
  for tree in forest . iter_mut () {
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    change_repeated_to_indefinitive_recursive (
      tree,
      root_id ); }}

fn change_repeated_to_indefinitive_recursive (
  tree    : &mut Tree < OrgNode >,
  node_id : ego_tree::NodeId,
) {
  { // Process current node
    let mut node_mut : ego_tree::NodeMut < OrgNode > =
      tree . get_mut ( node_id ) . unwrap ();
    if node_mut . value () . metadata . repeat {
      node_mut . value () . metadata . indefinitive = true;
      node_mut . value () . metadata . repeat = false; }}
  { // Recurse to children
    let child_ids : Vec < ego_tree::NodeId > =
      tree . get ( node_id ) . unwrap ()
      . children ()
      . map ( |c| c . id () )
      . collect ();
    for child_id in child_ids {
      change_repeated_to_indefinitive_recursive (
        tree,
        child_id ); }} }
