use crate::types::viewnode::{ViewNode, ViewNodeKind, ViewRequest};
use crate::types::tree::generic::read_at_node_in_tree;

use ego_tree::{Tree, NodeId};
use std::error::Error;

pub fn collectViewRequestsFromForest<T> (
  forest : &Tree<T>,
) -> Result < Vec < (NodeId, ViewRequest) >, Box<dyn Error> >
where T: AsRef<ViewNode>,
{ let mut view_requests : Vec < (NodeId, ViewRequest) > =
    Vec::new ();
  let tree_root_ids : Vec < NodeId > =
    forest . root () . children ()
    . map ( |c| c . id () )
    . collect ();
  for tree_root_id in tree_root_ids {
    collectViewRequestsFromNode (
      forest, tree_root_id, &mut view_requests ) ?; }
  Ok ( view_requests ) }

fn collectViewRequestsFromNode<T> (
  tree              : &Tree<T>,
  node_id           : NodeId,
  view_requests_out : &mut Vec < (NodeId, ViewRequest) >,
) -> Result < (), Box<dyn Error> >
where T: AsRef<ViewNode>,
{ let node_view_requests : Vec < ViewRequest > =
    read_at_node_in_tree (
      tree, node_id,
      |value| match &value . as_ref () . kind {
        ViewNodeKind::True ( t ) =>
          t . view_requests . iter () . cloned () . collect (),
        ViewNodeKind::Scaff ( _ ) => Vec::new (), }
    ). map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
  for request in node_view_requests {
    view_requests_out . push ( (node_id, request) ); }
  let child_ids : Vec < NodeId > =
    tree . get ( node_id )
    . ok_or ( "collectViewRequestsFromNode: node not found" ) ?
    . children ()
    . map ( |c| c . id () )
    . collect ();
  for child_id in child_ids {
    collectViewRequestsFromNode (
      tree, child_id, view_requests_out ) ?; }
  Ok (( )) }

impl AsRef<ViewNode> for ViewNode {
  fn as_ref(&self) -> &ViewNode {
    self }}

impl AsMut<ViewNode> for ViewNode {
  fn as_mut(&mut self) -> &mut ViewNode {
    self }}
