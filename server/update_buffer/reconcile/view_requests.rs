use crate::to_org::extend::editable::execute_view_requests;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::SkgConfig;
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_node_in_tree};
use crate::types::viewnode::{Viewnode, ViewnodeKind, ViewRequest};
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId, Tree};
use std::collections::HashSet;
use std::error::Error;

pub fn execute_unrestrictedVognode_view_requests (
  node               : NodeId,
  tree               : &mut Tree<Viewnode>,
  graph              : &crate::dbs::in_rust_graph::InRustGraph,
  config             : &SkgConfig,
  errors             : &mut Vec<String>,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, node,
    |vn : &Viewnode| matches!( &vn . kind,
                                ViewnodeKind::Vognode (Vognode::Unrestricted (_)) ),
    "execute_unrestrictedVognode_view_requests: expected UnrestrictedVognode" )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  let requests : Vec<(NodeId, ViewRequest)> =
    extract_view_requests( tree, node ) ?;
  if ! requests . is_empty() {
    execute_view_requests(
      tree, requests, graph, config, errors,
      skgrepo_restriction ) ?; }
  Ok(( )) }

/// Read the node's non-consumed view_requests as a Vec. View completion
/// (dispatch_node_update) settles
/// every Editable request at the node's own visit (the editability rule; see 'to_org/extend/editable.rs'), so
/// only Folder/Path requests should remain here; execute_view_requests
/// errors loudly if a Editable one survives.
fn extract_view_requests (
  tree : &Tree<Viewnode>,
  node : NodeId,
) -> Result<Vec<(NodeId, ViewRequest)>, Box<dyn Error>> {
  let mut view_requests : HashSet<ViewRequest> =
    read_at_node_in_tree( tree, node,
      |vn : &Viewnode| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => t . view_requests . clone(),
        _ => HashSet::new() } )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  // A Fork request is consumed on the save path (fork detection in
  // from_text), not during rendering; it must never reach the
  // render-time view-request pass.
  view_requests . remove (& ViewRequest::Fork);
  Ok ( view_requests . into_iter () . map ( |req| (node, req) ) . collect () ) }
