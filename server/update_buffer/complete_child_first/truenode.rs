use crate::to_org::expand::definitive::execute_view_requests;
use crate::to_org::util::DefinitiveMap;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::viewnode::{ViewNode, ViewNodeKind, ViewRequest};
use crate::types::skgnodemap::SkgNodeMap;
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_node_in_tree};
use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Postorder (child-first) TrueNode handler.
///
/// Extracts this node's view requests
/// and delegates to execute_view_requests,
/// processing the definitive view request first if present.
pub async fn complete_truenode (
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  map                : &mut SkgNodeMap,
  visited            : &mut DefinitiveMap,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
  errors             : &mut Vec<String>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, node,
    |vn : &ViewNode| matches!( &vn.kind, ViewNodeKind::True( _ ) ),
    "complete_truenode: expected TrueNode" )
    .map_err( |e| -> Box<dyn Error> { e.into() } )?;
  let requests : Vec<(NodeId, ViewRequest)> =
    extract_view_requests_definitive_first( tree, node ) ?;
  if requests.is_empty() { return Ok(( )); }
  execute_view_requests(
    tree, map, requests, config, driver,
    visited, errors, deleted_since_head_pid_src_map ). await ?;
  Ok(( )) }

/// Read the node's view_requests set and return them as a Vec
/// with Definitive first (if present), then the rest.
fn extract_view_requests_definitive_first (
  tree : &Tree<ViewNode>,
  node : NodeId,
) -> Result<Vec<(NodeId, ViewRequest)>, Box<dyn Error>> {
  let view_requests : HashSet<ViewRequest> =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn.kind {
        ViewNodeKind::True( t ) => t.view_requests.clone(),
        _ => HashSet::new() } )
    .map_err( |e| -> Box<dyn Error> { e.into() } )?;
  let mut result : Vec<(NodeId, ViewRequest)> = Vec::new();
  if view_requests.contains( &ViewRequest::Definitive ) {
    result.push( (node, ViewRequest::Definitive) ); }
  for req in &view_requests {
    if *req != ViewRequest::Definitive {
      result.push( (node, *req) ); } }
  Ok( result ) }
