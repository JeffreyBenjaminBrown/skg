use crate::to_org::complete::sharing::{ maybe_add_hiddenInSubscribeeCol_branch, type_and_parent_type_consistent_with_subscribee };
use crate::to_org::expand::definitive::execute_view_requests;
use crate::source_sets::ActiveSourceSet;
use crate::to_org::util::DefinitiveMap;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_node_in_tree};
use crate::types::viewnode::{ViewNode, ViewNodeKind, ViewRequest};

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_truenode_view_requests (
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  visited            : &mut DefinitiveMap,
  source_diffs       : &Option<HashMap<SourceName, SourceDiff>>,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
  errors             : &mut Vec<String>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  active_source_set  : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, node,
    |vn : &ViewNode| matches!( &vn . kind, ViewNodeKind::True (_) ),
    "execute_truenode_view_requests: expected TrueNode" )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  let requests : Vec<(NodeId, ViewRequest)> =
    extract_view_requests_definitive_first( tree, node ) ?;
  if ! requests . is_empty() {
    execute_view_requests(
      tree, requests, source_diffs, config, driver,
      visited, errors, deleted_since_head_pid_src_map,
      active_source_set ) . await ?; }
  Ok(( )) }

pub async fn ensure_hiddenin_col_under_definitive_subscribee (
  tree   : &mut Tree<ViewNode>,
  node   : NodeId,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let is_subscribee : bool =
    type_and_parent_type_consistent_with_subscribee (
      tree, node ) ?;
  if ! is_subscribee { return Ok (( )); }
  let is_indefinitive : bool =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) => t . is_indefinitive (),
        _ => false } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
  if is_indefinitive { return Ok (( )); }
  maybe_add_hiddenInSubscribeeCol_branch (
    tree, node, config, driver ) . await }

/// Read the node's view_requests set and return them as a Vec
/// with Definitive first (if present), then the rest.
fn extract_view_requests_definitive_first (
  tree : &Tree<ViewNode>,
  node : NodeId,
) -> Result<Vec<(NodeId, ViewRequest)>, Box<dyn Error>> {
  let view_requests : HashSet<ViewRequest> =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::True (t) => t . view_requests . clone(),
        _ => HashSet::new() } )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  let mut result : Vec<(NodeId, ViewRequest)> = Vec::new();
  if view_requests . contains (&ViewRequest::Definitive) {
    result . push( (node, ViewRequest::Definitive) ); }
  for req in &view_requests {
    if *req != ViewRequest::Definitive {
      result . push( (node, *req) ); } }
  Ok (result) }
