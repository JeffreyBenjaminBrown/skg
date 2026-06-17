use crate::to_org::complete::partner_col::{ maybe_add_hiddenInSubscribeeCol_branch, type_and_parent_type_consistent_with_subscribee };
use crate::to_org::expand::definitive::execute_view_requests;
use crate::source_sets::ActiveSourceSet;
use crate::types::git::SourceDiff;
use crate::types::misc::{SkgConfig, SourceName};
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_node_in_tree};
use crate::types::viewnode::{ViewNode, ViewNodeKind, ViewRequest};
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_activeNode_view_requests (
  node               : NodeId,
  tree               : &mut Tree<ViewNode>,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
  errors             : &mut Vec<String>,
  active_source_set  : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, node,
    |vn : &ViewNode| matches!( &vn . kind,
                                ViewNodeKind::Vognode (Vognode::Active (_)) ),
    "execute_activeNode_view_requests: expected ActiveNode" )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  let requests : Vec<(NodeId, ViewRequest)> =
    extract_view_requests( tree, node ) ?;
  if ! requests . is_empty() {
    execute_view_requests(
      tree, requests, config, driver, errors,
      active_source_set ) . await ?; }
  Ok(( )) }

pub async fn ensure_hiddenin_col_under_definitive_subscribee (
  tree   : &mut Tree<ViewNode>,
  node   : NodeId,
  config : &SkgConfig,
  driver : &TypeDBDriver,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
) -> Result<(), Box<dyn Error>> {
  let is_subscribee : bool =
    type_and_parent_type_consistent_with_subscribee (
      tree, node ) ?;
  if ! is_subscribee { return Ok (( )); }
  let is_indefinitive : bool =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => t . is_indefinitive (),
        _ => false } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
  if is_indefinitive { return Ok (( )); }
  maybe_add_hiddenInSubscribeeCol_branch (
    tree, node, config, driver, source_diffs ) . await }

/// Read the node's non-consumed view_requests as a Vec. View completion
/// (dispatch_node_update) settles
/// every Definitive request at the node's own visit (the TODO/DONE/local-view-update/plan_v2.org §5.2 draw rule), so
/// only Aliases/Containerward/Sourceward should remain here; execute_view_requests
/// errors loudly if a Definitive one survives.
fn extract_view_requests (
  tree : &Tree<ViewNode>,
  node : NodeId,
) -> Result<Vec<(NodeId, ViewRequest)>, Box<dyn Error>> {
  let mut view_requests : HashSet<ViewRequest> =
    read_at_node_in_tree( tree, node,
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => t . view_requests . clone(),
        _ => HashSet::new() } )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  // A view-ROOT's Containerward request is NOT fulfilled here. finish_viewforest
  // builds root containerward as a separate AncestryTree subtree (handling a
  // cyclic root) and drops the request; routing a root through
  // build_and_integrate_containerward here would instead merge the ancestry into
  // existing content and panic on a cyclic root (one whose containerward cycles
  // back to the root). So leave view-root Containerward for the tail. A
  // Containerward request a USER put on a non-root node still expands here.
  let is_view_root : bool =
    tree . get (node)
      . and_then ( |n| n . parent () )
      . map ( |p| matches! ( p . value () . kind, ViewNodeKind::BufferRoot ) )
      . unwrap_or (false);
  if is_view_root {
    view_requests . remove (& ViewRequest::Containerward); }
  Ok ( view_requests . into_iter () . map ( |req| (node, req) ) . collect () ) }
