pub mod complete_aliascol;
pub use complete_aliascol::completeAliasCol;

pub mod complete_contents;
pub use complete_contents::completeContents;

pub mod integrate_backpath;
pub use integrate_backpath::{build_and_integrate_containerward_path, integrate_containerward_path};

use crate::types::{ID, SkgConfig, OrgNode, RelToParent, NodeRequest};
use ego_tree::Tree;
use std::collections::HashSet;
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

/// Complete a forest of OrgNode trees by traversing preorder DFS
/// and calling completeAliasCol or completeContents as appropriate.
///
/// Uses a single shared visited set across all trees in the forest
/// to handle cycles that span multiple trees.
///
/// For each node in preorder traversal:
/// - If treatment is AliasCol: call completeAliasCol
/// - Otherwise, if NOT indefinitive: call completeContents
/// - If node has containerward-view request: integrate containerward path
pub async fn completeOrgnodeForest (
  forest        : &mut Vec < Tree < OrgNode > >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let mut visited : HashSet < ID > =
    HashSet::new ();
  for tree in forest . iter_mut () {
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let root_pid_opt : Option < ID > = {
      let root_ref : ego_tree::NodeRef < OrgNode > =
        tree . get ( root_id )
        . ok_or ( "Root not found in tree" ) ?;
      root_ref . value () . metadata . id . clone () };
    let mut ancestor_path : Vec < ID > =
      if let Some ( root_pid ) = root_pid_opt {
        vec! [ root_pid ]
      } else {
        vec! [] };
    complete_node_preorder (
      tree, root_id, config, typedb_driver,
      &mut visited, &mut ancestor_path, errors ) . await ?; }
  Ok (( )) }

fn complete_node_preorder<'a> (
  tree          : &'a mut Tree < OrgNode >,
  node_id       : ego_tree::NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  visited       : &'a mut HashSet < ID >,
  ancestor_path : &'a mut Vec < ID >, // path from root to current node (for cycle detection)
  errors        : &'a mut Vec < String >,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let has_containerward_request : bool = {
      let node_ref : ego_tree::NodeRef < OrgNode > =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      node_ref . value () . metadata . code . nodeRequests
        . contains ( &NodeRequest::ContainerwardView ) };

    let (treatment, might_contain_more) : (RelToParent, bool) = {
      let node_ref : ego_tree::NodeRef < OrgNode > =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      let node : &OrgNode =
        node_ref . value ();
      ( node . metadata . code.relToParent . clone (),
        node . metadata . code.indefinitive ) };
    if treatment == RelToParent::AliasCol {
      completeAliasCol (
        tree, node_id, config ) ?;
      // Don't recurse - completeAliasCol handles the whole subtree
    } else {
      if ! might_contain_more {
        completeContents (
          tree, node_id, config, visited ) ?; }
      map_complete_node_preorder_over_children (
        tree, node_id, config, typedb_driver,
        visited, ancestor_path, errors ) . await ?; }

    if has_containerward_request {
      process_containerward_view_request (
        tree, node_id, config, typedb_driver, errors ) . await ?; }

    Ok (( )) }) }

/// Recurse to children, marking cycles and calling complete_node_preorder.
fn map_complete_node_preorder_over_children<'a> (
  tree          : &'a mut Tree < OrgNode >,
  node_id       : ego_tree::NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  visited       : &'a mut HashSet < ID >,
  ancestor_path : &'a mut Vec < ID >,
  errors        : &'a mut Vec < String >,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    let child_ids : Vec < ego_tree::NodeId > = {
      let node_ref : ego_tree::NodeRef < OrgNode > =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      node_ref . children ()
        . map ( |c| c . id () )
        . collect () };
    for child_id in child_ids {
      let child_pid_opt : Option < ID > = {
        let child_ref : ego_tree::NodeRef < OrgNode > =
          tree . get ( child_id )
          . ok_or ( "Child not found in tree" ) ?;
        child_ref . value () . metadata . id . clone () };
      if let Some ( ref child_pid ) = child_pid_opt {
        let mut child_mut : ego_tree::NodeMut < OrgNode > =
          tree . get_mut ( child_id )
          . ok_or ( "Child not found in tree" ) ?;
        if ancestor_path . contains ( child_pid ) {
          // Mark as a cycle
          child_mut . value () . metadata . viewData.cycle = true;
        } else {
          // Mark as not a cycle
          child_mut . value () . metadata . viewData.cycle = false; }
        ancestor_path . push ( child_pid . clone () ); }
      complete_node_preorder (
        tree, child_id, config, typedb_driver,
        visited, ancestor_path, errors ) . await ?;
      if child_pid_opt . is_some () {
        ancestor_path . pop (); }}
    Ok (( )) }) }

/// Process containerward-view request after completing children.
/// This order is helpful, because if a content child added during completion
/// matches the head of the path, the path will be integrated there
/// (where treatment=Content), instead of creating a duplicate child
/// with treatment=ParentIgnores.
fn process_containerward_view_request<'a> (
  tree          : &'a mut Tree < OrgNode >,
  node_id       : ego_tree::NodeId,
  config        : &'a SkgConfig,
  typedb_driver : &'a TypeDBDriver,
  errors        : &'a mut Vec < String >,
) -> Pin<Box<dyn Future<Output = Result<(), Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    if let Err ( e ) = build_and_integrate_containerward_path (
      tree,
      node_id,
      config,
      typedb_driver ) . await
    {
      errors . push (
        format! ( "Failed to integrate containerward path: {}", e )); }
    { // Strip the request from metadata (always, to avoid retry loops)
      let mut node_mut : ego_tree::NodeMut < OrgNode > =
        tree . get_mut ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      node_mut . value () . metadata . code . nodeRequests
        . remove ( &NodeRequest::ContainerwardView ); }
    Ok (( )) }) }
