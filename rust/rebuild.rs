pub mod complete_aliascol;
pub use complete_aliascol::completeAliasCol;

pub mod complete_contents;
pub use complete_contents::completeContents;

use crate::types::{ID, SkgConfig, OrgNode, RelToParent};
use ego_tree::Tree;
use std::collections::HashSet;
use std::error::Error;

/// Complete a forest of OrgNode trees by traversing preorder DFS
/// and calling completeAliasCol or completeContents as appropriate.
///
/// Uses a single shared visited set across all trees in the forest
/// to handle cycles that span multiple trees.
///
/// For each node in preorder traversal:
/// - If treatment is AliasCol: call completeAliasCol
/// - Otherwise, if NOT indefinitive: call completeContents
pub fn completeOrgnodeForest (
  forest : &mut Vec < Tree < OrgNode > >,
  config : &SkgConfig,
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
      tree, root_id, config, &mut visited, &mut ancestor_path ) ?; }
  Ok (( )) }

fn complete_node_preorder (
  tree          : &mut Tree < OrgNode >,
  node_id       : ego_tree::NodeId,
  config        : &SkgConfig,
  visited       : &mut HashSet < ID >,
  ancestor_path : &mut Vec < ID >, // path from root to current node (for cycle detection)
) -> Result < (), Box<dyn Error> > {
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
    // Recurse to children
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
        } else { // Mark as not a cycle
          child_mut . value () . metadata . viewData.cycle = false; }
        ancestor_path . push ( child_pid . clone () ); }
      complete_node_preorder (
        tree, child_id, config, visited, ancestor_path ) ?;
      if child_pid_opt . is_some () {
        ancestor_path . pop (); }}}
  Ok (( )) }
