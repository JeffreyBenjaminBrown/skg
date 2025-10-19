pub mod complete_aliascol;
pub use complete_aliascol::completeAliasCol;

pub mod complete_contents;
pub use complete_contents::completeContents;

use crate::types::{ID, OrgNode, SkgConfig, Treatment};
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
/// - Otherwise, if NOT mightContainMore: call completeContents
pub fn completeOrgnodeForest (
  forest : &mut Vec < Tree < OrgNode > >,
  config : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let mut visited : HashSet < ID > =
    HashSet::new ();
  for tree in forest . iter_mut () {
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    complete_node_preorder (
      tree, root_id, config, &mut visited ) ?; }
  Ok (( )) }

fn complete_node_preorder (
  tree    : &mut Tree < OrgNode >,
  node_id : ego_tree::NodeId,
  config  : &SkgConfig,
  visited : &mut HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  let (treatment, might_contain_more) : (Treatment, bool) = {
    let node_ref : ego_tree::NodeRef < OrgNode > =
      tree . get ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    let node : &OrgNode =
      node_ref . value ();
    ( node . metadata . treatment . clone (),
      node . metadata . mightContainMore ) };
  if treatment == Treatment::AliasCol {
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
      complete_node_preorder (
        tree, child_id, config, visited ) ?; }}
  Ok (( )) }
