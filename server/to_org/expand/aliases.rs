use crate::dbs::filesystem::one_node::fetch_aliases_from_file;
use crate::to_org::util::{get_id_from_treenode, remove_completed_view_request};
use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::{ViewNode, ViewRequest, Scaffold};
use crate::types::skgnodemap::SkgNodeMap;
use crate::types::tree::viewnode_skgnode::{
  insert_scaffold_as_child, unique_scaffold_child};

use ego_tree::Tree;
use std::error::Error;
use neo4rs::Graph;

pub async fn build_and_integrate_aliases_view_then_drop_request (
  tree          : &mut Tree<ViewNode>,
  _map          : &mut SkgNodeMap,
  node_id       : ego_tree::NodeId,
  config        : &SkgConfig,
  graph         : &Graph,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_aliases (
      tree, node_id, config, graph ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Aliases,
    "Failed to integrate aliases view",
    errors, result ) }

/// Integrate an AliasCol child and, under it, Alias grandchildren
/// into the ViewNode tree containing the target node.
///
/// PITFALL: This function fetches aliases from disk and
/// populates them immediately, whereas 'completeAliasCol' (in
/// update_buffer) is only called on an AliasCol already in the tree.
/// These two distinct ways of populating an AliasCol are necessary,
/// because in 'complete_or_restore_each_node_in_branch',
/// view requests are only processed AFTER recursing to children
/// (for reasons explained in that function's header comment),
/// so any newly-created empty AliasCol
/// would not be visited in the same save cycle.
pub async fn build_and_integrate_aliases (
  tree      : &mut Tree<ViewNode>,
  node_id   : ego_tree::NodeId,
  config    : &SkgConfig,
  graph     : &Graph,
) -> Result < (), Box<dyn Error> > {
  let node_id_val : ID =
    get_id_from_treenode ( tree, node_id ) ?;
  if unique_scaffold_child (
    tree, node_id, &Scaffold::AliasCol )? . is_some ()
  { // If it already has an AliasCol child,
    // then completeAliasCol (in update_buffer) already handled it.
    return Ok (( )); }
  let aliases : Vec < String > =
    fetch_aliases_from_file (
      config, graph, node_id_val ). await;
  let aliascol_id : ego_tree::NodeId =
    insert_scaffold_as_child ( tree, node_id,
      Scaffold::AliasCol, true ) ?;
  for alias in & aliases {
    insert_scaffold_as_child (
      tree, aliascol_id,
      Scaffold::Alias { text: alias . clone (),
                        diff: None },
      false ) ?; }
  Ok (( )) }
