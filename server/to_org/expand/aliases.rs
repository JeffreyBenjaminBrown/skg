use crate::dbs::filesystem::one_node::fetch_aliases_from_file;
use crate::to_org::util::{get_pid_in_tree, remove_completed_view_request};
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{OrgNode, ViewRequest, Scaffold};
use crate::types::skgnode::SkgNodeMap;
use crate::types::tree::orgnode_skgnode::{
  insert_scaffold_as_child_in_orgtree, unique_orgnode_scaffold_child};

use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;


/// V2: Tree<OrgNode> + SkgNodeMap version.
pub async fn build_and_integrate_aliases_view_then_drop_request (
  tree          : &mut Tree<OrgNode>,
  _map          : &mut SkgNodeMap,
  node_id       : ego_tree::NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let result = build_and_integrate_aliases (
    tree, node_id, config, typedb_driver ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Aliases,
    "Failed to integrate aliases view",
    errors, result ) }

/// V2: Tree<OrgNode> version of build_and_integrate_aliases.
pub async fn build_and_integrate_aliases (
  tree      : &mut Tree<OrgNode>,
  node_id   : ego_tree::NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let node_id_val : ID =
    get_pid_in_tree ( tree, node_id ) ?;
  if unique_orgnode_scaffold_child (
    tree, node_id, &Scaffold::AliasCol )? . is_some ()
  { return Ok (( )); }
  let aliases : Vec < String > =
    fetch_aliases_from_file (
      config, driver, node_id_val ). await;
  let aliascol_id : ego_tree::NodeId =
    insert_scaffold_as_child_in_orgtree ( tree, node_id,
      Scaffold::AliasCol, true ) ?;
  for alias in & aliases {
    insert_scaffold_as_child_in_orgtree ( tree, aliascol_id,
      Scaffold::Alias ( alias . clone () ), false ) ?; }
  Ok (( )) }
