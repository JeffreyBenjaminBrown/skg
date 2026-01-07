use crate::dbs::filesystem::one_node::fetch_aliases_from_file;
use crate::to_org::util::{get_pid_in_pairtree, remove_completed_view_request};
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{Interp, ViewRequest};
use crate::types::tree::PairTree;
use crate::types::tree::orgnode_skgnode::{insert_col_node, unique_child_with_interp};

use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn build_and_integrate_aliases_view_then_drop_request (
  tree          : &mut PairTree,
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

/// Integrate an AliasCol child and, under it, Alias grandchildren
/// into the OrgNode tree containing the target node.
///
/// PITFALL: This function fetches aliases from disk and populates them immediately,
/// whereas 'completeAliasCol' is only called on an AliasCol already in the tree.
/// These two distinct ways of populating an AliasCol are necessary,
/// because in 'completeAndRestoreNode_collectingDefinitiveRequests',
/// view requests are only processed AFTER recursing to children
/// (for reasons explained in that function's header comment),
/// so any newly-created empty AliasCol
/// would not be visited in the same save cycle.
pub async fn build_and_integrate_aliases (
  tree      : &mut PairTree,
  node_id   : ego_tree::NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let node_id_val : ID =
    get_pid_in_pairtree ( tree, node_id ) ?;
  if unique_child_with_interp (
    tree, node_id, Interp::AliasCol )? . is_some ()
  { // If it already has an AliasCol child,
    // then completeAliasCol already handled it.
    return Ok (( )); }
  let aliases : Vec < String > =
    fetch_aliases_from_file (
      config, driver, node_id_val ). await;
  let aliascol_id : ego_tree::NodeId =
    insert_col_node ( tree, node_id,
      Interp::AliasCol, "", true ) ?;
  for alias in & aliases {
    insert_col_node ( tree, aliascol_id,
      Interp::Alias, alias, false ) ?; }
  Ok (( )) }
