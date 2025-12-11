use crate::media::file_io::one_node::fetch_aliases_from_file;
use crate::to_org::util::{get_pid_in_pairtree, remove_completed_view_request, orgnode_from_title_and_rel};
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{OrgNode, RelToParent, ViewRequest};
use crate::types::trees::PairTree;

use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn wrapped_build_and_integrate_aliases_view (
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
/// because in 'completeNodePreorder_collectingDefinitiveRequests',
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
  { // If it already has an AliasCol child, then do nothing,
    // because completeAliasCol already handled it.
    let has_aliascol : bool = {
      let node_ref =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      node_ref . children ()
        . any ( |child|
                 child . value () . 1 . metadata . code.relToParent
                 == RelToParent::AliasCol ) };
    if has_aliascol {
      return Ok (( )); }}
  let aliases : Vec < String > =
    fetch_aliases_from_file (
      config,
      driver,
      node_id_val ) . await;
  let aliascol : OrgNode =
    orgnode_from_title_and_rel ( RelToParent::AliasCol, String::new () );
  let aliascol_id : ego_tree::NodeId = {
    // prepend an AliasCol to the node's children
    let mut node_mut =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    node_mut . prepend ( (None, aliascol) ) . id () };
  for alias in aliases {
    // append each Alias to the AliasCol's children
    let alias_node : OrgNode =
      orgnode_from_title_and_rel ( RelToParent::Alias, alias );
    let mut aliascol_mut =
      tree . get_mut ( aliascol_id )
      . ok_or ( "AliasCol node not found" ) ?;
    aliascol_mut . append ( (None, alias_node) ); }
  Ok (( )) }
