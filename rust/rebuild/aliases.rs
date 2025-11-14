use crate::media::file_io::one_node::fetch_aliases_from_file;
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{OrgNode, OrgnodeMetadata, RelToParent, ViewRequest, default_metadata};

use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn wrapped_build_and_integrate_aliases_view (
  tree          : &mut Tree < OrgNode >,
  node_id       : ego_tree::NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  if let Err ( e ) = build_and_integrate_aliases (
    tree, node_id, config, typedb_driver ) . await
  { errors . push ( format! (
    "Failed to integrate aliases view: {}", e )); }
  tree . get_mut ( node_id )
    . ok_or ( "Node not found in tree" ) ?
    . value () . metadata . code . viewRequests
    . remove ( &ViewRequest::Aliases );
  Ok (( )) }

/// Integrate an AliasCol child and, under it, Alias grandchildren
/// into the OrgNode tree containing the target node.
///
/// PITFALL: This function fetches aliases from disk and populates them immediately,
/// whereas 'completeAliasCol' is only called on an AliasCol already in the tree.
/// These two distinct ways of populating an AliasCol are necessary,
/// because in 'complete_node_preorder',
/// view requests are only processed AFTER recursing to children
/// (for reasons explained in that function's header comment),
/// so any newly-created empty AliasCol
/// would not be visited in the same save cycle.
pub async fn build_and_integrate_aliases (
  tree      : &mut Tree < OrgNode >,
  node_id   : ego_tree::NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let node_id_val : ID =
    tree . get ( node_id ) . unwrap ()
    . value () . metadata . id . clone ()
    . ok_or ( "Node has no ID" ) ?;
  { // If it already has an AliasCol child, then do nothing,
    // because completeAliasCol already handled it.
    let has_aliascol : bool = {
      let node_ref =
        tree . get ( node_id )
        . ok_or ( "Node not found in tree" ) ?;
      node_ref . children ()
        . any ( |child|
                 child . value () . metadata . code.relToParent
                 == RelToParent::AliasCol ) };
    if has_aliascol {
      return Ok (( )); }}
  let aliases : Vec < String > =
    fetch_aliases_from_file (
      config,
      driver,
      node_id_val ) . await;
  let aliascol : OrgNode = {
    let mut aliascol_md : OrgnodeMetadata =
      default_metadata ();
    aliascol_md . code.relToParent = RelToParent::AliasCol;
    OrgNode {
      metadata : aliascol_md,
      title    : String::new (),
      body     : None, }};
  let aliascol_id : ego_tree::NodeId = {
    // prepend an AliasCol to the node's children
    let mut node_mut =
      tree . get_mut ( node_id )
      . ok_or ( "Node not found in tree" ) ?;
    node_mut . prepend ( aliascol ) . id () };
  for alias in aliases {
    // append each Alias to the AliasCol's children
    let mut md_alias : OrgnodeMetadata =
      default_metadata ();
    md_alias . code.relToParent =
      RelToParent::Alias;
    let alias_node : OrgNode = OrgNode {
      metadata : md_alias,
      title    : alias,
      body     : None, };
    let mut aliascol_mut =
      tree . get_mut ( aliascol_id )
      . ok_or ( "AliasCol node not found" ) ?;
    aliascol_mut . append ( alias_node ); }
  Ok (( )) }
