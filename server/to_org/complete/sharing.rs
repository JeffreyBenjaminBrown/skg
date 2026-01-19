use crate::dbs::typedb::search::hidden_in_subscribee_content::{
  partition_subscribee_content_for_subscriber,
  what_node_hides,
  what_nodes_contain };
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{OrgNode, OrgNodeKind, Scaffold};
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::orgnode_skgnode::{
  append_indefinitive_from_disk_as_child_in_orgtree,
  insert_scaffold_as_child_in_orgtree,
  pids_for_subscriber_and_its_subscribees_in_orgtree,
  unique_orgnode_scaffold_child };
use crate::types::skgnode::SkgNodeMap;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Check if a node's type and parent type are consistent with being a Subscribee.
/// A Subscribee is a TrueNode whose parent is a SubscribeeCol scaffold.
/// TODO ? Also check that the grandparent is a TrueNode.
pub fn type_and_parent_type_consistent_with_subscribee (
  tree    : &ego_tree::Tree<OrgNode>,
  node_id : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_ref : NodeRef < OrgNode > =
    tree . get ( node_id )
    . ok_or ( "type_and_parent_type_consistent_with_subscribee: node not found" ) ?;
  let is_truenode : bool = matches! (
    & node_ref . value () . kind,
    OrgNodeKind::True ( _ ));
  let parent_is_subscribee_col : bool =
    node_ref . parent ()
    . map ( |p| matches! (
              & p . value () . kind,
              OrgNodeKind::Scaff ( Scaffold::SubscribeeCol )))
    . unwrap_or ( false );
  Ok ( is_truenode && parent_is_subscribee_col ) }

/// If appropriate, prepend a SubscribeeCol child containing:
/// - for each subscribee, an indefinitive Subscribee child
/// - if any hidden nodes are outside subscribee content,
///   a HiddenOutsideOfSubscribeeCol
pub async fn maybe_add_subscribeeCol_branch (
  tree    : &mut Tree<OrgNode>,
  map     : &mut SkgNodeMap,
  node_id : NodeId, // if applicable, this is the subscriber
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { let node_kind: OrgNodeKind =
      read_at_node_in_tree (
        tree, node_id, |orgnode| orgnode.kind.clone() )
      . map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;
    match node_kind {
      OrgNodeKind::Scaff ( _ ) =>
        return Err ( "maybe_add_subscribeeCol_branch: \
                      caller should not pass a Scaffold".into() ),
      OrgNodeKind::True ( t ) => // skip indefinitive nodes
        if t.indefinitive { return Ok(( )) }} }
  { // Skip if there already is one.
    if unique_orgnode_scaffold_child (
      tree, node_id, &Scaffold::SubscribeeCol )? . is_some ()
    { return Ok (( )); }}
  let ( subscriber_pid, subscribee_ids ) : ( ID, Vec < ID > ) =
    pids_for_subscriber_and_its_subscribees_in_orgtree ( tree, map, node_id ) ?;
  if subscribee_ids . is_empty () { // Skip because it would be empty.
    return Ok (( )); }

  let hidden_outside_content : HashSet < ID > = {
    // hidden IDs that are outside all subscribee content
    let r_hides : HashSet < ID > =
      what_node_hides (
        &config.db_name, driver, & subscriber_pid ) . await ?;
    let all_subscribee_content : HashSet < ID > =
      what_nodes_contain (
        &config.db_name, driver, & subscribee_ids ) . await ?;
    r_hides . iter ()
      . filter ( | id | ! all_subscribee_content . contains ( id ) )
      . cloned () . collect () };

  let subscribee_col_nid : NodeId =
    insert_scaffold_as_child_in_orgtree ( tree, node_id,
      Scaffold::SubscribeeCol, true ) ?;

  { // mutate the tree
    if ! hidden_outside_content . is_empty () {
      let hidden_outside_col_nid : NodeId =
        insert_scaffold_as_child_in_orgtree (
          tree, subscribee_col_nid,
          Scaffold::HiddenOutsideOfSubscribeeCol, false ) ?;
      for hidden_id in hidden_outside_content {
        append_indefinitive_from_disk_as_child_in_orgtree (
          tree, map, hidden_outside_col_nid, & hidden_id,
          false, config, driver
        ). await ?; }}
    for subscribee_id in subscribee_ids {
      append_indefinitive_from_disk_as_child_in_orgtree (
        tree, map, subscribee_col_nid, & subscribee_id,
        false, config, driver
      ). await ?; }}
  Ok (( )) }

/// If this node is a Subscribee,
/// and the corresponding subscriber hides any of its content,
/// then prepend a HiddenInSubscribeeCol to hold those hidden nodes.
/// The subscriber is the Subscribee's grandparent:
///   subscriber -> SubsribeeCol -> Subscribee
pub async fn maybe_add_hiddenInSubscribeeCol_branch (
  tree              : &mut ego_tree::Tree<OrgNode>,
  map               : &mut crate::types::skgnode::SkgNodeMap,
  subscribee_treeid : NodeId,
  config            : &SkgConfig,
  driver            : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  if ! type_and_parent_type_consistent_with_subscribee (
    tree, subscribee_treeid )?
  { return Err ( "maybe_add_hiddenInSubscribeeCol_branch called on non-subscribee" . into ( )); }
  if crate::types::tree::orgnode_skgnode::unique_orgnode_scaffold_child (
       tree, subscribee_treeid, &Scaffold::HiddenInSubscribeeCol
     )? . is_some ()
  { return Ok (( )); }
  let ( subscribee_pid, subscriber_pid ) : ( ID, ID ) =
    crate::types::tree::orgnode_skgnode::pid_for_subscribee_and_its_subscriber_grandparent_in_orgtree (
      tree, map, subscribee_treeid ) ?;
  let ( _visible, hidden_in_content )
    : ( HashSet < ID >, HashSet < ID > )
    = partition_subscribee_content_for_subscriber (
        & config.db_name, driver,
        & subscriber_pid, & subscribee_pid ) . await ?;
  if hidden_in_content . is_empty () {
    return Ok (( )); }
  let hidden_col_nid : NodeId =
    crate::types::tree::orgnode_skgnode::insert_scaffold_as_child_in_orgtree (
      tree, subscribee_treeid,
      Scaffold::HiddenInSubscribeeCol, true ) ?;
  for hidden_id in hidden_in_content {
    // populate the collection
    crate::types::tree::orgnode_skgnode::append_indefinitive_from_disk_as_child_in_orgtree (
      tree, map, hidden_col_nid, & hidden_id,
      false, config, driver ). await ?; }
  Ok (( )) }
