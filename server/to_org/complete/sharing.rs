use crate::dbs::typedb::search::hidden_in_subscribee_content::{
  partition_subscribee_content_for_subscriber,
  what_node_hides,
  what_nodes_contain };
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{OrgNodeKind, EffectOnParent, Scaffold};
use crate::types::tree::PairTree;
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::orgnode_skgnode::{
  append_indefinitive_from_disk_as_child, insert_scaffold_as_child,
  pid_for_subscribee_and_its_subscriber_grandparent,
  pids_for_subscriber_and_its_subscribees,
  unique_scaffold_child };

use ego_tree::NodeId;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// If appropriate, prepend a SubscribeeCol child containing:
/// - for each subscribee, an indefinitive Subscribee child
/// - if any hidden nodes are outside subscribee content,
///   a HiddenOutsideOfSubscribeeCol
pub async fn maybe_add_subscribeeCol_branch (
  tree    : &mut PairTree,
  node_id : NodeId, // if applicable, this is the subscriber
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { // Skip indefinitive nodes.
    let is_indefinitive : bool =
      read_at_node_in_tree (
        tree, node_id,
        |np| np . orgnode . is_indefinitive () ) ?;
    if is_indefinitive { return Ok (( )); }}
  { // Skip if there already is one.
    // TODO: Should not assume it's correct, but instead 'integrate' it, as is done somewhere else for something similar.
    if unique_scaffold_child (
      tree, node_id, &Scaffold::SubscribeeCol )? . is_some ()
    { return Ok (( )); }}
  let ( subscriber_pid, subscribee_ids ) : ( ID, Vec < ID > ) =
    pids_for_subscriber_and_its_subscribees ( tree, node_id ) ?;
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
    insert_scaffold_as_child ( tree, node_id,
      Scaffold::SubscribeeCol, true ) ?;

  { // mutate the tree
    if ! hidden_outside_content . is_empty () {
      let hidden_outside_col_nid : NodeId =
        insert_scaffold_as_child (
          tree, subscribee_col_nid,
          Scaffold::HiddenOutsideOfSubscribeeCol, false ) ?;
      for hidden_id in hidden_outside_content {
        append_indefinitive_from_disk_as_child (
          tree, hidden_outside_col_nid, & hidden_id,
          EffectOnParent::HiddenFromSubscribees, config, driver ). await ?; }}
    for subscribee_id in subscribee_ids {
      append_indefinitive_from_disk_as_child (
        tree, subscribee_col_nid, & subscribee_id,
        EffectOnParent::Subscribee, config, driver ) . await ?; }}
  Ok (( )) }

/// If this node is a Subscribee,
/// and the corresponding subscriber hides any of its content,
/// then prepend a HiddenInSubscribeeCol to hold those hidden nodes.
/// The subscriber is the Subscribee's grandparent:
///   subscriber -> SubsribeeCol -> Subscribee
pub async fn maybe_add_hiddenInSubscribeeCol_branch (
  tree              : &mut PairTree,
  subscribee_treeid : NodeId,
  config            : &SkgConfig,
  driver            : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { // error if not a Subscribee
    let is_subscribee: bool =
      read_at_node_in_tree(
        tree, subscribee_treeid,
        |node| { matches! (
          &node.orgnode.kind,
          OrgNodeKind::True(t)
          if t.effect_on_parent == EffectOnParent::Subscribee ) } )?;
    if ! is_subscribee { return Err (
      "maybe_add_hiddenInSubscribeeCol_branch called on non-subscribee"
        . into () ); }}
  if unique_scaffold_child (
       // TODO: This assumes the existing Col is correct. Should instead 'integrate' it, as is done somewhere else for something similar.
       tree, subscribee_treeid, &Scaffold::HiddenInSubscribeeCol
     )? . is_some ()
  { return Ok (( )); }
  let ( subscribee_pid, subscriber_pid ) : ( ID, ID ) =
    pid_for_subscribee_and_its_subscriber_grandparent (
      tree, subscribee_treeid ) ?;
  let ( _visible, hidden_in_content )
    : ( HashSet < ID >, HashSet < ID > )
    = partition_subscribee_content_for_subscriber (
        & config.db_name, driver,
        & subscriber_pid, & subscribee_pid ) . await ?;
  if hidden_in_content . is_empty () {
    return Ok (( )); }
  let hidden_col_nid : NodeId =
    insert_scaffold_as_child ( tree, subscribee_treeid,
      Scaffold::HiddenInSubscribeeCol, true ) ?;
  for hidden_id in hidden_in_content {
    // populate the collection
    append_indefinitive_from_disk_as_child (
      tree, hidden_col_nid, & hidden_id,
      EffectOnParent::HiddenFromSubscribees, config, driver ). await ?; }
  Ok (( )) }
