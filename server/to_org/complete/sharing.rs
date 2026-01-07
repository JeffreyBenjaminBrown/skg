use crate::to_org::util::skgnode_and_orgnode_from_id;
use crate::dbs::typedb::search::hidden_in_subscribee_content::{
  partition_subscribee_content_for_subscriber,
  what_node_hides,
  what_nodes_contain };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, Interp, default_metadata};
use crate::types::tree::{NodePair, PairTree};
use crate::types::tree::accessors::{
  read_at_node_in_tree, unique_child_with_interp, with_node_mut };

use ego_tree::{NodeId, NodeRef};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// If appropriate, prepend a SubscribeeCol child containing:
///   - for each subscribee, an indefinitive Subscribee child
///   - if any hidden nodes are outside subscribee content,
///     a HiddenOutsideOfSubscribeeCol
pub async fn maybe_add_subscribee_col (
  tree    : &mut PairTree,
  node_id : NodeId, // if applicable, this is the subscriber
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { // Skip indefinitive nodes.
    let is_indefinitive : bool =
      read_at_node_in_tree (
        tree, node_id,
        |np| np . orgnode . metadata . code . indefinitive ) ?;
    if is_indefinitive { return Ok (( )); }}
  { // Skip if there already is one.
    // TODO: We should not  assume it's correct, but instead 'integrate' it, as is done somewhere else for something similar.
    if unique_child_with_interp (
      tree, node_id, Interp::SubscribeeCol )? . is_some ()
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
    insert_col_node ( tree, node_id,
      Interp::SubscribeeCol, "it subscribes to these", true ) ?;

  if ! hidden_outside_content . is_empty () {
    let hidden_outside_col_nid : NodeId =
      insert_col_node ( tree, subscribee_col_nid,
        Interp::HiddenOutsideOfSubscribeeCol,
        "hidden from all subscriptions", false ) ?;
    for hidden_id in hidden_outside_content {
      append_indefinitive_node (
        tree, hidden_outside_col_nid, & hidden_id,
        Interp::HiddenFromSubscribees, config, driver ) . await ?; } }
  for subscribee_id in subscribee_ids {
    append_indefinitive_node (
      tree, subscribee_col_nid, & subscribee_id,
      Interp::Subscribee, config, driver ) . await ?; }
  Ok (( )) }

/// If this node is a Subscribee,
/// and the corresponding subscriber hides any of its content,
/// then prepend a HiddenInSubscribeeCol to hold those hidden nodes.
/// The subscriber is the Subscribee's grandparent:
///   subscriber -> SubsribeeCol -> Subscribee
pub async fn maybe_add_hidden_in_subscribee_col (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  { // error if not a Subscribee
    let is_subscribee: bool =
      read_at_node_in_tree(tree, node_id, |node| {
        node.orgnode.metadata.code.interp == Interp::Subscribee
      })?;
    if ! is_subscribee { return Err (
      "maybe_add_hidden_in_subscribee_col called on non-subscribee"
        . into () ); }}
  if unique_child_with_interp (
    // TODO: We should not  assume it's correct, but instead 'integrate' it, as is done somewhere else for something similar.
    tree, node_id, Interp::HiddenInSubscribeeCol )? . is_some ()
  { return Ok (( )); }
  let ( subscribee_pid, subscriber_pid ) : ( ID, ID ) =
    pid_for_subscribee_and_its_subscriber_grandparent ( tree, node_id ) ?;
  let ( _visible, hidden_in_content )
    : ( HashSet < ID >, HashSet < ID > )
    = partition_subscribee_content_for_subscriber (
      &config.db_name, driver,
      & subscriber_pid,
      & subscribee_pid ) . await ?;
  if hidden_in_content . is_empty () {
    return Ok (( )); }
  let hidden_col_nid : NodeId =
    insert_col_node ( tree, node_id,
      Interp::HiddenInSubscribeeCol,
      "hidden from this subscription", true ) ?;
  for hidden_id in hidden_in_content {
    append_indefinitive_node (
      tree, hidden_col_nid, & hidden_id,
      Interp::HiddenFromSubscribees, config, driver ) . await ?; }
  Ok (( )) }

/// Extract PIDs for the subscriber and its subscribees.
/// Returns an error if the node has no SkgNode.
fn pids_for_subscriber_and_its_subscribees (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ( ID, Vec < ID > ), Box<dyn Error> > {
  read_at_node_in_tree (
    tree, node_id,
    |np| np . mskgnode . as_ref ()
      . map ( |skgnode|
              ( skgnode . ids [0] . clone (),
                skgnode . subscribes_to . clone ()
                . unwrap_or_default () ))
  )? . ok_or_else (
    || "pids_for_subscriber_and_its_subscribees: SkgNode should exist"
    . into () ) }

/// Extract PIDs for a Subscribee and its grandparent (the subscriber).
/// Expects: subscriber -> SubscribeeCol -> Subscribee (this node)
fn pid_for_subscribee_and_its_subscriber_grandparent (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ( ID, ID ), Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id ) . ok_or (
      "pid_for_subscribee_and_its_subscriber_grandparent: node not found" ) ?;
  let subscribee_pid : ID =
    node_ref . value () . orgnode . metadata . id . clone ()
    . ok_or ( "Subscribee has no ID" ) ?;
  let parent_ref : NodeRef < NodePair > =
    node_ref . parent ()
    . ok_or ( "Subscribee has no parent (SubscribeeCol)" ) ?;
  if parent_ref . value () . orgnode . metadata . code . interp
    != Interp::SubscribeeCol {
      return Err ( "Subscribee's parent is not a SubscribeeCol" .
                    into () ); }
  let grandparent_ref : NodeRef < NodePair > =
    parent_ref . parent ()
    . ok_or ( "SubscribeeCol has no parent (subscriber)" ) ?;
  let skgnode : &SkgNode =
    grandparent_ref . value () . mskgnode . as_ref ()
    . ok_or ( "Subscriber has no SkgNode" ) ?;
  Ok ( ( subscribee_pid,
         skgnode . ids [ 0 ] . clone () ) ) }

/// Insert a collection node (no SkgNode, fixed title) as a child.
/// If `prepend` is true, inserts at the beginning; otherwise appends.
pub fn insert_col_node (
  tree      : &mut PairTree,
  parent_id : NodeId,
  interp    : Interp,
  title     : &str,
  prepend   : bool,
) -> Result < NodeId, Box<dyn Error> > {
  let col_orgnode : OrgNode = {
    let mut md = default_metadata ();
    md . code . interp = interp;
    OrgNode {
      metadata : md,
      title : title . to_string (),
      body : None, } };
  let col_id : NodeId = with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      let pair = NodePair { mskgnode: None, orgnode: col_orgnode };
      if prepend { parent_mut . prepend ( pair ) . id () }
      else       { parent_mut . append  ( pair ) . id () } } ) ?;
  Ok ( col_id ) }

/// Fetch a node from disk and append it as an indefinitive child with the given Interp.
async fn append_indefinitive_node (
  tree      : &mut PairTree,
  parent_id : NodeId,
  node_id   : &ID,
  interp    : Interp,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let ( skgnode, mut orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, node_id ) . await ?;
  orgnode . metadata . code . interp = interp;
  orgnode . metadata . code . indefinitive = true;
  orgnode . body = None;
  with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      parent_mut . append (
        NodePair { mskgnode: Some ( skgnode ), orgnode } ); } ) ?;
  Ok (( )) }
