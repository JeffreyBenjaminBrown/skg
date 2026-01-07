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
    subscriber_and_subscribee_pids ( tree, node_id ) ?;
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
    prepend_subscribee_col ( tree, node_id ) ?;

  if ! hidden_outside_content . is_empty () {
    let hidden_outside_col_nid : NodeId =
      append_hidden_outside_of_subscribee_col (
        tree, subscribee_col_nid ) ?;
    { // its children, HiddenFromSubscribees (although since no subscribee contains them, they are not actually hidden anywhere)
      for hidden_id in hidden_outside_content {
        let ( skgnode, mut orgnode ) : ( SkgNode, OrgNode ) =
          skgnode_and_orgnode_from_id (
            config, driver, & hidden_id ) . await ?;
        orgnode . metadata . code . interp =
          Interp::HiddenFromSubscribees;
        orgnode . metadata . code . indefinitive = true;
        orgnode . body = None;
        with_node_mut (
          tree, hidden_outside_col_nid,
          |mut hidden_col_mut| {
            hidden_col_mut . append (
              NodePair { mskgnode: Some(skgnode), orgnode } ); } )?;
      }} }
  with_node_mut (
    tree, subscribee_col_nid,
    |mut col_mut| {
      for subscribee_id in subscribee_ids {
        // These subscribees are indefinitive leaves (trivial 'branches'). They can be expanded by requesting definitive expansion, the same way one would do for ordinary content.
        let subscribee_orgnode : OrgNode = {
          let mut md = default_metadata ();
          md . id = Some ( subscribee_id . clone () );
          md . code . interp = Interp::Subscribee;
          md . code . indefinitive = true;
          OrgNode {
            metadata : md,
            title : subscribee_id . 0, // Use the ID as the title
            body : None, } };
        col_mut . append (
          NodePair { mskgnode: None,
                     orgnode: subscribee_orgnode } ); } } ) ?;
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
    tree, node_id, Interp::HiddenInSubscribeeCol )? . is_some ()
  { return Ok (( )); }
  let ( subscribee_pid, subscriber_pid ) : ( ID, ID ) = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id ) . ok_or (
        "maybe_add_hidden_in_subscribee_col: node not found" ) ?;
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
    ( subscribee_pid,
      skgnode . ids [ 0 ] . clone () ) };
  let ( _visible, hidden_in_content )
    : ( HashSet < ID >, HashSet < ID > )
    = partition_subscribee_content_for_subscriber (
      &config.db_name, driver,
      & subscriber_pid,
      & subscribee_pid ) . await ?;
  if hidden_in_content . is_empty () {
    return Ok (( )); }
  let hidden_col_nid : NodeId = {
    // Create HiddenInSubscribeeCol and prepend to this subscribee
    let hidden_col_orgnode : OrgNode = {
      let mut md = default_metadata ();
      md . code . interp = Interp::HiddenInSubscribeeCol;
      OrgNode {
        metadata : md,
        title : "hidden from this subscription" . to_string (),
        body : None, } };
    with_node_mut (
      tree, node_id,
      |mut node_mut| node_mut
        . prepend ( NodePair { mskgnode: None,
                               orgnode: hidden_col_orgnode } )
        . id () ) ? };
  for hidden_id in hidden_in_content {
    // Add HiddenFromSubscribees children
    let ( skgnode, mut orgnode ) : ( SkgNode, OrgNode ) =
      skgnode_and_orgnode_from_id (
        config, driver, & hidden_id ) . await ?;
    orgnode . metadata . code . interp = Interp::HiddenFromSubscribees;
    orgnode . metadata . code . indefinitive = true;
    orgnode . body = None;
    with_node_mut (
      tree, hidden_col_nid,
      |mut hidden_col_mut| {
        hidden_col_mut . append (
          NodePair { mskgnode: Some ( skgnode ),
                     orgnode } ); } ) ?; }
  Ok (( )) }

/// Extract PIDs for the subscriber and its subscribees.
/// Returns an error if the node has no SkgNode.
fn subscriber_and_subscribee_pids (
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
    || "subscriber_and_subscribee_pids: SkgNode should exist"
    . into () ) }

/// Prepend a SubscribeeCol child to a node.
/// All SubscribeeCol nodes are identical
///   -- no SkgNode, fixed title.
fn prepend_subscribee_col (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result < NodeId, Box<dyn Error> > {
  let subscribee_col_orgnode : OrgNode = {
    let mut md = default_metadata ();
    md . code . interp = Interp::SubscribeeCol;
    OrgNode {
      metadata : md,
      title : "it subscribes to these" . to_string (),
      body : None, } };
  let col_id : NodeId = with_node_mut ( tree, node_id, |mut node_mut|
    node_mut . prepend (
      NodePair { mskgnode: None,
                 orgnode: subscribee_col_orgnode } )
    . id () ) ?;
  Ok ( col_id ) }

/// Append a HiddenOutsideOfSubscribeeCol child to a SubscribeeCol.
/// All HiddenOutsideOfSubscribeeCol nodes are identical
///   -- no SkgNode, fixed title.
fn append_hidden_outside_of_subscribee_col (
  tree              : &mut PairTree,
  subscribee_col_id : NodeId,
) -> Result < NodeId, Box<dyn Error> > {
  let hidden_outside_col_orgnode : OrgNode = {
    let mut md = default_metadata ();
    md . code . interp = Interp::HiddenOutsideOfSubscribeeCol;
    OrgNode {
      metadata : md,
      title : "hidden from all subscriptions" . to_string (),
      body : None, } };
  let col_id : NodeId = with_node_mut (
    tree, subscribee_col_id,
    |mut col_mut|
    col_mut . append (
      NodePair { mskgnode: None,
                 orgnode: hidden_outside_col_orgnode } )
    . id () ) ?;
  Ok ( col_id ) }
