pub mod child_data;
pub mod goal_list;
pub mod kind;

use crate::dbs::typedb::search::hidden_in_subscribee_content::{
  partition_subscribee_content_for_subscriber,
  what_node_hides,
  what_nodes_contain };
use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::to_org::complete::partner_col::child_data::{
  ChildData, reconcile_partnerCol_children_against_goal_list };
use crate::to_org::util::nodecomplete_and_viewnode_from_id;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind, PartnerCol};
use crate::types::viewnode::Vognode;
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_node_in_tree};
use crate::types::tree::viewnode_nodecomplete::{
  insert_scaffold_as_child,
  pids_for_subscriber_and_its_subscribees,
  pid_for_subscribee_and_its_subscriber_grandparent,
  unique_scaffold_child_of_viewnode };

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

/// Resolve each goal-list id to its primary-pid form (so extra_ids
/// collapse to their primary pid), keeping the original input order
/// and de-duplicating, and build a 'ChildData' for each.
///
/// Uses 'nodecomplete_and_viewnode_from_id' (TypeDB-aware) so
/// cross-source IDs and extra_id-to-primary resolution both work,
/// matching the source-resolution behavior of the per-id append
/// loops this code replaced.
///
/// Returns '(resolved_pids, child_data)': the resolved goal list
/// and the per-pid data map that 'reconcile_partnerCol_children_against_goal_list'
/// expects.
///
/// 'phantom' is always 'None' on initial render: there is no diff
/// view, no removed children, and no notion of a node that "used
/// to be here" from a previous state.
async fn build_initial_render_child_data (
  ids    : &[ID],
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<(Vec<ID>, HashMap<ID, ChildData>), Box<dyn Error>> {
  let mut goal     : Vec<ID>                = Vec::with_capacity (ids . len ());
  let mut resolved : HashMap<ID, ChildData> = HashMap::new ();
  for id in ids {
    let lookup : Option<(NodeComplete, ViewNode)> =
      nodecomplete_and_viewnode_from_id (config, driver, id) . await ?;
    let (primary_pid, source, title) : (ID, SourceName, String) = match lookup {
      Some ((nc, _vn)) =>
        ( nc . pid . clone (),
          nc . source . clone (),
          nc . title . clone () ),
      None => // No record anywhere; 'reconcile' will still need an
              // entry, but downstream rendering would treat this as
              // an PhantomUnknown case. We pass the raw id through with
              // a sentinel source/title so the reconcile call can
              // still run.
        ( id . clone (), SourceName::from (""), String::new () ), };
    if resolved . contains_key (&primary_pid) { continue; }
    goal . push (primary_pid . clone ());
    resolved . insert (
      primary_pid,
      ChildData { source, title, phantom : None } ); }
  Ok ((goal, resolved)) }

/// Check if a node's type and parent type are consistent with being a Subscribee.
/// A Subscribee is a TrueNode whose parent is a SubscribeeCol scaffold.
/// (Checking that its grandparent (the subscriber) is a TrueNode
/// happens from the SubscribeeCol, so needn't be repeated here.)
pub fn type_and_parent_type_consistent_with_subscribee (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_ref : NodeRef < ViewNode > =
    tree . get (node_id)
    . ok_or ("type_and_parent_type_consistent_with_subscribee: node not found") ?;
  let is_truenode_and_parentIs_affected : bool =
    node_ref . value () . is_truenode_and_parentIs_affected ();
  let parent_is_subscribee_col : bool =
    node_ref . parent ()
    . map ( |p| matches! (
              & p . value () . kind,
              ViewNodeKind::PartnerCol (PartnerCol::Subscribee)))
    . unwrap_or (false);
  Ok ( is_truenode_and_parentIs_affected
       && parent_is_subscribee_col ) }

/// If appropriate, prepend a SubscribeeCol child containing:
/// - for each subscribee, an indefinitive Subscribee child
/// - if any hidden nodes are outside subscribee content,
///   a HiddenOutsideOfSubscribeeCol
pub async fn maybe_add_subscribeeCol_branch (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId, // if applicable, this is the subscriber
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  error_unless_node_satisfies(
    tree, node_id,
    |vn| matches!( &vn . kind,
                    ViewNodeKind::Vognode (Vognode::Active (_))),
    "maybe_add_subscribeeCol_branch: expected TrueNode" ) ?;
  { let is_indefinitive : bool =
      read_at_node_in_tree(
        tree, node_id,
        |vn| matches!( &vn . kind,
                        ViewNodeKind::Vognode (Vognode::Active (t))
                        if t . is_indefinitive () ))
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_indefinitive { return Ok(( )); } }
  { // Pre-existing SubscribeeCol children are reconciled by view completion (complete_nodes_in_level_order), which dispatches to 'reconcile_subscribee_col_children'.
    if unique_scaffold_child_of_viewnode (
      tree, node_id,
      &ViewNodeKind::PartnerCol (PartnerCol::Subscribee) )? . is_some ()
    { return Ok (( )); }}
  let ( subscriber_pid, subscribee_ids ) : ( ID, Vec < ID > ) =
    pids_for_subscriber_and_its_subscribees ( tree, node_id, config ) ?;
  if subscribee_ids . is_empty () { // Skip because it would be empty.
    return Ok (( )); }

  let hidden_outside_content : HashSet < ID > = {
    // hidden IDs that are outside all subscribee content
    let r_hides : HashSet < ID > =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "what_node_hides" ). entered();
        what_node_hides (
          &config . db_name, driver, & subscriber_pid ) . await } ?;
    let all_subscribee_content : HashSet < ID > =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "what_nodes_contain" ). entered();
        what_nodes_contain (
          &config . db_name, driver, & subscribee_ids ) . await } ?;
    r_hides . iter ()
      . filter ( | id | ! all_subscribee_content . contains (id) )
      . cloned () . collect () };

  let subscribee_col_nid : NodeId =
    insert_scaffold_as_child ( tree, node_id,
      ViewNodeKind::PartnerCol (PartnerCol::Subscribee), true ) ?;
  { let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
      build_initial_render_child_data (
        &subscribee_ids, config, driver ) . await ?;
    reconcile_partnerCol_children_against_goal_list (
      tree, subscribee_col_nid,
      PartnerCol::Subscribee,
      &goal, &data ) ?; }
  if ! hidden_outside_content . is_empty () {
    // HiddenOutsideOfSubscribeeCol presents last, if it exists.
    let hidden_outside_col_nid : NodeId =
      insert_scaffold_as_child (
        tree, subscribee_col_nid,
        ViewNodeKind::PartnerCol (PartnerCol::HiddenOutsideOfSubscribee),
        false ) ?;
    let hidden_outside_ids : Vec<ID> =
      hidden_outside_content . into_iter () . collect ();
    let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
      build_initial_render_child_data (
        &hidden_outside_ids, config, driver ) . await ?;
    reconcile_partnerCol_children_against_goal_list (
      tree, hidden_outside_col_nid,
      PartnerCol::HiddenOutsideOfSubscribee,
      &goal, &data ) ?; }
  Ok (( )) }

/// Handle maybe_add_subscribeeCol_branch separately,
/// and then run maybe_add_one_partnerCol
/// for the other kinds of PartnerCols.
pub async fn maybe_add_partnerCol_branches (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  error_unless_node_satisfies(
    tree, node_id,
    |vn| matches!( &vn . kind,
                    ViewNodeKind::Vognode (Vognode::Active (_) )),
    "maybe_add_partnerCol_branches: expected TrueNode" ) ?;
  { let is_indefinitive : bool =
      read_at_node_in_tree(
        tree, node_id,
        |vn| matches!( &vn . kind,
                        ViewNodeKind::Vognode (Vognode::Active (t))
                        if t . is_indefinitive () ) )
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_indefinitive { return Ok(( )); } }
  maybe_add_subscribeeCol_branch (
    tree, node_id, config, driver) . await ?;
  let Some (graph) = snapshot_global () else {
    return Ok (( )); };
  for kind in [
    PartnerCol::Subscriber,
    PartnerCol::Overridden,
    PartnerCol::Overrider,
    PartnerCol::Hider,
    PartnerCol::Hidden,
  ] { maybe_add_one_partnerCol (
        tree, node_id, kind, config, driver, &graph ) . await ?; }
  Ok (( )) }

/// Add a generated PartnerCol for `node_id` if it would
/// have visible members.
/// Conditions determining the 'maybe' are commented.
async fn maybe_add_one_partnerCol (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  kind    : PartnerCol,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  graph   : &Arc<InRustGraph>,
) -> Result < (), Box<dyn Error> > {
  if unique_scaffold_child_of_viewnode (
      tree, node_id, &ViewNodeKind::PartnerCol (kind)
    )? . is_some ()
  { // There already is one. Don't draw a new one.
    return Ok (( )); }
  let owner_pid : ID =
    read_at_node_in_tree (
      tree, node_id,
      |vn| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => Ok (t . id . clone ()),
        _ => Err ("expected TrueNode" . to_string ()), } )
    .map_err( |e| -> Box<dyn Error> { e . into() } ) ??;
  let Some (member_role) = kind . relation_member_role ()
    // The two Hidden*SubscribeeCol scaffolds lack this, hence end here.
    else { return Ok (( )); };
  let owner_role =
    member_role . opposite_role ();
  let member_ids : Vec<ID> =
    graph . other_member_pids (&owner_pid, owner_role);
  if member_ids . is_empty () {
    // It would be empty, so don't draw it.
    return Ok (( )); }
  let col_nid : NodeId =
    insert_scaffold_as_child (
      tree, node_id, ViewNodeKind::PartnerCol (kind), true) ?;
  let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
    build_initial_render_child_data (
      &member_ids, config, driver ) . await ?;
  reconcile_partnerCol_children_against_goal_list (
    tree, col_nid, kind, &goal, &data ) ?;
  Ok (( )) }

/// If this node is a Subscribee,
/// and the corresponding subscriber hides any of its content,
/// then prepend a HiddenInSubscribeeCol to hold those hidden nodes.
/// The subscriber is the Subscribee's grandparent:
///   subscriber -> SubsribeeCol -> Subscribee
pub async fn maybe_add_hiddenInSubscribeeCol_branch (
  tree              : &mut Tree<ViewNode>,
  subscribee_treeid : NodeId,
  config            : &SkgConfig,
  driver            : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  if ! type_and_parent_type_consistent_with_subscribee (
    tree, subscribee_treeid )?
  { return Err ( "maybe_add_hiddenInSubscribeeCol_branch called on non-subscribee" . into ( )); }
  if unique_scaffold_child_of_viewnode (
      // Pre-existing HiddenIn collections are instead reconciled by the rerender pipeline's 'reconcile_hiddenin_cols', which dispatches to 'reconcile_hiddenin_subscribee_col_children'.
       tree, subscribee_treeid,
       &ViewNodeKind::PartnerCol (PartnerCol::HiddenInSubscribee)
     )? . is_some ()
  { return Ok (( )); }
  let ( subscribee_pid, subscriber_pid ) : ( ID, ID ) =
    pid_for_subscribee_and_its_subscriber_grandparent (
      tree, subscribee_treeid, config ) ?;
  let ( _visible, hidden_in_content )
    : ( HashSet < ID >, HashSet < ID > )
    = partition_subscribee_content_for_subscriber (
        & config . db_name, driver,
        & subscriber_pid, & subscribee_pid ) . await ?;
  if hidden_in_content . is_empty () {
    return Ok (( )); }
  let hidden_in_ids : Vec<ID> =
    hidden_in_content . into_iter () . collect ();
  let hidden_col_nid : NodeId =
    insert_scaffold_as_child (
      tree, subscribee_treeid,
      ViewNodeKind::PartnerCol (PartnerCol::HiddenInSubscribee),
      true ) ?;
  let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
    build_initial_render_child_data (
      &hidden_in_ids, config, driver ) . await ?;
  reconcile_partnerCol_children_against_goal_list (
    tree, hidden_col_nid,
    PartnerCol::HiddenInSubscribee,
    &goal, &data ) ?;
  Ok (( )) }
