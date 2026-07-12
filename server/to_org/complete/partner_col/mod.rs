pub mod child_data;
pub mod goal_list;
pub mod inverse_scan;
pub mod kind;

use crate::source_sets::ActiveSourceSet;
use crate::types::phantom::source_from_disk;
use crate::update_buffer::reconcile::omit_inactive_members;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::dbs::typedb::search::hidden_in_subscribee_content::{
  partition_subscribee_content_for_subscriber,
  what_node_hides,
  what_nodes_contain };
use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::to_org::complete::partner_col::child_data::{
  ChildData, reconcile_partnerCol_children_against_goal_list };
use crate::to_org::complete::partner_col::goal_list::{
  goal_list_for_hiddeninsubscribee_col,
  goal_list_for_hiddenoutsideof_subscribeecol,
  goal_list_for_outbound_col };
use crate::to_org::complete::partner_col::inverse_scan::inverse_scan_for_inbound_col;
use crate::to_org::util::nodecomplete_and_viewnode_from_id;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SkgConfig, SourceName, members_of};
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind, PartnerCol};
use crate::types::viewnode::Vognode;
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_node_in_tree, with_node_mut};
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
/// A Subscribee is an ActiveNode whose parent is a SubscribeeCol scaffold.
/// (Checking that its grandparent (the subscriber) is an ActiveNode
/// happens from the SubscribeeCol, so needn't be repeated here.)
pub fn type_and_parent_type_consistent_with_subscribee (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_ref : NodeRef < ViewNode > =
    tree . get (node_id)
    . ok_or ("type_and_parent_type_consistent_with_subscribee: node not found") ?;
  let is_activeNode_and_parentIs_affected : bool =
    node_ref . value () . is_activeNode_and_parentIs_affected ();
  let parent_is_subscribee_col : bool =
    node_ref . parent ()
    . map ( |p| matches! (
              & p . value () . kind,
              ViewNodeKind::PartnerCol (PartnerCol::Subscribee)))
    . unwrap_or (false);
  Ok ( is_activeNode_and_parentIs_affected
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
  active_source_set : Option<&ActiveSourceSet>,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  force_create_when_empty : bool, // a Col view-request materializes the
    // writable subscribeeCol as an empty "add here" surface even with
    // no subscribees.
) -> Result < (), Box<dyn Error> > {
  error_unless_node_satisfies(
    tree, node_id,
    |vn| matches!( &vn . kind,
                    ViewNodeKind::Vognode (Vognode::Active (_))),
    "maybe_add_subscribeeCol_branch: expected ActiveNode" ) ?;
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
  let subscriber_source : SourceName =
    read_at_node_in_tree (
      tree, node_id,
      |vn| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => Some ( t . source . clone () ),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
    . ok_or ("maybe_add_subscribeeCol_branch: expected ActiveNode") ?;
  let subscribee_ids : Vec<ID> =
    // TODO/full-schema/9-2_source-set-safety.org: inactive
    // subscribees are omitted at de novo creation (no retained
    // members exist yet); a col left empty by this is not created.
    omit_inactive_members (
      subscribee_ids,
      active_source_set,
      |id : &ID| snapshot_global ()
                 . and_then ( |g| g . pid_and_source (id)
                                  . map ( |(_pid, src)| src ))
                 . or_else ( || source_from_disk (id, config) ));
  if subscribee_ids . is_empty () {
    // Skip because it would be empty -- unless, in diff mode, the
    // HEAD side of the membership is non-empty: a col emptied since
    // HEAD still renders, so its phantoms have a home (the col is
    // created bare; its own BFS visit reconciles the phantoms in).
    let head_side_occupied : bool =
      source_diffs . is_some ()
      && ! goal_list_for_outbound_col (
             &subscriber_pid, &subscriber_source,
             NodeRelation::Subscribes,
             source_diffs, &subscribee_ids ) . 0 . is_empty ();
    if ! head_side_occupied && ! force_create_when_empty {
      return Ok (( )); }}

  let hidden_outside_content : HashSet < ID > = {
    // hidden IDs that are outside all subscribee content. Read
    // edge-level-GATED from the in-Rust graph when the global
    // handle is present (production always; render-and-gating,
    // 5_plan.org): hides and memberships recorded above the active
    // prefix must not shape this derived col. TypeDB fallback is
    // ungated (it stores no levels; harness-only).
    let r_hides : HashSet < ID > =
      match snapshot_global () {
        Some (snap) =>
          snap . outbound_pids_for_relation_gated (
            & subscriber_pid,
            NodeRelation::HidesFromItsSubscriptions,
            active_source_set )
          . into_iter () . collect (),
        None =>
          { let _span : tracing::span::EnteredSpan = tracing::info_span!(
              "what_node_hides" ). entered();
            what_node_hides (
              &config . db_name, driver, & subscriber_pid ) . await } ? };
    let all_subscribee_content : HashSet < ID > =
      match snapshot_global () {
        Some (snap) =>
          subscribee_ids . iter ()
          . flat_map ( |id| snap . outbound_pids_for_relation_gated (
            id, NodeRelation::Contains, active_source_set ))
          . collect (),
        None =>
          { let _span : tracing::span::EnteredSpan = tracing::info_span!(
              "what_nodes_contain" ). entered();
            what_nodes_contain (
              &config . db_name, driver, & subscribee_ids ) . await } ? };
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
  let hidden_outside_head_side_occupied : bool =
    // Diff-mode col existence: a hiddenOutside membership emptied
    // since HEAD still warrants the col, for its phantoms.
    hidden_outside_content . is_empty ()
    && source_diffs . is_some ()
    && { let wt_hides : Vec<ID> =
           nodecomplete_rustFirst_by_pid_and_source (
             config, &subscriber_pid, &subscriber_source )
           . ok ()
           . map ( |skg| members_of (
                       skg . hides_from_its_subscriptions . or_default () ) )
           . unwrap_or_default ();
         ! goal_list_for_hiddenoutsideof_subscribeecol (
             &subscriber_pid, &subscriber_source,
             &wt_hides, &subscribee_ids,
             source_diffs, config ) . 0 . is_empty () };
  if ! hidden_outside_content . is_empty ()
     || hidden_outside_head_side_occupied {
    // HiddenOutsideOfSubscribeeCol presents last, if it exists.
    let hidden_outside_col_nid : NodeId =
      insert_scaffold_as_child (
        tree, subscribee_col_nid,
        ViewNodeKind::PartnerCol (PartnerCol::HiddenOutsideOfSubscribee),
        false ) ?;
    with_node_mut ( tree, hidden_outside_col_nid,
      |mut n| {
        // TODO/fork-fixes.org: a new hidden col begins folded. The
        // stamp moves to the members at the col's own BFS visit
        // ('fold_members_of_newborn_col').
        n . value () . folded = true; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
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
  active_source_set : Option<&ActiveSourceSet>,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
) -> Result < (), Box<dyn Error> > {
  error_unless_node_satisfies(
    tree, node_id,
    |vn| matches!( &vn . kind,
                    ViewNodeKind::Vognode (Vognode::Active (_) )),
    "maybe_add_partnerCol_branches: expected ActiveNode" ) ?;
  { let is_indefinitive : bool =
      read_at_node_in_tree(
        tree, node_id,
        |vn| matches!( &vn . kind,
                        ViewNodeKind::Vognode (Vognode::Active (t))
                        if t . is_indefinitive () ) )
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_indefinitive { return Ok(( )); } }
  maybe_add_subscribeeCol_branch (
    tree, node_id, config, driver, active_source_set,
    source_diffs, false ) . await ?;
  let Some (graph) = snapshot_global () else {
    return Ok (( )); };
  for kind in [
    PartnerCol::Subscriber,
    PartnerCol::Overridden,
    PartnerCol::Overrider,
    PartnerCol::Hider,
    PartnerCol::Hidden,
  ] { maybe_add_one_partnerCol (
        tree, node_id, kind, config, driver, &graph,
        active_source_set, source_diffs, false ) . await ?; }
  Ok (( )) }

/// Add a generated PartnerCol for `node_id` if it would
/// have visible members.
/// Conditions determining the 'maybe' are commented.
/// 'force_create_when_empty' overrides the empty-skip: a Col view-request
/// uses it to materialize the WRITABLE col (Overridden) as an empty
/// "add here" surface even when the relation has no members. (Only ever
/// passed 'true' for a writable col; an empty read-only col would just
/// be pruned again.)
pub async fn maybe_add_one_partnerCol (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  kind    : PartnerCol,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  graph   : &Arc<InRustGraph>,
  active_source_set : Option<&ActiveSourceSet>,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  force_create_when_empty : bool,
) -> Result < (), Box<dyn Error> > {
  if unique_scaffold_child_of_viewnode (
      tree, node_id, &ViewNodeKind::PartnerCol (kind)
    )? . is_some ()
  { // There already is one. Don't draw a new one.
    return Ok (( )); }
  let (owner_pid, owner_source) : (ID, SourceName) =
    read_at_node_in_tree (
      tree, node_id,
      |vn| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => Ok (( t . id . clone (), t . source . clone () )),
        _ => Err ("expected ActiveNode" . to_string ()), } )
    .map_err( |e| -> Box<dyn Error> { e . into() } ) ??;
  let Some (member_role) = kind . relation_member_role ()
    // The two Hidden*SubscribeeCol scaffolds lack this, hence end here.
    else { return Ok (( )); };
  let owner_role =
    member_role . opposite_role ();
  let member_ids : Vec<ID> =
    // TODO/full-schema/9-2_source-set-safety.org: inactive members
    // are omitted, so a col whose members are all inactive is not
    // created at all (de novo creation has no retained members).
    omit_inactive_members (
      graph . other_member_pids_gated (
        &owner_pid, owner_role, active_source_set ),
      active_source_set,
      |id : &ID| graph . pid_and_source (id)
                 . map ( |(_pid, src)| src )
                 . or_else ( || source_from_disk (id, config) ));
  if member_ids . is_empty () {
    // It would be empty, so don't draw it -- unless, in diff mode,
    // the HEAD side of the membership is non-empty: a col emptied
    // since HEAD still renders, so its phantoms have a home (the
    // col is created bare; its own BFS visit reconciles the
    // phantoms in).
    let head_side_occupied : bool =
      source_diffs . is_some ()
      && if owner_role . is_first_role () { // outbound
           ! goal_list_for_outbound_col (
               &owner_pid, &owner_source, member_role . relation,
               source_diffs, &member_ids ) . 0 . is_empty ()
         } else { // inbound: the inverse scan
           inverse_scan_for_inbound_col (
             &owner_pid, member_role . relation, source_diffs,
             active_source_set )
           . values ()
           . any ( |axes| ! axes . net_is_present () ) };
    if ! head_side_occupied && ! force_create_when_empty {
      return Ok (( )); }}
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
  active_source_set : Option<&ActiveSourceSet>,
  source_diffs      : &Option<HashMap<SourceName, SourceDiff>>,
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
    = match snapshot_global () {
      // Edge-level-GATED when the global handle is present
      // (production always; render-and-gating, 5_plan.org): hides
      // and memberships above the active prefix must not shape
      // this derived col. TypeDB fallback is ungated (no levels;
      // harness-only).
      Some (snap) => {
        let subscriber_hides : HashSet<ID> =
          snap . outbound_pids_for_relation_gated (
            & subscriber_pid,
            NodeRelation::HidesFromItsSubscriptions,
            active_source_set )
          . into_iter () . collect ();
        let subscribee_content : HashSet<ID> =
          snap . outbound_pids_for_relation_gated (
            & subscribee_pid, NodeRelation::Contains,
            active_source_set )
          . into_iter () . collect ();
        ( subscribee_content . iter ()
            . filter ( |id| ! subscriber_hides . contains (id) )
            . cloned () . collect (),
          subscribee_content . iter ()
            . filter ( |id| subscriber_hides . contains (id) )
            . cloned () . collect () ) }
      None =>
        partition_subscribee_content_for_subscriber (
          & config . db_name, driver,
          & subscriber_pid, & subscribee_pid ) . await ? };
  if hidden_in_content . is_empty () {
    // Skip because it would be empty -- unless, in diff mode, the
    // HEAD-side DERIVED membership is non-empty: a hidden-in
    // membership emptied since HEAD still warrants the col, for its
    // phantoms (created bare; the BFS reconciles them in).
    let head_side_occupied : bool =
      source_diffs . is_some ()
      && { let source_of = | pid : &ID | -> SourceName {
             snapshot_global ()
               . and_then ( |g| g . pid_and_source (pid)
                                . map ( |(_p, src)| src ))
               . or_else ( || source_from_disk (pid, config) )
               . unwrap_or_else ( SourceName::not_found ) };
           let subscribee_source : SourceName =
             source_of (&subscribee_pid);
           let subscriber_source : SourceName =
             source_of (&subscriber_pid);
           let subscribee_contains : Vec<ID> =
             nodecomplete_rustFirst_by_pid_and_source (
               config, &subscribee_pid, &subscribee_source )
             . ok () . map ( |skg| members_of (& skg . contains) )
             . unwrap_or_default ();
           let subscriber_hides : Vec<ID> =
             nodecomplete_rustFirst_by_pid_and_source (
               config, &subscriber_pid, &subscriber_source )
             . ok ()
             . map ( |skg| members_of (
                         skg . hides_from_its_subscriptions . or_default () ) )
             . unwrap_or_default ();
           ! goal_list_for_hiddeninsubscribee_col (
               &subscribee_pid, &subscribee_source,
               &subscriber_pid, &subscriber_source,
               &subscribee_contains, &subscriber_hides,
               source_diffs ) . 0 . is_empty () };
    if ! head_side_occupied { return Ok (( )); }}
  let hidden_in_ids : Vec<ID> =
    hidden_in_content . into_iter () . collect ();
  let hidden_col_nid : NodeId =
    insert_scaffold_as_child (
      tree, subscribee_treeid,
      ViewNodeKind::PartnerCol (PartnerCol::HiddenInSubscribee),
      true ) ?;
  with_node_mut ( tree, hidden_col_nid,
    |mut n| {
      // TODO/fork-fixes.org: a new hidden col begins folded. The
      // stamp moves to the members at the col's own BFS visit
      // ('fold_members_of_newborn_col').
      n . value () . folded = true; } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
    build_initial_render_child_data (
      &hidden_in_ids, config, driver ) . await ?;
  reconcile_partnerCol_children_against_goal_list (
    tree, hidden_col_nid,
    PartnerCol::HiddenInSubscribee,
    &goal, &data ) ?;
  Ok (( )) }
