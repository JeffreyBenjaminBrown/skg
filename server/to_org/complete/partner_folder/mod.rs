pub mod child_data;
pub mod goal_list;
pub mod inverse_scan;
pub mod kind;

use crate::source_sets::ActiveSourceSet;
use crate::types::phantom::home_from_disk;
use crate::update_buffer::reconcile::omit_inactive_members;
use crate::dbs::node_lookup::nodecomplete_graphFirst_by_pid_and_source;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::to_org::complete::partner_folder::child_data::{
  ChildData, reconcile_partnerFolder_children_against_goal_list };
use crate::to_org::complete::partner_folder::goal_list::{
  goal_list_for_hiddenInSubscribee_folder,
  goal_list_for_hiddenOutsideOfSubscribee_folder,
  goal_list_for_outbound_folder };
use crate::to_org::complete::partner_folder::inverse_scan::inverse_scan_for_inbound_folder;
use crate::to_org::util::nodecomplete_and_viewnode_from_id;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SkgConfig, SourceName, members_of};
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind, PartnerFolder};
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

/// Resolve each goal-list id to its primary-pid form (so extra_ids
/// collapse to their primary pid), keeping the original input order
/// and de-duplicating, and build a 'ChildData' for each.
///
/// Uses 'nodecomplete_and_viewnode_from_id' with the captured graph so
/// cross-source IDs and extra_id-to-primary resolution both work,
/// matching the source-resolution behavior of the per-id append
/// loops this code replaced.
///
/// Returns '(resolved_pids, child_data)': the resolved goal list
/// and the per-pid data map that 'reconcile_partnerFolder_children_against_goal_list'
/// expects.
///
/// 'phantom' is always 'None' on initial render: there is no diff
/// view, no removed children, and no notion of a node that "used
/// to be here" from a previous state.
fn build_initial_render_child_data (
  ids    : &[ID],
  graph  : &InRustGraph,
  config : &SkgConfig,
) -> Result<(Vec<ID>, HashMap<ID, ChildData>), Box<dyn Error>> {
  let mut goal     : Vec<ID>                = Vec::with_capacity (ids . len ());
  let mut resolved : HashMap<ID, ChildData> = HashMap::new ();
  for id in ids {
    let lookup : Option<(NodeComplete, ViewNode)> =
      nodecomplete_and_viewnode_from_id (graph, config, id) ?;
    let (primary_pid, source, title, unknown) : (ID, SourceName, String, bool) = match lookup {
      Some ((nc, _vn)) =>
        ( nc . pid . clone (),
          nc . source . clone (),
          nc . title . clone (), false ),
      None => // No record anywhere; 'reconcile' will still need an
              // entry, but downstream rendering would treat this as
              // an PhantomUnknown case. We pass the raw id through with
              // a sentinel source/title so the reconcile call can
              // still run.
        ( id . clone (), SourceName::from (""), String::new (), true ), };
    if resolved . contains_key (&primary_pid) { continue; }
    goal . push (primary_pid . clone ());
    resolved . insert (
      primary_pid,
      ChildData { source, title, phantom : None, unknown,
                  relSource : None } ); }
  Ok ((goal, resolved)) }

/// Check if a node's type and parent type are consistent with being a Subscribee.
/// A Subscribee is an ActiveNode whose parent is a SubscribeeFolder scaffold.
/// (Checking that its grandparent (the subscriber) is an ActiveNode
/// happens from the SubscribeeFolder, so needn't be repeated here.)
pub fn type_and_parent_type_consistent_with_subscribee (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_ref : NodeRef < ViewNode > =
    tree . get (node_id)
    . ok_or ("type_and_parent_type_consistent_with_subscribee: node not found") ?;
  let is_activeNode_and_affectsParent_true : bool =
    node_ref . value () . is_activeNode_and_affectsParent_true ();
  let affects_parent_subscribeeFolder : bool =
    node_ref . parent ()
    . map ( |p| matches! (
              & p . value () . kind,
              ViewNodeKind::PartnerFolder (PartnerFolder::Subscribee)))
    . unwrap_or (false);
  Ok ( is_activeNode_and_affectsParent_true
       && affects_parent_subscribeeFolder ) }

/// If appropriate, prepend a SubscribeeFolder child containing:
/// - for each subscribee, a write-protected Subscribee child
/// - if any hidden nodes are outside subscribee content,
///   a HiddenOutsideOfSubscribeeFolder
pub fn maybe_add_subscribeeFolder_branch (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId, // if applicable, this is the subscriber
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active_source_set : Option<&ActiveSourceSet>,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  force_create_when_empty : bool, // a Folder view-request materializes the
    // writable subscribeeFolder as an empty "add here" surface even with
    // no subscribees.
) -> Result < (), Box<dyn Error> > {
  error_unless_node_satisfies(
    tree, node_id,
    |vn| matches!( &vn . kind,
                    ViewNodeKind::Vognode (Vognode::Active (_))),
    "maybe_add_subscribeeFolder_branch: expected ActiveNode" ) ?;
  { let is_writeProtected : bool =
      read_at_node_in_tree(
        tree, node_id,
        |vn| matches!( &vn . kind,
                        ViewNodeKind::Vognode (Vognode::Active (t))
                        if t . is_writeProtected () ))
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_writeProtected { return Ok(( )); } }
  { // Pre-existing SubscribeeFolder children are reconciled by view completion (complete_nodes_in_level_order), which dispatches to 'reconcile_subscribeeFolder_children'.
    if unique_scaffold_child_of_viewnode (
      tree, node_id,
      &ViewNodeKind::PartnerFolder (PartnerFolder::Subscribee) )? . is_some ()
    { return Ok (( )); }}
  let ( subscriber_pid, subscribee_ids ) : ( ID, Vec < ID > ) =
    pids_for_subscriber_and_its_subscribees (
      tree, node_id, graph, config ) ?;
  let subscriber_source : SourceName =
    read_at_node_in_tree (
      tree, node_id,
      |vn| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (t))
          => Some ( t . source . clone () ),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
    . ok_or ("maybe_add_subscribeeFolder_branch: expected ActiveNode") ?;
  let subscribee_ids : Vec<ID> =
    // TODO/full-schema/9-2_source-set-safety.org: inactive
    // subscribees are omitted at de novo creation (no retained
    // members exist yet); a folder left empty by this is not created.
    omit_inactive_members (
      subscribee_ids,
      active_source_set,
      |id : &ID| graph . pid_and_source (id)
                 . map ( |(_pid, src)| src )
                 . or_else ( || home_from_disk (id, config) ));
  if subscribee_ids . is_empty () {
    // Skip because it would be empty -- unless, in diff mode, the
    // HEAD side of the membership is non-empty: a folder emptied since
    // HEAD still renders, so its phantoms have a home (the folder is
    // created bare; its own BFS visit reconciles the phantoms in).
    let head_side_occupied : bool =
      source_diffs . is_some ()
      && ! goal_list_for_outbound_folder (
             &subscriber_pid, &subscriber_source,
             NodeRelation::Subscribes,
             source_diffs, &subscribee_ids ) . 0 . is_empty ();
    if ! head_side_occupied && ! force_create_when_empty {
      return Ok (( )); }}

  let hidden_outside_content : HashSet < ID > = {
    // hidden IDs that are outside all subscribee content. Read
    // relSource-GATED from the captured graph: hides and memberships
    // recorded outside the active prefix must not shape this derived folder.
    let r_hides : HashSet < ID > =
          graph . outbound_pids_for_relation_gated (
            & subscriber_pid,
            NodeRelation::HidesFromItsSubscriptions,
            active_source_set )
          . into_iter () . collect ();
    let all_subscribee_content : HashSet < ID > =
          subscribee_ids . iter ()
          . flat_map ( |id| graph . outbound_pids_for_relation_gated (
            id, NodeRelation::Contains, active_source_set ))
          . collect ();
    r_hides . iter ()
      . filter ( | id | ! all_subscribee_content . contains (id) )
      . cloned () . collect () };

  let subscribee_folder_nid : NodeId =
    insert_scaffold_as_child ( tree, node_id,
      ViewNodeKind::PartnerFolder (PartnerFolder::Subscribee), true ) ?;
  { let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
      build_initial_render_child_data (
        &subscribee_ids, graph, config ) ?;
    reconcile_partnerFolder_children_against_goal_list (
      tree, subscribee_folder_nid,
      PartnerFolder::Subscribee,
      &goal, &data ) ?; }
  let hidden_outside_head_side_occupied : bool =
    // Diff-mode folder existence: a hiddenOutside membership emptied
    // since HEAD still warrants the folder, for its phantoms.
    hidden_outside_content . is_empty ()
    && source_diffs . is_some ()
    && { let wt_hides : Vec<ID> =
           nodecomplete_graphFirst_by_pid_and_source (
             graph, config, &subscriber_pid, &subscriber_source )
           . ok ()
           . map ( |skg| members_of (
                       skg . hides_from_its_subscriptions . or_default () ) )
           . unwrap_or_default ();
         ! goal_list_for_hiddenOutsideOfSubscribee_folder (
             graph,
             &subscriber_pid, &subscriber_source,
             &wt_hides, &subscribee_ids,
             source_diffs, config ) . 0 . is_empty () };
  if ! hidden_outside_content . is_empty ()
     || hidden_outside_head_side_occupied {
    // HiddenOutsideOfSubscribeeFolder presents last, if it exists.
    let hidden_outside_folder_nid : NodeId =
      insert_scaffold_as_child (
        tree, subscribee_folder_nid,
        ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee),
        false ) ?;
    with_node_mut ( tree, hidden_outside_folder_nid,
      |mut n| {
        // TODO/fork-fixes.org: a new hidden folder begins folded. The
        // stamp moves to the members at the folder's own BFS visit
        // ('fold_members_of_newborn_folder').
        n . value () . folded = true; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    let hidden_outside_ids : Vec<ID> =
      hidden_outside_content . into_iter () . collect ();
    let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
      build_initial_render_child_data (
        &hidden_outside_ids, graph, config ) ?;
    reconcile_partnerFolder_children_against_goal_list (
      tree, hidden_outside_folder_nid,
      PartnerFolder::HiddenOutsideOfSubscribee,
      &goal, &data ) ?; }
  Ok (( )) }

/// Add the PartnerFolders that a definitive node shows without an explicit
/// Folder view request.  Only a nonempty SubscribeeFolder is part of that
/// initial presentation; the other relation folders are available through
/// `(viewRequests (folder ...))` when their heralds indicate they are useful.
pub fn maybe_add_default_partnerFolder_branches (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active_source_set : Option<&ActiveSourceSet>,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
) -> Result < (), Box<dyn Error> > {
  error_unless_node_satisfies(
    tree, node_id,
    |vn| matches!( &vn . kind,
                    ViewNodeKind::Vognode (Vognode::Active (_) )),
    "maybe_add_default_partnerFolder_branches: expected ActiveNode" ) ?;
  { let is_writeProtected : bool =
      read_at_node_in_tree(
        tree, node_id,
        |vn| matches!( &vn . kind,
                        ViewNodeKind::Vognode (Vognode::Active (t))
                        if t . is_writeProtected () ) )
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_writeProtected { return Ok(( )); } }
  maybe_add_subscribeeFolder_branch (
    tree, node_id, graph, config, active_source_set,
    source_diffs, false ) ?;
  Ok (( )) }

/// Add a generated PartnerFolder for `node_id` if it would
/// have visible members.
/// Conditions determining the 'maybe' are commented.
/// 'force_create_when_empty' overrides the empty-skip: a Folder view-request
/// uses it to materialize the WRITABLE folder (Overridden) as an empty
/// "add here" surface even when the relation has no members. (Only ever
/// passed 'true' for a writable folder; an empty read-only folder would just
/// be pruned again.)
pub fn maybe_add_one_partnerFolder (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  kind    : PartnerFolder,
  config  : &SkgConfig,
  graph   : &InRustGraph,
  active_source_set : Option<&ActiveSourceSet>,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  force_create_when_empty : bool,
) -> Result < (), Box<dyn Error> > {
  if unique_scaffold_child_of_viewnode (
      tree, node_id, &ViewNodeKind::PartnerFolder (kind)
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
    // The two Hidden*SubscribeeFolder scaffolds lack this, hence end here.
    else { return Ok (( )); };
  let owner_role =
    member_role . opposite_role ();
  let member_ids : Vec<ID> =
    // TODO/full-schema/9-2_source-set-safety.org: inactive members
    // are omitted, so a folder whose members are all inactive is not
    // created at all (de novo creation has no retained members).
    omit_inactive_members (
      graph . other_member_pids_gated (
        &owner_pid, owner_role, active_source_set ),
      active_source_set,
      |id : &ID| graph . pid_and_source (id)
                 . map ( |(_pid, src)| src )
                 . or_else ( || home_from_disk (id, config) ));
  if member_ids . is_empty () {
    // It would be empty, so don't draw it -- unless, in diff mode,
    // the HEAD side of the membership is non-empty: a folder emptied
    // since HEAD still renders, so its phantoms have a home (the
    // folder is created bare; its own BFS visit reconciles the
    // phantoms in).
    let head_side_occupied : bool =
      source_diffs . is_some ()
      && if owner_role . is_first_role () { // outbound
           ! goal_list_for_outbound_folder (
               &owner_pid, &owner_source, member_role . relation,
               source_diffs, &member_ids ) . 0 . is_empty ()
         } else { // inbound: the inverse scan
           inverse_scan_for_inbound_folder (
             &owner_pid, member_role . relation, source_diffs,
             active_source_set )
           . values ()
           . any ( |axes| ! axes . net_is_present () ) };
    if ! head_side_occupied && ! force_create_when_empty {
      return Ok (( )); }}
  let folder_nid : NodeId =
    insert_scaffold_as_child (
      tree, node_id, ViewNodeKind::PartnerFolder (kind), true) ?;
  let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
    build_initial_render_child_data (
      &member_ids, graph, config ) ?;
  reconcile_partnerFolder_children_against_goal_list (
    tree, folder_nid, kind, &goal, &data ) ?;
  Ok (( )) }

/// If this node is a Subscribee,
/// and the corresponding subscriber hides any of its content,
/// then prepend a HiddenInSubscribeeFolder to hold those hidden nodes.
/// The subscriber is the Subscribee's grandparent:
///   subscriber -> SubscribeeFolder -> Subscribee
pub fn maybe_add_hiddenInSubscribeeFolder_branch (
  tree              : &mut Tree<ViewNode>,
  subscribee_treeid : NodeId,
  graph             : &InRustGraph,
  config            : &SkgConfig,
  active_source_set : Option<&ActiveSourceSet>,
  source_diffs      : &Option<HashMap<SourceName, SourceDiff>>,
) -> Result < (), Box<dyn Error> > {
  if ! type_and_parent_type_consistent_with_subscribee (
    tree, subscribee_treeid )?
  { return Err ( "maybe_add_hiddenInSubscribeeFolder_branch called on non-subscribee" . into ( )); }
  if unique_scaffold_child_of_viewnode (
      // Pre-existing HiddenIn folders are instead reconciled by the rerender pipeline's 'reconcile_hiddenIn_folders', which dispatches to 'reconcile_hiddenInSubscribeeFolder_children'.
       tree, subscribee_treeid,
       &ViewNodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)
     )? . is_some ()
  { return Ok (( )); }
  let ( subscribee_pid, subscriber_pid ) : ( ID, ID ) =
    pid_for_subscribee_and_its_subscriber_grandparent (
      tree, subscribee_treeid, graph, config ) ?;
  let ( _visible, hidden_in_content )
    : ( HashSet < ID >, HashSet < ID > )
    = {
      // relSource-GATED from the captured graph: hides and memberships
      // outside the active prefix must not shape this derived folder.
      {
        let subscriber_hides : HashSet<ID> =
          graph . outbound_pids_for_relation_gated (
            & subscriber_pid,
            NodeRelation::HidesFromItsSubscriptions,
            active_source_set )
          . into_iter () . collect ();
        let subscribee_content : HashSet<ID> =
          graph . outbound_pids_for_relation_gated (
            & subscribee_pid, NodeRelation::Contains,
            active_source_set )
          . into_iter () . collect ();
        ( subscribee_content . iter ()
            . filter ( |id| ! subscriber_hides . contains (id) )
            . cloned () . collect (),
          subscribee_content . iter ()
            . filter ( |id| subscriber_hides . contains (id) )
            . cloned () . collect () ) } };
  if hidden_in_content . is_empty () {
    // Skip because it would be empty -- unless, in diff mode, the
    // HEAD-side DERIVED membership is non-empty: a hidden-in
    // membership emptied since HEAD still warrants the folder, for its
    // phantoms (created bare; the BFS reconciles them in).
    let head_side_occupied : bool =
      source_diffs . is_some ()
      && { let source_of = | pid : &ID | -> SourceName {
             graph . pid_and_source (pid)
               . map ( |(_p, src)| src )
               . or_else ( || home_from_disk (pid, config) )
               . unwrap_or_else ( SourceName::not_found ) };
           let subscribee_source : SourceName =
             source_of (&subscribee_pid);
           let subscriber_source : SourceName =
             source_of (&subscriber_pid);
           let subscribee_contains : Vec<ID> =
             nodecomplete_graphFirst_by_pid_and_source (
               graph, config, &subscribee_pid, &subscribee_source )
             . ok () . map ( |skg| members_of (& skg . contains) )
             . unwrap_or_default ();
           let subscriber_hides : Vec<ID> =
             nodecomplete_graphFirst_by_pid_and_source (
               graph, config, &subscriber_pid, &subscriber_source )
             . ok ()
             . map ( |skg| members_of (
                         skg . hides_from_its_subscriptions . or_default () ) )
             . unwrap_or_default ();
           ! goal_list_for_hiddenInSubscribee_folder (
               graph,
               &subscribee_pid, &subscribee_source,
               &subscriber_pid, &subscriber_source,
               &subscribee_contains, &subscriber_hides,
               source_diffs ) . 0 . is_empty () };
    if ! head_side_occupied { return Ok (( )); }}
  let hidden_in_ids : Vec<ID> =
    hidden_in_content . into_iter () . collect ();
  let hidden_folder_nid : NodeId =
    insert_scaffold_as_child (
      tree, subscribee_treeid,
      ViewNodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee),
      true ) ?;
  with_node_mut ( tree, hidden_folder_nid,
    |mut n| {
      // TODO/fork-fixes.org: a new hidden folder begins folded. The
      // stamp moves to the members at the folder's own BFS visit
      // ('fold_members_of_newborn_folder').
      n . value () . folded = true; } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
    build_initial_render_child_data (
      &hidden_in_ids, graph, config ) ?;
  reconcile_partnerFolder_children_against_goal_list (
    tree, hidden_folder_nid,
    PartnerFolder::HiddenInSubscribee,
    &goal, &data ) ?;
  Ok (( )) }
