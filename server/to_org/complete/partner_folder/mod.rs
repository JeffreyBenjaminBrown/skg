pub mod child_data;
pub mod goal_list;
pub mod kind;

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::phantom::home_from_disk;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::to_org::complete::partner_folder::child_data::{
  ChildData, FolderGoalItem, reconcile_partnerFolder_children_against_goal_list };
use crate::to_org::util::graphnode_and_viewnode_from_skgid;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{Viewnode, ViewnodeKind, Folder};
use crate::types::viewnode::{Vognode};
use crate::types::tree::generic::{error_unless_node_satisfies, read_at_node_in_tree, with_node_mut};
use crate::types::tree::viewnode_graphnode::{
  insert_non_vognode_as_child,
  pid_for_subscribee_and_its_subscriber_grandparent,
  unique_non_vognode_child_of_viewnode };

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// Resolve each goal-list id to its primary-pid form (so extra_ids
/// collapse to their primary pid), keeping the original input order
/// and de-duplicating, and build a 'ChildData' for each.
///
/// Uses 'graphnode_and_viewnode_from_id' with the captured graph so
/// cross-repo IDs and extra_id-to-primary resolution both work,
/// matching the repo-resolution behavior of the per-id append
/// loops this code replaced.
///
/// Returns '(resolved_pids, child_data)': the resolved goal list
/// and the per-pid data map that 'reconcile_partnerFolder_children_against_goal_list'
/// expects.
fn build_initial_render_child_data (
  skgids : &[ID],
  graph  : &InRustGraph,
  config : &SkgConfig,
) -> Result<(Vec<ID>, HashMap<ID, ChildData>), Box<dyn Error>> {
  let mut goal     : Vec<ID>                = Vec::with_capacity (skgids . len ());
  let mut resolved : HashMap<ID, ChildData> = HashMap::new ();
  for skgid in skgids {
    let lookup : Option<(Graphnode, Viewnode)> =
      graphnode_and_viewnode_from_skgid (graph, config, skgid) ?;
    let (primary_pid, skgrepo, title, unknown) : (ID, SkgrepoName, String, bool) = match lookup {
      Some ((nc, _vn)) =>
        ( nc . pid . clone (),
          nc . home_skgrepo . clone (),
          nc . title . clone (), false ),
      None => // No record anywhere; 'reconcile' will still need an
              // entry, but downstream rendering would treat this as
              // an Unknown case. We pass the raw id through with
              // a sentinel skgrepo/title so the reconcile call can
              // still run.
        ( skgid . clone (), SkgrepoName::from (""), String::new (), true ), };
    if resolved . contains_key (&primary_pid) { continue; }
    goal . push (primary_pid . clone ());
    resolved . insert (
      primary_pid,
      ChildData { home_skgrepo: skgrepo, title, unknown,
                  relRepo : None } ); }
  Ok ((goal, resolved)) }

/// Check if a node's type and parent type are consistent with being a Subscribee.
/// A Subscribee is an UnrestrictedVognode whose parent is a SubscribeeFolder.
/// (Checking that its grandparent (the subscriber) is an UnrestrictedVognode
/// happens from the SubscribeeFolder, so needn't be repeated here.)
pub fn type_and_parent_type_consistent_with_subscribee (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_ref : NodeRef < Viewnode > =
    tree . get (treeid)
    . ok_or ("type_and_parent_type_consistent_with_subscribee: node not found") ?;
  let is_unrestrictedVognode_and_member : bool =
    node_ref . value () . is_unrestrictedVognode_and_member ();
  let parent_is_subscribeeFolder : bool =
    node_ref . parent ()
    . map ( |p| matches! (
              & p . value () . kind,
              ViewnodeKind::Folder (Folder::Subscribee)))
    . unwrap_or (false);
  Ok ( is_unrestrictedVognode_and_member
       && parent_is_subscribeeFolder ) }

/// If appropriate, prepend a SubscribeeFolder child containing:
/// - for each subscribee, a write-protected Subscribee child
/// - if any hidden nodes are outside subscribee content,
///   a HiddenOutsideOfSubscribeeFolder
pub fn maybe_add_subscribeeFolder_branch (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId, // if applicable, this is the subscriber
  graph   : &InRustGraph,
  config  : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  force_create_when_empty : bool, // a Folder view-request materializes the
    // editable subscribeeFolder as an empty "add here" surface even with
    // no subscribees.
) -> Result < (), Box<dyn Error> > {
  error_unless_node_satisfies(
    tree, treeid,
    |vn| matches!( &vn . kind,
                    ViewnodeKind::Vognode (Vognode::Unrestricted (_))),
    "maybe_add_subscribeeFolder_branch: expected UnrestrictedVognode" ) ?;
  { let is_writeProtected : bool =
      read_at_node_in_tree(
        tree, treeid,
        |vn| matches!( &vn . kind,
                        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
                        if t . is_writeProtected () ))
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_writeProtected { return Ok(( )); } }
  { // Pre-existing SubscribeeFolder children are reconciled by view completion (complete_nodes_in_level_order), which dispatches to 'reconcile_subscribeeFolder_children'.
    if unique_non_vognode_child_of_viewnode (
      tree, treeid,
      &ViewnodeKind::Folder (Folder::Subscribee) )? . is_some ()
    { return Ok (( )); }}
  let subscriber_pid : ID =
    read_at_node_in_tree (
      tree, treeid,
      |vn| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => Some ( graph . pid_of (&t . skgid)
                    . unwrap_or_else (|| t . skgid . clone ()) ),
        _ => None } )
    . map_err( |e| -> Box<dyn Error> { e . into() } ) ?
    . ok_or ("maybe_add_subscribeeFolder_branch: expected UnrestrictedVognode") ?;
  let subscribee_skgids : Vec<ID> = graph . outbound_skgids_for_relation_gated (
    &subscriber_pid, NodeRelation::SubscribesTo, skgrepo_restriction );
  let subscribee_skgids : Vec<ID> =
    // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): restricted
    // subscribees are omitted at de novo creation (no retained
    // members exist yet); a folder left empty by this is not created.
    omit_restricted_members (
      subscribee_skgids,
      skgrepo_restriction,
      |skgid : &ID| graph . pid_and_skgrepo (skgid)
                 . map ( |(_pid, src)| src )
                 . or_else ( || home_from_disk (skgid, config) ));
  if subscribee_skgids . is_empty () && ! force_create_when_empty {
    // Skip because it would be empty.
    return Ok (( )); }

  let hidden_outside_content : HashSet < ID > = {
    // hidden IDs that are outside all subscribee content. Read
    // relRepo-gated from the captured graph: hides and memberships
    // recorded outside the skgrepo restriction must not shape this derived folder.
    let r_hides : HashSet < ID > =
          graph . outbound_pids_for_relation_gated (
            & subscriber_pid,
            NodeRelation::HidesFromSubs,
            skgrepo_restriction )
          . into_iter () . collect ();
    let all_subscribee_content : HashSet < ID > =
          subscribee_skgids . iter ()
          . flat_map ( |skgid| graph . outbound_pids_for_relation_gated (
            skgid, NodeRelation::Contains, skgrepo_restriction ))
          . collect ();
    r_hides . iter ()
      . filter ( | skgid | ! all_subscribee_content . contains (skgid) )
      . cloned () . collect () };

  let subscribee_folder_treeid : NodeId =
    insert_non_vognode_as_child ( tree, treeid,
      ViewnodeKind::Folder (Folder::Subscribee), true ) ?;
  { let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
      build_initial_render_child_data (
        &subscribee_skgids, graph, config ) ?;
    reconcile_partnerFolder_children_against_goal_list (
      tree, subscribee_folder_treeid,
      &Folder::Subscribee,
      &FolderGoalItem::live_members (&goal), &data, graph ) ?; }
  if ! hidden_outside_content . is_empty () {
    // HiddenOutsideOfSubscribeeFolder presents last, if it exists.
    let hidden_outside_folder_treeid : NodeId =
      insert_non_vognode_as_child (
        tree, subscribee_folder_treeid,
        ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee),
        false ) ?;
    with_node_mut ( tree, hidden_outside_folder_treeid,
      |mut n| {
        // A new hidden folder begins folded. The
        // stamp moves to the members at the folder's own BFS visit
        // ('fold_members_of_newborn_folder').
        n . value () . foldAndFocus . folded = true; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    let hidden_outside_skgids : Vec<ID> =
      hidden_outside_content . into_iter () . collect ();
    let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
      build_initial_render_child_data (
        &hidden_outside_skgids, graph, config ) ?;
    reconcile_partnerFolder_children_against_goal_list (
      tree, hidden_outside_folder_treeid,
      &Folder::HiddenOutsideOfSubscribee,
      &FolderGoalItem::live_members (&goal), &data, graph ) ?; }
  Ok (( )) }

/// Add the PartnerFolders that an editable node shows without an explicit
/// Folder view request.  Only a nonempty SubscribeeFolder is part of that
/// initial presentation; the other relation folders are available through
/// `(viewRequests (folder ...))` when their heralds indicate they are useful.
pub fn maybe_add_default_partnerFolder_branches (
  tree               : &mut Tree<Viewnode>,
  treeid             : NodeId,
  graph              : &InRustGraph,
  config             : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < (), Box<dyn Error> > {
  error_unless_node_satisfies(
    tree, treeid,
    |vn| matches!( &vn . kind,
                    ViewnodeKind::Vognode (Vognode::Unrestricted (_) )),
    "maybe_add_default_partnerFolder_branches: expected UnrestrictedVognode" ) ?;
  { let is_writeProtected : bool =
      read_at_node_in_tree(
        tree, treeid,
        |vn| matches!( &vn . kind,
                        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
                        if t . is_writeProtected () ) )
      . map_err( |e| -> Box<dyn Error> { e . into() } ) ?;
    if is_writeProtected { return Ok(( )); } }
  maybe_add_subscribeeFolder_branch (
    tree, treeid, graph, config, skgrepo_restriction, false ) ?;
  Ok (( )) }

/// Add a generated PartnerFolder for `treeid` if it would
/// have visible members.
/// Conditions determining the 'maybe' are commented.
/// 'force_create_when_empty' overrides the empty-skip: a Folder view-request
/// uses it to materialize the editable folder (Overridden) as an empty
/// "add here" surface even when the relation has no members. (Only ever
/// passed 'true' for an editable folder; an empty write-protected folder would just
/// be pruned again.)
pub fn maybe_add_one_partnerFolder (
  tree    : &mut Tree<Viewnode>,
  treeid                  : NodeId,
  kind    : &Folder,
  config  : &SkgConfig,
  graph   : &InRustGraph,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  force_create_when_empty : bool,
) -> Result < (), Box<dyn Error> > {
  if unique_non_vognode_child_of_viewnode (
      tree, treeid, &ViewnodeKind::Folder (kind . clone ())
    )? . is_some ()
  { // There already is one. Don't create a new one.
    return Ok (( )); }
  let viewparent_pid : ID =
    read_at_node_in_tree (
      tree, treeid,
      |vn| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => Ok ( t . skgid . clone () ),
        _ => Err ("expected UnrestrictedVognode" . to_string ()), } )
    .map_err( |e| -> Box<dyn Error> { e . into() } ) ??;
  let Some (member_role) = kind . relation_member_role ()
    // The two Hidden*SubscribeeFolders lack this, hence end here.
    else { return Ok (( )); };
  let recorder_role =
    member_role . opposite_role ();
  let member_skgids : Vec<ID> =
    // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): restricted members
    // are omitted, so a folder whose members are all restricted is not
    // created at all (de novo creation has no retained members).
    omit_restricted_members (
      graph . other_member_pids_gated (
        &viewparent_pid, recorder_role, skgrepo_restriction ),
      skgrepo_restriction,
      |skgid : &ID| graph . pid_and_skgrepo (skgid)
                 . map ( |(_pid, src)| src )
                 . or_else ( || home_from_disk (skgid, config) ));
  if member_skgids . is_empty () && ! force_create_when_empty {
    // It would be empty, so don't create it.
    return Ok (( )); }
  let folder_treeid : NodeId =
    insert_non_vognode_as_child (
      tree, treeid, ViewnodeKind::Folder (kind . clone ()), true ) ?;
  let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
    build_initial_render_child_data (
      &member_skgids, graph, config ) ?;
  reconcile_partnerFolder_children_against_goal_list (
    tree, folder_treeid, kind, &FolderGoalItem::live_members (&goal), &data, graph ) ?;
  Ok (( )) }

/// If this node is a Subscribee,
/// and the corresponding subscriber hides any of its content,
/// then prepend a HiddenInSubscribeeFolder to hold those hidden nodes.
/// The subscriber is the Subscribee's grandparent:
///   subscriber -> SubscribeeFolder -> Subscribee
pub fn maybe_add_hiddenInSubscribeeFolder_branch (
  tree               : &mut Tree<Viewnode>,
  subscribee_treeid  : NodeId,
  graph              : &InRustGraph,
  config             : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < (), Box<dyn Error> > {
  if ! type_and_parent_type_consistent_with_subscribee (
    tree, subscribee_treeid )?
  { return Err ( "maybe_add_hiddenInSubscribeeFolder_branch called on non-subscribee" . into ( )); }
  if unique_non_vognode_child_of_viewnode (
      // Pre-existing HiddenIn folders are instead reconciled by the rerender pipeline's 'reconcile_hiddenIn_folders', which dispatches to 'reconcile_hiddenInSubscribeeFolder_children'.
       tree, subscribee_treeid,
       &ViewnodeKind::Folder (Folder::HiddenInSubscribee)
     )? . is_some ()
  { return Ok (( )); }
  let ( subscribee_pid, subscriber_pid ) : ( ID, ID ) =
    pid_for_subscribee_and_its_subscriber_grandparent (
      tree, subscribee_treeid, graph, config ) ?;
  let ( _visible, hidden_in_content )
    : ( HashSet < ID >, HashSet < ID > )
    = {
      // relRepo-gated from the captured graph: hides and memberships
      // outside the skgrepo restriction must not shape this derived folder.
      {
        let subscriber_hides : HashSet<ID> =
          graph . outbound_pids_for_relation_gated (
            & subscriber_pid,
            NodeRelation::HidesFromSubs,
            skgrepo_restriction )
          . into_iter () . collect ();
        let subscribee_content : HashSet<ID> =
          graph . outbound_pids_for_relation_gated (
            & subscribee_pid, NodeRelation::Contains,
            skgrepo_restriction )
          . into_iter () . collect ();
        ( subscribee_content . iter ()
            . filter ( |skgid| ! subscriber_hides . contains (skgid) )
            . cloned () . collect (),
          subscribee_content . iter ()
            . filter ( |skgid| subscriber_hides . contains (skgid) )
            . cloned () . collect () ) } };
  if hidden_in_content . is_empty () {
    // Skip because it would be empty.
    return Ok (( )); }
  let hidden_in_skgids : Vec<ID> =
    hidden_in_content . into_iter () . collect ();
  let hidden_folder_treeid : NodeId =
    insert_non_vognode_as_child (
      tree, subscribee_treeid,
      ViewnodeKind::Folder (Folder::HiddenInSubscribee),
      true ) ?;
  with_node_mut ( tree, hidden_folder_treeid,
    |mut n| {
      // A new hidden folder begins folded. The
      // stamp moves to the members at the folder's own BFS visit
      // ('fold_members_of_newborn_folder').
      n . value () . foldAndFocus . folded = true; } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  let (goal, data) : (Vec<ID>, HashMap<ID, ChildData>) =
    build_initial_render_child_data (
      &hidden_in_skgids, graph, config ) ?;
  reconcile_partnerFolder_children_against_goal_list (
    tree, hidden_folder_treeid,
    &Folder::HiddenInSubscribee,
    &FolderGoalItem::live_members (&goal), &data, graph ) ?;
  Ok (( )) }
