// MANUAL RECURSION:
// This uses manual async recursions, rather than calls to
// `do_everywhere_in_tree_dfs`, because some dispatch targets
// are async, and `do_everywhere_in_tree_dfs` takes a sync
// `FnMut` closure which cannot ``.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::expand::editable::{ apply_editable_draw_rule, apply_writeProtect_request, DrawOutcome};
use crate::to_org::util::EditableMap;
use crate::types::env::RuntimeGeneration;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs_readonly, read_at_node_in_tree, read_at_ancestor_in_tree};
use crate::to_org::complete::partner_folder::maybe_add_default_partnerFolder_branches;
use crate::update_buffer::ancestry::{ folder_is_generalized_orphan, deaden_generalized_orphan_folder, is_folder_kind};
use crate::update_buffer::util::detach_viewnode_transferring_focus;
use crate::update_buffer::warnings::CompletionWarning;
use crate::to_org::render::changes::{
  HistoricalPhantomVisit, align_content_members_with_straddle,
  deleted_phantom_becomes_historical, process_historical_phantom_visit,
  process_unrestrictedVognode_changes};
use crate::graph_straddle::types::GraphStraddle;
use crate::pcsvf::DiffViewSource;
use crate::types::tree::viewnode_graphnode::{
  pid_and_skgrepo_from_viewnode_at, write_at_unrestrictedVognode_in_tree};
use crate::types::viewnode::{Viewnode, ViewnodeKind, PartnerFolder, ViewRequest, Editability};
use crate::types::viewnode::{Vognode, Phantom, PropertyFolder};
use super::reconcile::hiddeninsubscribee_folder::reconcile_hiddenInSubscribeeFolder_children;
use super::reconcile::hiddenoutsideof_subscribeefolder::reconcile_hiddenoutsideSubscribeeFolder_children;
use super::reconcile::partner_folder::{
  PartnerFolderGoalSource, reconcile_partnerFolder_children};
use super::reconcile::subscribee_folder::reconcile_subscribeeFolder_children;
use super::reconcile::content::{
  expand_true_content_at_unrestrictedVognode, mutate_unrestrictedVognode_to_deletednode};

use ego_tree::{Tree, NodeId};
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::sync::Arc;

pub(super) struct CompletionContext<'a> {
  pub(super) editable_map                         : &'a mut EditableMap,
  pub(super) runtime                        : &'a RuntimeGeneration,
  pub(super) graph_snap                     : &'a Arc<InRustGraph>,
  pub(super) errors                         : &'a mut Vec<String>,
  pub(super) deleted_by_this_save_pids      : &'a HashSet<ID>,
  pub(super) deleted_by_this_save_extra_ids : &'a HashMap<ID, HashSet<ID>>,
  pub(super) skgrepo_restriction            : Option<&'a SkgrepoRestriction>,
  /// TODO/DONE/local-view-update/plan_v2.org §5.5 per-buffer node limit: the remaining budget of *new* Viewnodes
  /// the ordinary update pass may create. Initialized once per rerender to
  /// `config.initial_node_limit` and decremented as content children are
  /// created; when it reaches 0 no further new content is drawn (and the
  /// cascade stops).
  pub(super) node_budget                    : usize,
  /// Phase 8 (TODO/DONE/local-view-update/plan_v2.org §13): true only for a DE-NOVO (initial) render driven through
  /// view completion. When set, each fresh editable node gets its default
  /// PartnerFolders created at its visit, the way
  /// build_node_branch_minus_content does at node birth.
  /// FALSE for post-save rerender, where existing PartnerFolders round-trip
  /// from the saved buffer. A node made editable by a request still gets its
  /// defaults independently of this flag.
  pub(super) create_partnerFolders_for_fresh_nodes : bool,
  /// Replace an already-present raw content member with its newly visible
  /// overrider. This is true only for the view whose edit created the override;
  /// ordinary collateral rerenders retain their existing raw positions.
  pub(super) substitute_existing_content_overrides : bool,
  /// Some only when this completion serves the view the user just
  /// saved: write-protected PartnerFolder reconcilers report their repairs
  /// here, and the save response surfaces them as warnings. None for
  /// de-novo renders, collateral rerenders and rerender-all, whose
  /// repairs do not correspond to edits the user just made.
  pub(super) warning_sink : Option<&'a mut Vec<CompletionWarning>>,
  /// Some only when completing a total diff view ('server/pcsvf.rs').
  /// Each unrestricted vognode's visit then ends with
  /// 'process_unrestrictedVognode_changes', each historical phantom's
  /// draws its old content ('process_historical_phantom_visit'),
  /// the partner folders draw the union of the straddle, the node
  /// budget bounds only the expansions an editable-view request makes
  /// outside U, and no default folders are created.
  pub(super) diffView_source : Option<DiffViewSource<'a>>,
  /// The viewnodes the viewforest held before this completion began.
  /// In a total diff view, a fold state is set only on a viewnode
  /// not among them.
  pub(super) nodes_before_completion : &'a HashSet<NodeId>,
  /// The editable unrestricted vognodes among them. In a total diff
  /// view, a content member outside U is write-protected only when it
  /// is not among these: nothing writable is write-protected again.
  pub(super) editable_before_completion : &'a HashSet<NodeId>,
}

pub(super) fn complete_viewforest (
  viewforest : &mut Tree<Viewnode>,
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = viewforest . root () . id ();
  // The single TODO/DONE/local-view-update/plan_v2.org §3 level-order BFS: each node's visit
  // (dispatch_node_update) does ALL of its own update -- content
  // reconcile, the TODO/DONE/local-view-update/plan_v2.org §5.2 draw rule + TODO/DONE/local-view-update/plan_v2.org §5.3 cascade for editable-view
  // requests, the other view requests, ensuring an editable
  // subscribee's HiddenInSubscribeeFolder, and per-folder reconciliation. Folders and
  // content children created during a node's visit are reached later in the
  // same BFS.
  complete_nodes_in_level_order (
    viewforest, &[root_treeid], context ) ?;
  prune_self_deletable_when_empty (viewforest) ?;
  Ok(( )) }

/// Complete the subtrees at 'roots', appended to a viewforest that
/// 'context' has already completed, in level order among themselves,
/// continuing that completion's editable map and node budget.
pub(super) fn complete_appended_roots (
  viewforest : &mut Tree<Viewnode>,
  roots      : &[NodeId],
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  complete_nodes_in_level_order (viewforest, roots, context) ?;
  prune_self_deletable_when_empty (viewforest) ?;
  Ok(( )) }

/// Visit every node in *level order* (BFS), completing each at its own visit.
///
/// This is the core of view completion: a single top-down level-order traversal
/// (TODO/DONE/local-view-update/plan_v2.org §3) in which each node, when visited, may create viewdescendants
/// (content children, subscribee/relation members) that are then visited later
/// in the same traversal. A FIFO queue (not depth-first recursion) is what makes
/// the TODO/DONE/local-view-update/plan_v2.org §5.5 node-limit and TODO/DONE/local-view-update/plan_v2.org §5.2 draw-rule semantics order-correct: the first
/// occurrence of a duplicate id reached in BFS order -- not in depth-first order
/// -- is the one that wins, and the Finalizable draw rule makes that safe. A
/// node's expansion depends only on its ancestors and the graph, never on
/// siblings or descendants, so it can be processed the moment it is dequeued.
fn complete_nodes_in_level_order (
  tree             : &mut Tree<Viewnode>,
  starting_treeids : &[NodeId],
  context          : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let mut queue : VecDeque<NodeId> =
    starting_treeids . iter () . copied () . collect ();
  while let Some (treeid) = queue . pop_front () {
    dispatch_node_update (tree, treeid, context) ?;
    // Enqueue the node's *current* children -- including any this visit
    // just created -- so they are completed after every node already in
    // their level. (See 'MANUAL RECURSION' comment at top of file: the
    // dispatch is async, so we hand-roll the traversal.)
    if let Some (node) = tree . get (treeid) {
      for child in node . children () {
        queue . push_back (child . id ()); }}}
  Ok(( )) }

/// One BFS visit: dispatch on (kind, parent-kind) to the node's update rule
/// (TODO/DONE/local-view-update/plan_v2.org §3/§4). Folders are reconciled at their own visit (the BFS reaches a
/// folder after its Unrestricted parent created it).
fn dispatch_node_update (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewnodeKind =
    tree . get (treeid) . unwrap () . value () . kind . clone ();
  // Death-leafward (TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5): before reconciling any
  // folder, self-check its required ancestry. If it is a generalized orphan (some
  // required ancestor is the wrong viewnode kind -- full chain, not just the
  // parent), deaden it and dispose its children instead of reconciling, so the
  // reconcile never reads a since-deleted ancestor's missing Graphnode.
  // The BFS visits every folder, and the check walks the folder's whole required
  // ancestry, so a folder whose grandparent (not just its parent) died is deadened.
  if is_folder_kind (&kind)
    && folder_is_generalized_orphan (tree, treeid) ? {
    deaden_generalized_orphan_folder (tree, treeid) ?;
    return Ok (( )); }
  match &kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (_)) =>
      visit_normal_node (tree, treeid, context) ?,
    ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)
      if context . diffView_source . is_none () =>
      // A folder fills its members WHOLE and is budget-neutral (TODO/DONE/local-view-update/plan_v2.org §5.5): the owning
      // vognode already spent its 1 budget unit when it expanded, so drawing all
      // the members here costs nothing more and never truncates a group.
      reconcile_subscribeeFolder_children (
        treeid, tree, context . runtime,
        context . deleted_by_this_save_extra_ids,
        context . skgrepo_restriction ) ?,
    ViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee) =>
      reconcile_hiddenInSubscribeeFolder_children (
        treeid, tree, context . runtime,
        context . deleted_by_this_save_extra_ids,
        context . skgrepo_restriction,
        context . warning_sink . as_deref_mut () ) ?,
    ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
      reconcile_hiddenoutsideSubscribeeFolder_children (
        treeid, tree, context . runtime,
        context . deleted_by_this_save_extra_ids,
        context . skgrepo_restriction,
        context . warning_sink . as_deref_mut () ) ?,
    ViewnodeKind::PartnerFolder (role)
      // This arm serves one FolderPolicy::EditableSet folder (Overridden;
      // Subscribee has its own completer above) and every
      // FolderPolicy::WriteProtectedSet folder. The reconciler branches on
      // 'PartnerFolder::policy' where their treatment differs.
      if role . relation_member_role () . is_some () =>
      // In a total diff view (diffView_source set) the folder draws the
      // union of the straddle; the Subscribee folder comes here too.
      reconcile_partnerFolder_children (
        treeid, tree, *role,
        match context . diffView_source {
          Some (drawing) => PartnerFolderGoalSource::GraphStraddle (drawing . straddle),
          None        => PartnerFolderGoalSource::LiveGraph, },
        context . runtime, context . graph_snap,
        context . deleted_by_this_save_extra_ids,
        context . skgrepo_restriction,
        context . warning_sink . as_deref_mut () ) ?,
    // In a total diff view, the node's visit fills its alias, ID and
    // flags folders from the graph straddle
    // ('process_unrestrictedVognode_changes').
    ViewnodeKind::PropertyFolder (_) if context . diffView_source . is_some () => {},
    ViewnodeKind::PropertyFolder (PropertyFolder::Alias) =>
      super::reconcile::aliasfolder::reconcile_aliasFolder_children (
        tree, treeid, &context . runtime . graph,
        &context . runtime . config ) ?,
    ViewnodeKind::PropertyFolder (PropertyFolder::ID) =>
      super::reconcile::id_folder::reconcile_idFolder_children (
        treeid, tree, &context . runtime . graph,
        &context . runtime . config ) ?,
    ViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. }) =>
      super::reconcile::flags_folder::reconcile_flags_folder_children (
        tree, treeid, &context . runtime . graph,
        &context . runtime . config ) ?,
    // In a total diff view, a historical phantom where content stands
    // draws its old content.
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_)))
      if context . diffView_source . is_some () => {
      let straddle : &GraphStraddle = context . diffView_source . unwrap () . straddle;
      if process_historical_phantom_visit (tree, treeid, straddle) ?
        == HistoricalPhantomVisit::RootIsLive {
        visit_normal_node (tree, treeid, context) ?; }},
    _ => {
      // No-op for: Restricted (an anonymous restricted vognode -- it carries no
      // identity, and flipping it to a "DELETED" placeholder would leak that
      // a hidden node vanished; it just lingers until the next full
      // rerender drops it), Unknown (unresolvable-id placeholder),
      // Deleted, Historical, DeadViewnode, Property leaves and BufferRoot.
      // The prune sweep handles empty/dead nodes.
    } }
  Ok(( )) }

/// The Unrestricted-node visit (TODO/DONE/local-view-update/plan_v2.org §6.1/§6.2): settle the Finalizable state
/// for a editable-view request *before* drawing content (so a Final node
/// can cascade), reconcile content, run the remaining (non-Editable) view
/// requests, ensure an editable subscribee's HiddenInSubscribeeFolder, and
/// finally (in a total diff view) draw this node's changes. The BFS reaches
/// every folder/child this creates and reconciles it in turn.
fn visit_normal_node (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  // Same-save deletion is an occurrence-wide state transition, not content
  // reconciliation. Do it before editable arbitration, budget handling, or
  // write-protected refresh: all of those paths may consult the post-save
  // graph, where the node is necessarily absent. This also ensures every
  // write-protected image becomes Deleted rather than Unknown.
  let (pid, skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_viewnode_at (
      tree, treeid, "visit_normal_node deletion preflight" ) ?;
  if context . deleted_by_this_save_pids . contains (&pid) {
    mutate_unrestrictedVognode_to_deletednode (
      tree, treeid, &pid, &skgrepo ) ?;
    if let Some (drawing) = context . diffView_source {
      // In a total diff view, a node HEAD has is drawn as history.
      if deleted_phantom_becomes_historical (tree, treeid, drawing . straddle) ? {
        process_historical_phantom_visit (tree, treeid, drawing . straddle) ?; }}
    return Ok (( )); }
  // A write-protect request is applied before anything else reads
  // the occurrence's editability, so that what follows -- the budget,
  // the draw rule, the expansion -- sees it write-protected.
  apply_writeProtect_request (
    tree, treeid, &context . runtime . graph,
    &context . runtime . config, context . editable_map ) ?;
  let had_editableViewRequest : bool =
    read_at_node_in_tree ( tree, treeid,
      |vn : &Viewnode| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          t . view_requests . contains (& ViewRequest::Editable),
        _ => false } ) ?;
  let in_u : bool = // of the total diff view being drawn
    context . diffView_source . is_some_and (
      |drawing| drawing . sets () . u . contains (&pid) );
  let editableViewRequest_applies : bool = had_editableViewRequest;
  let budgeted : bool = // In a total diff view the budget bounds only the expansions requests make outside U.
    context . diffView_source . is_none ()
    || ( editableViewRequest_applies && ! in_u );
  let mut settled : bool = false; // TODO/DONE/local-view-update/plan_v2.org §5.2 draw rule already ran
  let mut cascade : bool = false; // node is Final -> hand EVRs to children
  // TODO/DONE/local-view-update/plan_v2.org §5.5: the budget counts vognode *expansions* (each costs 1, charged in
  // expand_true_content_at_unrestrictedVognode); once it hits 0 every later vognode is left
  // write-protected -- a visible, collapsed headline. We never truncate a group
  // mid-way (whole groups already drawn keep all their members); we only stop
  // STARTING new expansions. EXCEPTION: a view root (child of the BufferRoot) is
  // the node the user explicitly opened, so it always expands -- never truncated.
  // (The ancestor read is short-circuited to only run when the budget is spent.)
  if context . node_budget == 0
    && budgeted
    && ! read_at_ancestor_in_tree ( tree, treeid, 1,
           |vn : &Viewnode| matches! ( &vn . kind, ViewnodeKind::BufferRoot ) )
         . unwrap_or (false) {
    // Budget spent and this is not a view root: draw it write-protected and expand
    // nothing under it; strip any EVR so it is not treated as Final. The content
    // engine (settled) then clobbers+returns.
    write_at_unrestrictedVognode_in_tree (
      tree, treeid,
      |t| { t . view_requests . remove (& ViewRequest::Editable);
            t . editability = Editability::WriteProtected; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    settled = true;
  } else if editableViewRequest_applies {
    match apply_editable_draw_rule (
      tree, treeid, &context . runtime . graph,
      &context . runtime . config, context . editable_map ) ? {
      DrawOutcome::Deferred => {
        // Deferred to an existing Final occurrence: the node is now
        // write-protected; the content engine (settled) will clobber+return.
        settled = true; }
      DrawOutcome::MadeFinal => {
        settled = true; cascade = true; } } }
  if let Some (drawing) = context . diffView_source {
    align_content_members_with_straddle (tree, treeid, drawing . straddle) ?; }
  expand_true_content_at_unrestrictedVognode (
    treeid, tree, context . editable_map,
    &context . runtime . config, context . graph_snap,
    context . deleted_by_this_save_extra_ids,
    context . skgrepo_restriction,
    settled, cascade,
    if budgeted { Some (&mut context . node_budget) } else { None },
    context . diffView_source . is_none (), // substitution is off in the total diff view: it shows raw graph facts
    context . substitute_existing_content_overrides ) ?;
  // The steps below apply only while the node is still an Unrestricted vognode.
  let still_normal : bool =
    read_at_node_in_tree ( tree, treeid,
      |vn : &Viewnode| matches! ( &vn . kind,
        ViewnodeKind::Vognode (Vognode::Unrestricted (_)) ) ) ?;
  if ! still_normal { return Ok (( )); }
  // Create the default folders when this node is first presented as
  // editable: on a de-novo render, or when a editable-view request has
  // just expanded an existing occurrence.  Folder members are included;
  // hid*/overrid*-as-such nodes get the same defaults as ordinary nodes, and
  // the subscribee-specific HiddenIn folder is added by the pass below.
  // A total diff view draws no default folders.
  if ( context . create_partnerFolders_for_fresh_nodes || editableViewRequest_applies )
    && context . diffView_source . is_none () {
    maybe_add_default_partnerFolder_branches (
      tree, treeid, &context . runtime . graph,
      &context . runtime . config,
      context . skgrepo_restriction ) ?; }
  // Remaining view requests (Aliases / Containerward / Mentionerward); the
  // Editable request was already consumed by apply_editable_draw_rule.
  super::reconcile::view_requests::execute_unrestrictedVognode_view_requests (
    treeid, tree, &context . runtime . graph,
    &context . runtime . config,
    context . errors, context . skgrepo_restriction ) ?;
  // Ensure an editable subscribee's HiddenInSubscribeeFolder exists; the BFS
  // reconciles it on reaching it. (A default folder, so not in a
  // total diff view.)
  if context . diffView_source . is_none () {
    super::reconcile::view_requests::ensure_hiddenInFolder_under_editable_subscribee (
      tree, treeid, &context . runtime . graph,
      &context . runtime . config,
      context . skgrepo_restriction ) ?; }
  if let Some (drawing) = context . diffView_source {
    // Tag the content, draw what was removed, and add the change
    // folders, markers and properties.
    process_unrestrictedVognode_changes (
      tree, treeid, drawing,
      ! context . nodes_before_completion . contains (&treeid),
      context . editable_before_completion ) ?; }
  Ok(( )) }

/// Whether a node of this kind is *self-deletable when empty* (TODO/DONE/local-view-update/plan_v2.org §3.4): the
/// single postorder prune sweep removes it once it is childless. This is the
/// one place self-deletion lives -- per-kind reconcilers no longer detach
/// themselves when empty.
/// - DeadViewnode and a childless Vognode::Deleted (TODO/DONE/local-view-update/plan_v2.org §6.6).
/// - the write-protected PartnerFolders Subscriber/Overrider/Hider/Hidden/
///   Container/Mentioner/AcquiredBy/Acquired (a graph
///   relationship the user cannot edit from this side, so an emptied one is
///   just noise) -- but NOT Overridden (the editable interface for adding
///   overrides, preserved when empty like AliasFolder).
/// - HiddenInSubscribeeFolder / HiddenOutsideOfSubscribeeFolder.
/// - PropertyFolder::ID (largely moot -- an IDFolder always holds at least the PID).
/// PRESERVED when empty (so NOT here): PartnerFolder(Subscribee), PartnerFolder
/// (Overridden), PropertyFolder(Alias) -- each an *editable interface onto the origin*
/// that the user must be able to refill, removed only by hand (TODO/DONE/local-view-update/plan_v2.org §3.4 exception).
fn is_self_deletable_when_empty (
  kind : &ViewnodeKind,
) -> bool {
  matches! ( kind,
    ViewnodeKind::DeadViewnode
    | ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (_)))
    | ViewnodeKind::PartnerFolder (PartnerFolder::Subscriber)
    | ViewnodeKind::PartnerFolder (PartnerFolder::Overrider)
    | ViewnodeKind::PartnerFolder (PartnerFolder::Hider)
    | ViewnodeKind::PartnerFolder (PartnerFolder::Hidden)
    | ViewnodeKind::PartnerFolder (PartnerFolder::Container)
    | ViewnodeKind::PartnerFolder (PartnerFolder::Mentioner)
    | ViewnodeKind::PartnerFolder (PartnerFolder::AcquiredBy)
    | ViewnodeKind::PartnerFolder (PartnerFolder::Acquired)
    | ViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)
    | ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee)
    | ViewnodeKind::PropertyFolder (PropertyFolder::ID) ) }

/// The TODO/DONE/local-view-update/plan_v2.org §3.4 postorder prune sweep -- the *one* place self-deletion happens.
/// Removes, bottom-up, every childless `is_self_deletable_when_empty` node:
/// deadened folders whose members all died, a TODO/DONE/local-view-update/plan_v2.org §6.6 childless Deleted, and empty
/// write-protected relation / Hidden* / ID folders. Postorder so a chain (e.g.
/// Dead -> Deleted, or a folder emptied by the removal of its last member)
/// collapses completely in one pass. Focus is transferred to the surviving
/// parent if the removed node held it. Never prunes a *view root* (child of the
/// invisible BufferRoot): the user should still see a deleted root.
fn prune_self_deletable_when_empty (
  tree : &mut Tree<Viewnode>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_treeids (
      tree,
      |vn| is_self_deletable_when_empty (&vn . kind) ) ?;
  for treeid in nodes {
    let node = match tree . get (treeid) {
      Some (node) => node,
      None => continue };
    let has_children : bool =
      node . children () . next () . is_some ();
    let affects_parent_buffer_root : bool =
      node . parent ()
      . map ( |p| matches! ( &p . value () . kind,
                             ViewnodeKind::BufferRoot ) )
      . unwrap_or (false);
    if ! has_children && ! affects_parent_buffer_root {
      detach_viewnode_transferring_focus (tree, treeid) ?; }
  }
  Ok (( )) }

fn collect_matching_treeids<Predicate> (
  tree      : &Tree<Viewnode>,
  predicate : Predicate,
) -> Result<Vec<NodeId>, Box<dyn Error>>
where Predicate : Fn (&Viewnode) -> bool {
  let root_treeid : NodeId = tree . root () . id ();
  let mut result : Vec<NodeId> = Vec::new ();
  do_everywhere_in_tree_dfs_readonly (
    tree, root_treeid,
    false, // postorder: a chain (Dead -> Deleted, or a folder emptied by the
           // removal of its last child) collapses bottom-up in one sweep.
    &mut |node| {
      if predicate (node . value ()) {
        result . push (node . id ()); }
      Ok (( )) } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (result) }
