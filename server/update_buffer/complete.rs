// MANUAL RECURSION:
// This uses manual async recursions, rather than calls to
// `do_everywhere_in_tree_dfs`, because some dispatch targets
// are async, and `do_everywhere_in_tree_dfs` takes a sync
// `FnMut` closure which cannot ``.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::GnidPresence;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::extend::editable::{ apply_editability_rule, apply_writeProtect_request, EditabilityOutcome};
use crate::to_org::util::{EditableMap, make_member_children_nonMembers};
use crate::types::env::RuntimeGeneration;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs_readonly, read_at_node_in_tree, read_at_ancestor_in_tree};
use crate::update_buffer::ancestry::{ folder_is_generalized_orphan, entombstone_generalized_orphan_folder, is_folder_kind};
use crate::update_buffer::util::detach_viewnode_transferring_focus;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::views_state::DiffViewBufferKind;
use crate::diff_view::visit::{
  DiffViewVisit, NormalNodeUpdate,
  diffView_visit_after_extension, diffView_visit_before_extension};
use super::completion_mode::CompletionMode;
use crate::types::tree::viewnode_graphnode::{
  pid_and_skgrepo_from_viewnode_at, write_at_unrestrictedVognode_in_tree};
use crate::types::viewnode::{Viewnode, ViewnodeKind, Folder, ViewRequest};
use crate::types::viewnode::{Vognode};
use super::reconcile::hiddeninsubscribee_folder::reconcile_hiddenInSubscribeeFolder_children;
use super::reconcile::hiddenoutsideof_subscribeefolder::reconcile_hiddenoutsideSubscribeeFolder_children;
use super::reconcile::partner_folder::{
  reconcile_partnerFolder_children};
use super::reconcile::content::extend_true_content_at_unrestrictedVognode;

use ego_tree::{Tree, NodeId};
use std::collections::{HashSet, VecDeque};
use std::error::Error;
use std::sync::{Arc, LazyLock};

pub(crate) struct CompletionContext<'a> {
  pub(super) editable_map                         : &'a mut EditableMap,
  pub(super) runtime                        : &'a RuntimeGeneration,
  pub(super) graph_snap                     : &'a Arc<InRustGraph>,
  pub(super) errors                         : &'a mut Vec<String>,
  pub(super) skgrepo_restriction            : Option<&'a SkgrepoRestriction>,
  /// The node budget (see the module comment of 'to_org/extend/editable.rs'): how
  /// many more vognodes may extend their content. Initialized once per
  /// completion to `config.initial_node_limit` and decremented by one at
  /// each extension of an editable vognode (a folder fills for free, and
  /// a content list is extended whole); when it reaches 0 no later
  /// vognode other than a root extends (and the cascade stops).
  pub(super) node_budget                    : usize,
  /// The completion mode: a content view, or a buffer of the diff view
  /// over a straddle ('docs/diff-view.org', "Completion over the
  /// straddle"). Read at the calls of 'diffView_visit_before_extension'
  /// and 'diffView_visit_after_extension' ('server/diff_view/visit.rs',
  /// whose module comment says how diff-view mode changes completion),
  /// and wherever a visit asks what the mode allows.
  pub(super) mode : CompletionMode<'a>,
  /// The viewnodes the viewforest held before this completion began.
  /// In a diff view, a fold state is set only on a viewnode
  /// not among them.
  pub(super) nodes_before_completion : &'a HashSet<NodeId>,
  /// The editable unrestricted vognodes among them. In a diff
  /// view, a content member outside U is write-protected only when it
  /// is not among these: nothing writable is write-protected again.
  pub(super) editable_before_completion : &'a HashSet<NodeId>,
}

/// What a completion that shows nothing that was there before is
/// given for the sets it reads.
static NO_NODES                  : LazyLock<HashSet<NodeId>> = LazyLock::new (HashSet::new);

impl<'a> CompletionContext<'a> {
  /// The first rendering of a buffer of the diff view
  /// ('complete_viewforest_with_graph_straddle'), the buffer of kind
  /// 'diffView_buffer_kind' over 'straddle'. It
  /// is never restricted by skgrepo (the diff view requires the set
  /// 'all'), creates no default partner folders, substitutes no
  /// overrider. Every viewnode of a first
  /// rendering is created by it.
  pub(crate) fn for_diffView (
    runtime      : &'a RuntimeGeneration,
    graph_snap   : &'a Arc<InRustGraph>,
    editable_map : &'a mut EditableMap,
    errors       : &'a mut Vec<String>,
    straddle     : &'a GraphStraddle,
    diffView_buffer_kind : DiffViewBufferKind,
  ) -> CompletionContext<'a> {
    CompletionContext {
      editable_map, runtime, graph_snap, errors,
      skgrepo_restriction            : None,
      node_budget                    : runtime . config . initial_node_limit,
      mode                           : CompletionMode::DiffView {
        straddle, buffer_kind : diffView_buffer_kind },
      nodes_before_completion        : &NO_NODES,
      editable_before_completion     : &NO_NODES, } }

  /// A rerender of a buffer of the diff view ('rerender_view'):
  /// after a save, for rerender-all, or after a commit, over the
  /// straddle of 'mode'. It has the settings of 'for_diffView', and
  /// carries what the view held before.
  pub(super) fn for_rerender (
    runtime                        : &'a RuntimeGeneration,
    graph_snap                     : &'a Arc<InRustGraph>,
    editable_map                   : &'a mut EditableMap,
    errors                         : &'a mut Vec<String>,
    mode                           : CompletionMode<'a>,
    nodes_before_completion        : &'a HashSet<NodeId>,
    editable_before_completion     : &'a HashSet<NodeId>,
  ) -> CompletionContext<'a> {
    let CompletionMode::DiffView { straddle, buffer_kind } = mode
      else { panic! ("for_rerender: a content view is refreshed, not completed") };
    CompletionContext {
      nodes_before_completion,
      editable_before_completion,
      .. CompletionContext::for_diffView (
           runtime, graph_snap, editable_map, errors, straddle, buffer_kind ) } }
}

pub(crate) fn complete_viewforest (
  viewforest : &mut Tree<Viewnode>,
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let root_treeid : NodeId = viewforest . root () . id ();
  // The single level-order BFS: each node's visit
  // (dispatch_node_update) does all of its own update -- content
  // reconcile, the editability rule and the cascade for editable-view
  // requests, the other view requests, ensuring an editable
  // subscribee's HiddenInSubscribeeFolder, and per-folder reconciliation. Folders and
  // content children created during a node's visit are reached later in the
  // same BFS.
  complete_nodes_in_level_order (
    viewforest, &[root_treeid], context ) ?;
  prune_childless_tombstones (viewforest) ?;
  Ok(( )) }

/// Complete the subtrees at 'roots', appended to a viewforest that
/// 'context' has already completed, in level order among themselves,
/// continuing that completion's editable map and node budget.
pub(crate) fn complete_appended_roots (
  viewforest : &mut Tree<Viewnode>,
  roots      : &[NodeId],
  context    : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  complete_nodes_in_level_order (viewforest, roots, context) ?;
  prune_childless_tombstones (viewforest) ?;
  Ok(( )) }

/// Visit every node in *level order* (BFS), completing each at its own visit.
///
/// This is the core of view completion: a single top-down level-order traversal
/// in which each node, when visited, may create viewdescendants
/// (content children, subscribee/relation members) that are then visited later
/// in the same traversal. A FIFO queue (not depth-first recursion) is what makes
/// the node budget and the editability rule (see 'to_org/extend/editable.rs') order-correct: the first
/// occurrence of a duplicate id reached in BFS order -- not in depth-first order
/// -- is the one that wins, and the Finalizable editability rule makes that safe. A
/// node's extension depends only on its ancestors and the graph, never on
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
/// Folders are reconciled at their own visit (the BFS reaches a
/// folder after its Unrestricted parent created it).
fn dispatch_node_update (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  context : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let kind : ViewnodeKind =
    tree . get (treeid) . unwrap () . value () . kind . clone ();
  // Death-leafward ('update_buffer/ancestry.rs'): before reconciling any
  // folder, self-check its required ancestry. If it is a generalized orphan (some
  // required ancestor is the wrong viewnode kind -- full chain, not just the
  // parent), entombstone it and dispose its children instead of reconciling, so the
  // reconcile never reads a since-deleted ancestor's missing Graphnode.
  // The BFS visits every folder, and the check walks the folder's whole required
  // ancestry, so a folder whose grandparent (not just its parent) died is entombstoned.
  if is_folder_kind (&kind)
    && folder_is_generalized_orphan (tree, treeid) ? {
    entombstone_generalized_orphan_folder (tree, treeid) ?;
    return Ok (( )); }
  let visit_as_unrestrictedVognode : bool =
    match diffView_visit_before_extension (
      tree, treeid, &kind, context . mode ) ? {
      DiffViewVisit::Done => return Ok (( )),
      DiffViewVisit::Complete { visit_as_unrestrictedVognode } =>
        visit_as_unrestrictedVognode, };
  match &kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (_)) =>
      visit_normal_node (tree, treeid, context) ?,
    ViewnodeKind::Folder (Folder::HiddenInSubscribee) =>
      reconcile_hiddenInSubscribeeFolder_children (
        treeid, tree, context . runtime,
        context . skgrepo_restriction ) ?,
    ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) =>
      reconcile_hiddenoutsideSubscribeeFolder_children (
        treeid, tree, context . runtime,
        context . skgrepo_restriction ) ?,
    ViewnodeKind::Folder (role)
      // This arm serves every folder that reads one relation role: the
      // editable one (Overridden; Subscribee has its own completer
      // above) and the write-protected ones. The reconciler branches
      // on 'Folder::goal_is_ordered' and 'Folder::is_editable' where
      // their treatment differs.
      if role . relation_member_role () . is_some () =>
      // In a diff view the folder shows the union of the straddle;
      // the Subscribee folder comes here too.
      reconcile_partnerFolder_children (
        treeid, tree, role,
        context . mode,
        context . runtime ) ?,
    // In a diff view, 'diffView_visit_before_extension' has finished
    // a changed property folder's visit: the node's visit filled it
    // from the graph straddle. Any other is refreshed here.
    ViewnodeKind::Folder (Folder::Alias) =>
      super::reconcile::aliasfolder::reconcile_aliasFolder_children (
        tree, treeid, &context . runtime . graph,
        &context . runtime . config ) ?,
    ViewnodeKind::Folder (Folder::ID) =>
      super::reconcile::id_folder::reconcile_idFolder_children (
        treeid, tree, &context . runtime . graph,
        &context . runtime . config ) ?,
    ViewnodeKind::Folder (Folder::Flags { .. }) =>
      super::reconcile::flags_folder::reconcile_flags_folder_children (
        tree, treeid, &context . runtime . graph,
        &context . runtime . config ) ?,
    // In a diff view, a ghost where content stands has
    // extended its old content; a root whose node is back is now an
    // editable vognode.
    ViewnodeKind::Vognode (Vognode::Ghost (_))
      if visit_as_unrestrictedVognode =>
      visit_normal_node (tree, treeid, context) ?,
    _ => {
      // No-op for: Unknown (unresolvable-id placeholder), a ghost the
      // diff view finished (or one outside a diff view), Tombstone,
      // Property leaves and BufferRoot. The prune sweep handles
      // empty/dead nodes.
    } }
  // Over the straddle, the diff view's folder reconciler reconciles
  // its members' containerward chains itself.
  Ok(( )) }

/// The Unrestricted-node visit: settle the Finalizable state
/// for a editable-view request *before* extending content (so a Final node
/// can cascade), reconcile content, run the remaining (non-Editable) view
/// requests, and finally extend this node's changes. The BFS reaches
/// every folder/child this creates and reconciles it in turn.
fn visit_normal_node (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  context  : &mut CompletionContext<'_>,
) -> Result<(), Box<dyn Error>> {
  let update : NormalNodeUpdate =
    update_normal_node (tree, treeid, context) ?;
  diffView_visit_after_extension (
    tree, treeid, update, context . mode,
    context . nodes_before_completion,
    context . editable_before_completion ) }

/// Everything 'visit_normal_node' does before the diff view's part
/// at its end.
fn update_normal_node (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  context  : &mut CompletionContext<'_>,
) -> Result<NormalNodeUpdate, Box<dyn Error>> {
  // An occurrence whose node vanished (a save deleted it) is an
  // occurrence-wide state transition, not content reconciliation. Do it
  // before editable arbitration, budget handling, or write-protected
  // refresh: all of those paths may consult the graph, where the node
  // is absent. It becomes a tombstone, which keeps what stands under
  // it; in a diff view, a node HEAD has becomes its ghost instead.
  let (pid, _skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_viewnode_at (
      tree, treeid, "visit_normal_node deletion preflight" ) ?;
  if context . graph_snap . gnid_presence_gated (&pid, None)
     == GnidPresence::Absent {
    if ! context . mode . vanished_occurrence_becomes_ghost (tree, treeid) ? {
      tree . get_mut (treeid) . unwrap () . value ()
        . replace_kind (ViewnodeKind::Tombstone); }
    return Ok (NormalNodeUpdate::NodeVanished); }
  // A write-protect request is applied before anything else reads
  // the occurrence's editability, so that what follows -- the budget,
  // the editability rule, the extension -- sees it write-protected.
  apply_writeProtect_request (
    tree, treeid, &context . runtime . graph,
    &context . runtime . config, context . editable_map ) ?;
  let had_editableViewRequest : bool =
    read_at_node_in_tree ( tree, treeid,
      |vn : &Viewnode| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          t . view_requests . contains (& ViewRequest::Editable),
        _ => false } ) ?;
  let budgeted : bool = // whether this extension is charged to the node budget
    context . mode . charges_node_budget (&pid, had_editableViewRequest);
  let mut settled : bool = false; // the editability rule already ran
  let mut cascade : bool = false; // node is Final -> hand EVRs to children
  // The node budget counts vognode *extensions* (each costs 1, charged in
  // extend_true_content_at_unrestrictedVognode); once it hits 0 every later vognode is left
  // write-protected -- a visible, collapsed headline. We never truncate a group
  // mid-way (whole groups already extended keep all their members); we only stop
  // starting new extensions. EXCEPTION: a view root (child of the BufferRoot) is
  // the node the user explicitly opened, so it always extends -- never truncated.
  // (The ancestor read is short-circuited to only run when the budget is spent.)
  if context . node_budget == 0
    && budgeted
    && ! read_at_ancestor_in_tree ( tree, treeid, 1,
           |vn : &Viewnode| matches! ( &vn . kind, ViewnodeKind::BufferRoot ) )
         . unwrap_or (false) {
    // Budget spent and this is not a view root: leave it write-protected and extend
    // nothing under it; strip any EVR so it is not treated as Final. The content
    // engine (settled) then clobbers+returns. A write-protected occurrence holds
    // no member, so the children it already shows become non-members.
    write_at_unrestrictedVognode_in_tree (
      tree, treeid,
      |t| { t . write_protect (); } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
    make_member_children_nonMembers ( tree, treeid ) ?;
    settled = true;
  } else if had_editableViewRequest {
    match apply_editability_rule (
      tree, treeid, &context . runtime . graph,
      &context . runtime . config, context . editable_map ) ? {
      EditabilityOutcome::ClaimedElsewhere => {
        // Yielded to an existing Final occurrence: the node is now
        // write-protected; the content engine (settled) will clobber+return.
        settled = true; }
      EditabilityOutcome::MadeFinal => {
        settled = true; cascade = true; } } }
  context . mode . align_content_members_before_extension (tree, treeid) ?;
  extend_true_content_at_unrestrictedVognode (
    treeid, tree, context . editable_map,
    &context . runtime . config, context . graph_snap,
    context . skgrepo_restriction,
    settled, cascade,
    // The diff view substitutes no overrider: it shows raw graph facts.
    if budgeted { Some (&mut context . node_budget) } else { None } ) ?;
  // The steps below apply only while the node is still an Unrestricted vognode.
  let still_normal : bool =
    read_at_node_in_tree ( tree, treeid,
      |vn : &Viewnode| matches! ( &vn . kind,
        ViewnodeKind::Vognode (Vognode::Unrestricted (_)) ) ) ?;
  if ! still_normal { return Ok (NormalNodeUpdate::NoLongerUnrestricted); }
  // A diff view creates no default folders.
  // Remaining view requests (Aliases / Containerward / Mentionerward); the
  // Editable request was already consumed by apply_editability_rule.
  super::reconcile::view_requests::execute_unrestrictedVognode_view_requests (
    treeid, tree, &context . runtime . graph,
    &context . runtime . config,
    context . errors, context . skgrepo_restriction ) ?;
  Ok (NormalNodeUpdate::Updated) }

/// Whether a folder of this kind, once childless, has nothing left to
/// show, so that the prune sweep removes it as it removes a childless
/// tombstone ('prune_childless_tombstones'):
/// - the write-protected PartnerFolders Subscriber/Overrider/Hider/Hidden/
///   Container/Mentioner/AcquiredBy/Acquired (a graph
///   relationship the user cannot edit from this side, so an emptied one is
///   just noise) -- but not Overridden (the editable interface for adding
///   overrides, preserved when empty like AliasFolder).
/// - HiddenInSubscribeeFolder / HiddenOutsideOfSubscribeeFolder.
/// - Folder::ID (largely moot -- an IDFolder always holds at least the PID).
/// Preserved when empty (so not here): PartnerFolder(Subscribee), PartnerFolder
/// (Overridden), PropertyFolder(Alias) -- each an *editable interface onto the origin*
/// that the user must be able to refill, removed only by hand.
fn folder_is_entombstoned_when_empty (
  kind : &ViewnodeKind,
) -> bool {
  matches! ( kind,
    ViewnodeKind::Folder (Folder::Subscriber)
    | ViewnodeKind::Folder (Folder::Overrider)
    | ViewnodeKind::Folder (Folder::Hider)
    | ViewnodeKind::Folder (Folder::Hidden)
    | ViewnodeKind::Folder (Folder::Container)
    | ViewnodeKind::Folder (Folder::Mentioner)
    | ViewnodeKind::Folder (Folder::AcquiredBy)
    | ViewnodeKind::Folder (Folder::Acquired)
    | ViewnodeKind::Folder (Folder::HiddenInSubscribee)
    | ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee)
    | ViewnodeKind::Folder (Folder::ID) ) }

/// The postorder prune sweep -- the *one* place self-deletion happens.
/// It removes every childless tombstone, a view root included, so a
/// buffer may end with no roots, and every childless folder that
/// 'folder_is_entombstoned_when_empty' admits. Postorder, so that a chain (a tombstone under a
/// tombstone, or a folder emptied by the removal of its last member)
/// collapses completely in one pass. Focus is transferred to the
/// surviving parent if the removed node held it.
pub fn prune_childless_tombstones (
  tree : &mut Tree<Viewnode>,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<NodeId> =
    collect_matching_treeids (
      tree,
      |vn| matches! ( vn . kind, ViewnodeKind::Tombstone )
           || folder_is_entombstoned_when_empty (&vn . kind) ) ?;
  for treeid in nodes {
    let Some (node) = tree . get (treeid) else { continue };
    if node . children () . next () . is_some () { continue; }
    detach_viewnode_transferring_focus (tree, treeid) ?; }
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
    false, // postorder: a chain (a tombstone under a tombstone, or a folder emptied
           // by the removal of its last child) collapses bottom-up in one sweep.
    &mut |node| {
      if predicate (node . value ()) {
        result . push (node . id ()); }
      Ok (( )) } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (result) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/completion_context.rs"]
mod tests;
