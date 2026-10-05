use crate::dbs::in_rust_graph::InRustGraph;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::complete::contents::clobberWriteProtectedViewnode;
use crate::to_org::complete::partner_folder::maybe_add_default_partnerFolder_branches;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::misc::{ID, SkgConfig, SkgRepoName, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::tree::generic::{read_at_node_in_tree, read_at_ancestor_in_tree, with_node_mut};
use crate::types::tree::viewnode_graphnode::write_at_unrestrictedVognode_in_tree;
use crate::types::viewnode::ViewRequest;
use crate::types::viewnode::{ Birth, Viewnode, ViewnodeKind, Editability, AffectsParent, UnrestrictedVognode, mk_editable_viewnode, mk_unknown_viewnode };
use crate::types::viewnode::{Vognode, Phantom};
use crate::types::tree::forest::{ViewForest, tree_forest_root_skgids};

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
use ego_tree::iter::Edge;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::time;


/// Whether an ID's editable occurrence is Final (claimed by a
/// editable view request, EVR) or merely Tentative (an ordinary
/// saved/completed editable). A EVR clobbers a Tentative occurrence
/// but defers to a Final one (TODO/DONE/local-view-update/plan_v2.org §5.2). The EVR cascade (TODO/DONE/local-view-update/plan_v2.org §5.3) is
/// not yet implemented, so today no second EVR ever reaches an
/// already-Final ID (validation forbids two user EVRs per ID); the
/// defer-to-Final branch is therefore correct-but-dormant until cascade
/// lands. NodeId is the occurrence's position in the viewforest.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Finalizable {
  Tentative (NodeId),
  Final     (NodeId),
}

impl Finalizable {
  pub fn treeid (&self) -> NodeId {
    match self { Finalizable::Tentative (n) | Finalizable::Final (n) => *n } }
  pub fn is_final (&self) -> bool {
    matches! (self, Finalizable::Final (_)) }
}

/// Tracks which IDs have been rendered as editable and where.
/// - Key: the ID that was visited
/// - Value: Finalizable (its NodeId in the viewforest, tagged Tentative/Final)
///
/// Uses:
/// - prevent duplicate editable expansions
/// - locate the conflict when an earlier editable view
///   conflicts with a new editable view request
pub type EditableMap =
  HashMap < ID, Finalizable >;


// ======================================================
// Fetching, building and modifying Graphnodes and Viewnodes
// ======================================================

/// Fetch a Graphnode from the in-Rust graph or disk. Resolves id→(pid,skgrepo)
/// via 'pid_and_repo_from_id', then reads. Makes a Viewnode with
/// validated title. Returns both.
/// Returns Ok(None) when SKGID has no record anywhere -- not as a
/// primary pid or extra_id in the captured graph.
/// Callers should substitute a PhantomUnknown. A real
/// query error still surfaces as Err.
pub fn graphnode_and_viewnode_from_skgid (
  graph  : &InRustGraph,
  config : &SkgConfig,
  skgid  : &ID,
) -> Result < Option<( Graphnode, Viewnode )>, Box<dyn Error> > {
  let resolved : Option<(ID, SkgRepoName)> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "graphnode_and_viewnode_from_id" ). entered();
      graph . pid_and_skgrepo (skgid) };
  match resolved {
    None => Ok (None),
    Some ((pid_resolved, skgrepo)) =>
      match graphnode_and_viewnode_from_pid_and_skgrepo (
        graph, config, &pid_resolved, &skgrepo ) {
        Ok (node) => Ok ( Some (node) ),
        // A graph member can be dangling when its file is absent. This is not
        // a render failure: callers turn `None` into Unknown.
        Err (e) if e . downcast_ref::<io::Error> ()
          . is_some_and (|io_error| io_error . kind () == io::ErrorKind::NotFound)
          => Ok (None),
        Err (e) => Err (e), } } }

/// Fetch a Graphnode from the in-Rust graph or disk given PID and skgrepo.
/// Makes an Viewnode with validated title. Returns both.
pub(super) fn graphnode_and_viewnode_from_pid_and_skgrepo (
  graph   : &InRustGraph,
  config  : &SkgConfig,
  pid     : &ID,
  skgrepo : &SkgRepoName,
) -> Result < ( Graphnode, Viewnode ), Box<dyn Error> > {
  let graphnode : Graphnode =
    graphnode_graphFirst_by_pid_and_skgrepo (
      graph, config, pid, skgrepo )?;
  let title : String = graphnode . title . replace ( '\n', " " );
  if title . is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "Graphnode with ID {} has an empty title",
                 pid ), )) ); }
  let viewnode : Viewnode = mk_editable_viewnode (
    pid . clone (),
    skgrepo . clone (),
    title,
    graphnode . body . clone () );
  Ok (( graphnode, viewnode )) }

/// Set node to write-protected,
/// and reset title and skgrepo.
pub(super) fn makeWriteProtectedAndClobber (
  tree    : &mut Tree<Viewnode>,
  treeid : NodeId,
  graph   : &crate::dbs::in_rust_graph::InRustGraph,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  write_at_unrestrictedVognode_in_tree (
    tree, treeid,
    |t| { t . editability = Editability::WriteProtected; }
    ) . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  clobberWriteProtectedViewnode ( tree, treeid, graph, config ) ?;
  Ok (( )) }

/// This function's callers add a pristine, out-of-context
/// (graphnode, viewnode) pair to the tree.
/// Integrating the pair into the tree requires more work
/// (and later will require even more, probably),
/// which this function does:
/// - handle repeats, cycles and the visited map
/// - build a subscribee branch if needed
pub fn complete_branch_minus_content (
  tree     : &mut Tree<Viewnode>,
  treeid : NodeId,
  visited  : &mut EditableMap,
  graph    : &crate::dbs::in_rust_graph::InRustGraph,
  config   : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  detect_and_mark_cycle_v1 ( tree, treeid ) ?;
  make_writeProtected_if_repeat_then_extend_editable_map (
    tree, treeid, visited ) ?;
  if unrestrictedVognode_in_tree_is_writeProtected ( tree, treeid )?
  { clobberWriteProtectedViewnode (
      tree, treeid, graph, config ) ?; }
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "maybe_add_default_partnerFolder_branches" ). entered();
    maybe_add_default_partnerFolder_branches (
      tree, treeid, graph, config, skgrepo_restriction,
      // This birth path runs outside the diff-aware BFS (search
      // results, ancestry attachment, stubs); diff-mode folder
      // existence is decided at each node's completion visit, which
      // passes the real diffs.
      &None ) } ?;
  Ok (( )) }

/// Does only what it says -- in particular,
/// does not clobber the node after making it write-protected.
///
/// The two jobs in the name cannot be unbundled --
/// we have to interleave extending the editable_map
/// with marking things write-protected, because the editable_map
/// is how we know whether to mark something write-protected.
pub fn make_writeProtected_if_repeat_then_extend_editable_map (
  tree    : &mut Tree<Viewnode>,
  treeid : NodeId,
  editableMap  : &mut EditableMap,
) -> Result<(), Box<dyn Error>> {
  let pid : ID = // Will error if node is a Non-vognode.
    get_skgid_from_viewnode_at ( tree, treeid ) ?;
  let is_writeProtected : bool =
    write_at_unrestrictedVognode_in_tree (
      tree, treeid,
      |t| { if editableMap . contains_key (&pid)
               { // It's a repeat, so make it write-protected.
                 t . editability = Editability::WriteProtected; }
             t . is_writeProtected () } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  if !is_writeProtected {
    // Ordinary completed/saved editable -> Tentative (TODO/DONE/local-view-update/plan_v2.org §5.2).
    editableMap . insert ( pid, Finalizable::Tentative (treeid) ); }
  Ok (( )) }

/// Check if the node's PID appears in its ancestors,
/// and if so, mark viewData.cycle = true.
pub fn detect_and_mark_cycle_v1 (
  tree    : &mut Tree<Viewnode>,
  treeid : NodeId,
) -> Result<(), Box<dyn Error>> {
  let is_cycle : bool = {
    let pid : ID = get_skgid_from_viewnode_at ( tree, treeid ) ?;
    is_ancestor_skgid ( tree, treeid, &pid ) ? };
  write_at_unrestrictedVognode_in_tree
    ( tree, treeid,
      |t| { t . viewStats . cycle = is_cycle; } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }


// ==============================================
// Reading and manipulating trees, esp. via IDs
// ==============================================

/// Create a viewforest containing just view roots,
/// and complete each via build_node_branch_minus_content.
pub fn stub_viewforest_from_root_skgids (
  root_skgids : &[ID],
  graph    : &crate::dbs::in_rust_graph::InRustGraph,
  config   : &SkgConfig,
  visited  : &mut EditableMap,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < ViewForest, Box<dyn Error> > {
  let mut viewforest : ViewForest =
    ViewForest::new ();
  let viewforest_root_treeid : NodeId =
    viewforest . internal_root_skgid ();
  for root_skgid in root_skgids {
    build_node_branch_minus_content (
      Some ( (viewforest . as_internal_tree_mut (),
              viewforest_root_treeid) ),
      root_skgid, graph, config, visited, skgrepo_restriction
    ) ?; }
  Ok (viewforest) }

/// Mark forest-root UnrestrictedVognodes as having no parent in the view.
pub fn mark_view_roots_parent_na (
  viewforest : &mut Tree<Viewnode>,
) {
  let root_skgids : Vec<NodeId> =
    tree_forest_root_skgids (viewforest);
  for root_skgid in root_skgids {
    let mut node_mut : NodeMut<Viewnode> =
      viewforest . get_mut (root_skgid) . unwrap ();
    let vn : &mut Viewnode = node_mut . value ();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
      = vn . kind
      { t . affectsParent = AffectsParent::NA; }}}

/// Walk the view and correct any UnrestrictedVognode whose metadata claims a
/// relationship to its parent that the actual graph doesn't support.
/// Silently clears stale birth claims or flips stale membership claims
/// to non-member;
/// the rendered herald then no longer misleads.
///
/// Three kinds of claim are checked:
/// - 'Birth::RoleGraft(role)' on child C with UnrestrictedVognode parent P:
///   claim is "C plays 'role' toward P" (e.g. CONTAINER -> C contains
///   P; MENTIONER -> C's body/title links to P). Verified against the
///   in-Rust graph via 'relation_membership_is_real', keyed by the role.
/// - 'AffectsParent::True' on child C with WRITE_PROTECTED UnrestrictedVognode
///   parent P: claim is "C is part of P's content". Verified
///   against P's 'contains' in the in-Rust graph. Editable parents are
///   skipped because the save just redefined their 'contains' to
///   include this very child; the check would always pass.
///
/// Both sides of every comparison resolve through 'graph.pid_of'
/// so extra_id aliasing (typically a nodeMerge side-effect) doesn't
/// produce false mismatches.
///
/// Forest roots have no UnrestrictedVognode parent and therefore
/// do not fall through this check. Also relies on the save pipeline's
/// invariant that the prepared graph swap-in has updated the in-Rust-graph
/// graph before the rerender pass runs (see
/// 'update_views_after_save').
pub fn validate_affectsParent_relationships (
  viewforest : &mut Tree<Viewnode>,
  graph  : &InRustGraph,
) {
  // Collect correction targets in a read-only first pass so the
  // &mut Tree write phase doesn't need simultaneous read access.
  let mut to_independent : Vec<NodeId> = Vec::new ();
    // these will be marked affectsParent = independent
  let mut to_affected : Vec<NodeId> = Vec::new ();
    // these will be marked affectsParent = affected
  let mut to_unremarkable : Vec<NodeId> = Vec::new ();
    // these will be marked birth = unremarkable
  for edge in viewforest . root () . traverse () {
    if let Edge::Open (child_ref) = edge {
      let child_tn : &UnrestrictedVognode =
        match & child_ref . value () . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => t,
          _ => continue };
      let parent_ref : NodeRef<Viewnode> = match child_ref . parent () {
        Some (p) => p, None => continue };
      let parent_tn : &UnrestrictedVognode =
        match & parent_ref . value () . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => t,
          // A non-UnrestrictedVognode parent (BufferRoot, a property or folder, Deleted, DeadViewnode) is not a legitimate subject for any of these relational claims; skip without correcting.
          _ => continue };
      if child_tn . affectsParent == AffectsParent::NA {
        // The child was a root, and the user gave it a parent, so let the parent contain it.
        to_affected . push ( child_ref . id () );
        continue; }
      let affects_parent_claim_ok : bool = match child_tn . affectsParent {
        AffectsParent::True => {
          if parent_tn . is_writeProtected () {
            child_contained_by_parent (graph, &child_tn . skgid, &parent_tn . skgid)
          } else { // The editable parent *defines* content, so cannot be incorrect.
            true }}
        AffectsParent::False => true,
        AffectsParent::NA => false, };
      if ! affects_parent_claim_ok { to_independent . push ( child_ref . id () ); }
      let birth_claim_ok : bool = match child_tn . birth {
        Birth::RoleGraft (role) => {
          // The child plays 'role' toward the parent (the origin).
          // Resolve both through pid_of so extra_id aliasing (a
          // nodeMerge side-effect) doesn't produce a false mismatch.
          let parent_pid : ID =
            graph . pid_of (&parent_tn . skgid)
              . unwrap_or_else ( || parent_tn . skgid . clone () );
          let child_pid : ID =
            graph . pid_of (&child_tn . skgid)
              . unwrap_or_else ( || child_tn . skgid . clone () );
          graph . relation_membership_is_real (
            &parent_pid, &child_pid, role ) }
        Birth::Unremarkable => true, };
      if ! birth_claim_ok { to_unremarkable . push ( child_ref . id () ); }}}
  for skgid in to_independent {
    let mut node_mut : NodeMut<Viewnode> =
      viewforest . get_mut (skgid) . unwrap ();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
      = node_mut . value () . kind
    { t . affectsParent = AffectsParent::False; } }
  for skgid in to_affected {
    let mut node_mut : NodeMut<Viewnode> =
      viewforest . get_mut (skgid) . unwrap ();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
      = node_mut . value () . kind
    { t . affectsParent = AffectsParent::True; } }
  for skgid in to_unremarkable {
    let mut node_mut : NodeMut<Viewnode> =
      viewforest . get_mut (skgid) . unwrap ();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
      = node_mut . value () . kind
    { t . birth = Birth::Unremarkable; } } }

/// Jeff's invariant (TODO/DONE/local-view-update/progress.org §11 thread): a *non-dead generalized orphan*
/// must have AffectsParent=False. An Unrestricted node whose PARENT is a
/// non-container -- a Diff phantom, a Deleted, or a DeadViewnode -- is exactly
/// that: it survives (is not itself dead) but its container is gone, so its
/// member claim (that it is part of that parent's membership) cannot hold.
/// Demote it to non-member so it renders as its own graph-contains root rather
/// than claiming to affect a parent that no longer contains anything.
///
/// Scope, deliberately narrow:
/// - PARENT is Diff phantom / Deleted / DeadViewnode -> demote a member child.
/// - PARENT is a Folder (PropertyFolder / PartnerFolder): the child is a legitimate folder
///   MEMBER; membership is correct -> leave. (A folder whose own ancestry broke is
///   deadened to DeadViewnode first, and then THIS pass catches its members.)
/// - PARENT is an Unrestricted vognode: handled by validate_affectsParent_relationships.
/// - PARENT is BufferRoot: the child is a forest root, handled by
///   mark_view_roots_parent_na.
/// Belt-and-suspenders: most cases are already demoted during the BFS
/// (mark_erroneous_content_children_as_indep for content children;
/// dispose_orphaned_folder_child for a deadened folder's members). This final pass
/// GUARANTEES the invariant for any survivor those miss (e.g. a removedHere
/// phantom's content children), in both the post-save and de-novo paths. Purely
/// structural -- no graph read.
pub fn mark_orphans_under_dead_parents_false (
  viewforest : &mut Tree<Viewnode>,
) {
  let mut targets : Vec<NodeId> = Vec::new ();
  for edge in viewforest . root () . traverse () {
    if let Edge::Open (child_ref) = edge {
      let is_affected_normal : bool =
        matches! ( & child_ref . value () . kind,
          ViewnodeKind::Vognode (Vognode::Unrestricted (t))
            if t . affectsParent == AffectsParent::True );
      if ! is_affected_normal { continue; }
      let affects_parent_non_container : bool =
        child_ref . parent () . map_or ( false, |p|
          matches! ( & p . value () . kind,
            ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (_)))
              | ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (_)))
              | ViewnodeKind::DeadViewnode ) );
      if affects_parent_non_container { targets . push ( child_ref . id () ); }}}
  for skgid in targets {
    let mut node_mut : NodeMut<Viewnode> =
      viewforest . get_mut (skgid) . unwrap ();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
      = node_mut . value () . kind
    { t . affectsParent = AffectsParent::False; } } }

/// Does 'parent's 'contains' list include 'child' (modulo extra_id
/// aliasing on either side)? The inverse of "child contains parent".
fn child_contained_by_parent (
  graph        : &InRustGraph,
  child_skgid  : &ID,
  parent_skgid : &ID,
) -> bool {
  let child_pid : ID =
    graph . pid_of (child_skgid) . unwrap_or_else ( || child_skgid . clone () );
  let parent_node : &GraphnodeInRust =
    match graph . get (parent_skgid) {
      Some (n) => n,
      None     => return false };
  members_of (& parent_node . contains) . iter () . any ( |x|
    graph . pid_of (x) . unwrap_or_else ( || x . clone () )
      == child_pid ) }

pub fn skgids_that_can_have_graphnodestats (
  tree : &Tree<Viewnode>,
) -> Vec < ID > {
  let mut skgids : Vec < ID > = Vec::new ();
  for edge in tree . root () . traverse () {
    if let Edge::Open (node_ref) = edge {
      if let Some (vid) =
        node_ref . value () . unrestricted_or_diff_phantom_skgid ()
      { skgids . push ( vid . clone () ); }}}
  skgids }

/// Check if `target_skgid` appears in the ancestor path of `treeid`.
/// Used for cycle detection.
fn is_ancestor_skgid (
  tree          : &Tree<Viewnode>,
  origin_treeid : NodeId,
  target_skgid  : &ID,
) -> Result<bool, Box<dyn Error>> {
  read_at_node_in_tree(
    tree, origin_treeid,
    |_| ())
    . map_err(|_| "is_ancestor_id: NodeId not in tree")?;
  for generation in 1.. {
    match read_at_ancestor_in_tree(
      tree, origin_treeid, generation,
      |viewnode| viewnode . skgid_if_current_graphnode () . cloned () )
    { Ok(Some (skgid)) if &skgid == target_skgid
        => return Ok (true),
      Ok (_) => continue,
      Err (_) => return Ok (false), }}
  unreachable!() }

/// Errors if the node is a phantom, a non-vognode, or not found.
pub fn get_skgid_from_viewnode_at (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Result < ID, Box<dyn Error> > {
  let node_kind: ViewnodeKind =
    read_at_node_in_tree (
      tree, treeid, |viewnode| viewnode . kind . clone() )?;
  match node_kind {
    ViewnodeKind::Vognode (v) if v . is_current_graphnode ()
      => v . skgid () . cloned () . ok_or_else (
           || "get_skgid_from_viewnode_at: restricted vognode has no id"
              . into () ),
    _ => Err ( "get_skgid_from_viewnode_at: caller must pass a non-phantom vognode" . into() ),
  }}



/// Builds a node from disk, place it in a tree,
/// complete the branch it implies except for 'content' descendents,
/// and return the NodeId of the branch root.
/// - If tree_and_parent is None, creates a new tree (not returned).
/// - If tree_and_parent is Some, appends to the existing tree.
pub fn build_node_branch_minus_content (
  tree_and_parent    : Option<(&mut Tree<Viewnode>, NodeId)>, // if modifying an existing tree, attach as a child here
  skgid              : &ID, // what to fetch
  graph              : &crate::dbs::in_rust_graph::InRustGraph,
  config             : &SkgConfig,
  visited            : &mut EditableMap,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < NodeId, Box<dyn Error> > {
  let t0 : time::Instant = time::Instant::now();
  let result : Result < NodeId, Box<dyn Error> > =
    match tree_and_parent {
      Some ( (tree, parent_treeid) ) => {
        let lookup : Option<(Graphnode, Viewnode)> =
          graphnode_and_viewnode_from_skgid (
            graph, config, skgid ) ?;
        match lookup {
          Some ((_nc, viewnode)) => {
            let child_treeid : NodeId = // Add Viewnode to tree
              with_node_mut (
                tree, parent_treeid,
                ( |mut parent_mut|
                  parent_mut . append (viewnode) . id () ))
              . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
            complete_branch_minus_content (
              tree, child_treeid, visited,
              graph, config, skgrepo_restriction ) ?;
            Ok (child_treeid) },
          None => { // Uknown node. Add it, don't 'complete' it.
            let viewnode : Viewnode =
              mk_unknown_viewnode (skgid . clone ());
            let child_treeid : NodeId =
              with_node_mut (
                tree, parent_treeid,
                ( |mut parent_mut|
                  parent_mut . append (viewnode) . id () ))
              . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
            Ok (child_treeid) }} },
      None => {
        let lookup : Option<(Graphnode, Viewnode)> =
          graphnode_and_viewnode_from_skgid (
            graph, config, skgid ) ?;
        match lookup {
          Some ((_nc, viewnode)) => {
            let mut tree : Tree<Viewnode> =
              Tree::new (viewnode);
            let root_treeid : NodeId = tree . root () . id ();
            complete_branch_minus_content (
              &mut tree, root_treeid, visited,
              graph, config, skgrepo_restriction ) ?;
            Ok (root_treeid) },
          None => { // A singleton tree with a PhantomUnknown.
            let viewnode : Viewnode =
              mk_unknown_viewnode (skgid . clone ());
            let tree : Tree<Viewnode> = Tree::new (viewnode);
            Ok (tree . root () . id ()) }} }, };
  tracing::info!("{}: {:.3}s",
                 format! ("build_node_branch_minus_content({})", skgid),
                 t0 . elapsed () . as_secs_f64());
  result }

// ==============================================
// Reading from Graphnodes and Viewnodes, esp. in trees
// ==============================================

/// Check if an UnrestrictedVognode is write-protected.
/// Errs if given a Non-vognode.
pub fn unrestrictedVognode_in_tree_is_writeProtected (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_kind: ViewnodeKind =
    read_at_node_in_tree ( tree, treeid,
                           |viewnode| viewnode . kind . clone() )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  match node_kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => Ok (t . is_writeProtected ()),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p))) => Ok (p . is_writeProtected ()),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (_)))
      | ViewnodeKind::Vognode (Vognode::Restricted (_))
      | ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (_))) => Ok (false),
    _                                                => Err (
      "is_writeProtected: caller must pass a vognode" . into( )),
  }}

/// Collect all child tree NodeIds from a node.
/// Returns an error if the node is not found.
pub fn collect_child_treeids (
  tree    : &Tree<Viewnode>,
  treeid : NodeId,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let node_ref : NodeRef < Viewnode > =
    tree . get (treeid)
    . ok_or ("collect_child_treeids: NodeId not in tree") ?;
  Ok ( node_ref . children () . map ( |c| c . id () ) . collect () ) }


// ==============================================
// 'remove_completed_view_request'
// ==============================================

/// Log any error and remove the request from the node.
/// Does *not* verify that the request was completed;
/// that's just the only situation in which it would be used.
pub(super) fn remove_completed_view_request<T> (
  tree         : &mut Tree<T>,
  treeid : NodeId,
  view_request : ViewRequest,
  error_msg    : &str,
  errors       : &mut Vec < String >,
  result       : Result < (), Box<dyn Error> >,
) -> Result < (), Box<dyn Error> >
where T: AsMut<Viewnode>,
{
  if let Err (e) = result {
    errors . push ( format! ( "{}: {}", error_msg, e )); }
  let mut node_mut : NodeMut<T> =
    tree . get_mut (treeid) . ok_or ("remove_completed_view_request: node not found") ?;
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
    = &mut node_mut . value () . as_mut () . kind
    { t . view_requests . remove (&view_request); }
  Ok (()) }

#[cfg(test)]
#[path = "../../tests/unit/to_org_util.rs"]
mod validate_affectsParent_relationships_tests;
