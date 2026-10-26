use crate::dbs::in_rust_graph::InRustGraph;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::complete::contents::clobberWriteProtectedViewnode;
use crate::to_org::complete::partner_folder::maybe_add_default_partnerFolder_branches;
use crate::to_org::extend::editable::take_claim_from_tentative_nonMember;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::misc::{ID, SkgConfig, SkgrepoName, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::tree::generic::{read_at_node_in_tree, read_at_ancestor_in_tree, with_node_mut};
use crate::types::tree::viewnode_graphnode::write_at_unrestrictedVognode_in_tree;
use crate::types::viewnode::ViewRequest;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, Editability, UnrestrictedVognode, mk_editable_viewnode, mk_unknown_viewnode };
use crate::types::viewnode::{Vognode};
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
/// but defers to a Final one: the editability rule, described with the editable
/// map, the cascade and the node budget in the module comment of
/// 'server/to_org/extend/editable.rs'. A user writes at most one EVR
/// per ID (validation forbids two), so a second EVR reaching a Final
/// ID is one the cascade passed down. NodeId is the occurrence's
/// position in the viewforest.
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

/// The editable map: tracks which IDs have been rendered as editable
/// and where (see the module comment of 'server/to_org/extend/editable.rs').
/// - Key: the ID that was visited
/// - Value: Finalizable (its NodeId in the viewforest, tagged Tentative/Final)
///
/// Uses:
/// - prevent duplicate editable extensions
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
/// Callers should substitute a Unknown. A real
/// query error still surfaces as Err.
pub fn graphnode_and_viewnode_from_skgid (
  graph  : &InRustGraph,
  config : &SkgConfig,
  skgid  : &ID,
) -> Result < Option<( Graphnode, Viewnode )>, Box<dyn Error> > {
  let resolved : Option<(ID, SkgrepoName)> =
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
  skgrepo : &SkgrepoName,
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
    tree, treeid, visited, graph, config ) ?;
  if unrestrictedVognode_in_tree_is_writeProtected ( tree, treeid )?
  { clobberWriteProtectedViewnode (
      tree, treeid, graph, config ) ?; }
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "maybe_add_default_partnerFolder_branches" ). entered();
    maybe_add_default_partnerFolder_branches (
      tree, treeid, graph, config, skgrepo_restriction ) } ?;
  Ok (( )) }

/// Does only what it says, and makes the members of a write-protected
/// node non-members -- in particular,
/// does not clobber the node after making it write-protected.
/// A repeat is an occurrence whose node another occurrence claimed,
/// unless that one is a Tentative non-member and this an editable
/// member, which takes the claim
/// ('take_claim_from_tentative_nonMember').
///
/// The two jobs in the name cannot be unbundled --
/// we have to interleave extending the editable_map
/// with marking things write-protected, because the editable_map
/// is how we know whether to mark something write-protected.
pub fn make_writeProtected_if_repeat_then_extend_editable_map (
  tree    : &mut Tree<Viewnode>,
  treeid : NodeId,
  editableMap  : &mut EditableMap,
  graph   : &InRustGraph,
  config  : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let pid : ID = // Will error if node is a Non-vognode.
    get_skgid_from_viewnode_at ( tree, treeid ) ?;
  take_claim_from_tentative_nonMember (
    tree, treeid, editableMap, graph, config ) ?;
  let is_writeProtected : bool =
    write_at_unrestrictedVognode_in_tree (
      tree, treeid,
      |t| { if editableMap . contains_key (&pid)
               { // It's a repeat, so make it write-protected.
                 t . editability = Editability::WriteProtected; }
             t . is_writeProtected () } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  if !is_writeProtected {
    // Ordinary completed/saved editable -> Tentative (the editability rule).
    editableMap . insert ( pid, Finalizable::Tentative (treeid) ); }
  else { make_member_children_nonMembers ( tree, treeid ) ?; }
  Ok (( )) }

/// A write-protected occurrence cannot write its content, so the
/// server leaves no member under one: each child Unrestricted vognode
/// that is a member becomes a non-member, keeping its own editability
/// and subtree. Returns those children.
pub fn make_member_children_nonMembers (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
) -> Result<Vec<NodeId>, Box<dyn Error>> {
  let members : Vec<NodeId> =
    tree . get (treeid)
    . ok_or ( "make_member_children_nonMembers: NodeId not in tree" ) ?
    . children ()
    . filter ( |child| matches! ( &child . value () . kind,
                                  ViewnodeKind::Vognode (Vognode::Unrestricted (t))
                                  if t . claimsMembership ))
    . map ( |child| child . id () )
    . collect ();
  for member in &members {
    write_at_unrestrictedVognode_in_tree (
      tree, *member,
      |t| { t . claimsMembership = false; } )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?; }
  Ok (members) }

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

/// Append one view root to the viewforest, completed via
/// build_node_branch_minus_content. Returns its treeid.
pub fn append_plain_stub_root (
  viewforest : &mut ViewForest,
  root_skgid : &ID,
  graph      : &crate::dbs::in_rust_graph::InRustGraph,
  config     : &SkgConfig,
  visited    : &mut EditableMap,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < NodeId, Box<dyn Error> > {
  let viewforest_root_treeid : NodeId =
    viewforest . internal_root_skgid ();
  build_node_branch_minus_content (
    Some ( (viewforest . as_internal_tree_mut (),
            viewforest_root_treeid) ),
    root_skgid, graph, config, visited, skgrepo_restriction ) }

/// Mark forest-root UnrestrictedVognodes as members, so that a root
/// carries no 'noClaim' atom; a root has no viewparent whose
/// membership it could take part in.
pub fn mark_view_roots_as_members (
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
      { t . claimsMembership = true; }}}

/// Walk the view and correct any UnrestrictedVognode whose metadata claims a
/// relationship to its parent that the actual graph doesn't support.
/// Silently flips stale membership claims to non-member;
/// the rendered herald then no longer misleads.
///
/// The claim checked is 'claimsMembership' on child C with
/// WRITE_PROTECTED UnrestrictedVognode parent P: "C is part of P's
/// content". Verified against P's 'contains' in the in-Rust graph.
/// Editable parents are skipped because the save just redefined their
/// 'contains' to include this very child; the check would always pass.
///
/// Both sides of the comparison resolve through 'graph.pid_of'
/// so extra_id aliasing (typically a nodeMerge side-effect) doesn't
/// produce false mismatches.
///
/// Forest roots have no UnrestrictedVognode parent and therefore
/// do not fall through this check. Also relies on the save pipeline's
/// invariant that the prepared graph swap-in has updated the in-Rust-graph
/// graph before the rerender pass runs (see
/// 'update_views_after_save').
pub fn validate_membership_claims (
  viewforest : &mut Tree<Viewnode>,
  graph  : &InRustGraph,
) {
  // Collect correction targets in a read-only first pass so the
  // &mut Tree write phase doesn't need simultaneous read access.
  let mut to_independent : Vec<NodeId> = Vec::new ();
    // these will be made non-members
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
          // A non-UnrestrictedVognode parent (BufferRoot, a property or folder, Deleted, Tombstone) is not a legitimate subject for any of these relational claims; skip without correcting.
          _ => continue };
      let member_claim_ok : bool = match child_tn . claimsMembership {
        true => {
          if parent_tn . is_writeProtected () {
            child_contained_by_parent (graph, &child_tn . skgid, &parent_tn . skgid)
          } else { // The editable parent *defines* content, so cannot be incorrect.
            true }}
        false => true, };
      if ! member_claim_ok { to_independent . push ( child_ref . id () ); }}}
  for skgid in to_independent {
    let mut node_mut : NodeMut<Viewnode> =
      viewforest . get_mut (skgid) . unwrap ();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
      = node_mut . value () . kind
    { t . claimsMembership = false; } } }

/// An invariant: a *non-dead generalized orphan*
/// must not claim membership. An Unrestricted node whose parent is a
/// non-container -- a tombstone -- is exactly
/// that: it survives (is not itself dead) but its container is gone, so its
/// member claim (that it is part of that parent's membership) cannot hold.
/// Make it a non-member so it renders as its own graph-contains root rather
/// than claiming to affect a parent that no longer contains anything.
///
/// Scope, deliberately narrow:
/// - parent is a Tombstone -> make a member child a non-member.
/// - parent is a ghost (the diff view): left alone.
/// - parent is a Folder (PropertyFolder / PartnerFolder): the child is a legitimate folder
///   member; membership is correct -> leave. (A folder whose own ancestry broke is
///   entombstoned first, and then this pass catches its members.)
/// - parent is an Unrestricted vognode: handled by validate_membership_claims.
/// - parent is BufferRoot: the child is a forest root, handled by
///   mark_view_roots_as_members.
/// Belt-and-suspenders: most such children are already made non-members during the BFS
/// (mark_erroneous_content_children_as_indep for content children;
/// dispose_orphaned_folder_child for an entombstoned folder's members). This final pass
/// guarantees the invariant for any survivor those miss, in both the
/// post-save and de-novo paths. Purely structural -- no graph read.
pub fn mark_orphans_under_dead_parents_false (
  viewforest : &mut Tree<Viewnode>,
) {
  let mut targets : Vec<NodeId> = Vec::new ();
  for edge in viewforest . root () . traverse () {
    if let Edge::Open (child_ref) = edge {
      let is_unrestricted_member : bool =
        matches! ( & child_ref . value () . kind,
          ViewnodeKind::Vognode (Vognode::Unrestricted (t))
            if t . claimsMembership );
      if ! is_unrestricted_member { continue; }
      let parent_is_non_container : bool =
        child_ref . parent () . map_or ( false, |p|
          matches! ( & p . value () . kind,
            ViewnodeKind::Tombstone ) );
      if parent_is_non_container { targets . push ( child_ref . id () ); }}}
  for skgid in targets {
    let mut node_mut : NodeMut<Viewnode> =
      viewforest . get_mut (skgid) . unwrap ();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted ( ref mut t ))
      = node_mut . value () . kind
    { t . claimsMembership = false; } } }

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
      if let Some (skgid) =
        node_ref . value () . unrestricted_skgid ()
      { skgids . push ( skgid . clone () ); }}}
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
      => Ok ( v . skgid () . clone () ),
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
          None => { // A singleton tree with a Unknown.
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
    ViewnodeKind::Vognode (Vognode::Ghost (_)) => Ok (true),
    ViewnodeKind::Vognode (Vognode::Unknown (_)) => Ok (false),
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
mod validate_membership_claims_tests;
