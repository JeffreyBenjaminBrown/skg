use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::source_sets::ActiveSourceSet;
use crate::to_org::complete::contents::clobberIndefinitiveViewnode;
use crate::to_org::complete::partner_col::maybe_add_partnerCol_branches;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::env::find_source_with_optional_tantivy;
use crate::types::git::MembershipAxes;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;
use crate::types::tree::generic::{read_at_node_in_tree, read_at_ancestor_in_tree, with_node_mut};
use crate::types::tree::viewnode_nodecomplete::write_at_truenode_in_tree;
use crate::types::viewnode::ViewRequest;
use crate::types::viewnode::{ Birth, ViewNode, ViewNodeKind, IndefOrDef, ParentIs, TrueNode, mk_definitive_viewnode, mk_inactive_viewnode, mk_unknown_viewnode };
use crate::types::viewnode::{Vognode, Phantom};
use crate::types::tree::forest::{ViewForest, tree_forest_root_ids};

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
use ego_tree::iter::Edge;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::time;
use typedb_driver::TypeDBDriver;


/// Whether an ID's definitive occurrence is Final (claimed by a
/// definitive view request, DVR) or merely Tentative (an ordinary
/// saved/completed definitive). A DVR clobbers a Tentative occurrence
/// but defers to a Final one (TODO/DONE/local-view-update/plan_v2.org §5.2). The DVR cascade (TODO/DONE/local-view-update/plan_v2.org §5.3) is
/// not yet implemented, so today no second DVR ever reaches an
/// already-Final ID (validation forbids two user DVRs per ID); the
/// defer-to-Final branch is therefore correct-but-dormant until cascade
/// lands. NodeId is the occurrence's position in the viewforest.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Finalizable {
  Tentative (NodeId),
  Final     (NodeId),
}

impl Finalizable {
  pub fn node_id (&self) -> NodeId {
    match self { Finalizable::Tentative (n) | Finalizable::Final (n) => *n } }
  pub fn is_final (&self) -> bool {
    matches! (self, Finalizable::Final (_)) }
}

/// Tracks which IDs have been rendered definitively and where.
/// - Key: the ID that was visited
/// - Value: Finalizable (its NodeId in the viewforest, tagged Tentative/Final)
///
/// Uses:
/// - prevent duplicate definitive expansions
/// - locate the conflict when an earlier definitive view
///   conflicts with a new definitive view request
pub type DefinitiveMap =
  HashMap < ID, Finalizable >;


// ======================================================
// Fetching, building and modifying NodeCompletes and ViewNodes
// ======================================================

/// Fetch a NodeComplete from the in-Rust graph or disk. Resolves id→(pid,source)
/// via 'pid_and_source_from_id', then reads. Makes a ViewNode with
/// validated title. Returns both.
/// Returns Ok(None) when SKGID has no record anywhere -- not as a
/// primary pid, not as an extra_id, and not via TypeDB lookup.
/// Callers should substitute an PhantomUnknown placeholder. A real
/// query error still surfaces as Err.
pub async fn nodecomplete_and_viewnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skgid  : &ID,
) -> Result < Option<( NodeComplete, ViewNode )>, Box<dyn Error> > {
  let resolved : Option<(ID, SourceName)> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "nodecomplete_and_viewnode_from_id" ). entered();
      pid_and_source_from_id(
        &config . db_name, driver, skgid) . await ? };
  match resolved {
    None => Ok (None),
    Some ((pid_resolved, source)) =>
      Ok ( Some (
        nodecomplete_and_viewnode_from_pid_and_source (
          config, &pid_resolved, &source ) ? )) } }

/// Fetch a NodeComplete from the in-Rust graph or disk given PID and source.
/// Makes an ViewNode with validated title. Returns both.
pub(super) fn nodecomplete_and_viewnode_from_pid_and_source (
  config : &SkgConfig,
  pid    : &ID,
  source : &SourceName,
) -> Result < ( NodeComplete, ViewNode ), Box<dyn Error> > {
  let nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_source ( config, pid, source )?;
  let title : String = nodecomplete . title . replace ( '\n', " " );
  if title . is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "NodeComplete with ID {} has an empty title",
                 pid ), )) ); }
  let viewnode : ViewNode = mk_definitive_viewnode (
    pid . clone (),
    source . clone (),
    title,
    nodecomplete . body . clone () );
  Ok (( nodecomplete, viewnode )) }

/// Set node to indefinitive,
/// and reset title and source.
pub(super) fn makeIndefinitiveAndClobber (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  write_at_truenode_in_tree (
    tree, node_id,
    |t| { t . indef_or_def = IndefOrDef::Indefinitive; }
    ) . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  clobberIndefinitiveViewnode ( tree, node_id, config ) ?;
  Ok (( )) }

/// This function's callers add a pristine, out-of-context
/// (nodecomplete, viewnode) pair to the tree.
/// Integrating the pair into the tree requires more work
/// (and later will require even more, probably),
/// which this function does:
/// - handle repeats, cycles and the visited map
/// - build a subscribee branch if needed
pub async fn complete_branch_minus_content (
  tree     : &mut Tree<ViewNode>,
  node_id  : NodeId,
  visited  : &mut DefinitiveMap,
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  detect_and_mark_cycle_v1 ( tree, node_id ) ?;
  make_indef_if_repeat_then_extend_defmap (
    tree, node_id, visited ) ?;
  if truenode_in_tree_is_indefinitive ( tree, node_id )?
  { clobberIndefinitiveViewnode (
      tree, node_id, config ) ?; }
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "maybe_add_partnerCol_branches" ). entered();
    maybe_add_partnerCol_branches (
      tree, node_id, config, driver, active_source_set,
      // This birth path runs outside the diff-aware BFS (search
      // results, ancestry attachment, stubs); diff-mode col
      // existence is decided at each node's completion visit, which
      // passes the real diffs.
      &None ) . await } ?;
  Ok (( )) }

/// Does only what it says -- in particular,
/// does not clobber the node after making it indefinitive.
///
/// The two jobs in the name cannot be unbundled --
/// we have to interleave extending the defmap
/// with marking things indefinitive, because the defmap
/// is how we know whether to mark something indefinitive.
pub fn make_indef_if_repeat_then_extend_defmap (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  defMap  : &mut DefinitiveMap,
) -> Result<(), Box<dyn Error>> {
  let pid : ID = // Will error if node is a Scaffold.
    get_id_from_treenode ( tree, node_id ) ?;
  let is_indefinitive : bool =
    write_at_truenode_in_tree (
      tree, node_id,
      |t| { if defMap . contains_key (&pid)
               { // It's a repeat, so make it indefinitive.
                 t . indef_or_def = IndefOrDef::Indefinitive; }
             t . is_indefinitive () } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  if !is_indefinitive {
    // Ordinary completed/saved definitive -> Tentative (TODO/DONE/local-view-update/plan_v2.org §5.2).
    defMap . insert ( pid, Finalizable::Tentative (node_id) ); }
  Ok (( )) }

/// Check if the node's PID appears in its ancestors,
/// and if so, mark viewData.cycle = true.
pub fn detect_and_mark_cycle_v1 (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
) -> Result<(), Box<dyn Error>> {
  let is_cycle : bool = {
    let pid : ID = get_id_from_treenode ( tree, node_id ) ?;
    is_ancestor_id ( tree, node_id, &pid ) ? };
  write_at_truenode_in_tree
    ( tree, node_id,
      |t| { t . viewStats . cycle = is_cycle; } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }


// ==============================================
// Reading and manipulating trees, esp. via IDs
// ==============================================

/// Create a viewforest containing just view roots,
/// and complete each via build_node_branch_minus_content.
pub async fn stub_viewforest_from_root_ids (
  root_skgids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
  visited  : &mut DefinitiveMap,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < ViewForest, Box<dyn Error> > {
  let mut viewforest : ViewForest =
    ViewForest::new ();
  let viewforest_root_treeid : NodeId =
    viewforest . internal_root_id ();
  for root_skgid in root_skgids {
    build_node_branch_minus_content (
      Some ( (viewforest . as_internal_tree_mut (),
              viewforest_root_treeid) ),
      root_skgid, config, driver, visited, active_source_set
    ) . await ?; }
  Ok (viewforest) }

/// Mark forest-root TrueNodes as having no parent in the view.
pub fn mark_view_roots_parent_absent (
  viewforest : &mut Tree<ViewNode>,
) {
  let root_ids : Vec<NodeId> =
    tree_forest_root_ids (viewforest);
  for root_id in root_ids {
    let mut node_mut : NodeMut<ViewNode> =
      viewforest . get_mut (root_id) . unwrap ();
    let vn : &mut ViewNode = node_mut . value ();
    if let ViewNodeKind::Vognode (Vognode::Active ( ref mut t ))
      = vn . kind
      { t . parentIs = ParentIs::Absent; }}}

/// Walk the view and correct any TrueNode whose metadata claims a
/// relationship to its parent that the actual graph doesn't support.
/// Silently clears stale birth claims or flips stale membership claims
/// to Independent;
/// the rendered herald then no longer misleads.
///
/// Three kinds of claim are checked:
/// - 'Birth::ContainsParent' on child C with TrueNode parent P:
///   claim is "C contains P". Verified against C's 'contains'
///   list in the in-Rust graph.
/// - 'Birth::LinksToParent' on child C with TrueNode parent P:
///   claim is "C's body/title links to P". Verified against C's
///   'textlinks_to' in the in-Rust graph.
/// - 'ParentIs::Affected' on child C with INDEFINITIVE TrueNode
///   parent P: claim is "C is part of P's content". Verified
///   against P's 'contains' in the in-Rust graph. Definitive parents are
///   skipped because the save just redefined their 'contains' to
///   include this very child; the check would always pass.
///
/// Both sides of every comparison resolve through 'graph.pid_of'
/// so extra_id aliasing (typically a nodeMerge side-effect) doesn't
/// produce false mismatches.
///
/// Forest roots have no TrueNode parent and therefore
/// do not fall through this check. Also relies on the save pipeline's
/// invariant that 'apply_definenodes' has updated the in-Rust-graph
/// graph before the rerender pass runs (see
/// 'update_views_after_save').
pub fn validate_parentIs_relationships (
  viewforest : &mut Tree<ViewNode>,
  graph  : &InRustGraph,
) {
  // Collect correction targets in a read-only first pass so the
  // &mut Tree write phase doesn't need simultaneous read access.
  let mut to_independent : Vec<NodeId> = Vec::new ();
    // these will be marked parentIs = independent
  let mut to_affected : Vec<NodeId> = Vec::new ();
    // these will be marked parentIs = affected
  let mut to_unremarkable : Vec<NodeId> = Vec::new ();
    // these will be marked birth = unremarkable
  for edge in viewforest . root () . traverse () {
    if let Edge::Open (child_ref) = edge {
      let child_tn : &TrueNode =
        match & child_ref . value () . kind {
          ViewNodeKind::Vognode (Vognode::Active (t)) => t,
          _ => continue };
      let parent_ref : NodeRef<ViewNode> = match child_ref . parent () {
        Some (p) => p, None => continue };
      let parent_tn : &TrueNode =
        match & parent_ref . value () . kind {
          ViewNodeKind::Vognode (Vognode::Active (t)) => t,
          // A non-TrueNode parent (BufferRoot, Scaffold, Deleted, DeletedScaff) is not a legitimate subject for any of these relational claims; skip without correcting.
          _ => continue };
      if child_tn . parentIs == ParentIs::Absent {
        // The child was a root, and the user gave it a parent, so let the parent contain it.
        to_affected . push ( child_ref . id () );
        continue; }
      let parent_is_claim_ok : bool = match child_tn . parentIs {
        ParentIs::Affected => {
          if parent_tn . is_indefinitive () {
            child_contained_by_parent (graph, &child_tn . id, &parent_tn . id)
          } else { // The definitive parent *defines* content, so cannot be incorrect.
            true }}
        ParentIs::Independent => true,
        ParentIs::Absent => false, };
      if ! parent_is_claim_ok { to_independent . push ( child_ref . id () ); }
      let birth_claim_ok : bool = match child_tn . birth {
        Birth::ContainsParent =>
          child_contains_parent (graph, &child_tn . id, &parent_tn . id),
        Birth::LinksToParent =>
          child_linksto_parent (graph, &child_tn . id, &parent_tn . id),
        Birth::Unremarkable => true, };
      if ! birth_claim_ok { to_unremarkable . push ( child_ref . id () ); }}}
  for id in to_independent {
    let mut node_mut : NodeMut<ViewNode> =
      viewforest . get_mut (id) . unwrap ();
    if let ViewNodeKind::Vognode (Vognode::Active ( ref mut t ))
      = node_mut . value () . kind
    { t . parentIs = ParentIs::Independent; } }
  for id in to_affected {
    let mut node_mut : NodeMut<ViewNode> =
      viewforest . get_mut (id) . unwrap ();
    if let ViewNodeKind::Vognode (Vognode::Active ( ref mut t ))
      = node_mut . value () . kind
    { t . parentIs = ParentIs::Affected; } }
  for id in to_unremarkable {
    let mut node_mut : NodeMut<ViewNode> =
      viewforest . get_mut (id) . unwrap ();
    if let ViewNodeKind::Vognode (Vognode::Active ( ref mut t ))
      = node_mut . value () . kind
    { t . birth = Birth::Unremarkable; } } }

/// Jeff's invariant (TODO/DONE/local-view-update/progress.org §11 thread): a *non-dead generalized orphan*
/// must have ParentIs=Independent. A Active node whose PARENT is a
/// non-container -- a Diff phantom, a Deleted, or a DeadScaffold -- is exactly
/// that: it survives (is not itself dead) but its container is gone, so its
/// =Affected= claim (that it is part of that parent's collection) cannot hold.
/// Demote it to Independent so it renders as its own graph-contains root rather
/// than claiming to affect a parent that no longer contains anything.
///
/// Scope, deliberately narrow:
/// - PARENT is Diff phantom / Deleted / DeadScaffold -> demote an Affected child.
/// - PARENT is a Col (QualCol / PartnerCol): the child is a legitimate col
///   MEMBER; Affected is correct -> leave. (A col whose own ancestry broke is
///   deadened to DeadScaffold first, and then THIS pass catches its members.)
/// - PARENT is an Active vognode: handled by validate_parentIs_relationships.
/// - PARENT is BufferRoot: the child is a forest root, handled by
///   mark_view_roots_parent_absent.
/// Belt-and-suspenders: most cases are already demoted during the BFS
/// (mark_erroneous_content_children_as_indep for content children;
/// dispose_orphaned_col_child for a deadened col's members). This final pass
/// GUARANTEES the invariant for any survivor those miss (e.g. a removedHere
/// phantom's content children), in both the post-save and de-novo paths. Purely
/// structural -- no graph read.
pub fn mark_orphans_under_dead_parents_independent (
  viewforest : &mut Tree<ViewNode>,
) {
  let mut targets : Vec<NodeId> = Vec::new ();
  for edge in viewforest . root () . traverse () {
    if let Edge::Open (child_ref) = edge {
      let is_affected_normal : bool =
        matches! ( & child_ref . value () . kind,
          ViewNodeKind::Vognode (Vognode::Active (t))
            if t . parentIs == ParentIs::Affected );
      if ! is_affected_normal { continue; }
      let parent_is_non_container : bool =
        child_ref . parent () . map_or ( false, |p|
          matches! ( & p . value () . kind,
            ViewNodeKind::Phantom (Phantom::Diff (_))
              | ViewNodeKind::Phantom (Phantom::Deleted (_))
              | ViewNodeKind::DeadScaffold ) );
      if parent_is_non_container { targets . push ( child_ref . id () ); }}}
  for id in targets {
    let mut node_mut : NodeMut<ViewNode> =
      viewforest . get_mut (id) . unwrap ();
    if let ViewNodeKind::Vognode (Vognode::Active ( ref mut t ))
      = node_mut . value () . kind
    { t . parentIs = ParentIs::Independent; } } }

/// Does 'child's 'contains' list include 'parent' (modulo
/// extra_id aliasing on either side)?
fn child_contains_parent (
  graph     : &InRustGraph,
  child_id  : &ID,
  parent_id : &ID,
) -> bool {
  let parent_pid : ID =
    graph . pid_of (parent_id) . unwrap_or_else ( || parent_id . clone () );
  let child_node : &NodeRust =
    match graph . get (child_id) {
      Some (n) => n,
      None     => return false };
  child_node . contains . iter () . any ( |x|
    graph . pid_of (x) . unwrap_or_else ( || x . clone () )
      == parent_pid ) }

/// Does 'child's 'textlinks_to' include 'parent' (modulo
/// extra_id aliasing on either side)?
fn child_linksto_parent (
  graph     : &InRustGraph,
  child_id  : &ID,
  parent_id : &ID,
) -> bool {
  let parent_pid : ID =
    graph . pid_of (parent_id) . unwrap_or_else ( || parent_id . clone () );
  let child_node : &NodeRust =
    match graph . get (child_id) {
      Some (n) => n,
      None     => return false };
  child_node . textlinks_to . iter () . any ( |x|
    graph . pid_of (x) . unwrap_or_else ( || x . clone () )
      == parent_pid ) }

/// Symmetric to 'child_contains_parent': does 'parent's
/// 'contains' list include 'child'?
fn child_contained_by_parent (
  graph     : &InRustGraph,
  child_id  : &ID,
  parent_id : &ID,
) -> bool {
  let child_pid : ID =
    graph . pid_of (child_id) . unwrap_or_else ( || child_id . clone () );
  let parent_node : &NodeRust =
    match graph . get (parent_id) {
      Some (n) => n,
      None     => return false };
  parent_node . contains . iter () . any ( |x|
    graph . pid_of (x) . unwrap_or_else ( || x . clone () )
      == child_pid ) }

pub fn ids_that_can_have_graphnodestats (
  tree : &Tree<ViewNode>,
) -> Vec < ID > {
  let mut ids : Vec < ID > = Vec::new ();
  for edge in tree . root () . traverse () {
    if let Edge::Open (node_ref) = edge {
      if let Some (vid) =
        node_ref . value () . active_or_diff_phantom_id ()
      { ids . push ( vid . clone () ); }}}
  ids }

/// Check if `target_skgid` appears in the ancestor path of `treeid`.
/// Used for cycle detection.
fn is_ancestor_id (
  tree          : &Tree<ViewNode>,
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
      |viewnode| viewnode . id_if_vognode () . cloned () )
    { Ok(Some (id)) if &id == target_skgid
        => return Ok (true),
      Ok (_) => continue,
      Err (_) => return Ok (false), }}
  unreachable!() }

/// Errors if the node is a Scaffold or not found.
pub fn get_id_from_treenode (
  tree   : &Tree<ViewNode>,
  treeid : NodeId,
) -> Result < ID, Box<dyn Error> > {
  let node_kind: ViewNodeKind =
    read_at_node_in_tree (
      tree, treeid, |viewnode| viewnode . kind . clone() )?;
  match node_kind {
    ViewNodeKind::Vognode (v)
      => Ok ( v . id () . clone () ),
    _ => Err ( "get_id_from_treenode: caller must pass a vognode" . into() ),
  }}

/// Build a node from disk and
/// append it at 'parent_treeid' as a child.
/// Returns the new node's ego_tree::NodeId.
///
/// Does *not* take its ancestors into account,
/// and does *not* build any of its descendents.
/// Those can be done later via 'complete_branch_minus_content',
/// or they can all be done at once via
/// 'build_node_branch_minus_content.
pub async fn make_and_append_child_pair (
  tree          : &mut Tree<ViewNode>,
  parent_treeid : NodeId, // will parent the new node
  child_skgid   : &ID, // how to find the new node
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < NodeId, // the new node
              Box<dyn Error> > {
  let child_viewnode : ViewNode = match
    nodecomplete_and_viewnode_from_id (
      config, driver, child_skgid ) . await ?
    { Some ((_nc, v)) => v,
      None => mk_unknown_viewnode (child_skgid . clone ()) };
  let child_treeid : NodeId =
    with_node_mut ( // append child
      tree, parent_treeid,
      ( |mut parent_mut|
        parent_mut . append (child_viewnode) . id() ))
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (child_treeid) }

pub async fn make_and_append_child_pair_with_source_set (
  tree          : &mut Tree<ViewNode>,
  parent_treeid : NodeId,
  child_skgid   : &ID,
  config        : &SkgConfig,
  driver        : &TypeDBDriver,
  active        : Option<&ActiveSourceSet>,
) -> Result < (NodeId, bool), Box<dyn Error> > {
  if let Some (active) = active {
    if ! active . is_all () {
      let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
        HashMap::new ();
      if let Some (source) =
        find_source_with_optional_tantivy (
          child_skgid, &deleted_since_head_pid_src_map, None, config )
      {
        if ! active . contains_source (&source) {
          let inactive : ViewNode =
            mk_inactive_viewnode (
              child_skgid . clone (), source,
              MembershipAxes::default (), None );
          let child_treeid : NodeId =
            with_node_mut (
              tree, parent_treeid,
              ( |mut parent_mut|
                parent_mut . append (inactive) . id () ))
            . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
          return Ok ((child_treeid, false)); }}}}
  let child_treeid : NodeId =
    make_and_append_child_pair (
      tree, parent_treeid, child_skgid, config, driver ) . await ?;
  Ok ((child_treeid, true)) }

/// Builds a node from disk, place it in a tree,
/// complete the branch it implies except for 'content' descendents,
/// and return the NodeId of the branch root.
/// - If tree_and_parent is None, creates a new tree (not returned).
/// - If tree_and_parent is Some, appends to the existing tree.
pub async fn build_node_branch_minus_content (
  tree_and_parent : Option<(&mut Tree<ViewNode>, NodeId)>, // if modifying an existing tree, attach as a child here
  skgid           : &ID, // what to fetch
  config          : &SkgConfig,
  driver          : &TypeDBDriver,
  visited         : &mut DefinitiveMap,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < NodeId, Box<dyn Error> > {
  let t0 : time::Instant = time::Instant::now();
  let result : Result < NodeId, Box<dyn Error> > =
    match tree_and_parent {
      Some ( (tree, parent_treeid) ) => {
        let lookup : Option<(NodeComplete, ViewNode)> =
          nodecomplete_and_viewnode_from_id (
            config, driver, skgid ) . await ?;
        match lookup {
          Some ((_nc, viewnode)) => {
            let child_treeid : NodeId = // Add ViewNode to tree
              with_node_mut (
                tree, parent_treeid,
                ( |mut parent_mut|
                  parent_mut . append (viewnode) . id () ))
              . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
            complete_branch_minus_content (
              tree, child_treeid, visited,
              config, driver, active_source_set ) . await ?;
            Ok (child_treeid) },
          None => { // Uknown node. Add it, don't 'complete' it.
            let viewnode : ViewNode =
              mk_unknown_viewnode (skgid . clone ());
            let child_treeid : NodeId =
              with_node_mut (
                tree, parent_treeid,
                ( |mut parent_mut|
                  parent_mut . append (viewnode) . id () ))
              . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
            Ok (child_treeid) }} },
      None => {
        let lookup : Option<(NodeComplete, ViewNode)> =
          nodecomplete_and_viewnode_from_id (
            config, driver, skgid ) . await ?;
        match lookup {
          Some ((_nc, viewnode)) => {
            let mut tree : Tree<ViewNode> =
              Tree::new (viewnode);
            let root_treeid : NodeId = tree . root () . id ();
            complete_branch_minus_content (
              &mut tree, root_treeid, visited,
              config, driver, active_source_set ) . await ?;
            Ok (root_treeid) },
          None => { // A singleton tree with an PhantomUnknown.
            let viewnode : ViewNode =
              mk_unknown_viewnode (skgid . clone ());
            let tree : Tree<ViewNode> = Tree::new (viewnode);
            Ok (tree . root () . id ()) }} }, };
  tracing::info!("{}: {:.3}s",
                 format! ("build_node_branch_minus_content({})", skgid),
                 t0 . elapsed () . as_secs_f64());
  result }

// ==============================================
// Reading from NodeCompletes and ViewNodes, esp. in trees
// ==============================================

/// Check if a TrueNode is indefinitive.
/// Errs if given a Scaffold.
pub fn truenode_in_tree_is_indefinitive (
  tree   : &Tree<ViewNode>,
  treeid : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_kind: ViewNodeKind =
    read_at_node_in_tree ( tree, treeid,
                           |viewnode| viewnode . kind . clone() )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  match node_kind {
    ViewNodeKind::Vognode (Vognode::Active (t))   => Ok (t . is_indefinitive ()),
    ViewNodeKind::Phantom (Phantom::Diff (p)) => Ok (p . is_indefinitive ()),
    ViewNodeKind::Phantom (Phantom::Deleted (_))
      | ViewNodeKind::Vognode (Vognode::Inactive (_))
      | ViewNodeKind::Phantom (Phantom::Unknown (_)) => Ok (false),
    _                                                => Err (
      "is_indefinitive: caller must pass a vognode" . into( )),
  }}

/// Collect all child tree NodeIds from a node.
/// Returns an error if the node is not found.
pub fn collect_child_treeids (
  tree    : &Tree<ViewNode>,
  treeid : NodeId,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let node_ref : NodeRef < ViewNode > =
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
  node_id      : NodeId,
  view_request : ViewRequest,
  error_msg    : &str,
  errors       : &mut Vec < String >,
  result       : Result < (), Box<dyn Error> >,
) -> Result < (), Box<dyn Error> >
where T: AsMut<ViewNode>,
{
  if let Err (e) = result {
    errors . push ( format! ( "{}: {}", error_msg, e )); }
  let mut node_mut : NodeMut<T> =
    tree . get_mut (node_id) . ok_or ("remove_completed_view_request: node not found") ?;
  if let ViewNodeKind::Vognode (Vognode::Active (t))
    = &mut node_mut . value () . as_mut () . kind
    { t . view_requests . remove (&view_request); }
  Ok (()) }

#[cfg(test)]
#[path = "../../tests/unit/to_org_util.rs"]
mod validate_parentIs_relationships_tests;
