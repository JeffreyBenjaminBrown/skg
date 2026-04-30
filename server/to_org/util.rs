use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::typedb::search::pid_and_source_from_id;
use crate::to_org::complete::contents::clobberIndefinitiveViewnode;
use crate::to_org::complete::sharing::maybe_add_subscribeeCol_branch;
use crate::types::views_state::nodecomplete_from_in_rust_graph_or_disk;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;
use crate::types::tree::generations::collect_generation_ids;
use crate::types::tree::generic::{read_at_node_in_tree, read_at_ancestor_in_tree, with_node_mut};
use crate::types::tree::viewnode_nodecomplete::{pid_and_source_from_treenode, write_at_truenode_in_tree};
use crate::types::viewnode::ViewRequest;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, IndefOrDef, Birth, TrueNode, viewforest_root_viewnode, mk_definitive_viewnode, mk_unknown_viewnode };

use ego_tree::{Tree, NodeId, NodeRef, NodeMut};
use ego_tree::iter::Edge;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;


/// Tracks which IDs have been rendered definitively and where.
/// - Key: the ID that was visited
/// - Value: NodeId within the viewforest tree
///
/// Uses:
/// - prevent duplicate definitive expansions
/// - locate the conflict when an earlier definitive view
///   conflicts with a new definitive view request
pub type DefinitiveMap =
  HashMap < ID, NodeId >;


// ======================================================
// Fetching, building and modifying NodeCompletes and ViewNodes
// ======================================================

/// Fetch a NodeComplete from the in-Rust graph or disk. Resolves id→(pid,source)
/// via 'pid_and_source_from_id', then reads. Makes a ViewNode with
/// validated title. Returns both.
/// Returns Ok(None) when SKGID has no record anywhere -- not as a
/// primary pid, not as an extra_id, and not via TypeDB lookup.
/// Callers should substitute an UnknownNode placeholder. A real
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
    nodecomplete_from_in_rust_graph_or_disk ( config, pid, source )?;
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
) -> Result<(), Box<dyn Error>> {
  detect_and_mark_cycle_v1 ( tree, node_id ) ?;
  make_indef_if_repeat_then_extend_defmap (
    tree, node_id, visited ) ?;
  if truenode_in_tree_is_indefinitive ( tree, node_id )?
  { clobberIndefinitiveViewnode (
      tree, node_id, config ) ?; }
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "maybe_add_subscribeeCol_branch" ). entered();
    maybe_add_subscribeeCol_branch (
      tree, node_id, config, driver ) . await } ?;
  Ok (( )) }

/// Does only what it says -- in particular,
/// does not clobber the node after making it indefinitive.
///
/// The two jobs in the name of this are inseparable --
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
    defMap . insert ( pid, node_id ); }
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
  write_at_truenode_in_tree ( tree, node_id, |t| {
    t . viewStats . cycle = is_cycle; } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }


// ==============================================
// Reading and manipulating trees, esp. via IDs
// ==============================================

/// Create a viewforest (single tree with BufferRoot at root)
/// containing just "tree root" nodes (no grandchildren yet),
/// and complete each via build_node_branch_minus_content.
pub async fn stub_viewforest_from_root_ids (
  root_skgids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
  visited  : &mut DefinitiveMap,
) -> Result < Tree<ViewNode>, Box<dyn Error> > {
  let mut viewforest : Tree<ViewNode> =
    Tree::new ( viewforest_root_viewnode () );
  let viewforest_root_treeid : NodeId = viewforest . root () . id ();
  for root_skgid in root_skgids {
    build_node_branch_minus_content (
      Some ( (&mut viewforest, viewforest_root_treeid) ),
      root_skgid, config, driver, visited
    ) . await ?; }
  mark_view_roots_independent ( &mut viewforest );
  Ok (viewforest) }

/// Mark direct TrueNode children of BufferRoot as birth=Independent.
/// View roots are not rendered because of a parent relationship;
/// they are the user's chosen entry points into the graph.
pub fn mark_view_roots_independent (
  viewforest : &mut Tree<ViewNode>,
) {
  let root_id : NodeId = viewforest . root () . id ();
  let child_ids : Vec<NodeId> =
    viewforest . get (root_id) . unwrap ()
    . children () . map ( |c| c . id () ) . collect ();
  for child_id in child_ids {
    let mut node_mut : NodeMut<ViewNode> =
      viewforest . get_mut (child_id) . unwrap ();
    let vn : &mut ViewNode = node_mut . value ();
    if let ViewNodeKind::True ( ref mut t ) = vn . kind {
      t . birth = Birth::Independent; } } }

/// Walk the view and correct any TrueNode whose birth claims a
/// relationship to its parent that the actual graph doesn't
/// support. Silently flips the birth to Independent on mismatch;
/// the rendered herald then no longer misleads.
///
/// Three kinds of claim are checked:
/// - 'Birth::ContainerOf' on child C with TrueNode parent P:
///   claim is "C contains P". Verified against C's 'contains'
///   list in the in-Rust graph.
/// - 'Birth::LinksTo' on child C with TrueNode parent P:
///   claim is "C's body/title links to P". Verified against C's
///   'textlinks_to' in the in-Rust graph.
/// - 'Birth::ContentOf' on child C with INDEFINITIVE TrueNode
///   parent P: claim is "C is part of P's content". Verified
///   against P's 'contains' in the in-Rust graph. Definitive parents are
///   skipped because the save just redefined their 'contains' to
///   include this very child; the check would always pass.
///
/// Both sides of every comparison resolve through 'graph.pid_of'
/// so extra_id aliasing (typically a merge side-effect) doesn't
/// produce false mismatches.
///
/// Intended to run AFTER 'mark_view_roots_independent' so direct
/// children of BufferRoot have already been coerced and don't
/// fall through this check. Also relies on the save pipeline's
/// invariant that 'apply_definenodes' has updated the in-Rust-graph
/// graph before the rerender pass runs (see
/// 'update_views_after_save').
pub fn validate_birth_relationships (
  viewforest : &mut Tree<ViewNode>,
  graph  : &InRustGraph,
) {
  // Collect correction targets in a read-only first pass so the
  // &mut Tree write phase doesn't need simultaneous read access.
  let mut to_flip : Vec<NodeId> = Vec::new ();
  for edge in viewforest . root () . traverse () {
    if let Edge::Open (child_ref) = edge {
      let child_tn : &TrueNode =
        match & child_ref . value () . kind {
          ViewNodeKind::True (t) => t,
          _ => continue };
      let parent_ref : NodeRef<ViewNode> = match child_ref . parent () {
        Some (p) => p, None => continue };
      let parent_tn : &TrueNode =
        match & parent_ref . value () . kind {
          ViewNodeKind::True (t) => t,
          // A non-TrueNode parent (BufferRoot, Scaffold, Deleted,
          // DeletedScaff) is not a legitimate subject for any of
          // these relational claims; skip without correcting. The
          // top-level BufferRoot case is handled by
          // 'mark_view_roots_independent'.
          _ => continue };
      let claim_ok : bool = match child_tn . birth {
        Birth::ContainerOf =>
          child_contains_parent (graph, &child_tn . id, &parent_tn . id),
        Birth::LinksTo =>
          child_linksto_parent (graph, &child_tn . id, &parent_tn . id),
        Birth::ContentOf => {
          if parent_tn . is_indefinitive () {
            child_contained_by_parent (graph, &child_tn . id, &parent_tn . id)
          } else { true } }
        Birth::Independent => true, };
      if ! claim_ok { to_flip . push ( child_ref . id () ); } } }
  for id in to_flip {
    let mut node_mut : NodeMut<ViewNode> =
      viewforest . get_mut (id) . unwrap ();
    if let ViewNodeKind::True ( ref mut t )
      = node_mut . value () . kind
    { t . birth = Birth::Independent; } } }

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

pub fn collect_ids_from_tree (
  tree : &Tree<ViewNode>,
) -> Vec < ID > {
  let mut pids : Vec < ID > = Vec::new ();
  for edge in tree . root () . traverse () {
    if let Edge::Open (node_ref) = edge {
      match &node_ref . value () . kind {
        ViewNodeKind::True (t)    =>
          pids . push ( t . id . clone () ),
        ViewNodeKind::Deleted (d) =>
          pids . push ( d . id . clone () ),
        _ => {} } } }
  pids }

/// Check if `target_skgid` appears in the ancestor path of `treeid`.
/// Used for cycle detection.
fn is_ancestor_id (
  tree          : &Tree<ViewNode>,
  origin_treeid : NodeId,
  target_skgid  : &ID,
) -> Result<bool, Box<dyn Error>> {
  read_at_node_in_tree(tree, origin_treeid, |_| ())
    . map_err(|_| "is_ancestor_id: NodeId not in tree")?;
  for generation in 1.. {
    match read_at_ancestor_in_tree(
      tree, origin_treeid, generation,
      |viewnode| match &viewnode . kind {
        ViewNodeKind::True (t)    => Some ( t . id . clone () ),
        ViewNodeKind::Deleted (d) => Some ( d . id . clone () ),
        ViewNodeKind::Unknown (u) => Some ( u . id . clone () ),
        ViewNodeKind::Scaff (_) |
        ViewNodeKind::DeletedScaff (_) => None } )
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
    ViewNodeKind::True (t)    => Ok ( t . id ),
    ViewNodeKind::Deleted (d) => Ok ( d . id ),
    ViewNodeKind::Unknown (u) => Ok ( u . id ),
    ViewNodeKind::Scaff (_)   => Err ( "get_id_from_treenode: caller should not pass a Scaffold" . into() ),
    ViewNodeKind::DeletedScaff (_) => Err ( "get_id_from_treenode: caller should not pass a DeletedScaff" . into() ),
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
) -> Result < NodeId, Box<dyn Error> > {
  let t0 : std::time::Instant = std::time::Instant::now();
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
              config, driver ) . await ?;
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
              config, driver ) . await ?;
            Ok (root_treeid) },
          None => { // A singleton tree with an UnknownNode.
            let viewnode : ViewNode =
              mk_unknown_viewnode (skgid . clone ());
            let tree : Tree<ViewNode> = Tree::new (viewnode);
            Ok (tree . root () . id ()) }} }, };
  tracing::info!("{}: {:.3}s",
                 format! ("build_node_branch_minus_content({})", skgid),
                 t0 . elapsed () . as_secs_f64());
  result }

/// Collect content child IDs from a node.
/// Returns empty vec if the node is indefinitive or has no NodeComplete.
/// Errors if passed a Scaffold.
pub(super) fn content_ids_if_definitive_else_empty (
  tree   : &Tree<ViewNode>,
  treeid : NodeId,
  config : &SkgConfig,
) -> Result < Vec < ID >, Box<dyn Error> > {
  if truenode_in_tree_is_indefinitive ( tree, treeid ) ? {
    return Ok ( Vec::new () ); }
  let (pid, source) : (ID, SourceName) =
    match pid_and_source_from_treenode (
      tree, treeid, "content_ids_if_definitive_else_empty" ) {
      Ok (p) => p,
      Err (_) => return Ok ( Vec::new () ), };
  Ok ( nodecomplete_from_in_rust_graph_or_disk ( config, &pid, &source )
    . map ( |nodecomplete| nodecomplete . contains . clone () )
    . unwrap_or_default () ) }

/// Collect ego_tree::NodeIds after
///   some member of some generation of a tree.
/// 'effective_root' should be some ancestor.
/// It affects both the meaning of generation numbers,
/// and the scope of which nodes are collected
/// (only its descendents are collected).
/// If effective root is None,
/// the true root is used as the effective root.
pub(super) fn nodes_after_in_generation (
  tree                     : &Tree<ViewNode>,
  generation               : usize,
  generation_member_treeid : NodeId,
  effective_root           : Option < NodeId >,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let treeids_in_gen : Vec < NodeId > =
    collect_generation_ids ( tree, generation, effective_root ) ?;
  let mut result : Vec < NodeId > = Vec::new ();
  let mut found_target : bool = false;
  for treeid in treeids_in_gen {
    if found_target {
      result . push (treeid);
    } else if treeid == generation_member_treeid {
      found_target = true; } }
  Ok (result) }


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
    ViewNodeKind::True (t)    => Ok (t . is_indefinitive ()),
    ViewNodeKind::Deleted (_) => Ok (false),
    ViewNodeKind::Unknown (_) => Ok (false),
    ViewNodeKind::Scaff (_)   => Err ( "is_indefinitive: caller should not pass a Scaffold" . into( )),
    ViewNodeKind::DeletedScaff (_) => Err ( "is_indefinitive: caller should not pass a DeletedScaff" . into( )),
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
  if let ViewNodeKind::True (t)
    = &mut node_mut . value () . as_mut () . kind
    { t . view_requests . remove (&view_request); }
  Ok (()) }

#[cfg(test)]
mod validate_birth_relationships_tests {
  use super::*;
  use crate::types::misc::MSV;
  use crate::types::viewnode::mk_indefinitive_viewnode;

  fn src () -> SourceName { SourceName::from ("main") }
  fn id  (s: &str) -> ID { ID ( s . to_string () ) }

  /// Build a NodeRust directly (no disk I/O) for fixture graphs.
  fn mk_node (
    pid          : &str,
    extra_ids    : &[&str],
    contains     : &[&str],
    textlinks_to : &[&str],
  ) -> NodeRust {
    NodeRust {
      pid:          id (pid),
      source:       src (),
      extra_ids:    extra_ids . iter () . map ( |s| id (s) ) . collect (),
      title:        pid . to_string (),
      aliases:      MSV::Unspecified,
      body:         None,
      contains:     contains . iter () . map ( |s| id (s) ) . collect (),
      subscribes_to:                MSV::Unspecified,
      hides_from_its_subscriptions: MSV::Unspecified,
      overrides_view_of:            MSV::Unspecified,
      misc:         Vec::new (),
      textlinks_to: textlinks_to . iter () . map ( |s| id (s) ) . collect (),
    } }

  /// Insert nodes into a fresh InRustGraph.
  fn graph_with (nodes: Vec<NodeRust>) -> InRustGraph {
    let mut g : InRustGraph = InRustGraph::new ();
    // Populate extra_id_to_pid first so inverse indexes resolve right.
    for n in &nodes {
      for eid in &n . extra_ids {
        g . extra_id_to_pid . insert (eid . clone (), n . pid . clone ()); } }
    for n in nodes {
      g . nodes . insert (n . pid . clone (), n); }
    g }

  fn true_child_birth (
    viewforest : &Tree<ViewNode>,
    nid    : NodeId,
  ) -> Birth {
    match & viewforest . get (nid) . unwrap () . value () . kind {
      ViewNodeKind::True (t) => t . birth,
      _ => panic! ("expected TrueNode") } }

  #[test]
  fn linksto_false_claim_flipped_to_independent () {
    // Parent P, child C with birth=LinksTo, but C's textlinks_to
    // does NOT include P. The claim is false → flip.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("P", &[], &[], &[]),
      mk_node ("C", &[], &[], &[]),   // textlinks_to: empty
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::LinksTo) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::Independent,
      "LinksTo claim with no backing textlink should flip to Independent");
  }

  #[test]
  fn linksto_true_claim_preserved () {
    // Parent P, child C with birth=LinksTo and C's textlinks_to
    // DOES include P. The claim is true → preserve.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("P", &[], &[], &[]),
      mk_node ("C", &[], &[], &["P"]), // C textlinks to P
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::LinksTo) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::LinksTo,
      "LinksTo claim with a backing textlink must be preserved");
  }

  #[test]
  fn containerof_false_claim_flipped () {
    // Parent P, child C with birth=ContainerOf, but C's contains
    // does NOT include P.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("P", &[], &[],    &[]),
      mk_node ("C", &[], &["X"], &[]),  // C contains some other pid
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContainerOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::Independent);
  }

  #[test]
  fn containerof_via_merged_extra_id_preserved () {
    // The merge scenario. Before merge: P's pid was "P-old". Then
    // P-old got merged into P (P-old is now an extra_id of P).
    // C's contains list still has "P-old" (disk unchanged). The
    // view's parent is still titled/labelled as P (canonical).
    // Both sides must resolve through pid_of — C still contains
    // P in spirit.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("P", &["P-old"], &[],        &[]),
      mk_node ("C", &[],        &["P-old"], &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContainerOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::ContainerOf,
      "Extra_id-aliased parent pid should still satisfy the claim");
  }

  #[test]
  fn containerof_view_parent_is_acquiree_preserved () {
    // Mirror of the prior test from the parent side: view tree has
    // parent_id = "P-old" (the pre-merge pid), which is now an
    // extra_id of "P". C's contains has "P" directly. Resolve both
    // sides → both map to P.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("P", &["P-old"], &[],    &[]),
      mk_node ("C", &[],        &["P"], &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("P-old"), src (), "P" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContainerOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::ContainerOf,
      "When view parent id is the acquiree pid, the claim should \
       still hold via pid_of resolution on the parent side");
  }

  #[test]
  fn contentof_indefinitive_parent_false_claim_flipped () {
    // Parent is indefinitive; its contains list does NOT include C.
    // So C's ContentOf claim is false.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("P", &[], &[], &[]),   // P.contains empty
      mk_node ("C", &[], &[], &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContentOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::Independent);
  }

  #[test]
  fn contentof_definitive_parent_skipped () {
    // Parent is DEFINITIVE; the check is skipped regardless of whether
    // the graph agrees (save just redefined P's contains to match
    // the buffer, so asking the graph would be circular). Claim preserved.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("P", &[], &[], &[]),  // P.contains empty in the in-Rust graph
      mk_node ("C", &[], &[], &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_definitive_viewnode (id ("P"), src (),
                              "P" . to_string (), None) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContentOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::ContentOf,
      "Definitive parent: ContentOf claim is never flipped here");
  }

  #[test]
  fn independent_always_preserved () {
    // Independent birth is never touched.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("P", &[], &[], &[]),
      mk_node ("C", &[], &[], &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::Independent) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::Independent);
  }

  // ===== 3x2 matrix: moving a {containerOf, contentOf, linksTo}
  // child under a NEW parent where the relationship to the new
  // parent does / doesn't hold. =====================================
  //
  // Each test sets up:
  //   - p_old: the child's original parent (the relationship it
  //     was originally marked with).
  //   - p_new: the new parent it has been moved under.
  //   - The view viewforest has the child under p_new.
  //   - The graph sometimes has a relationship from the child to
  //     p_new ("holds"), and sometimes doesn't ("no longer holds").

  // ---- containerOf ------------------------------------------------

  #[test]
  fn moved_containerof_relationship_holds_preserved () {
    // Child C once had birth=containerOf under p_old (C.contains had
    // p_old). User moved C under p_new. Test case: C's contains
    // ALSO includes p_new → the containerOf claim still holds for
    // the new parent.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("p_old", &[], &[],                   &[]),
      mk_node ("p_new", &[], &[],                   &[]),
      mk_node ("C",     &[], &["p_old", "p_new"],   &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContainerOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::ContainerOf,
      "Moved containerOf child still contains its new parent — keep");
  }

  #[test]
  fn moved_containerof_relationship_broken_flipped () {
    // Same setup but C's contains only has p_old, not p_new. Moving
    // C under p_new broke the claim.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("p_old", &[], &[],        &[]),
      mk_node ("p_new", &[], &[],        &[]),
      mk_node ("C",     &[], &["p_old"], &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContainerOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::Independent,
      "Moved containerOf child no longer contains new parent — flip");
  }

  // ---- contentOf (indefinitive parent on both sides) -------------

  #[test]
  fn moved_contentof_indef_parent_relationship_holds_preserved () {
    // Child C was content of an indefinitive p_old. Moved under
    // indefinitive p_new. Test case: p_new.contains ALSO includes C.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("p_old", &[], &["C"], &[]),
      mk_node ("p_new", &[], &["C"], &[]), // p_new really contains C
      mk_node ("C",     &[], &[],    &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContentOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::ContentOf,
      "Moved contentOf: indef p_new actually contains C — keep");
  }

  #[test]
  fn moved_contentof_indef_parent_relationship_broken_flipped () {
    // Same setup but p_new.contains does NOT include C.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("p_old", &[], &["C"], &[]),
      mk_node ("p_new", &[], &[],    &[]), // p_new doesn't contain C
      mk_node ("C",     &[], &[],    &[]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::ContentOf) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::Independent,
      "Moved contentOf: indef p_new doesn't contain C — flip");
  }

  // ---- linksTo ---------------------------------------------------

  #[test]
  fn moved_linksto_relationship_holds_preserved () {
    // C's body linked to both p_old and p_new. User moved C under
    // p_new; the linksTo claim holds.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("p_old", &[], &[], &[]),
      mk_node ("p_new", &[], &[], &[]),
      mk_node ("C",     &[], &[], &["p_old", "p_new"]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::LinksTo) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::LinksTo,
      "Moved linksTo: C still links to p_new — keep");
  }

  #[test]
  fn moved_linksto_relationship_broken_flipped () {
    // C's body linked only to p_old, not p_new.
    let graph : InRustGraph = graph_with (vec! [
      mk_node ("p_old", &[], &[], &[]),
      mk_node ("p_new", &[], &[], &[]),
      mk_node ("C",     &[], &[], &["p_old"]),
    ]);
    let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = viewforest . root () . id ();
    let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
      mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                                Birth::ContentOf) ) . id ();
    let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
      mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                                Birth::LinksTo) ) . id ();

    validate_birth_relationships (&mut viewforest, &graph);

    assert_eq! (true_child_birth (&viewforest, c_id), Birth::Independent,
      "Moved linksTo: C doesn't link to p_new — flip");
  }
}
