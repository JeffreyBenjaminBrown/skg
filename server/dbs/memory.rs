//! The in-Rust partial projection of the graph.
//!
//! A single 'Graph' value holds every node (as a 'NodeRust') the
//! render / save pipeline can read from, plus inverse indexes for
//! every outbound relation and for extra_ids. It lives behind an
//! 'ArcSwap' so readers never block writers and writers never block
//! readers — writers clone via 'im''s structural sharing (O(log n)
//! per mutation) and atomically publish a new snapshot.

use arc_swap::ArcSwap;
use std::sync::Arc;

use crate::types::misc::ID;
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;
use crate::types::save::{DefineNode, DeleteNode, SaveNode};

/// The in-memory projection of the graph.
///
/// Values are 'NodeRust' — everything a 'NodeComplete' has except
/// 'misc', plus 'textlinks_to' parsed from body text.
///
/// The six inverse indexes mirror the five outbound relations and
/// the extra_ids list, so a reader can ask "who points at X?" in
/// O(1) / O(log n) lookups rather than walking every node.
#[derive(Clone, Debug)]
pub struct Graph {
  pub nodes            : im::HashMap<ID, NodeRust>,
  /// 'X → {pids of nodes whose contains includes X}'
  pub contained_by     : im::HashMap<ID, im::HashSet<ID>>,
  /// 'X → {pids of nodes whose subscribes_to includes X}'
  pub subscribers_of   : im::HashMap<ID, im::HashSet<ID>>,
  /// 'X → {pids of nodes whose hides_from_its_subscriptions includes X}'
  pub hiders_of        : im::HashMap<ID, im::HashSet<ID>>,
  /// 'X → {pids of nodes whose overrides_view_of includes X}'
  pub replacements_of  : im::HashMap<ID, im::HashSet<ID>>,
  /// 'X → {pids of nodes whose textlinks_to includes X}'
  pub textlinks_in     : im::HashMap<ID, im::HashSet<ID>>,
  /// Maps any of a node's extra_ids to that node's pid. Invariant:
  /// an extra_id is on at most one node at any visible snapshot.
  pub extra_id_to_pid  : im::HashMap<ID, ID>,
}

impl Graph {
  pub fn new () -> Self {
    Graph {
      nodes            : im::HashMap::new (),
      contained_by     : im::HashMap::new (),
      subscribers_of   : im::HashMap::new (),
      hiders_of        : im::HashMap::new (),
      replacements_of  : im::HashMap::new (),
      textlinks_in     : im::HashMap::new (),
      extra_id_to_pid  : im::HashMap::new (), } }

  /// Build from a slice of NodeCompletes. Typically called at
  /// startup after reading all .skg files from disk.
  pub fn from_nodecompletes (completes: &[NodeComplete]) -> Self {
    let mut g : Graph = Graph::new ();
    for c in completes {
      let rust : NodeRust = NodeRust::from (c);
      g . nodes . insert ( rust . pid . clone (), rust . clone () );
      add_to_inverse_indexes (&mut g, &rust); }
    g }

  pub fn get (&self, pid: &ID) -> Option<&NodeRust> {
    self . nodes . get (pid) }

  pub fn len (&self) -> usize {
    self . nodes . len () }

  /// Resolve an ID (primary or extra) to the node's primary ID.
  /// Returns None if the ID is unknown.
  pub fn pid_of (&self, id: &ID) -> Option<ID> {
    if self . nodes . contains_key (id) {
      Some (id . clone ())
    } else {
      self . extra_id_to_pid . get (id) . cloned () } }
}

/// Add a node's contributions to every inverse index. Idempotent if
/// the node is not already contributing.
fn add_to_inverse_indexes (g: &mut Graph, node: &NodeRust) {
  let pid : &ID = &node . pid;
  for peer in &node . contains {
    add_to_inverse_map (&mut g . contained_by, peer, pid); }
  for peer in node . subscribes_to . or_default () {
    add_to_inverse_map (&mut g . subscribers_of, peer, pid); }
  for peer in node . hides_from_its_subscriptions . or_default () {
    add_to_inverse_map (&mut g . hiders_of, peer, pid); }
  for peer in node . overrides_view_of . or_default () {
    add_to_inverse_map (&mut g . replacements_of, peer, pid); }
  for peer in &node . textlinks_to {
    add_to_inverse_map (&mut g . textlinks_in, peer, pid); }
  for extra in &node . extra_ids {
    g . extra_id_to_pid . insert ( extra . clone (), pid . clone () ); } }

/// Remove a node's contributions from every inverse index. Used
/// during update (before inserting the new NodeRust) and during
/// delete.
fn remove_from_inverse_indexes (g: &mut Graph, node: &NodeRust) {
  let pid : &ID = &node . pid;
  for peer in &node . contains {
    remove_from_inverse_map (&mut g . contained_by, peer, pid); }
  for peer in node . subscribes_to . or_default () {
    remove_from_inverse_map (&mut g . subscribers_of, peer, pid); }
  for peer in node . hides_from_its_subscriptions . or_default () {
    remove_from_inverse_map (&mut g . hiders_of, peer, pid); }
  for peer in node . overrides_view_of . or_default () {
    remove_from_inverse_map (&mut g . replacements_of, peer, pid); }
  for peer in &node . textlinks_to {
    remove_from_inverse_map (&mut g . textlinks_in, peer, pid); }
  for extra in &node . extra_ids {
    // Only remove if it still points at this pid — defensive against
    // cross-batch races where a later save has already claimed the
    // extra_id.
    if g . extra_id_to_pid . get (extra) == Some (pid) {
      g . extra_id_to_pid . remove (extra); } } }

fn add_to_inverse_map (
  map   : &mut im::HashMap<ID, im::HashSet<ID>>,
  key   : &ID,
  value : &ID,
) {
  let mut set : im::HashSet<ID> =
    map . get (key) . cloned () . unwrap_or_default ();
  set . insert ( value . clone () );
  map . insert ( key . clone (), set ); }

fn remove_from_inverse_map (
  map   : &mut im::HashMap<ID, im::HashSet<ID>>,
  key   : &ID,
  value : &ID,
) {
  if let Some (mut set) = map . get (key) . cloned () {
    set . remove (value);
    if set . is_empty () { map . remove (key); }
    else                 { map . insert ( key . clone (), set ); } } }

/// Apply a batch of DefineNodes to the shared graph atomically.
/// Clones the current Graph (cheap due to 'im''s structural
/// sharing), applies Save/Delete mutations along with their inverse-
/// index updates, and publishes the new snapshot via ArcSwap.
///
/// For each Save of an existing node, the pre-save NodeRust is read
/// /before/ the new one is inserted so the inverse-index diff is
/// correct: old entries are stripped, new ones added. For each
/// Delete, the pre-delete NodeRust is read and its contributions are
/// stripped before it's removed from the node map.
///
/// PITFALL: During a merge batch the ordering is Delete(acquiree) →
/// Save(updated_acquirer). Between those two instructions the
/// 'extra_id_to_pid' entry for 'acquiree.pid' briefly points at
/// nothing. That never-visible state is fine because apply_definenodes
/// processes the batch as a single atomic COW + publish — no reader
/// ever sees the intermediate map.
///
/// Called from the save pipeline after the filesystem write has
/// succeeded.
pub fn apply_definenodes (
  graph     : &GraphHandle,
  node_defs : &[DefineNode],
) {
  let old : Arc<Graph> = graph . load_full ();
  let mut new_graph : Graph = (*old) . clone ();
  for instr in node_defs {
    match instr {
      DefineNode::Save (SaveNode (node)) => {
        let new_rust : NodeRust = NodeRust::from (node);
        if let Some (old_rust) = new_graph . nodes . get (&new_rust . pid) . cloned () {
          remove_from_inverse_indexes (&mut new_graph, &old_rust); }
        new_graph . nodes . insert (
          new_rust . pid . clone (), new_rust . clone () );
        add_to_inverse_indexes (&mut new_graph, &new_rust); }
      DefineNode::Delete (DeleteNode { id, .. }) => {
        if let Some (old_rust) = new_graph . nodes . get (id) . cloned () {
          remove_from_inverse_indexes (&mut new_graph, &old_rust); }
        new_graph . nodes . remove (id); } } }
  graph . store ( Arc::new (new_graph) ); }

/// Server-wide handle to the shared graph. Readers call
/// '.load_full()' to snap a consistent 'Arc<Graph>'; writers
/// build a new 'Arc<Graph>' (using 'im''s cheap clone +
/// structural-sharing mutations) and '.store()' it atomically.
pub type GraphHandle = Arc<ArcSwap<Graph>>;

/// Construct a fresh handle wrapping the given graph.
pub fn new_handle (graph: Graph) -> GraphHandle {
  Arc::new ( ArcSwap::from ( Arc::new (graph) )) }
