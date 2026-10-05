//! The in-Rust partial projection of the graph.
//!
//! A single 'InRustGraph' value holds every node (as a 'GraphnodeInRust') the
//! render / save pipeline can read from, plus inverse indexes for
//! every outbound relation and for extra_ids. It lives behind an
//! 'ArcSwap' so readers never block writers and writers never block
//! readers — writers clone via 'im''s structural sharing (O(log n)
//! per mutation) and atomically publish a new snapshot.

pub mod complete_validation;
pub mod containerward_role_tree;
pub mod id_resolution;
pub mod paths;
pub mod query;
pub mod stats;
pub mod internal_index_validation;
pub mod override_invariants;
pub mod override_resolution;
pub(crate) mod prepared_update;
pub mod relation_accessors;

use arc_swap::ArcSwap;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::types::misc::{ID, SkgRepoName, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::save::{NodeInstruction, DeleteNode, SaveNode};

/// The in-Rust-graph projection of the graph.
///
/// Values are 'GraphnodeInRust' — everything a 'Graphnode' has except
/// 'flags', plus 'links_to' parsed from body text.
///
/// The six inverse indexes mirror the five outbound relations and
/// the extra_ids list, so a reader can ask "who points at X?" in
/// O(1) / O(log n) lookups rather than walking every node.
#[derive(Clone, Debug)]
pub struct InRustGraph {
  pub nodes            : im::HashMap<ID, GraphnodeInRust>,
  /// 'X → {pids of nodes whose contains includes X}'
  pub contained_by     : im::HashMap<ID, im::HashSet<ID>>,
  /// 'X → {pids of nodes whose subscribes_to includes X}'
  pub subscribers_of   : im::HashMap<ID, im::HashSet<ID>>,
  /// 'X → {pids of nodes whose hides_from_its_subscriptions includes X}'
  pub hiders_of        : im::HashMap<ID, im::HashSet<ID>>,
  /// 'X → {pids of nodes whose overrides_view_of includes X}'
  pub overriders_of    : im::HashMap<ID, im::HashSet<ID>>,
  /// 'X → {pids of nodes whose links_to includes X}'
  pub mentioners_of     : im::HashMap<ID, im::HashSet<ID>>,
  /// Maps any of a node's extra_ids to that node's pid. Invariant:
  /// an extra_id is on at most one node at any visible snapshot.
  pub extra_id_to_pid  : im::HashMap<ID, ID>,
}

impl InRustGraph {
  pub fn new () -> Self {
    InRustGraph {
      nodes            : im::HashMap::new (),
      contained_by     : im::HashMap::new (),
      subscribers_of   : im::HashMap::new (),
      hiders_of        : im::HashMap::new (),
      overriders_of    : im::HashMap::new (),
      mentioners_of     : im::HashMap::new (),
      extra_id_to_pid  : im::HashMap::new (), } }

  /// Build from a slice of Graphnodes. Typically called at
  /// startup after reading all .skg files from disk.
  ///
  /// Two-pass, because canonical-keyed inverse indexes need to
  /// map each outbound relation's second member (see
  /// [[docs/data-model_technical.org]]) to its corresponding pid (which might
  /// be the id itself) via 'extra_id_to_pid' at index time. A
  /// single-pass load couldn't do this for a reference to an
  /// extra_id of a not-yet-loaded node. First pass populates
  /// 'extra_id_to_pid' only; second pass inserts nodes and builds
  /// inverse entries with full lookup available.
  pub fn from_graphnodes (completes: &[Graphnode]) -> Self {
    let mut g : InRustGraph = InRustGraph::new ();
    for c in completes {
      for extraid in c . normalized_extra_ids () {
        g . extra_id_to_pid . insert (
          extraid, c . pid . clone () ); } }
    for c in completes {
      let rust : GraphnodeInRust = GraphnodeInRust::from (c);
      g . nodes . insert ( rust . pid . clone (), rust ); }
    let identity : InRustGraph = g . clone ();
    for node in identity . nodes . values () {
      add_relationship_contributions (&mut g, node, &identity); }
    g }

  pub fn get (&self, pid: &ID) -> Option<&GraphnodeInRust> {
    self . nodes . get (pid) }

  pub fn len (&self) -> usize {
    self . nodes . len () }

  /// Resolve an ID (primary or extra) to the node's primary ID.
  /// Returns None if the ID is unknown.
  pub fn pid_of (&self, skgid: &ID) -> Option<ID> {
    if self . nodes . contains_key (skgid) {
      Some (skgid . clone ())
    } else {
      self . extra_id_to_pid . get (skgid) . cloned () } }

  /// Resolve an ID (primary or extra) to its '(pid, repo)'.
  /// Returns None if the ID is unknown.
  pub fn pid_and_skgrepo (&self, skgid: &ID) -> Option<(ID, SkgRepoName)> {
    let pid : ID = self . pid_of (skgid) ?;
    let node : &GraphnodeInRust = self . nodes . get (&pid) ?;
    Some ( ( pid, node . home_skgrepo . clone () ) ) }
}

/// Resolve a raw relationship ID to its inverse-index key under an explicit
/// identity snapshot. Unknown IDs remain their own keys.
fn canonical_key (
  identity : &InRustGraph,
  raw      : &ID,
) -> ID {
  identity . pid_of (raw) . unwrap_or_else (|| raw . clone ())
}

/// Add one recorder's five relationship contributions. Identity claims are
/// installed separately, before relationship indexing begins.
fn add_relationship_contributions (
  graph    : &mut InRustGraph,
  node     : &GraphnodeInRust,
  identity : &InRustGraph,
) {
  let pid : &ID = &node . pid;
  for second_member in members_of ( &node . contains ) {
    let key : ID = canonical_key (identity, &second_member);
    add_to_inverse_map (&mut graph . contained_by, &key, pid); }
  for second_member in members_of ( node . subscribes_to . or_default () ) {
    let key : ID = canonical_key (identity, &second_member);
    add_to_inverse_map (&mut graph . subscribers_of, &key, pid); }
  for second_member in members_of ( node . hides_from_its_subscriptions . or_default () ) {
    let key : ID = canonical_key (identity, &second_member);
    add_to_inverse_map (&mut graph . hiders_of, &key, pid); }
  for second_member in members_of ( node . overrides_view_of . or_default () ) {
    let key : ID = canonical_key (identity, &second_member);
    add_to_inverse_map (&mut graph . overriders_of, &key, pid); }
  for second_member in &node . links_to {
    let key : ID = canonical_key (identity, second_member);
    add_to_inverse_map (&mut graph . mentioners_of, &key, pid); } }

/// Narrow fixture helper for unit tests that construct `GraphnodeInRust` directly.
#[cfg(test)]
pub(crate) fn add_to_inverse_indexes (
  graph : &mut InRustGraph,
  node  : &GraphnodeInRust,
) {
  for extra in &node . extra_ids {
    graph . extra_id_to_pid . insert (
      extra . clone (), node . pid . clone ()); }
  let mut identity : InRustGraph = graph . clone ();
  identity . nodes . insert (node . pid . clone (), node . clone ());
  add_relationship_contributions (graph, node, &identity);
}

/// Remove a node's contributions from every inverse index. Used
/// during update (before inserting the new GraphnodeInRust) and during
/// delete.
///
/// The resolver is explicit because inherited entries must be removed under
/// base keys even after the batch's final identity claims are installed.
fn remove_relationship_contributions (
  graph    : &mut InRustGraph,
  node     : &GraphnodeInRust,
  identity : &InRustGraph,
) {
  let pid : &ID = &node . pid;
  for second_member in members_of ( &node . contains ) {
    let key : ID = canonical_key (identity, &second_member);
    remove_from_inverse_map (&mut graph . contained_by, &key, pid); }
  for second_member in members_of ( node . subscribes_to . or_default () ) {
    let key : ID = canonical_key (identity, &second_member);
    remove_from_inverse_map (&mut graph . subscribers_of, &key, pid); }
  for second_member in members_of ( node . hides_from_its_subscriptions . or_default () ) {
    let key : ID = canonical_key (identity, &second_member);
    remove_from_inverse_map (&mut graph . hiders_of, &key, pid); }
  for second_member in members_of ( node . overrides_view_of . or_default () ) {
    let key : ID = canonical_key (identity, &second_member);
    remove_from_inverse_map (&mut graph . overriders_of, &key, pid); }
  for second_member in &node . links_to {
    let key : ID = canonical_key (identity, second_member);
    remove_from_inverse_map (&mut graph . mentioners_of, &key, pid); } }

pub(crate) fn inbound_recorders_at (
  graph : &InRustGraph,
  key   : &ID,
) -> HashSet<ID> {
  let mut recorders : HashSet<ID> = HashSet::new ();
  for index in [
    &graph . contained_by,
    &graph . subscribers_of,
    &graph . hiders_of,
    &graph . overriders_of,
    &graph . mentioners_of,
  ] {
    if let Some (indexed) = index . get (key) {
      recorders . extend (indexed . iter () . cloned ()); }}
  recorders
}

fn coalesced_final_nodeInstructions (
  nodeInstructions : &[NodeInstruction],
) -> Vec<&NodeInstruction> {
  let mut by_pid : HashMap<&ID, (usize, &NodeInstruction)> = HashMap::new ();
  for (position, nodeInstruction) in nodeInstructions . iter () . enumerate () {
    let pid : &ID = match nodeInstruction {
      NodeInstruction::Save (SaveNode (node))       => &node . pid,
      NodeInstruction::Delete (DeleteNode { skgid, .. }) => skgid,
    };
    by_pid . insert (pid, (position, nodeInstruction)); }
  let mut positioned : Vec<(usize, &NodeInstruction)> =
    by_pid . into_values () . collect ();
  positioned . sort_by_key (|(position, _)| *position);
  positioned . into_iter () . map (|(_, nodeInstruction)| nodeInstruction) . collect ()
}

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

/// Apply a batch of NodeInstructions to an ordinary in-memory graph value.
///
/// This is the shared mutation path for the live graph update and for
/// save-time validation simulations.  Keep graph mutation semantics in
/// this helper so the validator asks the same "what graph would this
/// produce?" question as the real save path.
///
/// NodeInstructions have simultaneous, last-nodeInstruction-per-PID graph semantics.
/// Touched recorders and inbound recorders whose raw IDs change canonical target are
/// removed under the base resolver, then re-added under the complete final
/// resolver. This covers relationship edits, alias acquisition/transfer, deletion
/// rekeying, and body-derived text links with one rule.
pub fn apply_nodeInstructions_to_inRustGraph (
  candidate : &mut InRustGraph,
  node_defs : &[NodeInstruction],
) {
  let base : InRustGraph = candidate . clone ();
  let nodeInstructions : Vec<&NodeInstruction> =
    coalesced_final_nodeInstructions (node_defs);
  let touched_pids : HashSet<ID> = nodeInstructions . iter () . map (|nodeInstruction|
    match nodeInstruction {
      NodeInstruction::Save (SaveNode (node))       => node . pid . clone (),
      NodeInstruction::Delete (DeleteNode { skgid, .. }) => skgid . clone (),
    }) . collect ();
  let mut affected_skgids : HashSet<ID> = touched_pids . clone ();

  // Install the complete final identity state before rebuilding any forward
  // contribution. The inherited inverse maps are deliberately left in place
  // until affected recorders have been discovered from the base snapshot.
  for pid in &touched_pids {
    if let Some (old) = base . nodes . get (pid) {
      affected_skgids . extend (old . extra_ids . iter () . cloned ());
      for extra in &old . extra_ids {
        if candidate . extra_id_to_pid . get (extra) == Some (pid) {
          candidate . extra_id_to_pid . remove (extra); }} }
    candidate . nodes . remove (pid); }
  for nodeInstruction in &nodeInstructions {
    if let NodeInstruction::Save (SaveNode (node)) = nodeInstruction {
      let rust : GraphnodeInRust = GraphnodeInRust::from (node);
      affected_skgids . extend (rust . extra_ids . iter () . cloned ());
      candidate . nodes . insert (rust . pid . clone (), rust . clone ());
      for extra in &rust . extra_ids {
        candidate . extra_id_to_pid . insert (
          extra . clone (), rust . pid . clone ()); }} }

  let final_identity : InRustGraph = candidate . clone ();
  let mut recorders_to_reindex : HashSet<ID> = touched_pids;
  for raw in affected_skgids {
    let old_key : ID = canonical_key (&base, &raw);
    let final_key : ID = canonical_key (&final_identity, &raw);
    if old_key != final_key {
      recorders_to_reindex . extend (inbound_recorders_at (&base, &old_key)); }}
  for recorder in &recorders_to_reindex {
    if let Some (old) = base . nodes . get (recorder) {
      remove_relationship_contributions (candidate, old, &base); }}
  for recorder in &recorders_to_reindex {
    if let Some (final_node) = final_identity . nodes . get (recorder) {
      add_relationship_contributions (candidate, final_node, &final_identity); }}
}

/// Check that in-Rust graph reflects the expected post-apply state
/// for a batch of nodeInstructions: every Save's pid is present in
/// in_rust_graph, and every Delete's id is absent. Used as a 'debug_assert!'
/// invariant guard at the top of 'update_views_after_save' to catch
/// pipeline-ordering regressions (someone reshuffles the pipeline so
/// rerender runs before prepared graph publication). Returns Ok (()) on
/// coherence, Err with the offending pid's detail otherwise. Never
/// panics — the caller wraps in 'debug_assert!' so release builds pay
/// no cost.
pub fn in_rust_graph_coherent_with_nodeInstructions_in (
  graph : &InRustGraph,
  nodeInstructions : &[NodeInstruction],
) -> Result<(), String> {
  for instr in nodeInstructions {
    match instr {
      NodeInstruction::Save (SaveNode (node)) => {
        if ! graph . nodes . contains_key (&node . pid) {
          return Err ( format! (
            "Save instruction pid {} absent from the in-Rust graph",
            node . pid )); }}
      NodeInstruction::Delete (DeleteNode { skgid, .. }) => {
        if graph . nodes . contains_key (skgid) {
          return Err ( format! (
            "Delete instruction id {} still present in the in-Rust graph",
            skgid )); }} } }
  Ok (( )) }

/// Server-wide handle to the shared graph. Readers call
/// '.load_full()' to snap a consistent 'Arc<InRustGraph>'; writers
/// build a new 'Arc<InRustGraph>' (using 'im''s cheap clone +
/// structural-sharing mutations) and '.store()' it atomically.
pub type InRustGraphHandle = Arc<ArcSwap<InRustGraph>>;

/// Construct a fresh handle wrapping the given graph.
pub fn new_handle (graph: InRustGraph) -> InRustGraphHandle {
  Arc::new ( ArcSwap::from ( Arc::new (graph) )) }
