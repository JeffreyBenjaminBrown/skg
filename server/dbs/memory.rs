//! The in-Rust partial projection of the graph.
//!
//! A single 'InRustGraph' value holds every node (as a 'NodeRust') the
//! render / save pipeline can read from, plus inverse indexes for
//! every outbound relation and for extra_ids. It lives behind an
//! 'ArcSwap' so readers never block writers and writers never block
//! readers — writers clone via 'im''s structural sharing (O(log n)
//! per mutation) and atomically publish a new snapshot.

pub mod audit;
pub mod scheduled_audit;

use arc_swap::ArcSwap;
use std::sync::{Arc, OnceLock};

use crate::types::misc::{ID, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;
use crate::types::save::{DefineNode, DeleteNode, SaveNode};

/// Process-global handle to the in-Rust memory.
///
/// Set once, at server startup (see 'init_global_handle'). Read-only
/// afterwards. Functions on hot read paths (notably
/// 'pid_and_source_from_id') consult this handle to bypass TypeDB
/// without requiring every caller to thread a '&InRustGraph' parameter.
///
/// PITFALL: tests that don't initialize this will see 'None' here
/// and must fall back to whatever they were using before. That's the
/// reason the consulting callers check and degrade gracefully rather
/// than panicking.
static GLOBAL_HANDLE : OnceLock<InRustGraphHandle> = OnceLock::new ();

/// Set the process-global handle. Must be called exactly once, at
/// server startup, after the initial 'InRustGraph' has been built.
pub fn init_global_handle (handle: InRustGraphHandle) {
  GLOBAL_HANDLE . set (handle) . ok ()
    . expect ("GLOBAL_HANDLE initialized twice"); }

/// Snap the current in-Rust memory if the global handle has been
/// initialized; returns None otherwise. In the running server
/// 'init_global_handle' is called during startup before any request
/// is served, so None here indicates a test that bypassed startup
/// (or code running before startup completes).
pub fn snapshot_global () -> Option<Arc<InRustGraph>> {
  GLOBAL_HANDLE . get () . map ( |h| h . load_full () ) }

/// The in-memory projection of the graph.
///
/// Values are 'NodeRust' — everything a 'NodeComplete' has except
/// 'misc', plus 'textlinks_to' parsed from body text.
///
/// The six inverse indexes mirror the five outbound relations and
/// the extra_ids list, so a reader can ask "who points at X?" in
/// O(1) / O(log n) lookups rather than walking every node.
#[derive(Clone, Debug)]
pub struct InRustGraph {
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

impl InRustGraph {
  pub fn new () -> Self {
    InRustGraph {
      nodes            : im::HashMap::new (),
      contained_by     : im::HashMap::new (),
      subscribers_of   : im::HashMap::new (),
      hiders_of        : im::HashMap::new (),
      replacements_of  : im::HashMap::new (),
      textlinks_in     : im::HashMap::new (),
      extra_id_to_pid  : im::HashMap::new (), } }

  /// Build from a slice of NodeCompletes. Typically called at
  /// startup after reading all .skg files from disk.
  ///
  /// Two-pass, because canonical-keyed inverse indexes need to
  /// map each outbound relation's second member (see
  /// [[../../schema.tql]]) to its corresponding pid (which might
  /// be the id itself) via 'extra_id_to_pid' at index time. A
  /// single-pass load couldn't do this for a reference to an
  /// extra_id of a not-yet-loaded node. First pass populates
  /// 'extra_id_to_pid' only; second pass inserts nodes and builds
  /// inverse entries with full lookup available.
  pub fn from_nodecompletes (completes: &[NodeComplete]) -> Self {
    let mut g : InRustGraph = InRustGraph::new ();
    for c in completes {
      for extraid in &c . extra_ids {
        g . extra_id_to_pid . insert (
          extraid . clone (), c . pid . clone () ); } }
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

  /// Resolve an ID (primary or extra) to its '(pid, source)'.
  /// Returns None if the ID is unknown.
  pub fn pid_and_source (&self, id: &ID) -> Option<(ID, SourceName)> {
    let pid : ID = self . pid_of (id) ?;
    let node : &NodeRust = self . nodes . get (&pid) ?;
    Some ( ( pid, node . source . clone () ) ) }
}

/// Resolves any known ID (primary pid or extra_id) to the primary
/// pid. Primary pids pass through unchanged; extra_ids are mapped via
/// 'extra_id_to_pid'. IDs unknown to memory fall through as-is, so
/// callers can index-under-unknown without having to check first.
fn id_to_pid (g: &InRustGraph, id: &ID) -> ID {
  g . extra_id_to_pid . get (id) . cloned ()
    . unwrap_or_else ( || id . clone () ) }

/// Add a node's contributions to every inverse index. Idempotent if
/// the node is not already contributing.
///
/// For each outbound relation ('contains', 'subscribes_to', etc.) on
/// 'node', 'node' is the first member and each ID in the list is the
/// second member (see [[../../schema.tql]]). We index each
/// second-member ID's corresponding pid (which might be the id
/// itself, if the id isn't an extra_id of any other node) mapped to
/// 'node.pid'.
///
/// PITFALL: the 'extra_id_to_pid' update happens FIRST, so the
/// loops below see this node's newly-acquired aliases when they look
/// up corresponding pids. This matters in a merge batch: if a
/// subsequent neighbor Save references an acquiree pid that's now an
/// extra_id of this node, 'id_to_pid' during that neighbor's add
/// maps the reference to this node's canonical pid.
fn add_to_inverse_indexes (g: &mut InRustGraph, node: &NodeRust) {
  let pid : &ID = &node . pid;
  for extra in &node . extra_ids {
    g . extra_id_to_pid . insert ( extra . clone (), pid . clone () ); }
  for second_member in &node . contains {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    add_to_inverse_map (&mut g . contained_by, &corresponding_pid, pid); }
  for second_member in node . subscribes_to . or_default () {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    add_to_inverse_map (&mut g . subscribers_of, &corresponding_pid, pid); }
  for second_member in node . hides_from_its_subscriptions . or_default () {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    add_to_inverse_map (&mut g . hiders_of, &corresponding_pid, pid); }
  for second_member in node . overrides_view_of . or_default () {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    add_to_inverse_map (&mut g . replacements_of, &corresponding_pid, pid); }
  for second_member in &node . textlinks_to {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    add_to_inverse_map (&mut g . textlinks_in, &corresponding_pid, pid); } }

/// Remove a node's contributions from every inverse index. Used
/// during update (before inserting the new NodeRust) and during
/// delete.
///
/// Each outbound relation's second member ID (see [[../../schema.tql]])
/// is mapped to its corresponding pid (which might be the id itself)
/// via the CURRENT 'extra_id_to_pid'. Correct because the prior
/// 'add_to_inverse_indexes' for this node used the same table, and
/// 'apply_definenodes' only grows that table — never shrinks —
/// so the key each second-member's contribution originally landed
/// under is still reachable through the same lookup now.
fn remove_from_inverse_indexes (g: &mut InRustGraph, node: &NodeRust) {
  let pid : &ID = &node . pid;
  for second_member in &node . contains {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    remove_from_inverse_map (&mut g . contained_by, &corresponding_pid, pid); }
  for second_member in node . subscribes_to . or_default () {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    remove_from_inverse_map (&mut g . subscribers_of, &corresponding_pid, pid); }
  for second_member in node . hides_from_its_subscriptions . or_default () {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    remove_from_inverse_map (&mut g . hiders_of, &corresponding_pid, pid); }
  for second_member in node . overrides_view_of . or_default () {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    remove_from_inverse_map (&mut g . replacements_of, &corresponding_pid, pid); }
  for second_member in &node . textlinks_to {
    let corresponding_pid : ID = id_to_pid (g, second_member);
    remove_from_inverse_map (&mut g . textlinks_in, &corresponding_pid, pid); }
  for extra in &node . extra_ids {
    // Only remove if it still points at this pid — defensive against
    // cross-batch races where a later save has already claimed the
    // extra_id.
    if g . extra_id_to_pid . get (extra) == Some (pid) {
      g . extra_id_to_pid . remove (extra); } } }

/// When a Save promotes an ID to an extra_id of 'target_pid', any
/// inverse-index entries previously keyed under that ID need to move
/// to 'target_pid' — otherwise a subsequent "who points at target_pid?"
/// query wouldn't find the pre-promotion references.
///
/// Applied for each new extra_id (not already present in old_rust).
/// Idempotent when 'inverse_X[extra_id]' is empty.
fn migrate_inverse_entries_for_new_extraids (
  g            : &mut InRustGraph,
  target_pid   : &ID,
  new_rust     : &NodeRust,
  old_rust_opt : Option<&NodeRust>,
) {
  fn migrate_one (
    map  : &mut im::HashMap<ID, im::HashSet<ID>>,
    from : &ID,
    to   : &ID,
  ) {
    if let Some (entries) = map . remove (from) {
      let mut merged : im::HashSet<ID> =
        map . get (to) . cloned () . unwrap_or_default ();
      merged . extend (entries);
      map . insert ( to . clone (), merged ); } }
  let old_extraids : std::collections::HashSet<&ID> = match old_rust_opt {
    Some (old) => old . extra_ids . iter () . collect (),
    None       => std::collections::HashSet::new (), };
  for extraid in &new_rust . extra_ids {
    if old_extraids . contains (extraid) { continue; }
    if extraid == target_pid { continue; }
    migrate_one (&mut g . contained_by, extraid, target_pid);
    migrate_one (&mut g . subscribers_of, extraid, target_pid);
    migrate_one (&mut g . hiders_of, extraid, target_pid);
    migrate_one (&mut g . replacements_of, extraid, target_pid);
    migrate_one (&mut g . textlinks_in, extraid, target_pid); } }

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
/// Clones the current InRustGraph (cheap due to 'im''s structural
/// sharing), applies Save/Delete mutations along with their inverse-
/// index updates, and publishes the new snapshot via ArcSwap.
///
/// Per-Save ordering: remove-old-inverse → migrate-inverse-for-new-
/// extraids → insert-new-node → add-new-inverse. The migration step
/// is load-bearing for merges: when an acquirer gains the acquiree's
/// pid as an extra_id, any inverse-index entries keyed under the
/// acquiree pid (pre-merge references from neighbors) need to move
/// to the acquirer pid. The add step then runs with extra_id_to_pid
/// already updated, so subsequent Saves in the same batch look up
/// corresponding pids through the new alias.
///
/// PITFALL (monotonic alias acquisition): we rely on extra_id_to_pid
/// only growing within a batch, never retracting. 'remove_from_inverse_
/// indexes' looks up each second-member's (see [[../../schema.tql]]) PID
/// through the current alias map, which finds
/// the key the old add originally landed under because nothing revoked
/// that mapping between then and now. If revocation is ever added as
/// a real operation, the remove-side lookup needs to be rethought.
///
/// PITFALL (extra_id revocation unsupported): a Save whose new
/// NodeRust /drops/ an extra_id that the old NodeRust had is not
/// handled gracefully. Neighbor raw references to the dropped
/// extra_id would fall through 'id_to_pid' (since 'id_to_pid' would
/// no longer know the extra_id) and land under the raw id —
/// inconsistent with TypeDB. No production path exercises this today;
/// if a user feature ever needs it, a dedicated migration (symmetric
/// to the acquire path) would be required.
///
/// Called from the save pipeline after the filesystem write has
/// succeeded.
pub fn apply_definenodes (
  graph     : &InRustGraphHandle,
  node_defs : &[DefineNode],
) {
  let old : Arc<InRustGraph> = graph . load_full ();
  let mut new_graph : InRustGraph = (*old) . clone ();
  for instr in node_defs {
    match instr {
      DefineNode::Save (SaveNode (node)) => {
        let new_rust : NodeRust = NodeRust::from (node);
        let old_rust_opt : Option<NodeRust> =
          new_graph . nodes . get (&new_rust . pid) . cloned ();
        if let Some (old_rust) = &old_rust_opt {
          remove_from_inverse_indexes (&mut new_graph, old_rust); }
        migrate_inverse_entries_for_new_extraids (
          &mut new_graph, &new_rust . pid,
          &new_rust, old_rust_opt . as_ref () );
        new_graph . nodes . insert (
          new_rust . pid . clone (), new_rust . clone () );
        add_to_inverse_indexes (&mut new_graph, &new_rust); }
      DefineNode::Delete (DeleteNode { id, .. }) => {
        if let Some (old_rust) = new_graph . nodes . get (id) . cloned () {
          remove_from_inverse_indexes (&mut new_graph, &old_rust); }
        new_graph . nodes . remove (id); } } }
  graph . store ( Arc::new (new_graph) ); }

/// Server-wide handle to the shared graph. Readers call
/// '.load_full()' to snap a consistent 'Arc<InRustGraph>'; writers
/// build a new 'Arc<InRustGraph>' (using 'im''s cheap clone +
/// structural-sharing mutations) and '.store()' it atomically.
pub type InRustGraphHandle = Arc<ArcSwap<InRustGraph>>;

/// Construct a fresh handle wrapping the given graph.
pub fn new_handle (graph: InRustGraph) -> InRustGraphHandle {
  Arc::new ( ArcSwap::from ( Arc::new (graph) )) }
