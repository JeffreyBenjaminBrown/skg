use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::override_resolution::resolve_override;
use crate::types::misc::{ID, SkgConfig, SourceName, members_of};

use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OverrideInvariantViolation {
  UnknownSource {
    node: ID,
    source: SourceName },
  MultipleUserOwnedOverriders {
    overridden: ID,
    overriders: Vec<ID> },
  UserOwnedOverrideCycle {
    // The cycle's nodes in walk order, canonicalized (rotated to the
    // min pid) so the same cycle reached from different entry points
    // collapses to one violation.
    cycle: Vec<ID> }}

pub fn error_unless_override_invariants_hold (
  config : &SkgConfig,
  graph  : &InRustGraph,
) -> Result<(), String> {
  let violations : Vec<OverrideInvariantViolation> =
    validate_override_invariants (config, &graph);
  if violations . is_empty () {
    Ok (())
  } else { Err (
    format_override_invariant_violations (&violations))
  }}

/// User-owned data must adhere to two constraints:
/// - monogamy: No node is overridden by more than one user-owned node.
/// - no cycles: following the user-owned override edges out of a node
///   must never return to that node. Linear chains (D overrides C
///   overrides N, all owned) are allowed.
pub fn validate_override_invariants (
  config : &SkgConfig,
  graph  : &InRustGraph,
) -> Vec<OverrideInvariantViolation> {
  let mut violations : Vec<OverrideInvariantViolation> = Vec::new ();

  // First pass: collect automatic replacement candidates by the
  // resolved node they replace.  Only user-owned overriders count for
  // automatic replacement; foreign override edges remain graph facts
  // but do not participate in substitution.
  let mut user_owned_overriders_by_overridden
    : HashMap<ID, Vec<ID>> =
      HashMap::new ();

  for (pid, node) in graph . nodes . iter () {
    let Some (user_owns_node) = user_owns_node (
      // A missing source means we cannot know whether this node's override edges should be automatic. We record such an offense in 'violations' rather than guessing "foreign".
      config, pid, &node . source, &mut violations )
    else { continue; };
    if ! user_owns_node { continue; }
    for target in members_of ( node . overrides_view_of . or_default () ) {
      // Override targets can be written as primary IDs or extra IDs. Validate against the effective primary PID, matching how the in-Rust graph and TypeDB relationship creation resolve edges.
      let overridden : ID =
        graph . pid_of (&target)
        . unwrap_or_else ( || target . clone () );
      user_owned_overriders_by_overridden
        . entry (overridden)
        . or_default ()
        . push (pid . clone ()); }}

  for (overridden, overriders) in user_owned_overriders_by_overridden {
    // Monogamy constraint. Sorting keeps the error stable.
    if overriders . len () > 1 {
      let mut overriders : Vec<ID> = overriders;
      overriders . sort ();
      violations . push (
        OverrideInvariantViolation::MultipleUserOwnedOverriders {
          overridden,
          overriders, } ); }}

  { // No cycles constraint. For each user-owned node, walk its
    // user-owned overrider edges (the shared 'resolve_override' walk,
    // ungated so it is source-set-independent); a returned cycle is a
    // violation. Different entry points into the same cycle yield
    // rotations of one trail, so canonicalizing collapses them.
    let mut seen_cycles : HashSet<Vec<ID>> = HashSet::new ();
    for (pid, node) in graph . nodes . iter () {
      let user_owned : bool =
        config . sources . get (&node . source)
        . map ( |sc| sc . user_owns_it )
        . unwrap_or (false); // unknown source reported by pass 1 above
      if ! user_owned { continue; }
      let resolution = resolve_override (config, graph, None, pid);
      if resolution . cycle_detected {
        let canonical : Vec<ID> =
          canonicalize_cycle (resolution . cycle);
        if seen_cycles . insert (canonical . clone ()) {
          violations . push (
            OverrideInvariantViolation::UserOwnedOverrideCycle {
              cycle : canonical } ); }}}}
  dedup_violations (violations) }

/// Rotate a cycle's nodes so the minimum pid leads, preserving cycle
/// order. The same directed cycle reached from different entry points
/// is a rotation of one sequence, so this canonical form lets dedup
/// collapse them.
fn canonicalize_cycle (
  cycle : Vec<ID>,
) -> Vec<ID> {
  if cycle . is_empty () { return cycle; }
  let min_index : usize =
    cycle . iter () . enumerate ()
    . min_by ( |(_, a), (_, b)| a . cmp (b) )
    . map ( |(i, _)| i )
    . unwrap ();
  let mut out : Vec<ID> = Vec::with_capacity ( cycle . len () );
  out . extend_from_slice ( &cycle [min_index ..] );
  out . extend_from_slice ( &cycle [.. min_index] );
  out }

/// Save-time override-invariant check, scoped to the nodes a save
/// touched. The rest of the graph satisfied the invariants before the
/// save and is unchanged, so a new violation can only involve an
/// override edge incident to a touched node (whose 'overrides_view_of'
/// or 'source' changed). Checking each touched node's edges therefore
/// suffices, and the cost is O(touched) rather than O(whole graph).
///
/// 'graph' must be the post-save (simulated) graph, with its inverse
/// indexes ('overriders_of') already reflecting the save.
///
/// For each touched user-owned node T:
/// - monogamy: for each X that T overrides, flag X if it now has more
///   than one user-owned overrider. Only this (target) end needs
///   checking — adding the edge T → X can only over-subscribe X.
/// - no cycles: walk the user-owned override edges out of T (the shared
///   'resolve_override' walk, ungated). A save that closes a user-owned
///   cycle leaves a written (touched) node on the cycle, and monogamy
///   makes the walk functional, so the walk from that T returns to a
///   repeat. Cost O(touched · chain length).
pub fn validate_touched_override_invariants (
  config  : &SkgConfig,
  graph   : &InRustGraph,
  touched : &HashSet<ID>,
) -> Vec<OverrideInvariantViolation> {
  let mut violations : Vec<OverrideInvariantViolation> = Vec::new ();
  for pid in touched {
    let Some (node) = graph . nodes . get (pid)
      // A touched id absent post-save (e.g. deleted) has no out-edges,
      // and deletions can only resolve violations, never create them.
      else { continue; };
    let Some (user_owns_pid) = user_owns_node (
      config, pid, &node . source, &mut violations )
      else { continue; };
    if ! user_owns_pid { continue; }
    let targets : Vec<ID> = // T's override targets, resolved to pids
      members_of ( node . overrides_view_of . or_default () ) . iter ()
      . map ( |t| graph . pid_of (t)
              . unwrap_or_else ( || t . clone () ) )
      . collect ();
    for overridden in &targets {
      // monogamy (target end only)
      let mut overriders : Vec<ID> =
        user_owned_overriders_of (config, graph, overridden);
      if overriders . len () > 1 {
        overriders . sort ();
        violations . push (
          OverrideInvariantViolation::MultipleUserOwnedOverriders {
            overridden : overridden . clone (),
            overriders, } ); } }
    { // no cycles: the functional, ungated walk out of this touched
      // user-owned node returns to a repeat iff the save put it on a
      // cycle.
      let resolution = resolve_override (config, graph, None, pid);
      if resolution . cycle_detected {
        violations . push (
          OverrideInvariantViolation::UserOwnedOverrideCycle {
            cycle : canonicalize_cycle (resolution . cycle) } ); } } }
  dedup_violations (violations) }

/// The single user-owned node (by pid) that already overrides
/// 'overridden', if any -- the monogamy pre-check a fork runs before
/// minting a new clone. Returns the first such overrider (monogamy
/// guarantees at most one in a valid graph). 'overridden' may be a
/// primary or extra id; it is resolved to a pid first, matching how the
/// graph and override edges resolve. Read against the LIVE graph before
/// the save, so a fork of an already-forked node is rejected with a
/// helpful "you already forked this; your clone is X" rather than the
/// raw MultipleUserOwnedOverriders crash at commit.
pub fn existing_user_owned_overrider_of (
  config     : &SkgConfig,
  graph      : &InRustGraph,
  overridden : &ID,
) -> Option<ID> {
  let pid : ID =
    graph . pid_of (overridden) . unwrap_or_else ( || overridden . clone () );
  user_owned_overriders_of (config, graph, &pid) . into_iter () . next () }

/// The user-owned nodes (by pid) that override 'overridden' (a pid), via
/// the in-Rust graph's 'overriders_of' inverse index. Overriders whose
/// source is unknown are treated as not-user-owned here: a touched
/// node's own unknown source is still reported by 'user_owns_node' at
/// the call site, and untouched neighbors are validated at init.
fn user_owned_overriders_of (
  config     : &SkgConfig,
  graph      : &InRustGraph,
  overridden : &ID,
) -> Vec<ID> {
  let mut result : Vec<ID> = Vec::new ();
  if let Some (overriders) = graph . overriders_of . get (overridden) {
    for overrider in overriders {
      if let Some (overrider_node) = graph . nodes . get (overrider) {
        if config . sources . get (&overrider_node . source)
          . map ( |sc| sc . user_owns_it )
          . unwrap_or (false)
        { result . push (overrider . clone ()); } } } }
  result }

/// Returns Some if it can determine the answer.
/// If it can't, adds to 'violations' and returns None.
fn user_owns_node (
  config     : &SkgConfig,
  pid        : &ID,
  source     : &SourceName,
  violations : &mut Vec<OverrideInvariantViolation>,
) -> Option<bool> {
  match config . sources . get (source) {
    Some (source_config) => Some (source_config . user_owns_it),
    None => {
      violations . push (
        OverrideInvariantViolation::UnknownSource {
          node: pid . clone (),
          source: source . clone (), } );
      None }}}

fn dedup_violations (
  violations : Vec<OverrideInvariantViolation>,
) -> Vec<OverrideInvariantViolation> {
  let mut out : Vec<OverrideInvariantViolation> = Vec::new ();
  for violation in violations {
    if ! out . contains (&violation) {
      out . push (violation); }}
  out }

pub fn format_override_invariant_violations (
  violations : &[OverrideInvariantViolation],
) -> String {
  let mut lines : Vec<String> = vec![
    "Override invariant validation failed:".to_string()
  ];
  for violation in violations {
    match violation {
      OverrideInvariantViolation::UnknownSource { node, source } => {
        lines . push (format!(
          "* node {} has unknown source {}", node, source ));
      }
      OverrideInvariantViolation::MultipleUserOwnedOverriders {
        overridden,
        overriders,
      } => {
        let list : String =
          overriders . iter ()
          . map ( |id| id . to_string () )
          . collect::<Vec<String>> ()
          . join (", ");
        lines . push (format!(
          "* node {} is overridden by user-owned nodes {}",
          overridden, list ));
      }
      OverrideInvariantViolation::UserOwnedOverrideCycle {
        cycle,
      } => {
        let arrow : String = { // a -> b -> ... -> a
          let mut nodes : Vec<String> =
            cycle . iter () . map ( |id| id . to_string () ) . collect ();
          if let Some (first) = cycle . first () {
            nodes . push ( first . to_string () ); }
          nodes . join (" -> ") };
        lines . push (format!(
          "* user-owned override cycle: {}", arrow ));
      }}}
  lines . join ("\n") }
