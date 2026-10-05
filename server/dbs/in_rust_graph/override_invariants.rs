use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::override_resolution::resolve_override;
use crate::types::misc::{ID, SkgConfig, SkgRepoName, members_of};

use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct OverrideCheckScope {
  pub skgrepos : HashSet<ID>,
  pub targets : HashSet<ID>,
}

pub(crate) struct AffectedOverrideValidation {
  pub(crate) violations  : Vec<OverrideInvariantViolation>,
  pub(crate) chain_steps : usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OverrideInvariantViolation {
  UnknownSkgRepo {
    node: ID,
    skgrepo: SkgRepoName },
  MultipleOwnedOverriders {
    overridden: ID,
    overriders: Vec<ID> },
  OwnedOverrideCycle {
    // The cycle's nodes in walk order, canonicalized (rotated to the
    // min pid) so the same cycle reached from different entry points
    // collapses to one violation.
    cycle: Vec<ID> }}

/// Owned data must adhere to two constraints:
/// - monogamy: No node is overridden by more than one owned node.
/// - no cycles: following the owned override relationships out of a node
///   must never return to that node. Linear chains (D overrides C
///   overrides N, all owned) are allowed.
pub fn validate_override_invariants (
  config : &SkgConfig,
  graph  : &InRustGraph,
) -> Vec<OverrideInvariantViolation> {
  let mut violations : Vec<OverrideInvariantViolation> = Vec::new ();

  // First pass: collect automatic replacement candidates by the
  // resolved node they replace.  Only owned overriders count for
  // automatic replacement; foreign override relationships remain graph facts
  // but do not participate in substitution.
  let mut owned_overriders_by_overridden
    : HashMap<ID, Vec<ID>> =
      HashMap::new ();

  for (pid, node) in graph . nodes . iter () {
    let Some (node_is_owned) = node_is_owned (
      // A missing skgrepo means we cannot know whether this node's override relationships should be automatic. We record such an offense in 'violations' rather than guessing "foreign".
      config, pid, &node . home_skgrepo, &mut violations )
    else { continue; };
    if ! node_is_owned { continue; }
    for target in members_of ( node . overrides . or_default () ) {
      // Override targets can be primary or extra IDs. Validate against the
      // effective primary PID, matching graph relationship resolution.
      let overridden : ID =
        graph . pid_of (&target)
        . unwrap_or_else ( || target . clone () );
      owned_overriders_by_overridden
        . entry (overridden)
        . or_default ()
        . push (pid . clone ()); }}

  for (overridden, overriders) in owned_overriders_by_overridden {
    // Monogamy constraint. Sorting keeps the error stable.
    if overriders . len () > 1 {
      let mut overriders : Vec<ID> = overriders;
      overriders . sort ();
      violations . push (
        OverrideInvariantViolation::MultipleOwnedOverriders {
          overridden,
          overriders, } ); }}

  { // No cycles constraint. For each owned node, walk its
    // owned overrider relationships (the shared 'resolve_override' walk,
    // ungated so it is repo-set-independent); a returned cycle is a
    // violation. Different entry points into the same cycle yield
    // rotations of one trail, so canonicalizing collapses them.
    let mut seen_cycles : HashSet<Vec<ID>> = HashSet::new ();
    for (pid, node) in graph . nodes . iter () {
      let owned : bool =
        config . skgrepos . get (&node . home_skgrepo)
        . map ( |sc| sc . owned )
        . unwrap_or (false); // unknown skgrepo reported by pass 1 above
      if ! owned { continue; }
      let resolution = resolve_override (config, graph, None, pid);
      if resolution . cycle_detected {
        let canonical : Vec<ID> =
          canonicalize_cycle (resolution . cycle);
        if seen_cycles . insert (canonical . clone ()) {
          violations . push (
            OverrideInvariantViolation::OwnedOverrideCycle {
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

/// Derive every override skgrepo and target whose invariant truth can change
/// between two valid graph snapshots. Canonicalization changes pull in
/// untouched inbound overriders, which is the case a touched-only check misses
/// during node merge.
pub fn derive_affected_override_scope (
  base            : &InRustGraph,
  candidate       : &InRustGraph,
  touched_pids    : &HashSet<ID>,
  affected_skgids : &HashSet<ID>,
) -> OverrideCheckScope {
  let mut skgrepos : HashSet<ID> = touched_pids . clone ();
  for raw in affected_skgids {
    let old_key : ID = base . pid_of (raw)
      . unwrap_or_else (|| raw . clone ());
    let final_key : ID = candidate . pid_of (raw)
      . unwrap_or_else (|| raw . clone ());
    if old_key == final_key { continue; }
    for (graph, key) in [
      (base, &old_key), (base, &final_key),
      (candidate, &old_key), (candidate, &final_key),
    ] {
      if let Some (overriders) = graph . overriders_of . get (key) {
        skgrepos . extend (overriders . iter () . cloned ()); }} }

  let mut targets : HashSet<ID> = HashSet::new ();
  for skgrepo in &skgrepos {
    if let Some (node) = base . nodes . get (skgrepo) {
      targets . extend (
        members_of (node . overrides . or_default ()) . into_iter ()
          . map (|raw| base . pid_of (&raw) . unwrap_or (raw))); }
    if let Some (node) = candidate . nodes . get (skgrepo) {
      targets . extend (
        members_of (node . overrides . or_default ()) . into_iter ()
          . map (|raw| candidate . pid_of (&raw) . unwrap_or (raw))); }}
  OverrideCheckScope { skgrepos, targets }
}

/// Check monogamy at affected targets and acyclicity from affected owned
/// skgrepos. A valid base makes violations elsewhere irrelevant to this delta.
pub fn validate_affected_override_invariants (
  config : &SkgConfig,
  graph  : &InRustGraph,
  scope  : &OverrideCheckScope,
) -> Vec<OverrideInvariantViolation> {
  validate_affected_override_invariants_with_counts (config, graph, scope)
    . violations
}

pub(crate) fn validate_affected_override_invariants_with_counts (
  config : &SkgConfig,
  graph  : &InRustGraph,
  scope  : &OverrideCheckScope,
) -> AffectedOverrideValidation {
  let mut violations : Vec<OverrideInvariantViolation> = Vec::new ();
  let mut chain_steps : usize = 0;
  for target in &scope . targets {
    let mut overriders : Vec<ID> =
      owned_overriders_of (config, graph, target);
    if overriders . len () > 1 {
      overriders . sort ();
      violations . push (
        OverrideInvariantViolation::MultipleOwnedOverriders {
          overridden : target . clone (),
          overriders,
        }); }}
  for skgrepo in &scope . skgrepos {
    let Some (node) = graph . nodes . get (skgrepo) else { continue; };
    let Some (owned) = node_is_owned (
      config, skgrepo, &node . home_skgrepo, &mut violations)
      else { continue; };
    if ! owned { continue; }
    let resolution = resolve_override (config, graph, None, skgrepo);
    chain_steps += if resolution . cycle_detected {
      resolution . cycle . len ()
    } else {
      resolution . path . len ()
    };
    if resolution . cycle_detected {
      violations . push (
        OverrideInvariantViolation::OwnedOverrideCycle {
          cycle : canonicalize_cycle (resolution . cycle),
        }); }}
  AffectedOverrideValidation {
    violations : dedup_violations (violations),
    chain_steps,
  }
}

/// The single owned node (by pid) that already overrides
/// 'overridden', if any -- the monogamy pre-check a fork runs before
/// minting a new clone. Returns the first such overrider (monogamy
/// guarantees at most one in a valid graph). 'overridden' may be a
/// primary or extra id; it is resolved to a pid first, matching how the
/// graph and override relationships resolve. Read against the LIVE graph before
/// the save, so a fork of an already-forked node is rejected with a
/// helpful "you already forked this; your clone is X" rather than the
/// raw MultipleOwnedOverriders crash at skgsave-commit.
pub fn existing_owned_overrider_of (
  config     : &SkgConfig,
  graph      : &InRustGraph,
  overridden : &ID,
) -> Option<ID> {
  let pid : ID =
    graph . pid_of (overridden) . unwrap_or_else ( || overridden . clone () );
  owned_overriders_of (config, graph, &pid) . into_iter () . next () }

/// The owned nodes (by pid) that override 'overridden' (a pid), via
/// the in-Rust graph's 'overriders_of' inverse index. Overriders whose
/// skgrepo is unknown are treated as not-owned here: a touched
/// node's own unknown repo is still reported by 'node_is_owned' at
/// the call site, and untouched neighbors are validated at init.
fn owned_overriders_of (
  config     : &SkgConfig,
  graph      : &InRustGraph,
  overridden : &ID,
) -> Vec<ID> {
  let mut result : Vec<ID> = Vec::new ();
  if let Some (overriders) = graph . overriders_of . get (overridden) {
    for overrider in overriders {
      if let Some (overrider_node) = graph . nodes . get (overrider) {
        if config . skgrepos . get (&overrider_node . home_skgrepo)
          . map ( |sc| sc . owned )
          . unwrap_or (false)
        { result . push (overrider . clone ()); } } } }
  result }

/// Returns Some if it can determine the answer.
/// If it can't, adds to 'violations' and returns None.
fn node_is_owned (
  config     : &SkgConfig,
  pid        : &ID,
  skgrepo    : &SkgRepoName,
  violations : &mut Vec<OverrideInvariantViolation>,
) -> Option<bool> {
  match config . skgrepos . get (skgrepo) {
    Some (skgrepo_config) => Some (skgrepo_config . owned),
    None => {
      violations . push (
        OverrideInvariantViolation::UnknownSkgRepo {
          node: pid . clone (),
          skgrepo: skgrepo . clone (), } );
      None }}}

fn dedup_violations (
  mut violations : Vec<OverrideInvariantViolation>,
) -> Vec<OverrideInvariantViolation> {
  violations . sort_by_key (|violation| format! ("{violation:?}"));
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
      OverrideInvariantViolation::UnknownSkgRepo { node, skgrepo } => {
        lines . push (format!(
          "* node {} has unknown repo {}", node, skgrepo ));
      }
      OverrideInvariantViolation::MultipleOwnedOverriders {
        overridden,
        overriders,
      } => {
        let list : String =
          overriders . iter ()
          . map ( |skgid| skgid . to_string () )
          . collect::<Vec<String>> ()
          . join (", ");
        lines . push (format!(
          "* node {} is overridden by owned nodes {}",
          overridden, list ));
      }
      OverrideInvariantViolation::OwnedOverrideCycle {
        cycle,
      } => {
        let arrow : String = { // a -> b -> ... -> a
          let mut nodes : Vec<String> =
            cycle . iter () . map ( |skgid| skgid . to_string () ) . collect ();
          if let Some (first) = cycle . first () {
            nodes . push ( first . to_string () ); }
          nodes . join (" -> ") };
        lines . push (format!(
          "* owned override cycle: {}", arrow ));
      }}}
  lines . join ("\n") }
