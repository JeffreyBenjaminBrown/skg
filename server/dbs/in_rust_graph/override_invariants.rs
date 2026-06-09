use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SourceName};

use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OverrideInvariantViolation {
  UnknownSource {
    node: ID,
    source: SourceName },
  MultipleUserOwnedOverriders {
    overridden: ID,
    overriders: Vec<ID> },
  UserOwnedOverrideChain {
    first_overrider: ID,
    middle_overrider: ID,
    final_targets: Vec<ID> }}

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
/// - no chains: "X overrides Y + Y overrides Z" is invalid
///   if X and Y are user-owned (regardless of where Z is from)
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
    for target in node . overrides_view_of . or_default () {
      // Override targets can be written as primary IDs or extra IDs. Validate against the effective primary PID, matching how the in-Rust graph and TypeDB relationship creation resolve edges.
      let overridden : ID =
        graph . pid_of (target)
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

  for (first_pid, first_node) in graph . nodes . iter () {
    // No chains constraint.
    let Some (first_is_user_owned) = user_owns_node (
      config, first_pid, &first_node . source, &mut violations )
    else { continue; };
    if ! first_is_user_owned { continue; }
    for middle_id in first_node . overrides_view_of . or_default () {
      let Some (middle_pid) = graph . pid_of (middle_id) else {
        continue; };
      let Some (middle_node) = graph . nodes . get (&middle_pid) else {
        continue; };
      let Some (middle_is_user_owned) = user_owns_node (
        config, &middle_pid, &middle_node . source, &mut violations )
      else { continue; };
      if ! middle_is_user_owned { continue; }
      // The chain violation is the user-owned middle node having any
      // override target at all.  Resolve final targets for reporting,
      // but keep unknown raw IDs visible so the user can repair them.
      let final_targets : Vec<ID> =
        middle_node . overrides_view_of . or_default ()
        . iter ()
        . map ( |target|
          graph . pid_of (target)
          . unwrap_or_else ( || target . clone () ) )
        . collect ();
      if ! final_targets . is_empty () {
        violations . push (
          OverrideInvariantViolation::UserOwnedOverrideChain {
            first_overrider: first_pid . clone (),
            middle_overrider: middle_pid,
            final_targets,
          } ); }}}
  dedup_violations (violations) }

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
/// - no chains: the new edge T → B can sit at either end of a chain, so
///   check both: T as the chain's middle (some user-owned node
///   overrides T, while T overrides anything) and T as the chain's
///   first (T overrides a user-owned B that itself overrides anything).
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
      node . overrides_view_of . or_default () . iter ()
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
    if ! targets . is_empty () {
      // no chains, T as the middle: first(user-owned) → T → targets
      for first in user_owned_overriders_of (config, graph, pid) {
        violations . push (
          OverrideInvariantViolation::UserOwnedOverrideChain {
            first_overrider  : first,
            middle_overrider : pid . clone (),
            final_targets    : targets . clone (), } ); } }
    for middle_pid in &targets {
      // no chains, T as the first: T → middle(user-owned) → its targets
      let Some (middle_node) = graph . nodes . get (middle_pid)
        else { continue; };
      let Some (middle_is_user_owned) = user_owns_node (
        config, middle_pid, &middle_node . source, &mut violations )
        else { continue; };
      if ! middle_is_user_owned { continue; }
      let final_targets : Vec<ID> =
        middle_node . overrides_view_of . or_default () . iter ()
        . map ( |t| graph . pid_of (t)
                . unwrap_or_else ( || t . clone () ) )
        . collect ();
      if ! final_targets . is_empty () {
        violations . push (
          OverrideInvariantViolation::UserOwnedOverrideChain {
            first_overrider  : pid . clone (),
            middle_overrider : middle_pid . clone (),
            final_targets, } ); } } }
  dedup_violations (violations) }

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
      OverrideInvariantViolation::UserOwnedOverrideChain {
        first_overrider,
        middle_overrider,
        final_targets,
      } => {
        let list : String =
          final_targets . iter ()
          . map ( |id| id . to_string () )
          . collect::<Vec<String>> ()
          . join (", ");
        lines . push (format!(
          "* user-owned override chain: {} overrides {}, which overrides {}",
          first_overrider, middle_overrider, list ));
      }}}
  lines . join ("\n") }
