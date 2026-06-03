use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SourceName};

use std::collections::HashMap;

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
