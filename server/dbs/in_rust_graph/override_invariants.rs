//! The rules an owned note's forks, contents and hides must keep.
//!
//! A note *forks* another when it records a 'forks' relationship to
//! it. An owned note may not
//! - fork itself ('SelfFork'), contain itself ('SelfContains') or hide
//!   itself ('SelfHides'); a link to itself is fine;
//! - both fork and contain one note ('ForkeeContained'): a note it
//!   forks is the same note as it, and containing that is like
//!   containing itself (a dangling ID, naming no note, breaks none of
//!   these pair rules);
//! - contain a note that forks it, or fork a note that contains it
//!   ('ForkerContained'), for the same reason.
//! Any number of notes, owned or not, may fork one note, and forks may
//! form cycles: mirror substitution ('mirrors.rs') has no cycle case.
//!
//! The whole graph is checked at startup and rebuild
//! ('validate_override_invariants'); a save checks only the notes whose
//! answer it can change ('derive_affected_override_scope'). A save
//! that leaves a violation is refused. Startup and rebuild refuse only
//! a violation that owned records alone make: one that a foreign
//! note's record takes part in ('foreign_note_taking_part') is a
//! warning there, since only a hand edit of the owned note could
//! otherwise mend what a foreign author did.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};

use std::collections::HashSet;

/// The relations whose records the rules read, besides hides, which
/// only 'SelfHides' reads.
const RELATIONS_THE_RULES_PAIR : [NodeRelation; 2] =
  [ NodeRelation::Contains,
    NodeRelation::Forks ];

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OverrideInvariantViolation {
  UnknownSkgrepo {
    node: ID,
    skgrepo: SkgrepoName },
  SelfFork {
    node : ID },
  SelfContains {
    node : ID },
  SelfHides {
    node : ID },
  /// 'forker' forks 'forkee' and contains it too.
  ForkeeContained {
    forker : ID,
    forkee : ID },
  /// 'forkee' contains 'forker', which forks it.
  ForkerContained {
    forker : ID,
    forkee : ID }}

/// Every violation among the graph's owned notes.
pub fn validate_override_invariants (
  config : &SkgConfig,
  graph  : &InRustGraph,
) -> Vec<OverrideInvariantViolation> {
  let violations : Vec<OverrideInvariantViolation> =
    graph . nodes . keys ()
    . flat_map ( |pid| violations_of_note (config, graph, pid) )
    . collect ();
  dedup_violations (violations) }

/// The pids whose answer to the rules can differ between 'base' and
/// 'candidate', given that the save touched 'touched_pids' and changed
/// which note answers to each of 'affected_gnids':
/// - the touched notes;
/// - each note a touched note contains or forks: the touched note may
///   be foreign, and the owned partner the one the rules refuse (an
///   owned E forking a note that comes to contain it);
/// - each note that contains or forks a touched note: a note created
///   where a dangling ID stood is the first answer to that ID (an
///   untouched R forking and containing it then breaks a pair rule);
/// - each note whose contents or forks name an ID a nodeMerge re-keys
///   (an untouched R that forks A and contains B comes to fork and
///   contain one note when B becomes an ID of A).
/// A note's own hides change only when it is touched.
pub fn derive_affected_override_scope (
  base           : &InRustGraph,
  candidate      : &InRustGraph,
  touched_pids   : &HashSet<ID>,
  affected_gnids : &HashSet<ID>,
) -> HashSet<ID> {
  let mut pids : HashSet<ID> = touched_pids . clone ();
  for pid in touched_pids {
    for relation in RELATIONS_THE_RULES_PAIR {
      pids . extend ( outbound_keys (candidate, pid, relation) );
      pids . extend ( candidate . inbound_pids_for_relation (pid, relation) ); }}
  for raw in affected_gnids {
    let old_key : ID = base . pid_else_raw_id (raw);
    let final_key : ID = candidate . pid_else_raw_id (raw);
    if old_key == final_key { continue; }
    for relation in RELATIONS_THE_RULES_PAIR {
      pids . extend (
        base . inbound_pids_for_relation (&old_key, relation)); }}
  pids }

/// The violations among 'pids', in 'graph'. A valid base makes
/// violations elsewhere irrelevant to this save.
pub fn validate_affected_override_invariants (
  config : &SkgConfig,
  graph  : &InRustGraph,
  pids   : &HashSet<ID>,
) -> Vec<OverrideInvariantViolation> {
  let violations : Vec<OverrideInvariantViolation> =
    pids . iter ()
    . flat_map ( |pid| violations_of_note (config, graph, pid) )
    . collect ();
  dedup_violations (violations) }

/// What the rules say of the note 'pid': nothing when the graph lacks
/// it or it is foreign, and 'UnknownSkgrepo' when its home is not
/// configured, since then nobody knows whether it is owned.
fn violations_of_note (
  config : &SkgConfig,
  graph  : &InRustGraph,
  pid    : &ID,
) -> Vec<OverrideInvariantViolation> {
  let Some (node) = graph . nodes . get (pid)
    else { return Vec::new (); };
  match config . skgrepos . get (&node . home_skgrepo) {
    None => vec! [ OverrideInvariantViolation::UnknownSkgrepo {
      node    : pid . clone (),
      skgrepo : node . home_skgrepo . clone () } ],
    Some (skgrepo) if ! skgrepo . owned => Vec::new (),
    Some (_) => violations_of_owned_note (graph, pid), }}

fn violations_of_owned_note (
  graph : &InRustGraph,
  pid   : &ID,
) -> Vec<OverrideInvariantViolation> {
  let forkees : HashSet<ID> =
    outbound_keys (graph, pid, NodeRelation::Forks);
  let contents : HashSet<ID> =
    outbound_keys (graph, pid, NodeRelation::Contains);
  let hidden : HashSet<ID> =
    outbound_keys (graph, pid, NodeRelation::Hides);
  let forkers : HashSet<ID> =
    graph . inbound_pids_for_relation (pid, NodeRelation::Forks)
    . into_iter () . collect ();
  let containers : HashSet<ID> =
    graph . inbound_pids_for_relation (pid, NodeRelation::Contains)
    . into_iter () . collect ();
  let mut violations : Vec<OverrideInvariantViolation> = Vec::new ();
  if forkees . contains (pid) {
    violations . push (
      OverrideInvariantViolation::SelfFork { node : pid . clone () } ); }
  if contents . contains (pid) {
    violations . push (
      OverrideInvariantViolation::SelfContains { node : pid . clone () } ); }
  if hidden . contains (pid) {
    violations . push (
      OverrideInvariantViolation::SelfHides { node : pid . clone () } ); }
  for forkee in forkees . iter () . filter ( |forkee|
    // A dangling ID names no note, so nothing says it is the same
    // as anything: only the self-rules read the note's own IDs.
    *forkee != pid && graph . nodes . contains_key (*forkee) )
  { if contents . contains (forkee) {
      violations . push (
        OverrideInvariantViolation::ForkeeContained {
          forker : pid . clone (),
          forkee : forkee . clone () } ); }
    if containers . contains (forkee) {
      violations . push (
        OverrideInvariantViolation::ForkerContained {
          forker : pid . clone (),
          forkee : forkee . clone () } ); }}
  for forker in forkers . iter () . filter ( |forker| *forker != pid ) {
    if contents . contains (forker) {
      violations . push (
        OverrideInvariantViolation::ForkerContained {
          forker : forker . clone (),
          forkee : pid . clone () } ); }}
  violations }

/// The foreign note, if any, whose record takes part in 'violation'.
/// The self-rules and 'ForkeeContained' read only the owned note's
/// own records. 'ForkerContained' reads the forker's fork and the
/// forkee's contents, and the rules check it from either note, so
/// either may be foreign.
pub fn foreign_note_taking_part (
  config    : &SkgConfig,
  graph     : &InRustGraph,
  violation : &OverrideInvariantViolation,
) -> Option<ID> {
  let is_foreign = |pid : &ID| -> bool {
    graph . nodes . get (pid) . is_some_and ( |node|
      ! config . skgrepo_is_owned (&node . home_skgrepo) ) };
  match violation {
    OverrideInvariantViolation::ForkerContained { forker, forkee } =>
      [ forker, forkee ] . into_iter ()
      . find ( |pid| is_foreign (pid) )
      . cloned (),
    OverrideInvariantViolation::UnknownSkgrepo { .. }
      | OverrideInvariantViolation::SelfFork { .. }
      | OverrideInvariantViolation::SelfContains { .. }
      | OverrideInvariantViolation::SelfHides { .. }
      | OverrideInvariantViolation::ForkeeContained { .. } => None, }}

/// The members 'pid' records under 'relation', each as the pid of the
/// note it names, else as the raw ID: how the graph's recorderward
/// relmaps key them.
fn outbound_keys (
  graph    : &InRustGraph,
  pid      : &ID,
  relation : NodeRelation,
) -> HashSet<ID> {
  graph . outbound_gnids_for_relation (pid, relation) . iter ()
    . map ( |gnid| graph . pid_else_raw_id (gnid) )
    . collect () }

fn dedup_violations (
  mut violations : Vec<OverrideInvariantViolation>,
) -> Vec<OverrideInvariantViolation> {
  violations . sort_by_key (|violation| format! ("{violation:?}"));
  let mut out : Vec<OverrideInvariantViolation> = Vec::new ();
  for violation in violations {
    if ! out . contains (&violation) {
      out . push (violation); }}
  out }

/// One violation, as a sentence the user can act on.
pub fn describe_override_invariant_violation (
  violation : &OverrideInvariantViolation,
) -> String {
  match violation {
    OverrideInvariantViolation::UnknownSkgrepo { node, skgrepo } =>
      format! ("node {} has unknown repo {}", node, skgrepo),
    OverrideInvariantViolation::SelfFork { node } =>
      format! ("node {} forks itself; a note may not fork itself", node),
    OverrideInvariantViolation::SelfContains { node } =>
      format! ("node {} contains itself; a note may not contain itself", node),
    OverrideInvariantViolation::SelfHides { node } =>
      format! ("node {} hides itself; a note may not hide itself", node),
    OverrideInvariantViolation::ForkeeContained { forker, forkee } =>
      format! ("node {} both forks and contains node {}; a note may not contain a note it forks", forker, forkee),
    OverrideInvariantViolation::ForkerContained { forker, forkee } =>
      format! ("node {} contains node {}, which forks it; a note may not contain a note that forks it, nor fork a note that contains it", forkee, forker),
  }}

/// A violation that 'foreign' takes part in, as a startup warning.
pub fn describe_violation_a_foreign_note_takes_part_in (
  violation : &OverrideInvariantViolation,
  foreign   : &ID,
) -> String {
  format! (
    "{}. Node {} is foreign, so the server starts anyway, but a save of the other, owned note is refused while this holds",
    describe_override_invariant_violation (violation), foreign ) }
