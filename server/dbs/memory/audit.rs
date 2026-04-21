//! Consistency audit: diff the in-Rust memory against TypeDB.
//!
//! For every node in the in-Rust memory, query TypeDB directly (bypassing
//! the memory shortcut) for each of the five outbound relations and each
//! of the corresponding inverse-role queries, and compare to NodeRust's
//! fields / the inverse indexes. Any difference is a 'Mismatch'. An
//! empty 'Vec<Mismatch>' means memory and TypeDB agree for every node.
//!
//! Intended for small test fixtures (< 100 nodes). Running this against
//! the full 29k-node production corpus issues 10 queries per node and
//! takes minutes — not suitable for CI.

use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

use crate::dbs::memory::InRustGraph;
use crate::dbs::typedb::relationships::OUTBOUND_RELATIONSHIP_TYPES;
use crate::dbs::typedb::search::find_related_nodes_for_one_id;
use crate::types::misc::ID;

/// A single disagreement between the in-Rust memory and TypeDB, for
/// a particular node in a particular relation in a particular role.
/// The audit produces zero or more of these per node per pass: a node
/// might have no mismatches (everything agrees), or several (e.g. a
/// 'contains' mismatch for the container role AND a 'subscribes'
/// mismatch for the subscribee role).
#[derive(Debug, Clone)]
pub struct Mismatch {
  /// The node being audited. Memory and TypeDB disagree about one of
  /// /this/ node's relationships. ('pid' is primary ID — the node's
  /// canonical identifier, as distinct from any extra_ids.)
  pub pid       : ID,
  /// Which of the five outbound relation types this disagreement is
  /// about. Takes a value from the first column of
  /// 'OUTBOUND_RELATIONSHIP_TYPES' in
  /// [[../../typedb/relationships.rs]] — so one of "contains",
  /// "textlinks_to", "subscribes", "hides_from_its_subscriptions",
  /// or "overrides_view_of".
  pub relation  : &'static str,
  /// Which role 'pid' plays in the relation. Takes a value from the
  /// second or third column of 'OUTBOUND_RELATIONSHIP_TYPES' in
  /// [[../../typedb/relationships.rs]] — specifically, one of the
  /// two roles associated with 'relation'. For 'contains' that's
  /// "container" or "contained"; for 'subscribes' that's "subscriber"
  /// or "subscribee"; etc. (See [[../../../schema.tql]] for the
  /// canonical schema definition.)
  ///
  /// The memory-side answer that's compared against TypeDB depends
  /// on which role this is:
  ///   If 'pid' plays the /first-column role/ (container, source,
  ///     subscriber, hider, replacement) — memory's answer is the
  ///     forward field on pid's NodeRust (e.g. NodeRust.contains),
  ///     resolved through extra_id_to_pid to match TypeDB's
  ///     has_extra_id-aware view.
  ///   If 'pid' plays the /second-column role/ (contained, dest,
  ///     subscribee, hidden, replaced) — memory's answer is the
  ///     inverse-index lookup (e.g. graph.contained_by[pid]),
  ///     already canonical-pid-keyed.
  pub role : &'static str,
  /// What the in-Rust memory says the peer set is (after any needed
  /// canonicalization — see the role-specific descriptions above).
  /// The set may be empty, which is a valid memory answer meaning
  /// "no known peers in this relation in this role."
  pub memory    : HashSet<ID>,
  /// What TypeDB says the peer set is. Obtained by issuing the
  /// has_extra_id-aware query for 'pid' in the complementary role
  /// (the one 'pid' is NOT playing here). Also may be empty.
  pub typedb    : HashSet<ID>,
}

/// Audit every node in memory against TypeDB. Returns the list of
/// mismatches (empty on success).
///
/// Vocabulary: each outbound relation (see [[../../schema.tql]]) is an
/// ordered pair (first member, second member). For 'contains' that's
/// (container, contained); for 'subscribes' it's (subscriber,
/// subscribee); etc. The node being audited is the first member;
/// the IDs in 'NodeRust.contains', 'NodeRust.subscribes_to', etc.,
/// are the second members.
///
/// Memory and TypeDB have an asymmetry worth naming:
/// - /Inverse/ indexes in memory are canonical-pid-keyed, matching
///   TypeDB's has_extra_id-aware query view. Direct 'inverse_X[pid]'
///   lookup compares apples-to-apples.
/// - /Forward/ fields on NodeRust remain raw (mirroring what the
///   .skg file literally has) — so we map each second-member ID
///   through 'pid_of' to its corresponding pid (which might be the
///   id itself) before comparing to TypeDB's answer (which already
///   reflects has_extra_id lookups).
pub async fn audit_memory_against_typedb (
  graph   : &InRustGraph,
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result < Vec<Mismatch>, Box<dyn Error> > {
  let mut mismatches : Vec<Mismatch> = Vec::new ();
  for pid in graph . nodes . keys () {
    mismatches . extend (
      audit_one_node (graph, db_name, driver, pid) . await ? ); }
  Ok (mismatches) }

/// Audit a single node against TypeDB. Issues ten queries (five
/// relations × two roles) and returns the Mismatches for this pid.
/// Empty vec means memory and TypeDB agree about every
/// relation/role for this pid.
///
/// Used by the paced scheduled audit (which calls this per-pid in
/// batches) and also by 'audit_memory_against_typedb' (which
/// iterates all pids).
pub async fn audit_one_node (
  graph   : &InRustGraph,
  db_name : &str,
  driver  : &TypeDBDriver,
  pid     : &ID,
) -> Result < Vec<Mismatch>, Box<dyn Error> > {
  let node : &crate::types::nodes::rust::NodeRust =
    match graph . nodes . get (pid) {
      Some (n) => n,
      None     => return Ok ( Vec::new () ) };
  let mut mismatches : Vec<Mismatch> = Vec::new ();
  for (relation, subject_role, object_role) in OUTBOUND_RELATIONSHIP_TYPES {
    { // pid plays the first-column role ('subject_role'): memory's
      // answer comes from NodeRust's forward field (raw), resolved
      // through pid_of to match TypeDB's has_extra_id-aware view.
      let memory_set : HashSet<ID> =
        outbound_second_members_from_memory (node, relation) . iter ()
        . map ( |id| graph . pid_of (id) . unwrap_or (id . clone ()) )
        . collect ();
      let typedb_set : HashSet<ID> =
        find_related_nodes_for_one_id (
          db_name, driver, pid,
          relation, subject_role, object_role ) . await ?;
      if memory_set != typedb_set {
        mismatches . push ( Mismatch {
          pid       : pid . clone (),
          relation,
          role      : subject_role,
          memory    : memory_set,
          typedb    : typedb_set, } ); } }
    { // pid plays the second-column role ('object_role'): memory's
      // answer is the canonical-pid-keyed inverse-index lookup.
      let memory_set : HashSet<ID> =
        inverse_first_members_from_memory (graph, pid, relation);
      let typedb_set : HashSet<ID> =
        find_related_nodes_for_one_id (
          db_name, driver, pid,
          relation, object_role, subject_role ) . await ?;
      if memory_set != typedb_set {
        mismatches . push ( Mismatch {
          pid       : pid . clone (),
          relation,
          role      : object_role,
          memory    : memory_set,
          typedb    : typedb_set, } ); } } }
  Ok (mismatches) }

/// IDs appearing as the second member of 'node''s outbound edges in
/// the named relation (see [[../../schema.tql]]).
fn outbound_second_members_from_memory (
  node     : &crate::types::nodes::rust::NodeRust,
  relation : &str,
) -> HashSet<ID> {
  match relation {
    "contains"                     => node . contains . iter () . cloned () . collect (),
    "subscribes"                   => node . subscribes_to . or_default () . iter () . cloned () . collect (),
    "hides_from_its_subscriptions" => node . hides_from_its_subscriptions . or_default () . iter () . cloned () . collect (),
    "overrides_view_of"            => node . overrides_view_of . or_default () . iter () . cloned () . collect (),
    "textlinks_to"                 => node . textlinks_to . iter () . cloned () . collect (),
    _ => HashSet::new (),
  } }

/// pids of nodes that play the first-member role in the named
/// relation with 'pid' as the second member (see [[../../schema.tql]]).
fn inverse_first_members_from_memory (
  graph    : &InRustGraph,
  pid      : &ID,
  relation : &str,
) -> HashSet<ID> {
  let map = match relation {
    "contains"                     => &graph . contained_by,
    "subscribes"                   => &graph . subscribers_of,
    "hides_from_its_subscriptions" => &graph . hiders_of,
    "overrides_view_of"            => &graph . replacements_of,
    "textlinks_to"                 => &graph . textlinks_in,
    _ => return HashSet::new (),
  };
  map . get (pid)
    . map ( |s| s . iter () . cloned () . collect () )
    . unwrap_or_default () }

/// Format a list of mismatches for log output.
pub fn format_mismatches (mismatches : &[Mismatch]) -> String {
  if mismatches . is_empty () {
    return "No mismatches between in-Rust memory and TypeDB." . to_string (); }
  let mut s : String = format! (
    "{} mismatch(es) between in-Rust memory and TypeDB:\n",
    mismatches . len () );
  for m in mismatches {
    s . push_str ( & format! (
      "  pid={} relation={} role={} memory={:?} typedb={:?}\n",
      m . pid, m . relation, m . role,
      m . memory, m . typedb ) ); }
  s }
