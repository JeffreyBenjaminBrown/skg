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
use crate::dbs::typedb::search::find_related_nodes_for_one_id;
use crate::types::misc::ID;

/// The five outbound-relation (relation, subject_role, object_role)
/// triples we audit. 'subject' plays the "active" end (first
/// relationship member in OUTBOUND_RELATIONSHIP_TYPES); 'object' plays
/// the "passive" end.
const OUTBOUND_RELATIONS : &[(&str, &str, &str)] = &[
  ("contains",                     "container",   "contained"),
  ("subscribes",                   "subscriber",  "subscribee"),
  ("hides_from_its_subscriptions", "hider",       "hidden"),
  ("overrides_view_of",            "replacement", "replaced"),
  ("textlinks_to",                 "source",      "dest"),
];

/// One disagreement between memory and TypeDB for one (node, relation)
/// pair. 'direction' is "outbound" or "inverse"; 'relation' names the
/// TypeDB relation type.
#[derive(Debug, Clone)]
pub struct Mismatch {
  pub pid       : ID,
  pub relation  : &'static str,
  pub direction : &'static str, // "outbound" | "inverse"
  pub memory    : HashSet<ID>,
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
  for (pid, node) in graph . nodes . iter () {
    for (relation, subject_role, object_role) in OUTBOUND_RELATIONS {
      { // Outbound: NodeRust forward fields are raw; map each
        // second-member ID to its corresponding pid (which might be
        // the id itself) via pid_of before comparing.
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
            direction : "outbound",
            memory    : memory_set,
            typedb    : typedb_set, } ); } }
      { // Inverse: canonical-pid-keyed, direct lookup.
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
            direction : "inverse",
            memory    : memory_set,
            typedb    : typedb_set, } ); } } } }
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
      "  pid={} relation={} direction={} memory={:?} typedb={:?}\n",
      m . pid, m . relation, m . direction,
      m . memory, m . typedb ) ); }
  s }
