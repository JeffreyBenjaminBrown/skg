//! Consistency audit: diff the in-Rust graph against TypeDB.
//!
//! For every node in the in-Rust in_rust_graph, query TypeDB directly (bypassing
//! the in-Rust graph shortcut) for each of the five outbound relations and each
//! of the corresponding inverse-role queries, and compare to NodeRust's
//! fields / the inverse indexes. Any difference is a 'Mismatch'. An
//! empty 'Vec<Mismatch>' means in-Rust graph and TypeDB agree for every node.
//!
//! Dangling outbound references (e.g. a '[[id:X]]' link in a body whose
//! target X has no node on disk) are NOT reported: TypeDB's
//! 'insert_relationship_from_list' silently drops such relations at
//! rebuild time (the '$to' match fails), and authored links to
//! unavailable (e.g., private)
//! nodes are routine, so in-Rust graph's own copy of them is
//! filtered out before the compare. See the filter in 'audit_one_node'.
//!
//! Intended for small test fixtures (< 100 nodes). Running this against
//! the full 29k-node production corpus issues 10 queries per node and
//! takes minutes — not suitable for CI.

use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{Transaction, TransactionType, TypeDBDriver};

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::typedb::relationships::OUTBOUND_RELATIONSHIP_TYPES;
use crate::dbs::typedb::search::find_related_nodes_for_one_primary_pid_in_tx;
use crate::dbs::typedb::sources::source_name_for_one_node_id_in_tx;
use crate::types::misc::{ID, SourceName, members_of};
use crate::types::nodes::rust::NodeRust;

/// A disagreement between the TypeDB and InRustGraph dbs.
#[derive(Debug, Clone)]
pub enum Mismatch {
  Relationship (RelationshipMismatch),
  Source (SourceMismatch),
  MissingTypedbNode { pid: ID } }

/// A single disagreement between the in-Rust graph and TypeDB, for
/// a particular node in a particular relation in a particular role.
#[derive(Debug, Clone)]
pub struct RelationshipMismatch {
  pub pid       : ID,
  pub relation : &'static str,
  /// The role of the node with the PID mentioned above.
  pub role     : &'static str,
  /// What the in-Rust graph says the other role-player set is (after any needed canonicalization — see the role-specific descriptions above). The set may be empty, which is a valid in-Rust graph answer meaning "no known other role players in this relation in this role."
  pub in_rust_graph : HashSet<ID>,
  /// Similarly, what TypeDB says.
  pub typedb        : HashSet<ID>,
}

#[derive(Debug, Clone)]
pub struct SourceMismatch {
  pub pid           : ID,
  pub in_rust_graph : SourceName,
  pub typedb        : SourceName,
}

/// Audit every node in the in-Rust graph against TypeDB. Returns the list of
/// mismatches (empty on success).
///
/// Vocabulary: each outbound relation (see [[../../schema.tql]]) is an
/// ordered pair (first member, second member). For 'contains' that's
/// (container, contained); for 'subscribes' it's (subscriber,
/// subscribee); etc. The node being audited is the first member;
/// the IDs in 'NodeRust.contains', 'NodeRust.subscribes_to', etc.,
/// are the second members.
///
/// The in-Rust graph and TypeDB have an asymmetry worth naming:
/// - /Inverse/ indexes in the in-Rust graph are canonical-pid-keyed, matching
///   TypeDB's has_extra_id-aware query view. Direct 'inverse_X[pid]'
///   lookup compares apples-to-apples.
/// - /Forward/ fields on NodeRust remain raw (mirroring what the
///   .skg file literally has) — so we map each second-member ID
///   through 'pid_of' to its corresponding pid (which might be the
///   id itself) before comparing to TypeDB's answer (which already
///   reflects has_extra_id lookups).
pub async fn audit_inrustgraph_against_typedb (
  graph   : &InRustGraph,
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result < Vec<Mismatch>, Box<dyn Error> > {
  let tx : Transaction = driver . transaction (
    db_name, TransactionType::Read ) . await ?;
  let mut mismatches : Vec<Mismatch> = Vec::new ();
  for pid in graph . nodes . keys () {
    mismatches . extend (
      audit_one_node (graph, &tx, pid) . await ? ); }
  Ok (mismatches) }

/// Audit a single node against TypeDB. Issues ten queries (five
/// relations × two roles) on the caller-provided read transaction
/// and returns the Mismatches for this pid. Empty vec means in-Rust graph
/// and TypeDB agree about every relation/role for this pid.
///
/// Takes '&Transaction' (not '&TypeDBDriver') so callers can
/// amortize transaction setup across many pids — opening a tx per
/// pid would dominate query cost. Used by the paced scheduled audit
/// (which opens one tx per batch) and by 'audit_inrustgraph_against_typedb'
/// (which opens one tx per whole corpus pass).
pub async fn audit_one_node (
  graph : &InRustGraph,
  tx    : &Transaction,
  pid   : &ID,
) -> Result < Vec<Mismatch>, Box<dyn Error> > {
  let node : &NodeRust =
    match graph . nodes . get (pid) {
      Some (n) => n,
      None     => return Ok ( Vec::new () ) };
  // Build the 10 expected in-Rust graph answers up front (cheap), then
  // fire all 10 TypeDB queries concurrently on the same tx. Results
  // come back in order via join_all. Previous version ran the 10
  // queries sequentially; parallelizing lets TypeDB use more of its
  // worker threads and cuts per-pid wall time.
  let mut in_rust_graph_answers : Vec<(HashSet<ID>, &'static str, &'static str)> =
    Vec::with_capacity (10);
  let mut query_futures = Vec::with_capacity (10);
  for (relation, subject_role, object_role) in OUTBOUND_RELATIONSHIP_TYPES {
    { // pid plays the first-column role ('subject_role')
      // 'filter_map' here drops outbound references whose target
      // doesn't resolve to a known node. TypeDB's
      // 'insert_relationship_from_list' silently drops those at
      // rebuild time (the 'match $to isa node, has id "..."' clause
      // finds nothing), so reporting them as mismatches is noise.
      // See TODO/problems.org 'Dangling outbound references ...'.
      let in_rust_graph_set : HashSet<ID> =
        outbound_second_members_from_in_rust_graph (node, relation) . iter ()
        . filter_map ( |id| graph . pid_of (id) )
        . collect ();
      in_rust_graph_answers . push ((in_rust_graph_set, relation, subject_role));
      query_futures . push (
        find_related_nodes_for_one_primary_pid_in_tx (
          tx, pid, relation, subject_role, object_role )); }
    { // pid plays the second-column role ('object_role')
      let in_rust_graph_set : HashSet<ID> =
        inverse_first_members_from_in_rust_graph (graph, pid, relation);
      in_rust_graph_answers . push ((in_rust_graph_set, relation, object_role));
      query_futures . push (
        find_related_nodes_for_one_primary_pid_in_tx (
          tx, pid, relation, object_role, subject_role )); } }
  let typedb_answers : Vec<Result<HashSet<ID>, Box<dyn Error>>> =
    futures::future::join_all (query_futures) . await;
  let mut mismatches : Vec<Mismatch> = Vec::new ();
  for ((in_rust_graph_set, relation, role), typedb_result)
    in in_rust_graph_answers . into_iter () . zip (typedb_answers) {
    let typedb_set : HashSet<ID> = typedb_result ?;
    if in_rust_graph_set != typedb_set {
      mismatches . push ( Mismatch::Relationship (
        RelationshipMismatch {
        pid       : pid . clone (),
        relation,
        role,
        in_rust_graph : in_rust_graph_set,
        typedb    : typedb_set, } ) ); } }
  let typedb_source : Option<SourceName> =
    source_name_for_one_node_id_in_tx (tx, pid) . await ?;
  let Some (typedb_source) = typedb_source else {
    mismatches . push (Mismatch::MissingTypedbNode {
      pid: pid . clone (), } );
    return Ok (mismatches); };
  if typedb_source != node . source {
    mismatches . push ( Mismatch::Source (
      SourceMismatch {
        pid           : pid . clone (),
        in_rust_graph : node . source . clone (),
        typedb        : typedb_source, } )); }
  Ok (mismatches) }

/// IDs appearing as the second member of 'node''s outbound edges in
/// the named relation (see [[../../schema.tql]]).
fn outbound_second_members_from_in_rust_graph (
  node     : &NodeRust,
  relation : &str,
) -> HashSet<ID> {
  match relation {
    "contains"                     => members_of ( &node . contains ) . into_iter () . collect (),
    "subscribes"                   => members_of ( node . subscribes_to . or_default () ) . into_iter () . collect (),
    "hides_from_its_subscriptions" => members_of ( node . hides_from_its_subscriptions . or_default () ) . into_iter () . collect (),
    "overrides_view_of"            => members_of ( node . overrides_view_of . or_default () ) . into_iter () . collect (),
    "textlinks_to"                 => node . textlinks_to . iter () . cloned () . collect (),
    _ => HashSet::new (),
  } }

/// pids of nodes that play the first-member role in the named
/// relation with 'pid' as the second member (see [[../../schema.tql]]).
fn inverse_first_members_from_in_rust_graph (
  graph    : &InRustGraph,
  pid      : &ID,
  relation : &str,
) -> HashSet<ID> {
  let map = match relation {
    "contains"                     => &graph . contained_by,
    "subscribes"                   => &graph . subscribers_of,
    "hides_from_its_subscriptions" => &graph . hiders_of,
    "overrides_view_of"            => &graph . overriders_of,
    "textlinks_to"                 => &graph . textlinks_in,
    _ => return HashSet::new (),
  };
  map . get (pid)
    . map ( |s| s . iter () . cloned () . collect () )
    . unwrap_or_default () }

/// Format a list of mismatches for log output.
pub fn format_mismatches (mismatches : &[Mismatch]) -> String {
  if mismatches . is_empty () {
    return "No mismatches between in-Rust graph and TypeDB." . to_string (); }
  let mut s : String = format! (
    "{} mismatch(es) between in-Rust graph and TypeDB:\n",
    mismatches . len () );
  for m in mismatches {
    match m {
      Mismatch::Relationship (m) => {
        s . push_str ( & format! (
          "  pid={} relation={} role={} in-Rust graph={:?} typedb={:?}\n",
          m . pid, m . relation, m . role,
          m . in_rust_graph, m . typedb ) ); }
      Mismatch::Source (m) => {
        s . push_str ( & format! (
          "  pid={} source mismatch in-Rust graph={} typedb={}\n",
          m . pid,
          m . in_rust_graph,
          m . typedb ) ); }
      Mismatch::MissingTypedbNode { pid } => {
        s . push_str ( & format! (
          "  pid={} missing TypeDB node\n",
          pid ) ); }}}
  s }
