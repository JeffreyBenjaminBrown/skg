use std::time::Duration;
use std::error::Error;

use crate::dbs::memory::snapshot_global;
use crate::types::misc::ID;
use crate::types::nodes::typedb::NodeTypedb;

use futures::stream::{self, StreamExt};
use typedb_driver::{
  Transaction,
  TransactionOptions,
  TransactionType,
  TypeDBDriver,
};

/// The 5 outbound relationship types and their two roles.
/// Each triple is (relation, role_a, role_b).
pub const OUTBOUND_RELATIONSHIP_TYPES : &[(&str, &str, &str)] = &[
  ("contains",                      "container",   "contained"),
  ("textlinks_to",                  "source",      "dest"),
  ("subscribes",                    "subscriber",  "subscribee"),
  ("hides_from_its_subscriptions",  "hider",       "hidden"),
  ("overrides_view_of",             "replacement", "replaced"),
];

/// Maps `create_relationships_for_one_node` over `nodes`,
/// bounded by TYPEDB_CONCURRENT_TRANSACTIONS
/// to avoid overwhelming TypeDB with concurrent transactions.
/// Each node gets its own transaction.
/// PITFALL : Does not create `has_extra_id` relationships.
pub async fn create_all_relationships (
  db_name    : &str,
  driver     : &TypeDBDriver,
  nodes      : &[NodeTypedb]
)-> Result < (), Box<dyn Error> > {
  tracing::info! ("Creating relationships ...");
  let results : Vec < Result < (), Box < dyn Error > > > =
    stream::iter ( nodes . iter ()
      . map ( |node| create_relationships_for_one_node (
                db_name, driver, node )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  for result in results {
    result ?; }
  Ok (()) }

async fn create_relationships_for_one_node (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &NodeTypedb,
) -> Result < (), Box<dyn Error> > {
  if count_relationships (node) == 0 { return Ok (()); }
  let primary_id : &ID = &node . pid;
  let options : TransactionOptions =
    TransactionOptions::new() . transaction_timeout (
      Duration::from_secs (
        crate::consts::TYPEDB_TRANSACTION_TIMEOUT_SECS));
  let tx : Transaction =
    driver . transaction_with_options (
      db_name, TransactionType::Write, options )
    . await ?;
  create_relationships_from_node (node, &tx)
    . await . map_err (
      |e| format! (
        "Failed to create relationships for node '{}': {}",
        primary_id . as_str (), e )) ?;
  tx . commit () . await
    . map_err (
      |e| format! (
        "Failed to commit relationships for node '{}': {}",
        primary_id . as_str (), e )) ?;
  Ok (()) }

pub async fn create_relationships_from_node (
  node : &NodeTypedb,
  tx   : &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  let primary_id : &ID = &node . pid;
  insert_relationship_from_list (
    primary_id . as_str (),
    &node . contains,
    "contains",
    "container",
    "contained",
    tx ) . await
    . map_err(|e| format!("Failed to create 'contains' relationships: {}", e))?;
  insert_relationship_from_list (
    primary_id . as_str (),
    &node . textlinks_to,
    "textlinks_to",
    "source",
    "dest",
    tx ) . await
    . map_err(|e| format!("Failed to create 'textlinks_to' relationships: {}", e))?;
  insert_relationship_from_list (
    primary_id . as_str (),
    node . subscribes_to . or_default(),
    "subscribes",
    "subscriber",
    "subscribee",
    tx ) . await
    . map_err(|e| format!("Failed to create 'subscribes' relationships: {}", e))?;
  insert_relationship_from_list (
    primary_id . as_str (),
    node . hides_from_its_subscriptions . or_default(),
    "hides_from_its_subscriptions",
    "hider",
    "hidden",
    tx ) . await
    . map_err(|e| format!("Failed to create 'hides_from_its_subscriptions' relationships: {}", e))?;
  insert_relationship_from_list(
    primary_id . as_str (),
    node . overrides_view_of . or_default(),
    "overrides_view_of",
    "replacement",
    "replaced",
    tx ) . await
    . map_err(|e| format!("Failed to create 'overrides_view_of' relationships: {}", e))?;
  Ok (()) }

/// Total relationship queries that will be issued for `node`.
/// Used by `create_all_relationships` to decide when to commit
/// a batch and start a new transaction.
fn count_relationships (
  node : &NodeTypedb,
) -> usize {
  let contains : usize =
    node . contains . len();
  let textlinks : usize =
    node . textlinks_to . len();
  let subscribes : usize =
    node . subscribes_to . or_default() . len();
  let hides : usize =
    node . hides_from_its_subscriptions . or_default() . len();
  let overrides : usize =
    node . overrides_view_of . or_default() . len();
  contains + textlinks + subscribes + hides + overrides }

pub async fn insert_relationship_from_list (
  // Instantiates a relationship in the database. Does not commit.
  primary_id: &str,
  id_list: &[ID],      // Length = 2. Order matters. Might equal node.contains, node.subscribes_to, etc.
  relation_name: &str, // "contains", "subscribes", etc.
  from_role: &str,     // "container", "subscriber", etc.
  to_role: &str,       // "contained", "subscribee", etc.
  tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {

  // If memory is initialized, resolve each target_id to its
  // primary pid up front. Avoids the disjunctive match below
  // ('or { extra_id ... }') which TypeDB's planner evaluates by
  // scanning every extra_id entity — catastrophically slow on
  // a non-trivial corpus. Only the unresolved-via-memory fallback
  // path (tests that bypass 'init_global_handle_for_first_time_or_panic') pays that cost.
  let snap = snapshot_global ();
  for target_id in id_list {
    let target_pid : Option<ID> =
      snap . as_deref () . and_then ( |g| g . pid_of (target_id) );
    let query : String = match target_pid {
      Some (pid) => format! ( r#"
                match
                  $from isa node, has id "{}";
                  $to   isa node, has id "{}";
                insert
                  $r isa {}
                    ({}: $from,
                     {}: $to);"#,
                primary_id,
                pid . as_str (),
                relation_name,
                from_role,
                to_role ),
      None => format! ( r#"
                match
                  $from isa node, has id "{}";
                  {{ $to isa node, has id "{}"; }} or
                  {{ $to isa node;
                     $e isa extra_id, has id "{}";
                     $rel isa has_extra_id ( node:     $to,
                                             extra_id: $e ); }};
                insert
                  $r isa {}
                    ({}: $from,
                     {}: $to);"#,
                primary_id,
                target_id . as_str(),
                target_id . as_str(),
                relation_name,
                from_role,
                to_role ), };

    tx . query(query . clone()) . await
      . map_err(|e| format!(
        "TypeQL query failed for relationship '{}' from '{}' to '{}': {}\nQuery was: {}",
        relation_name, primary_id, target_id . as_str(), e, query))?; }
  Ok (( )) }

/// Delete all 5 outbound relationship types
/// for the given IDs.
pub async fn delete_all_outbound_relationships (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids     : &Vec<ID>,
) -> Result<(), Box<dyn Error>> {
  for (relation, role, _) in OUTBOUND_RELATIONSHIP_TYPES {
    delete_out_links (
      db_name, driver, ids, relation, role
    ) . await ?; }
  Ok (( )) }

/// Delete every instance of `relation`
/// where one of the input IDs plays `role`.
/// Returns the number of IDs processed
/// (not the number of relations deleted,
/// which would be more work).
/// Bounded by TYPEDB_CONCURRENT_TRANSACTIONS.
pub async fn delete_out_links (
  db_name  : &str,
  driver   : &TypeDBDriver,
  ids      : &Vec<ID>,
  relation : &str,   // e.g. "contains"
  role     : &str,   // e.g. "container"
) -> Result < usize, Box<dyn Error> > {
  let results : Vec < Result < (), Box < dyn Error > > > =
    stream::iter ( ids . iter ()
      . map ( |id| delete_out_links_for_one_id (
                db_name, driver, id, relation, role )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  for result in results {
    result ?; }
  Ok ( ids . len () ) }

async fn delete_out_links_for_one_id (
  db_name  : &str,
  driver   : &TypeDBDriver,
  id       : &ID,
  relation : &str,
  role     : &str,
) -> Result < (), Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Write ) . await ?;
  tx . query ( format! (
    r#"match
         $n   isa node, has id "{}";
         $rel isa {} ( {}: $n );
       delete $rel; "#,
    id . as_str (), relation, role )
  ) . await ?;
  tx . commit () . await ?;
  Ok (()) }
