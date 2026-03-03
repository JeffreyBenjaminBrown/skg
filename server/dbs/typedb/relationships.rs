use std::time::Duration;
use std::error::Error;

use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::textlinks::textlinks_from_node;

use typedb_driver::{
  Transaction,
  TransactionOptions,
  TransactionType,
  TypeDBDriver,
};

/// TypeDB match-insert cost scales with uncommitted write-buffer size.
/// A single transaction for ~32k queries would degrade to ~135ms/query.
/// Committing every ~BATCH_SIZE queries keeps per-query cost at ~1.5ms.
/// See tools/init-perf-investigation.org for benchmarks.
const BATCH_SIZE : usize = 200;

/// The 5 outbound relationship types and their two roles.
/// Each triple is (relation, role_a, role_b).
pub const OUTBOUND_RELATIONSHIP_TYPES : &[(&str, &str, &str)] = &[
  ("contains",                      "container",   "contained"),
  ("textlinks_to",                  "source",      "dest"),
  ("subscribes",                    "subscriber",  "subscribee"),
  ("hides_from_its_subscriptions",  "hider",       "hidden"),
  ("overrides_view_of",             "replacement", "replaced"),
];

pub async fn create_all_relationships (
  // Maps `create_relationships_from_node` over `nodes`,
  // then commits.
  // PITFALL : Does not create `has_extra_id` relationships.
  db_name    : &str,
  driver     : &TypeDBDriver,
  nodes      : &[SkgNode]
)-> Result < (), Box<dyn Error> > {

  let options : TransactionOptions =
    TransactionOptions::new() . transaction_timeout (
      Duration::from_secs (super::TRANSACTION_TIMEOUT_SECS));
  let tx : Transaction =
    driver . transaction_with_options ( db_name,
                                       TransactionType::Write,
                                       options )
    . await ?;
  println! ("Creating relationships ...");
  let batch_size : usize = BATCH_SIZE;
  let mut queries_in_tx : usize = 0;
  let mut tx : Transaction = tx;
  for node in nodes {
    let primary_id : &ID = node . primary_id()?;
    let rel_count : usize =
      count_relationships (node);
    if queries_in_tx > 0 && queries_in_tx + rel_count > batch_size {
      tx . commit () . await . map_err(
        |e| format!("Failed to commit relationships batch: {}", e))?;
      let options : TransactionOptions =
        TransactionOptions::new() . transaction_timeout (
          Duration::from_secs (super::TRANSACTION_TIMEOUT_SECS));
      tx = driver . transaction_with_options ( db_name,
                                               TransactionType::Write,
                                               options )
        . await ?;
      queries_in_tx = 0; }
    create_relationships_from_node (node, &tx)
      . await . map_err(
        |e| format!(
          "Failed to create relationships for node '{}': {}",
          primary_id . as_str (),
          e ))? ;
    queries_in_tx += rel_count; }
  tx . commit () . await
    . map_err(|e| format!("Failed to commit relationships transaction: {}", e))?;
  Ok (()) }

pub async fn create_relationships_from_node (
  node : &SkgNode,
  tx   : &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  let primary_id : &ID = node . primary_id()?;
  insert_relationship_from_list (
    primary_id . as_str (),
    node . contains . as_ref() . unwrap_or(&vec![]),
    "contains",
    "container",
    "contained",
    tx ) . await
    . map_err(|e| format!("Failed to create 'contains' relationships: {}", e))?;
  insert_relationship_from_list (
    primary_id . as_str (),
    & ( textlinks_from_node (&node)
        . iter ()
        . map ( |textlink|
                 ID::from ( textlink . id . clone() ) )
        . collect::<Vec<ID>>() ),
    "textlinks_to",
    "source",
    "dest",
    tx ) . await
    . map_err(|e| format!("Failed to create 'textlinks_to' relationships: {}", e))?;
  insert_relationship_from_list (
    primary_id . as_str (),
    node . subscribes_to . as_ref() . unwrap_or(&vec![]),
    "subscribes",
    "subscriber",
    "subscribee",
    tx ) . await
    . map_err(|e| format!("Failed to create 'subscribes' relationships: {}", e))?;
  insert_relationship_from_list (
    primary_id . as_str (),
    node . hides_from_its_subscriptions . as_ref() . unwrap_or(&vec![]),
    "hides_from_its_subscriptions",
    "hider",
    "hidden",
    tx ) . await
    . map_err(|e| format!("Failed to create 'hides_from_its_subscriptions' relationships: {}", e))?;
  insert_relationship_from_list(
    primary_id . as_str (),
    node . overrides_view_of . as_ref() . unwrap_or(&vec![]),
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
  node : &SkgNode,
) -> usize {
  let contains : usize =
    node . contains . as_ref() . map_or (0, |v| v . len());
  let textlinks : usize =
    textlinks_from_node (node) . len();
  let subscribes : usize =
    node . subscribes_to . as_ref() . map_or (0, |v| v . len());
  let hides : usize =
    node . hides_from_its_subscriptions . as_ref() . map_or (0, |v| v . len());
  let overrides : usize =
    node . overrides_view_of . as_ref() . map_or (0, |v| v . len());
  contains + textlinks + subscribes + hides + overrides }

pub async fn insert_relationship_from_list (
  // Instantiates a relationship in the database. Does not commit.
  primary_id: &str,
  id_list: &Vec<ID>,   // Length = 2. Order matters. Might equal node.contains, node.subscribes_to, etc.
  relation_name: &str, // "contains", "subscribes", etc.
  from_role: &str,     // "container", "subscriber", etc.
  to_role: &str,       // "contained", "subscribee", etc.
  tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {

  for target_id in id_list {
    let query : String = format! ( r#"
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
                to_role );

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
/// where one of the ipnut IDs plays `role`.
/// Returns the number of IDs processed
/// (not the number of relations deleted,
/// which would be more work).
pub async fn delete_out_links (
  db_name  : &str,
  driver   : &TypeDBDriver,
  ids      : &Vec<ID>,
  relation : &str,   // e.g. "contains"
  role     : &str,   // e.g. "container"
) -> Result < usize, Box<dyn Error> > {

  let tx : Transaction =
    driver . transaction (
      db_name,
      TransactionType::Write
    ) . await ?;
  for id in ids {
    tx . query ( format! (
      r#"match
           $n   isa node, has id "{}";
           $rel isa {} ( {}: $n );
         delete $rel; "#,
      id . as_str (),
      relation,
      role )
    ) . await ?; }
  tx . commit () . await ?;
  Ok ( ids . len () ) }
