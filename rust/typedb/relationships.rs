use std::error::Error;

use crate::types::{ID, Node};
use crate::hyperlinks::hyperlinks_from_node;

use typedb_driver::{
  Transaction,
  TransactionType,
  TypeDBDriver,
};

pub async fn create_all_relationships (
  // Maps `create_relationships_from_node` over `nodes`,
  // then commits.
  // PITFALL : Does not create `has_extra_id` relationships.
  db_name    : &str,
  driver     : &TypeDBDriver,
  nodes      : &[Node]
)-> Result < (), Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction ( db_name,
                         TransactionType::Write )
    . await ?;
  println! ( "Creating relationships ..." );
  for node in nodes {
    create_relationships_from_node (node, &tx)
      . await ?; }
  tx . commit () . await ?;
  Ok (()) }

pub async fn create_relationships_from_node (
  node : &Node,
  tx   : &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  let primary_id = node . ids [0] . as_str ();
  insert_relationship_from_list (
    primary_id,
    &node.contains,
    "contains",
    "container",
    "contained",
    tx ) . await ?;
  insert_relationship_from_list (
    primary_id,
    & ( hyperlinks_from_node ( &node )
        . iter ()
        . map ( |hyperlink|
                 ID::from ( hyperlink.id.clone() ) )
        . collect::<Vec<ID>>() ),
    "hyperlinks_to",
    "source",
    "dest",
    tx ). await ?;
  insert_relationship_from_list (
    primary_id,
    &node.subscribes_to,
    "subscribes",
    "subscriber",
    "subscribee",
    tx ). await ?;
  insert_relationship_from_list (
    primary_id,
    &node.hides_from_its_subscriptions,
    "hides_from_its_subscriptions",
    "hider",
    "hidden",
    tx ). await ?;
  insert_relationship_from_list(
    primary_id,
    &node.overrides_view_of,
    "overrides_view_of",
    "replacement",
    "replaced",
    tx ). await ?;
  Ok (()) }

async fn insert_relationship_from_list (
  // Instantiates a relationship in the database. Does not commit.
  primary_id: &str,
  id_list: &Vec<ID>,   // Length = 2. Order matters. Might equal node.contains, node.subscribes_to, etc.
  relation_name: &str, // "contains", "subscribes", etc.
  from_role: &str,     // "container", "subscriber", etc.
  to_role: &str,       // "contained", "subscribee", etc.
  tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {

  for target_id in id_list {
    tx.query (
      format! ( r#"
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
                target_id.as_str(),
                target_id.as_str(),
                relation_name,
                from_role,
                to_role ) ).await?; }
  Ok (()) }

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
    let q : String = format! (
      r#"match
           $n   isa node, has id "{}";
           $rel isa {} ( {}: $n );
         delete $rel; "#,
      id.as_str (),
      relation,
      role );
    tx . query ( q ) . await ?; }
  tx . commit () . await ?;
  Ok ( ids.len () ) }
