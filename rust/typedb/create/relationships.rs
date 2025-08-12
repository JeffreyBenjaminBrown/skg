use std::error::Error;

use crate::types::{ID, FileNode};
use crate::hyperlinks::hyperlinks_from_filenode;

use typedb_driver::{
  Transaction,
  TransactionType,
  TypeDBDriver,
};

pub async fn create_all_relationships (
  // Maps `create_relationships_from_node` over `filenodes`,
  // then commits.
  // PITFALL: Does not create `has_extra_id` relationships.
  db_name   : &str,
  driver    : &TypeDBDriver,
  filenodes : &Vec<FileNode>
)-> Result < (), Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction ( db_name,
                         TransactionType::Write )
    . await ?;
  println!("Creating relationships ...");
  for node in filenodes {
    create_relationships_from_node (node, &tx)
      . await ?; }
  tx . commit () . await ?;
  Ok (()) }

pub async fn create_relationships_from_node (
  node : &FileNode,
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
    & ( hyperlinks_from_filenode ( &node )
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

pub async fn insert_relationship_from_list (
  primary_id: &str,
  id_list: &Vec<ID>,   // This would be node.contains, etc.
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
                     $rel isa has_extra_id (node: $to,
                                            extra_id: $e); }};
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
