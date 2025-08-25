use std::error::Error;
use typedb_driver::{
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::types::{ID, FileNode};

pub async fn create_all_nodes (
  // Maps `create_node` over `filenodes`, creating:
  //   all `node`         entities
  //   all `extra_id`     entities
  //   all `has_extra_id` relationships
  // Then commits.
  db_name   : &str,
  driver    : &TypeDBDriver,
  filenodes : &Vec <FileNode>
)-> Result < (), Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction ( db_name,
                         TransactionType::Write )
    . await ?;
  println!("Creating nodes ...");
  for node in filenodes {
    create_node ( node, &tx )
      . await ?; }
  tx . commit () . await ?;
  Ok (()) }

pub async fn create_node (
  // Creates: the `node`,
  //          any `extra_id` entities it needs.
  //          any `has_extra_id` relationships it needs.
  // Does *not* commit.
  node: &FileNode,
  tx: &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  if node . ids . is_empty () {
    return Err ( "Node with no IDs.".into() ); }
  let primary_id = node . ids [0] . as_str ();
  let insert_node_query = format! (
    r#"insert $n isa node,
                 has id "{}";"#,
    primary_id );
  tx . query ( insert_node_query ) . await ?;
  insert_extra_ids ( &node, tx ) . await ?; // PITFALL: This creates has_extra_id relationships, so you might expect it to belong in `create_relationships_from_node`. But it's important that these relationships be created before any others, because the others might refer to nodes via their `extra_id`s. They are basically optional attributes of a node; they have no meaning beyond being another way to refer to a node.
  Ok (()) }

pub async fn insert_extra_ids (
  node : &FileNode,
  tx   : &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  if node.ids.len () > 1 {
    let primary_id = node . ids [0] . as_str ();
    let extra_ids: Vec < &ID > =
      node . ids . iter() . skip(1) . collect();
    for extra_id in extra_ids {
      tx.query (
        format! ( r#"
                    match
                        $n isa node, has id "{}";
                    insert
                        $e isa extra_id, has id "{}";
                        $r isa has_extra_id
                           ( node: $n,
                             extra_id: $e ); "#,
                    primary_id,
                    extra_id.as_str () ))
        . await ?; }}
  Ok (()) }
