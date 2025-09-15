use std::collections::HashSet;
use futures::StreamExt;
use std::error::Error;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::types::ID;
use super::util::extract_payload_from_typedb_string_rep;


/// Runs a single TypeDB query.
/// Returns the containing nodes' IDs.
pub async fn find_containers_of (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < HashSet<ID>, Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction (
      db_name, TransactionType::Read
    ). await ?;
  let answer : QueryAnswer =
    tx.query (
      format!( r#" match
                     $container isa node, has id $container_id;
                  {{ $contained isa node, has id "{}"; }} or
                  {{ $contained isa node;
                     $e isa extra_id, has id "{}";
                     $extra_rel isa has_extra_id ( node:     $contained,
                                                   extra_id: $e ); }};
                     $contains_rel isa contains ( container: $container,
                                                  contained: $contained );
                   select $container_id;"#,
                   node, node )
    ) . await?;
  let mut stream = answer.into_rows ();
  let mut containers : HashSet<ID> = HashSet::new ();
  while let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row.get ("container_id") ? {
      containers.insert ( ID (
        extract_payload_from_typedb_string_rep (
          &concept . to_string () )) ); }}
  Ok (containers) }
