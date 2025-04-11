// TODO | PITFALL:
// Deletes any existing TypeDB database named `skg-test`,

use futures::StreamExt;
use std::error::Error;
use typedb_driver::{
    TypeDBDriver,
    TransactionType,
    Credentials,
    DriverOptions,
};

use skg::typedb::create::{make_db_destroying_earlier_one};

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
  do_typedb().await
}

async fn do_typedb() -> Result<(), Box<dyn Error>> {
  let driver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?
  ).await?;
  let db_name = "skg-test";

  make_db_destroying_earlier_one (
    db_name, &driver ) . await?;

  print_all_of_some_binary_rel ( // extra ids
    db_name,
    &driver,
    r#" match
          $n isa node, has id $ni;
          $e isa extra_id, has id $ei;
          $rel isa has_extra_id (node: $n,
                                 extra_id: $e);
        select $ni, $ei;"#,
    "has_extra_id",
    "ni",
    "ei" ).await?;

  print_all_of_some_binary_rel ( // comments
    db_name,
    &driver,
    r#" match
          $r isa node, has id $ri;
          $e isa node, has id $ei;
          $rel isa comments_on (commenter: $r,
                                commentee: $e);
        select $ri, $ei;"#,
    "comments_on",
    "ri",
    "ei" ).await?;

  print_all_of_some_binary_rel ( // contents
    db_name,
    &driver,
    r#" match
          $container isa node, has id $container_id;
          $contained isa node, has id $contained_id;
          $rel isa contains (container: $container,
                             contained: $contained);
        select $container_id, $contained_id;"#,
    "contains",
    "container_id",
    "contained_id" ).await?;

  print_all_of_some_binary_rel ( // subscription
    db_name,
    &driver,
    r#" match
          $subscriber isa node, has id $from;
          $subscribee isa node, has id $to;
          $rel isa subscribes (subscriber: $subscriber,
                               subscribee: $subscribee);
        select $from, $to;"#,
    "subscribes",
    "from",
    "to" ).await?;

  print_all_of_some_binary_rel ( // unsubscription
    db_name,
    &driver,
    r#" match
          $unsubscriber isa node, has id $from;
          $unsubscribee isa node, has id $to;
          $rel isa unsubscribes (unsubscriber: $unsubscriber,
                                 unsubscribee: $unsubscribee);
        select $from, $to;"#,
    "unsubscribes",
    "from",
    "to" ).await?;

  println! (
    "The node containing the node with ID '2' has ID: {}",
    find_node_containing_node (
      db_name, &driver, "2" ) . await? );
  Ok(()) }

async fn print_all_of_some_binary_rel (
    db_name: &str,
    driver: &TypeDBDriver,
    query: &str,
    rel_name: &str,
    member1_variable: &str, // PITFALL: Must correspond to `query`. It's not the role name, but rather a variable, i.e. preceded with `$`.
    member2_variable: &str, // PITFALL: Must correspond to `query`. It's not the role name, but rather a variable, i.e. preceded with `$`.
) -> Result<(), Box<dyn Error>> {
    let tx = driver.transaction(
        db_name, TransactionType::Read).await?;
    let answer = tx.query(query).await?;
    let mut stream = answer.into_rows();
    println!( "All '{}' relationships in the database:",
               rel_name);
    while let Some(row_result) = stream.next().await {
        let row = row_result?;
        let id1 = match row.get(member1_variable)? {
            Some(c) => c.to_string(),
            None => "unknown".to_string()
        };
        let id2 = match row.get(member2_variable)? {
            Some(c) => c.to_string(),
            None => "unknown".to_string()
        };
        println!("  Node '{}' {} node '{}'", id1, rel_name, id2); }
    println! ();
    Ok (()) }

async fn find_node_containing_node (
  db_name : &str,
  driver : &TypeDBDriver,
  target_id: &str
) -> Result<String, Box<dyn Error>> {
  let tx = driver.transaction(
    db_name, TransactionType::Read).await?;
  let answer = tx.query(
    format!( r#" match
                   $container isa node, has id $container_id;
                   $contained isa node, has id "{}";
                   $rel isa contains (container: $container,
                                      contained: $contained);
                 select $container_id;"#,
                 target_id ) ) . await?;
  let mut stream = answer.into_rows();
  if let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("container_id")? {
      return Ok(concept.to_string()); } }
  Err(format!("No container found for node with ID '{}'",
              target_id).into()) }
