// TODO | PITFALL:
// Deletes any existing TypeDB database named `skg-test`,

use futures::StreamExt;
use std::error::Error;
use std::collections::HashSet;
use typedb_driver::{
    TypeDBDriver,
    TransactionType,
    Credentials,
    DriverOptions,
};

use skg::typedb::create::{make_db_destroying_earlier_one};
use skg::typedb::search::{find_node_containing_node, extract_id};

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

  let has_extra_id_pairs = collect_all_of_some_binary_rel(
    db_name,
    &driver,
    r#" match
          $n isa node, has id $ni;
          $e isa extra_id, has id $ei;
          $rel isa has_extra_id (node: $n,
                                 extra_id: $e);
        select $ni, $ei;"#,
    "ni",
    "ei").await?;
  println!("All 'has_extra_id' relationships in the database:");
  for (id1, id2) in &has_extra_id_pairs {
    println!("  Node: {} has_extra_id node: {}", id1, id2); }
  println!();

  let comments_on_pairs = collect_all_of_some_binary_rel(
    db_name,
    &driver,
    r#" match
          $r isa node, has id $ri;
          $e isa node, has id $ei;
          $rel isa comments_on (commenter: $r,
                                commentee: $e);
        select $ri, $ei;"#,
    "ri",
    "ei").await?;
  println!("All 'comments_on' relationships in the database:");
  for (id1, id2) in &comments_on_pairs {
    println!("  Node: {} comments_on node: {}", id1, id2); }
  println!();

  let contains_pairs = collect_all_of_some_binary_rel(
    db_name,
    &driver,
    r#" match
          $container isa node, has id $container_id;
          $contained isa node, has id $contained_id;
          $rel isa contains (container: $container,
                             contained: $contained);
        select $container_id, $contained_id;"#,
    "container_id",
    "contained_id").await?;
  println!("All 'contains' relationships in the database:");
  for (id1, id2) in &contains_pairs {
    println!("  Node: {} contains node: {}", id1, id2); }
  println!();

  let subscribes_pairs = collect_all_of_some_binary_rel(
    db_name,
    &driver,
    r#" match
          $subscriber isa node, has id $from;
          $subscribee isa node, has id $to;
          $rel isa subscribes (subscriber: $subscriber,
                               subscribee: $subscribee);
        select $from, $to;"#,
    "from",
    "to").await?;
  println!("All 'subscribes' relationships in the database:");
  for (id1, id2) in &subscribes_pairs {
    println!("  Node: {} subscribes node: {}", id1, id2); }
  println!();

  let unsubscribes_pairs = collect_all_of_some_binary_rel(
    db_name,
    &driver,
    r#" match
          $unsubscriber isa node, has id $from;
          $unsubscribee isa node, has id $to;
          $rel isa unsubscribes (unsubscriber: $unsubscriber,
                                 unsubscribee: $unsubscribee);
        select $from, $to;"#,
    "from",
    "to").await?;
  println!("All 'unsubscribes' relationships in the database:");
  for (id1, id2) in &unsubscribes_pairs {
    println!("  Node: {} unsubscribes node: {}", id1, id2); }
  println!();

  println!(
    "The node containing the node with ID 2 has ID: {}",
    find_node_containing_node(
      db_name, &driver, "2").await?);

  Ok(()) }

async fn collect_all_of_some_binary_rel(
  db_name: &str,
  driver: &TypeDBDriver,
  query: &str,
  member1_variable: &str, // PITFALL: Must correspond to `query`. It's not the role name, but rather a variable, i.e. preceded with `$`.
  member2_variable: &str, // PITFALL: Must correspond to `query`. It's not the role name, but rather a variable, i.e. preceded with `$`.
) -> Result<HashSet<(String, String)>, Box<dyn Error>> {
  let tx = driver.transaction(
    db_name, TransactionType::Read).await?;
  let answer = tx.query(query).await?;
  let mut stream = answer.into_rows();
  let mut results: HashSet<(String, String)> = HashSet::new();

  while let Some(row_result) = stream.next().await {
    let row = row_result?;
    let id1_raw = match row.get(member1_variable)? {
      Some(c) => c.to_string(),
      None => "unknown".to_string() };
    let id2_raw = match row.get(member2_variable)? {
      Some(c) => c.to_string(),
      None => "unknown".to_string() };
    let id1 = extract_id(&id1_raw);
    let id2 = extract_id(&id2_raw);
    results.insert((id1, id2)); }
  Ok(results) }
