// TODO | PITFALL:
// Deletes any existing TypeDB database named `skg-test`,

use futures::executor::block_on;
use futures::StreamExt;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{
  TransactionType,
  TypeDBDriver,
  Credentials,
  DriverOptions,
};

use skg::typedb::create::{make_db_destroying_earlier_one};
use skg::typedb::search::{find_node_containing_node, extract_id};

#[test]
fn test_typedb_integration() -> Result<(), Box<dyn Error>> {
  // Use block_on to run async code in a synchronous test
  block_on(async {
    let driver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
    let db_name = "skg-test";

    make_db_destroying_earlier_one(
      db_name, &driver).await?;

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
      "ei"
    ).await?;
    let expected_has_extra_id: HashSet<_> = [
      ("2", "22"),
      ("3", "33"),
      ("4", "44"),
      ("5", "55"), ].iter()
      .map(|(a, b)| (a.to_string(), b.to_string()))
      .collect();
    assert_eq!(has_extra_id_pairs, expected_has_extra_id);

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
      "ei"
    ).await?;
    let mut expected_comments_on = HashSet::new();
    expected_comments_on.insert(("4".to_string(), "2".to_string()));
    assert_eq!(comments_on_pairs, expected_comments_on);

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
      "contained_id"
    ).await?;
    let mut expected_contains = HashSet::new();
    expected_contains.insert(("1".to_string(), "2".to_string()));
    expected_contains.insert(("1".to_string(), "3".to_string()));
    assert_eq!(contains_pairs, expected_contains);

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
      "to"
    ).await?;
    let expected_subscribes : HashSet<_> = [
      ("2", "4"),
      ("2", "5"),
      ("3", "4"),
      ("3", "5"), ].iter()
      .map(|(a, b)| (a.to_string(), b.to_string()))
      .collect();
    assert_eq!(subscribes_pairs, expected_subscribes);

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
      "to"
    ).await?;
    let mut expected_unsubscribes = HashSet::new();
    expected_unsubscribes.insert(("1".to_string(), "4".to_string()));
    expected_unsubscribes.insert(("1".to_string(), "5".to_string()));
    assert_eq!(unsubscribes_pairs, expected_unsubscribes);

    let container_node = find_node_containing_node(
      db_name, &driver, "2").await?;
    assert_eq!(container_node, "1");

    Ok (()) } ) }

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
