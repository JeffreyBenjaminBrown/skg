/* FINDING:
Cascading delete works how I hoped.
Deleting an entity deletes relationships involving it.
.
DESIGN:
Creates a temporary database with two nodes where a contains b,
deletes a, then verifies b still exists but the contains relationship is gone.
*/

use skg::typedb::init::populate_test_db_from_fixtures;
use skg::typedb::search::util::pid_from_id;
use skg::types::{ID, SkgConfig, SkgNode, empty_skgnode};
use skg::typedb::nodes::create_only_nodes_with_no_ids_present;

use futures::StreamExt;
use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  answer::{ ConceptRow, QueryAnswer },
  Credentials,
  DriverOptions,
  Transaction,
  TransactionType,
  TypeDBDriver,
};

#[test]
fn test_cascading_delete_behavior() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let driver: TypeDBDriver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;

    let config: SkgConfig = SkgConfig {
      db_name        : "skg-test-cascading-delete".into(),
      skg_folder     : "tests/typedb/fixtures".into(),
      tantivy_folder : "irrelevant".into(),
      port           : 1730
    };

    let index_folder: &str =
      config.skg_folder.to_str()
      .expect("Invalid UTF-8 in skg folder path");

    // Set up fresh database with schema
    populate_test_db_from_fixtures(
      index_folder,
      &config.db_name,
      &driver
    ).await?;

    // Create two test nodes: test_container and test_contained
    let mut node_container: SkgNode = empty_skgnode();
    node_container.title = "Test Container Node".to_string();
    node_container.ids = vec![ID::from("test_container")];

    let mut node_contained: SkgNode = empty_skgnode();
    node_contained.title = "Test Contained Node".to_string();
    node_contained.ids = vec![ID::from("test_contained")];

    let created_count: usize = create_only_nodes_with_no_ids_present(
      &config.db_name,
      &driver,
      &vec![node_container, node_contained]
    ).await?;
    assert_eq!(created_count, 2, "Should create exactly 2 nodes");

    // Create contains relationship: test_container contains test_contained
    let tx: Transaction = driver.transaction(
      &config.db_name,
      TransactionType::Write
    ).await?;

    tx.query(r#"
      match
        $from isa node, has id "test_container";
        $to isa node, has id "test_contained";
      insert
        $r isa contains
          (container: $from,
           contained: $to);
    "#).await?;

    tx.commit().await?;

    // Verify initial state: both nodes exist and relationship exists
    assert!(
      pid_from_id(&config.db_name, &driver, &ID::from("test_container")).await.unwrap().is_some(),
      "Node test_container should exist before deletion"
    );
    assert!(
      pid_from_id(&config.db_name, &driver, &ID::from("test_contained")).await.unwrap().is_some(),
      "Node test_contained should exist before deletion"
    );

    let initial_contains_count: usize = count_contains_relationships(
      &config.db_name,
      &driver
    ).await?;
    println!("Initial contains relationships: {}", initial_contains_count);

    // Delete node test_container
    let tx: Transaction = driver.transaction(
      &config.db_name,
      TransactionType::Write
    ).await?;

    let delete_query: &str = r#"
      match
        $node isa node, has id "test_container";
      delete
        $node;
    "#;

    tx.query(delete_query).await?;
    tx.commit().await?;

    // Verify results:
    // 1. Node test_container should be deleted
    assert!(
      pid_from_id(&config.db_name, &driver, &ID::from("test_container")).await.unwrap().is_none(),
      "Node test_container should be deleted"
    );

    // 2. Node test_contained should still exist
    assert!(
      pid_from_id(&config.db_name, &driver, &ID::from("test_contained")).await.unwrap().is_some(),
      "Node test_contained should still exist after deleting test_container"
    );

    // 3. Contains relationship should be gone (this tests cascading delete behavior)
    let final_contains_count: usize = count_contains_relationships(
      &config.db_name,
      &driver
    ).await?;

    println!("Final contains relationships: {}", final_contains_count);

    if final_contains_count < initial_contains_count {
      println!("RESULT: TypeDB DOES perform cascading deletes by default");
      println!("The contains relationship was automatically removed when test_container was deleted");
      println!("Relationships went from {} to {}", initial_contains_count, final_contains_count);
    } else {
      println!("RESULT: TypeDB does NOT perform cascading deletes by default");
      println!("Contains relationships remain the same even after deleting the container node");
      println!("Relationships: {} before, {} after", initial_contains_count, final_contains_count);
    }

    Ok(())
  })
}

/// Helper function to count all contains relationships in the database
async fn count_contains_relationships(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<usize, Box<dyn Error>> {
  let tx: Transaction = driver.transaction(
    db_name,
    TransactionType::Read
  ).await?;

  let answer: QueryAnswer = tx.query(r#"
    match
      $container isa node, has id $container_id;
      $contained isa node, has id $contained_id;
      $rel isa contains (container: $container,
                         contained: $contained);
    select $container_id, $contained_id;
  "#).await?;

  let mut rows = answer.into_rows();
  let mut count: usize = 0;
  while let Some(row_res) = rows.next().await {
    let _row: ConceptRow = row_res?;
    count += 1;
  }

  Ok(count)
}
