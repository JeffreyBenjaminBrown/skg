// cargo test --test typedb typedb::update_nodes_and_relationships2 -- --nocapture

use skg::typedb::init::populate_test_db_from_fixtures;
use skg::typedb::update::update_nodes_and_relationships2;
use skg::typedb::search::find_related_nodes;
use skg::typedb::nodes::which_ids_exist;
use skg::new::{org_to_uninterpreted_nodes2, orgnodes_to_save_instructions, find_inconsistent_instructions};
use skg::types::{ID, SkgConfig, OrgNode2};
use ego_tree::Tree;
use indoc::indoc;

use futures::executor::block_on;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{
  Credentials,
  DriverOptions,
  TypeDBDriver,
};

#[test]
fn test_update_nodes_and_relationships2 (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let driver : TypeDBDriver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
    let config : SkgConfig = SkgConfig {
      db_name        : "skg-test-update2"      . into(),
      skg_folder     : "tests/typedb/update_nodes_and_relationships2/fixtures" . into(),
      tantivy_folder : "irrelevant"            . into(),
      port           : 1730 };

    // Setup test database with initial data from new fixtures
    let index_folder : &str =
      config . skg_folder . to_str ()
      . expect ("Invalid UTF-8 in tantivy index path");
    populate_test_db_from_fixtures (
      index_folder,
      & config . db_name,
      & driver ). await ?;

    // Simulate user saving this org buffer:
    let org_text = indoc! {"
      * <skg<id:3,toDelete>> 33
      * <skg<id:2>> 22
      ** <skg<id:1,mightContainMore>> 1
    "};

    // Parse org text to uninterpreted nodes
    let trees : Vec<Tree<OrgNode2>> =
      org_to_uninterpreted_nodes2 ( org_text )?;

    // Check for inconsistent instructions
    let ( inconsistent_deletions, multiple_definers ) =
      find_inconsistent_instructions ( & trees );
    assert!( inconsistent_deletions . is_empty (),
             "Found inconsistent deletion instructions: {:?}",
             inconsistent_deletions );
    assert!( multiple_definers . is_empty (),
             "Found multiple definer instructions: {:?}",
             multiple_definers );

    // Convert to instructions (adds missing info and reconciles)
    let reconciled_instructions =
      orgnodes_to_save_instructions ( trees, & config, & driver ) . await ?;

    // Apply the update
    update_nodes_and_relationships2 (
      & config . db_name,
      & driver,
      & reconciled_instructions ). await ?;

    // Node 3 should not exist (deleted)
    let existing_node3_ids : HashSet<String> = which_ids_exist (
      & config . db_name,
      & driver,
      & ["3", "33"].iter().map(|s| s.to_string()).collect()
    ) . await ?;
    assert!(
      existing_node3_ids . is_empty (),
      "Node 3 should be completely deleted, but found IDs: {:?}",
      existing_node3_ids );

    // TODO: Node 4 (not mentioned in buffer) should still exist.
    let existing_node4_ids : HashSet<String> = which_ids_exist (
      & config . db_name,
      & driver,
      & ["4"].iter().map(|s| s.to_string()).collect()
    ) . await ?;
    assert!( ! existing_node4_ids . is_empty (),
               "Node 4 should still exist." );

    // Nodes 1 and 2 should contain each other
    let node1_contains : HashSet<ID> = find_related_nodes (
      & config . db_name,
      & driver,
      & ID("1".to_string()),
      "contains",
      "container",
      "contained"
    ) . await ?;
    assert!(
      node1_contains . contains (
        & ID("2".to_string()) ),
      "Node 1 should contain node 2" );
    assert_eq!(
      node1_contains . len (), 1,
      "Node 1 should only contain node 2, but contains: {:?}",
      node1_contains );

    let node2_contains : HashSet<ID> = find_related_nodes (
      & config . db_name,
      & driver,
      & ID("2".to_string()),
      "contains",
      "container",
      "contained"
    ) . await ?;
    assert!( node2_contains . contains ( & ID("1".to_string()) ),
             "Node 2 should contain node 1" );
    assert_eq!(
      node2_contains . len (),
      1,
      "Node 2 should only contain node 1, but contains: {:?}",
      node2_contains );

    Ok (( )) } ) }
