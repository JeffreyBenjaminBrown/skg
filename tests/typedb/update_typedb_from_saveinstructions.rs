// cargo test --test typedb typedb::update_typedb_from_saveinstructions -- --nocapture

use skg::test_utils::run_with_test_db;
use skg::save::update_typedb_from_saveinstructions;
use skg::dbs::typedb::search::find_related_nodes;
use skg::dbs::typedb::nodes::which_ids_exist;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::orgnodes_to_instructions::orgnode_forest_to_nonmerge_save_instructions;
use skg::from_text::buffer_to_orgnodes::validate_tree::contradictory_instructions::find_inconsistent_instructions;
use skg::types::misc::ID;
use skg::types::orgnode::OrgNode;
use skg::types::unchecked_orgnode::{UncheckedOrgNode, unchecked_to_checked_tree};
use skg::types::save::DefineOneNode;
use ego_tree::Tree;
use indoc::indoc;

use std::collections::HashSet;
use std::error::Error;

#[test]
fn test_update_nodes_and_relationships2 (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-update2",
    "tests/typedb/update_typedb_from_saveinstructions/fixtures",
    "/tmp/tantivy-test-update2",
    |config, driver, _tantivy| Box::pin ( async move {

    // Simulate user saving this org buffer:
    let org_text = indoc! {"
      * (skg (node (id 3) (source main) (editRequest delete))) 33
      * (skg (node (id 2) (source main))) 22
      ** (skg (node (id 1) (source main) indefinitive)) 1
    "};

    let unchecked_forest : Tree<UncheckedOrgNode> =
      org_to_uninterpreted_nodes ( org_text )?.0;

    // Check for inconsistent instructions
    let ( inconsistent_deletions, multiple_definers, inconsistent_sources ) =
      find_inconsistent_instructions ( & unchecked_forest );
    let forest : Tree<OrgNode> =
      unchecked_to_checked_tree ( unchecked_forest )?;
    assert!( inconsistent_deletions . is_empty (),
             "Found inconsistent deletion instructions: {:?}",
             inconsistent_deletions );
    assert!( multiple_definers . is_empty (),
             "Found multiple definer instructions: {:?}",
             multiple_definers );
    assert!( inconsistent_sources . is_empty (),
             "Found inconsistent source instructions: {:?}",
             inconsistent_sources );

    // Convert to instructions (adds missing info and reconciles)
    let reconciled_instructions : Vec<DefineOneNode> =
      orgnode_forest_to_nonmerge_save_instructions (
        & forest, & config, & driver ) . await ?;

    // Apply the update
    update_typedb_from_saveinstructions (
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

    // Node 4 (not mentioned in buffer) should still exist.
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
      & [ ID("1".to_string()) ],
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
      & [ ID("2".to_string()) ],
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

      Ok (( )) } )
  ) }
