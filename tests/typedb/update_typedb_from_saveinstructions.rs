// cargo test --test typedb typedb::update_typedb_from_saveinstructions -- --nocapture

use skg::test_utils::run_with_test_db;
use skg::save::update_typedb_from_saveinstructions;
use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::typedb::search::find_related_nodes;
use skg::dbs::typedb::nodes::which_ids_exist;
use skg::types::misc::SourceName;
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::save::{DefineNode, SaveNode};
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::viewnodes_to_instructions::extract_nonmergeSavePlan;
use skg::from_text::buffer_to_viewnodes::validate_tree::contradictory_instructions::find_inconsistent_instructions;
use skg::types::misc::ID;
use skg::types::viewnode::ViewNode;
use skg::types::maybe_placed_viewnode::{MpViewnode, maybePlaced_to_placed_tree};

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
      ** (skg (node (id 1) (source main) indef)) 1 "};

    let unchecked_viewforest : Tree<MpViewnode> =
      org_to_uninterpreted_nodes (org_text)?. 0;

    // Check for inconsistent instructions
    let ( inconsistent_deletions, multiple_definers, inconsistent_sources ) =
      find_inconsistent_instructions (& unchecked_viewforest);
    let viewforest : Tree<ViewNode> =
      maybePlaced_to_placed_tree (unchecked_viewforest)?;
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
    let nonmerge_plan =
      extract_nonmergeSavePlan (
        & viewforest, & config, & driver ) . await ?;

    // Apply the update
    update_typedb_from_saveinstructions (
      & config . db_name,
      & driver,
      & nonmerge_plan . define_nodes,
      &[],
      None ). await ?;

    // Node 3 should not exist (deleted)
    let existing_node3_ids : HashSet<String> = which_ids_exist (
      & config . db_name,
      & driver,
      & ["3", "33"] . iter() . map(|s| s . to_string()) . collect()
    ) . await ?;
    assert!(
      existing_node3_ids . is_empty (),
      "Node 3 should be completely deleted, but found IDs: {:?}",
      existing_node3_ids );

    // Node 4 (not mentioned in buffer) should still exist.
    let existing_node4_ids : HashSet<String> = which_ids_exist (
      & config . db_name,
      & driver,
      & ["4"] . iter() . map(|s| s . to_string()) . collect()
    ) . await ?;
    assert!( ! existing_node4_ids . is_empty (),
               "Node 4 should still exist." );

    // Nodes 1 and 2 should contain each other
    let node1_contains : HashSet<ID> = find_related_nodes (
      & config . db_name,
      & driver,
      & [ ID("1" . to_string()) ],
      "contains",
      "container",
      "contained"
    ) . await ?;
    assert!(
      node1_contains . contains (
        & ID("2" . to_string()) ),
      "Node 1 should contain node 2" );
    assert_eq!(
      node1_contains . len (), 1,
      "Node 1 should only contain node 2, but contains: {:?}",
      node1_contains );

    let node2_contains : HashSet<ID> = find_related_nodes (
      & config . db_name,
      & driver,
      & [ ID("2" . to_string()) ],
      "contains",
      "container",
      "contained"
    ) . await ?;
    assert!( node2_contains . contains ( & ID("1" . to_string()) ),
             "Node 2 should contain node 1" );
    assert_eq!(
      node2_contains . len (),
      1,
      "Node 2 should only contain node 1, but contains: {:?}",
      node2_contains );

      Ok (( )) } )
  ) }

/// The incremental (Some(old_graph)) relationship path writes only the
/// edge delta: an added child is created, a removed child's edge is
/// dropped (but the child node itself survives), an unchanged child is
/// left alone, and no edge is duplicated.
#[test]
fn delta_writes_only_the_changed_edges (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-delta-edges",
    "tests/typedb/update_typedb_from_saveinstructions/fixtures",
    "/tmp/tantivy-test-delta-edges",
    |config, driver, _tantivy| Box::pin ( async move {
      let nc = |pid : &str, contains : &[&str]| -> NodeComplete {
        let mut n : NodeComplete = empty_node_complete ();
        n . pid = ID::from (pid);
        n . title = pid . to_string ();
        n . source = SourceName::from ("main");
        n . contains =
          contains . iter () . map ( |c| ID::from (*c) ) . collect ();
        n };

      // Seed (bulk path): container dc contains [da, db].
      let seed_nodes : Vec<NodeComplete> = vec![
        nc ("dc", &["da", "db"]),
        nc ("da", &[]),
        nc ("db", &[]), ];
      let seed : Vec<DefineNode> =
        seed_nodes . iter () . cloned ()
        . map ( |n| DefineNode::Save ( SaveNode (n) )) . collect ();
      update_typedb_from_saveinstructions (
        & config . db_name, & driver, & seed, &[], None ) . await ?;
      let old_graph : InRustGraph =
        InRustGraph::from_nodecompletes (& seed_nodes);

      // Re-save (delta path): dc now contains [da, dd]; db removed,
      // dd added & new, da unchanged.
      let resave : Vec<DefineNode> = vec![
        DefineNode::Save ( SaveNode ( nc ("dc", &["da", "dd"]) )),
        DefineNode::Save ( SaveNode ( nc ("dd", &[]) )), ];
      update_typedb_from_saveinstructions (
        & config . db_name, & driver, & resave, &[],
        Some (& old_graph) ) . await ?;

      let dc_contains : HashSet<ID> = find_related_nodes (
        & config . db_name, & driver,
        & [ ID::from ("dc") ], "contains", "container", "contained"
      ) . await ?;
      assert_eq! (
        dc_contains,
        HashSet::from ([ ID::from ("da"), ID::from ("dd") ]),
        "after the delta dc should contain exactly {{da, dd}}, got {:?}",
        dc_contains );

      // db was only removed from dc's contains, not deleted.
      let still_exists : HashSet<String> = which_ids_exist (
        & config . db_name, & driver,
        & [ "db" . to_string () ] . iter () . cloned () . collect ()
      ) . await ?;
      assert! ( still_exists . contains ("db"),
        "db should still exist as a node after being un-contained" );
      Ok (( )) } )
  ) }
