// These tests have not been human-verified.
// cargo nextest run --test grouped_repos -E 'test(move_repo::)'

use indoc::indoc;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::filesystem::one_node::graphnode_from_skgid;
use skg::dbs::tantivy::search::{SearchOptions, search_index};
use skg::from_text::buffer_to_validated_saveplan;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::save::update_graph_minus_nodeMerges;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config, audit_inrustgraph_or_panic};
use skg::types::errors::{SaveError, BufferValidationError};

use skg::types::misc::{ID, SkgConfig, SkgRepoName, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::NodeInstruction;
use std::error::Error;
use std::path::PathBuf;
use tantivy::{DocAddress, TantivyDocument};
use tantivy::schema::document::Value;

/// Query Tantivy for a node by title and return its skgrepo.
fn tantivy_skgrepo_for_skgid (
  tantivy_index  : &TantivyIndex,
  query          : &str,
  expected_skgid : &str,
) -> Result<Option<String>, Box<dyn Error>> {
  // A save commits its Tantivy index update in the background, so wait
  // for it to land before reading — mirroring the production search
  // handler. (See server/dbs/tantivy/background_writer.rs.)
  skg::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
  let (matches, searcher)
    : (Vec<(f32, DocAddress)>, tantivy::Searcher) =
    search_index (tantivy_index, query, &SearchOptions::default ())?;
  for (_score, doc_address) in matches {
    let doc : TantivyDocument =
      searcher . doc (doc_address)?;
    let id_value : Option<String> =
      doc . get_first (tantivy_index . id_field)
      . and_then (|v| v . as_str() . map (String::from));
    if id_value . as_deref() == Some (expected_skgid) {
      let skgrepo_value : Option<String> =
        doc . get_first (tantivy_index . skgrepo_field)
        . and_then (|v| v . as_str() . map (String::from));
      return Ok (skgrepo_value); }}
  Ok (None) }


/////////////////
// Tests
/////////////////

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-move-repo",
    |s| Box::pin ( async move {
      s . reset ("test_move_node_to_another_owned_repo", "tests/move_repo/fixtures") ?;
      test_move_node_to_another_owned_skgrepo (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_move_node_referenced_by_extra_id", "tests/move_repo/fixtures") ?;
      test_move_node_referenced_by_extra_id (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_move_multiple_nodes", "tests/move_repo/fixtures") ?;
      test_move_multiple_nodes (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_move_to_foreign_repo_rejected", "tests/move_repo/fixtures") ?;
      test_move_to_foreign_skgrepo_rejected (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_move_from_foreign_repo_rejected", "tests/move_repo/fixtures") ?;
      test_move_from_foreign_skgrepo_rejected (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_move_and_merge_simultaneously_rejected", "tests/move_repo/fixtures") ?;
      test_move_and_merge_simultaneously_rejected (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_no_repo_change_produces_no_moves", "tests/move_repo/fixtures") ?;
      test_no_skgrepo_change_produces_no_moves (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_repo_only_change_with_populated_pool", "tests/move_repo/fixtures") ?;
      test_skgrepo_only_change_with_populated_pool (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// Basic move: change b's skgrepo from public to private.
/// Verify FS (old file gone, new file present),
/// the graph, and Tantivy all reflect the new skgrepo.
async fn test_move_node_to_another_owned_skgrepo (
  config : &SkgConfig,
  tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // a (public) contains b (public) contains c (public).
    // Edit b's skgrepo to private.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (repo public))) a
      ** (skg (node (id b) (repo private))) b
      *** (skg (node (id c) (repo public))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config
          , None ) ?;
    assert_eq!(save_plan . skgrepo_moves . len(), 1,
               "Expected exactly 1 repo move");
    assert_eq!(save_plan . skgrepo_moves[0] . pid . 0, "b");
    assert_eq!(save_plan . skgrepo_moves[0] . old_skgrepo . as_str(), "public");
    assert_eq!(save_plan . skgrepo_moves[0] . new_skgrepo . as_str(), "private");

    let graph : InRustGraphHandle =
      graph_handle_from_config (&config) ?;
    let replacement : Option<TantivyIndex> =
      update_graph_minus_nodeMerges (
        save_plan . node_instructions, &save_plan . skgrepo_moves,
        config . clone(), &tantivy_index,
        &graph, &skg::types::env::new_mutation_gate () ) . await?;
    if let Some (new_idx) = replacement {
      *tantivy_index = new_idx; }
    audit_inrustgraph_or_panic (&graph)?;

    { // FS: old file should be gone, new file should exist
      let old_path : PathBuf =
        temp_fixtures . join ("owned/public/b.skg");
      let new_path : PathBuf =
        temp_fixtures . join ("owned/private/b.skg");
      assert!( ! old_path . exists(),
               "b.skg should be deleted from public/");
      assert!( new_path . exists(),
               "b.skg should exist in private/"); }

    { // FS: read Graphnode back from disk via graph identity lookup
      let node_b : Graphnode =
        graphnode_from_skgid (&config, &ID::new ("b"))
?;
      assert_eq!(node_b . home_skgrepo, SkgRepoName::from ("private"),
                 "Graphnode read from disk should have repo=private"); }

    { // Graph: skgrepo should be updated
      let (pid, skgrepo) : (ID, SkgRepoName) =
        graph . load_full () . pid_and_skgrepo (&ID::new ("b"))
        . expect ("b should exist in graph");
      assert_eq!(pid . 0, "b");
      assert_eq!(skgrepo . as_str(), "private",
                 "graph should show repo=private for b"); }

    { // Tantivy: skgrepo should be updated
      let skgrepo : Option<String> =
        tantivy_skgrepo_for_skgid (&tantivy_index, "b", "b")?;
      assert_eq!(skgrepo . as_deref(), Some ("private"),
                 "Tantivy should show repo=private for b"); }

    { // Other nodes unchanged
      let node_a : Graphnode =
        graphnode_from_skgid (&config, &ID::new ("a"))
?;
      assert_eq!(node_a . home_skgrepo, SkgRepoName::from ("public"));
      let node_c : Graphnode =
        graphnode_from_skgid (&config, &ID::new ("c"))
?;
      assert_eq!(node_c . home_skgrepo, SkgRepoName::from ("public")); }

    { // Containment relationships should be unchanged
      let node_a : Graphnode =
        graphnode_from_skgid (&config, &ID::new ("a"))
?;
      assert!(members_of ( &node_a . contains ) . contains (&ID::new ("b")),
              "a should still contain b after move");
      let node_b : Graphnode =
        graphnode_from_skgid (&config, &ID::new ("b"))
?;
      assert!(members_of ( &node_b . contains ) . contains (&ID::new ("c")),
              "b should still contain c after move"); }

    Ok (()) }

/// Nodes referenced by extra_id in the buffer (not PID).
/// The save pipeline should resolve extra_ids to PIDs
/// and the move should still work correctly.
async fn test_move_node_referenced_by_extra_id (
  config : &SkgConfig,
  tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Use extra_ids (a-alias, b-alias, c-alias) instead of PIDs.
    let org_text : &str = indoc! {"
      * (skg (node (id a-alias) (repo public))) a
      ** (skg (node (id b-alias) (repo private))) b
      *** (skg (node (id c-alias) (repo public))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config , None ) ?;

    // repo_moves should use the PID, not the extra_id
    assert_eq!(save_plan . skgrepo_moves . len(), 1,
               "Expected exactly 1 repo move");
    assert_eq!(save_plan . skgrepo_moves[0] . pid . 0, "b",
               "RepoMove should use PID, not extra_id");

    let graph : InRustGraphHandle =
      graph_handle_from_config (&config) ?;
    let replacement : Option<TantivyIndex> =
      update_graph_minus_nodeMerges (
        save_plan . node_instructions, &save_plan . skgrepo_moves,
        config . clone(), &tantivy_index,
        &graph, &skg::types::env::new_mutation_gate () ) . await?;
    if let Some (new_idx) = replacement {
      *tantivy_index = new_idx; }
    audit_inrustgraph_or_panic (&graph)?;

    { // FS: old file gone, new file present
      let old_path : PathBuf =
        temp_fixtures . join ("owned/public/b.skg");
      let new_path : PathBuf =
        temp_fixtures . join ("owned/private/b.skg");
      assert!( ! old_path . exists(),
               "b.skg should be deleted from public/");
      assert!( new_path . exists(),
               "b.skg should exist in private/"); }

    { // Graph: skgrepo updated, extra_ids preserved
      let snapshot = graph . load_full ();
      let (pid, skgrepo) : (ID, SkgRepoName) =
        snapshot . pid_and_skgrepo (&ID::new ("b"))
        . expect ("b should exist in graph");
      assert_eq!(pid . 0, "b");
      assert_eq!(skgrepo . as_str(), "private");
      assert_eq!(snapshot . pid_of (&ID::new ("b-alias")), Some (pid),
              "extra_id b-alias should be preserved after move"); }

    { // Tantivy: skgrepo updated
      let skgrepo : Option<String> =
        tantivy_skgrepo_for_skgid (&tantivy_index, "b", "b")?;
      assert_eq!(skgrepo . as_deref(), Some ("private")); }

    Ok (()) }

/// Move two nodes in the same save.
async fn test_move_multiple_nodes (
  config : &SkgConfig,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Move both b and c to private.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (repo public))) a
      ** (skg (node (id b) (repo private))) b
      *** (skg (node (id c) (repo private))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config
          , None ) ?;
    assert_eq!(save_plan . skgrepo_moves . len(), 2,
               "Expected 2 repo moves");

    let move_pids : Vec<&str> =
      save_plan . skgrepo_moves . iter()
      . map (|sm| sm . pid . 0 . as_str()) . collect();
    assert!(move_pids . contains (&"b"), "Should move b");
    assert!(move_pids . contains (&"c"), "Should move c");

    let graph : InRustGraphHandle =
      graph_handle_from_config (&config) ?;
    let replacement : Option<TantivyIndex> =
      update_graph_minus_nodeMerges (
        save_plan . node_instructions, &save_plan . skgrepo_moves,
        config . clone(), &_tantivy_index,
        &graph, &skg::types::env::new_mutation_gate () ) . await?;
    if let Some (new_idx) = replacement {
      *_tantivy_index = new_idx; }
    audit_inrustgraph_or_panic (&graph)?;

    { // FS
      assert!( ! temp_fixtures . join ("owned/public/b.skg") . exists() );
      assert!( ! temp_fixtures . join ("owned/public/c.skg") . exists() );
      assert!( temp_fixtures . join ("owned/private/b.skg") . exists() );
      assert!( temp_fixtures . join ("owned/private/c.skg") . exists() );
      // a stays in public
      assert!( temp_fixtures . join ("owned/public/a.skg") . exists() ); }

    { // Graph
      let snapshot = graph . load_full ();
      let (_, skgrepo_b) = snapshot . pid_and_skgrepo (&ID::new ("b"))
        . expect ("b should exist");
      let (_, skgrepo_c) = snapshot . pid_and_skgrepo (&ID::new ("c"))
        . expect ("c should exist");
      assert_eq!(skgrepo_b . as_str(), "private");
      assert_eq!(skgrepo_c . as_str(), "private"); }

    Ok (()) }

/// Moving to a foreign skgrepo should be rejected.
async fn test_move_to_foreign_skgrepo_rejected (
  config : &SkgConfig,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Try to move b to foreign skgrepo.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (repo public))) a
      ** (skg (node (id b) (repo foreign))) b
      *** (skg (node (id c) (repo public))) c
    "};
    let result =
      buffer_to_validated_saveplan (
        org_text, &config
        , None ) ;
    assert!(result . is_err(),
            "Moving to foreign repo should be rejected");
    if let Err (SaveError::DatabaseError (e)) = &result {
      let inner : &dyn Error = e . as_ref();
      assert!(inner . downcast_ref::<BufferValidationError>()
              . map_or (false, |bve| matches!(
                bve, BufferValidationError::CannotMoveToOrFromForeignSkgRepo(_, _, _))),
              "Expected CannotMoveToOrFromForeignRepo, got: {}", e);
    } else if let Err (other) = &result {
      panic!("Expected DatabaseError wrapping CannotMoveToOrFromForeignRepo, got: {:?}", other);
    }

    { // FS: nothing should have changed
      assert!( temp_fixtures . join ("owned/public/b.skg") . exists(),
               "b.skg should still be in public/"); }

    Ok (()) }

/// Moving from a foreign skgrepo should be rejected.
async fn test_move_from_foreign_skgrepo_rejected (
  config : &SkgConfig,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Try to move foreign-node to public.
    let org_text : &str = indoc! {"
      * (skg (node (id foreign-node) (repo public))) foreign-node
    "};
    let result =
      buffer_to_validated_saveplan (
        org_text, &config
        , None ) ;
    assert!(result . is_err(),
            "Moving from foreign repo should be rejected");

    { // FS: nothing should have changed
      assert!( temp_fixtures . join ("foreign/foreign-node.skg") . exists(),
               "foreign-node.skg should still be in foreign/"); }

    Ok (()) }

/// Moving and merging the same node simultaneously should be rejected.
async fn test_move_and_merge_simultaneously_rejected (
  config : &SkgConfig,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    // Move b to private AND merge b into stay.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (repo public))) a
      ** (skg (node (id b) (repo private) (editRequest (merge stay)))) b
      *** (skg (node (id c) (repo public))) c
      * (skg (node (id stay) (repo public))) stay
    "};
    let result =
      buffer_to_validated_saveplan (
        org_text, &config
        , None ) ;
    assert!(result . is_err(),
            "Moving and merging same node should be rejected");
    match result {
      Err (SaveError::BufferValidationErrors { errors, .. }) => {
        assert!(errors . iter() . any (|e| matches!(
          e, BufferValidationError::CannotMoveAndMergeSimultaneously (_))),
          "Expected CannotMoveAndMergeSimultaneously error, got: {:?}",
          errors); },
      Err (other) =>
        panic!("Expected BufferValidationErrors, got: {:?}", other),
      Ok (_) =>
        panic!("Expected error, got Ok"), }

    Ok (()) }

/// No skgrepo change: no RepoMove should be produced.
async fn test_no_skgrepo_change_produces_no_moves (
  config : &SkgConfig,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    // Save with same skgrepos as on disk.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (repo public))) a
      ** (skg (node (id b) (repo public))) b
      *** (skg (node (id c) (repo public))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config
          , None ) ?;
    assert_eq!(save_plan . skgrepo_moves . len(), 0,
               "No repo changes => no repo moves");

    Ok (()) }

/// Reproduces the bug: changing only the skgrepo (nothing else)
/// with a populated pool caused the nodeInstruction to be filtered out
/// by filter_wouldbe_noop_nodeInstructions (which didn't compare skgrepo).
async fn test_skgrepo_only_change_with_populated_pool (
  config : &SkgConfig,
  tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Read all nodes (for test parity with earlier pool-populating variant).
    let _nodes : Vec<Graphnode> =
      read_all_skg_files_from_skgrepos (&config)?;

    // Change only b's skgrepo to private.
    // Title, body, contains — all identical to disk.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (repo public))) a
      ** (skg (node (id b) (repo private))) b
      *** (skg (node (id c) (repo public))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config , None ) ?;

    // The skgrepo move must be detected even with populated pool.
    assert_eq!(save_plan . skgrepo_moves . len(), 1,
               "Repo-only change should produce a RepoMove");
    assert_eq!(save_plan . skgrepo_moves[0] . pid . 0, "b");

    // The nodeInstruction for b must not have been filtered out.
    let b_in_instructions : bool =
      save_plan . node_instructions . iter() . any (|i| match i {
        NodeInstruction::Save (skg::types::save::SaveNode (n)) =>
          n . pid . 0 == "b",
        _ => false });
    assert!(b_in_instructions,
            "b's save instruction must survive filtering");

    let graph : InRustGraphHandle =
      graph_handle_from_config (&config) ?;
    let replacement : Option<TantivyIndex> =
      update_graph_minus_nodeMerges (
        save_plan . node_instructions, &save_plan . skgrepo_moves,
        config . clone(), &tantivy_index,
        &graph, &skg::types::env::new_mutation_gate () ) . await?;
    if let Some (new_idx) = replacement {
      *tantivy_index = new_idx; }
    audit_inrustgraph_or_panic (&graph)?;

    { // FS: old file gone, new file present
      assert!( ! temp_fixtures . join ("owned/public/b.skg") . exists(),
               "b.skg should be deleted from public/");
      assert!( temp_fixtures . join ("owned/private/b.skg") . exists(),
               "b.skg should exist in private/"); }

    { // Graph: skgrepo updated
      let (_, skgrepo) = graph . load_full ()
        . pid_and_skgrepo (&ID::new ("b"))
        . expect ("b should exist in graph");
      assert_eq!(skgrepo . as_str(), "private"); }

    { // Tantivy: skgrepo updated
      let skgrepo : Option<String> =
        tantivy_skgrepo_for_skgid (&tantivy_index, "b", "b")?;
      assert_eq!(skgrepo . as_deref(), Some ("private")); }

    Ok (()) }
