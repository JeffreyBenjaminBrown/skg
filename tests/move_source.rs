// These tests have not been human-verified.
// cargo nextest run --test grouped_sources -E 'test(move_source::)'

use indoc::indoc;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::one_node::nodecomplete_from_id;
use skg::dbs::tantivy::search::{SearchOptions, search_index};
use skg::from_text::buffer_to_validated_saveplan as buffer_to_validated_saveplan_with_graph;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::graph_queries::nodes::pid_and_source_from_id;
use skg::save::update_graph_minus_nodeMerges;
use skg::test_utils::{run_with_shared_test_graph, extra_ids_from_pid};
use skg::types::errors::{SaveError, BufferValidationError};
use skg::source_sets::ActiveSourceSet;

use skg::types::misc::{ID, SkgConfig, SourceName, TantivyIndex, members_of};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SavePlan};
use skg::types::tree::forest::ViewForest;
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;
use tantivy::{DocAddress, TantivyDocument};
use tantivy::schema::document::Value;
use skg::types::store_state::SelectedStoreState;

async fn buffer_to_validated_saveplan (
  buffer_text      : &str,
  config           : &SkgConfig,
  fixture_graph           : &InRustGraphHandle,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  let selected : Arc<SelectedStoreState> = fixture_graph . load_full ();
  buffer_to_validated_saveplan_with_graph (
    &selected . graph,
    buffer_text, config, active_source_set,
    &selected . manifest ) . await }

/// Query Tantivy for a node by title and return its source.
fn tantivy_source_for_id (
  tantivy_index : &TantivyIndex,
  query         : &str,
  expected_id   : &str,
) -> Result<Option<String>, Box<dyn Error>> {
  // A save commits its Tantivy index update in the background, so wait
  // for it to land before reading — mirroring the production search
  // handler. (See server/dbs/tantivy/background_writer.rs.)
  skg::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
  let (matches, searcher)
    : (Vec<(f32, DocAddress)>, tantivy::Searcher) =
    search_index (tantivy_index, &tantivy_index . reader . searcher (), query, &SearchOptions::default ())?;
  for (_score, doc_address) in matches {
    let doc : TantivyDocument =
      searcher . doc (doc_address)?;
    let id_value : Option<String> =
      doc . get_first (tantivy_index . id_field)
      . and_then (|v| v . as_str() . map (String::from));
    if id_value . as_deref() == Some (expected_id) {
      let source_value : Option<String> =
        doc . get_first (tantivy_index . source_field)
        . and_then (|v| v . as_str() . map (String::from));
      return Ok (source_value); }}
  Ok (None) }


/////////////////
// Tests
/////////////////

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_graph (
    "skg-test-move-source",
    |s| Box::pin ( async move {
      s . reset ("test_move_node_to_another_owned_source", "tests/move_source/fixtures") . await ?;
      test_move_node_to_another_owned_source (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_move_node_referenced_by_extra_id", "tests/move_source/fixtures") . await ?;
      test_move_node_referenced_by_extra_id (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_move_multiple_nodes", "tests/move_source/fixtures") . await ?;
      test_move_multiple_nodes (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_move_to_foreign_source_rejected", "tests/move_source/fixtures") . await ?;
      test_move_to_foreign_source_rejected (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_move_from_foreign_source_rejected", "tests/move_source/fixtures") . await ?;
      test_move_from_foreign_source_rejected (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_move_and_merge_simultaneously_rejected", "tests/move_source/fixtures") . await ?;
      test_move_and_merge_simultaneously_rejected (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_no_source_change_produces_no_moves", "tests/move_source/fixtures") . await ?;
      test_no_source_change_produces_no_moves (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_source_only_change_with_populated_pool", "tests/move_source/fixtures") . await ?;
      test_source_only_change_with_populated_pool (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// Basic move: change b's source from public to private.
/// Verify FS (old file gone, new file present),
/// the selected graph (source updated), and Tantivy (source field updated).
async fn test_move_node_to_another_owned_source (
  config        : &SkgConfig,
  fixture_graph        : &InRustGraphHandle,
  tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // a (public) contains b (public) contains c (public).
    // Edit b's source to private.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source private))) b
      *** (skg (node (id c) (source public))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config, &fixture_graph
          , None ) . await?;
    assert_eq!(save_plan . source_moves . len(), 1,
               "Expected exactly 1 source move");
    assert_eq!(save_plan . source_moves[0] . pid . 0, "b");
    assert_eq!(save_plan . source_moves[0] . old_source . as_str(), "public");
    assert_eq!(save_plan . source_moves[0] . new_source . as_str(), "private");

    let _outcome = update_graph_minus_nodeMerges (
        save_plan . define_nodes, &save_plan . source_moves,
        config . clone(), &tantivy_index, fixture_graph ) . await?;

    { // FS: old file should be gone, new file should exist
      let old_path : PathBuf =
        temp_fixtures . join ("owned/public/b.skg");
      let new_path : PathBuf =
        temp_fixtures . join ("owned/private/b.skg");
      assert!( ! old_path . exists(),
               "b.skg should be deleted from public/");
      assert!( new_path . exists(),
               "b.skg should exist in private/"); }

    { // Read NodeComplete back from disk via graph identity lookup.
      let node_b : NodeComplete =
        nodecomplete_from_id (
          &fixture_graph . load_full () . graph, config, &ID::new ("b"))
        . await?;
      assert_eq!(node_b . source, SourceName::from ("private"),
                 "NodeComplete read from disk should have source=private"); }

    { // The selected graph source should be updated.
      let (pid, source) : (ID, SourceName) =
        pid_and_source_from_id (
          &fixture_graph . load_full () . graph, &ID::new ("b") )
        . expect ("b should exist in the graph");
      assert_eq!(pid . 0, "b");
      assert_eq!(source . as_str(), "private",
                 "the graph should show source=private for b"); }

    { // Tantivy: source should be updated
      let source : Option<String> =
        tantivy_source_for_id (&tantivy_index, "b", "b")?;
      assert_eq!(source . as_deref(), Some ("private"),
                 "Tantivy should show source=private for b"); }

    { // Other nodes unchanged
      let node_a : NodeComplete =
        nodecomplete_from_id (
          &fixture_graph . load_full () . graph, config, &ID::new ("a"))
        . await?;
      assert_eq!(node_a . source, SourceName::from ("public"));
      let node_c : NodeComplete =
        nodecomplete_from_id (
          &fixture_graph . load_full () . graph, config, &ID::new ("c"))
        . await?;
      assert_eq!(node_c . source, SourceName::from ("public")); }

    { // Containment relationships should be unchanged
      let node_a : NodeComplete =
        nodecomplete_from_id (
          &fixture_graph . load_full () . graph, config, &ID::new ("a"))
        . await?;
      assert!(members_of ( &node_a . contains ) . contains (&ID::new ("b")),
              "a should still contain b after move");
      let node_b : NodeComplete =
        nodecomplete_from_id (
          &fixture_graph . load_full () . graph, config, &ID::new ("b"))
        . await?;
      assert!(members_of ( &node_b . contains ) . contains (&ID::new ("c")),
              "b should still contain c after move"); }

    Ok (()) }

/// Nodes referenced by extra_id in the buffer (not PID).
/// The save pipeline should resolve extra_ids to PIDs
/// and the move should still work correctly.
async fn test_move_node_referenced_by_extra_id (
  config        : &SkgConfig,
  fixture_graph        : &InRustGraphHandle,
  tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Use extra_ids (a-alias, b-alias, c-alias) instead of PIDs.
    let org_text : &str = indoc! {"
      * (skg (node (id a-alias) (source public))) a
      ** (skg (node (id b-alias) (source private))) b
      *** (skg (node (id c-alias) (source public))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config, &fixture_graph , None ) . await?;

    // source_moves should use the PID, not the extra_id
    assert_eq!(save_plan . source_moves . len(), 1,
               "Expected exactly 1 source move");
    assert_eq!(save_plan . source_moves[0] . pid . 0, "b",
               "SourceMove should use PID, not extra_id");

    let _outcome = update_graph_minus_nodeMerges (
        save_plan . define_nodes, &save_plan . source_moves,
        config . clone(), &tantivy_index, fixture_graph ) . await?;

    { // FS: old file gone, new file present
      let old_path : PathBuf =
        temp_fixtures . join ("owned/public/b.skg");
      let new_path : PathBuf =
        temp_fixtures . join ("owned/private/b.skg");
      assert!( ! old_path . exists(),
               "b.skg should be deleted from public/");
      assert!( new_path . exists(),
               "b.skg should exist in private/"); }

    { // Graph source updated, extra IDs preserved.
      let (pid, source) : (ID, SourceName) =
        pid_and_source_from_id (
          &fixture_graph . load_full () . graph, &ID::new ("b") )
        . expect ("b should exist in the graph");
      assert_eq!(pid . 0, "b");
      assert_eq!(source . as_str(), "private");
      let extra_ids : Vec<ID> =
        extra_ids_from_pid (
          &fixture_graph . load_full () . graph, &ID::new ("b") );
      assert!(extra_ids . contains (&ID::new ("b-alias")),
              "extra_id b-alias should be preserved after move"); }

    { // Tantivy: source updated
      let source : Option<String> =
        tantivy_source_for_id (&tantivy_index, "b", "b")?;
      assert_eq!(source . as_deref(), Some ("private")); }

    Ok (()) }

/// Move two nodes in the same save.
async fn test_move_multiple_nodes (
  config         : &SkgConfig,
  fixture_graph         : &InRustGraphHandle,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Move both b and c to private.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source private))) b
      *** (skg (node (id c) (source private))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config, &fixture_graph
          , None ) . await?;
    assert_eq!(save_plan . source_moves . len(), 2,
               "Expected 2 source moves");

    let move_pids : Vec<&str> =
      save_plan . source_moves . iter()
      . map (|sm| sm . pid . 0 . as_str()) . collect();
    assert!(move_pids . contains (&"b"), "Should move b");
    assert!(move_pids . contains (&"c"), "Should move c");

    let _outcome = update_graph_minus_nodeMerges (
        save_plan . define_nodes, &save_plan . source_moves,
        config . clone(), &_tantivy_index, fixture_graph ) . await?;

    { // FS
      assert!( ! temp_fixtures . join ("owned/public/b.skg") . exists() );
      assert!( ! temp_fixtures . join ("owned/public/c.skg") . exists() );
      assert!( temp_fixtures . join ("owned/private/b.skg") . exists() );
      assert!( temp_fixtures . join ("owned/private/c.skg") . exists() );
      // a stays in public
      assert!( temp_fixtures . join ("owned/public/a.skg") . exists() ); }

    { // Selected graph.
      let (_, source_b) =
        pid_and_source_from_id (
          &fixture_graph . load_full () . graph, &ID::new ("b") )
        . expect ("b should exist");
      let (_, source_c) =
        pid_and_source_from_id (
          &fixture_graph . load_full () . graph, &ID::new ("c") )
        . expect ("c should exist");
      assert_eq!(source_b . as_str(), "private");
      assert_eq!(source_c . as_str(), "private"); }

    Ok (()) }

/// Moving to a foreign source should be rejected.
async fn test_move_to_foreign_source_rejected (
  config         : &SkgConfig,
  fixture_graph         : &InRustGraphHandle,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Try to move b to foreign source.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source foreign))) b
      *** (skg (node (id c) (source public))) c
    "};
    let result =
      buffer_to_validated_saveplan (
        org_text, &config, &fixture_graph
        , None ) . await;
    assert!(result . is_err(),
            "Moving to foreign source should be rejected");
    if let Err (SaveError::DatabaseError (e)) = &result {
      let inner : &dyn Error = e . as_ref();
      assert!(inner . downcast_ref::<BufferValidationError>()
              . map_or (false, |bve| matches!(
                bve, BufferValidationError::CannotMoveToOrFromForeignSource(_, _, _))),
              "Expected CannotMoveToOrFromForeignSource, got: {}", e);
    } else if let Err (other) = &result {
      panic!("Expected DatabaseError wrapping CannotMoveToOrFromForeignSource, got: {:?}", other);
    }

    { // FS: nothing should have changed
      assert!( temp_fixtures . join ("owned/public/b.skg") . exists(),
               "b.skg should still be in public/"); }

    Ok (()) }

/// Moving from a foreign source should be rejected.
async fn test_move_from_foreign_source_rejected (
  config         : &SkgConfig,
  fixture_graph         : &InRustGraphHandle,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Try to move foreign-node to public.
    let org_text : &str = indoc! {"
      * (skg (node (id foreign-node) (source public))) foreign-node
    "};
    let result =
      buffer_to_validated_saveplan (
        org_text, &config, &fixture_graph
        , None ) . await;
    assert!(result . is_err(),
            "Moving from foreign source should be rejected");

    { // FS: nothing should have changed
      assert!( temp_fixtures . join ("foreign/foreign-node.skg") . exists(),
               "foreign-node.skg should still be in foreign/"); }

    Ok (()) }

/// Moving and merging the same node simultaneously should be rejected.
async fn test_move_and_merge_simultaneously_rejected (
  config         : &SkgConfig,
  fixture_graph         : &InRustGraphHandle,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    // Move b to private AND merge b into stay.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source private) (editRequest (merge stay)))) b
      *** (skg (node (id c) (source public))) c
      * (skg (node (id stay) (source public))) stay
    "};
    let result =
      buffer_to_validated_saveplan (
        org_text, &config, &fixture_graph
        , None ) . await;
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

/// No source change: no SourceMove should be produced.
async fn test_no_source_change_produces_no_moves (
  config         : &SkgConfig,
  fixture_graph         : &InRustGraphHandle,
  _tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    // Save with same sources as on disk.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source public))) b
      *** (skg (node (id c) (source public))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config, &fixture_graph
          , None ) . await?;
    assert_eq!(save_plan . source_moves . len(), 0,
               "No source changes => no source moves");

    Ok (()) }

/// Reproduces the bug: changing only the source (nothing else)
/// with a populated pool caused the instruction to be filtered out
/// by filter_wouldbe_noop_defineNodes (which didn't compare source).
async fn test_source_only_change_with_populated_pool (
  config        : &SkgConfig,
  fixture_graph        : &InRustGraphHandle,
  tantivy_index : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
    let temp_fixtures : &PathBuf = &config . data_root;

    // Read all nodes (for test parity with earlier pool-populating variant).
    let _nodes : Vec<NodeComplete> =
      read_all_skg_files_from_sources (&config)?;

    // Change only b's source to private.
    // Title, body, contains — all identical to disk.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source private))) b
      *** (skg (node (id c) (source public))) c
    "};
    let ( _viewforest, save_plan, _warnings )
      = buffer_to_validated_saveplan (
          org_text, &config, &fixture_graph , None ) . await?;

    // The source move must be detected even with populated pool.
    assert_eq!(save_plan . source_moves . len(), 1,
               "Source-only change should produce a SourceMove");
    assert_eq!(save_plan . source_moves[0] . pid . 0, "b");

    // The save instruction for b must not have been filtered out.
    let b_in_instructions : bool =
      save_plan . define_nodes . iter() . any (|i| match i {
        DefineNode::Save (skg::types::save::SaveNode (n)) =>
          n . pid . 0 == "b",
        _ => false });
    assert!(b_in_instructions,
            "b's save instruction must survive filtering");

    let _outcome = update_graph_minus_nodeMerges (
        save_plan . define_nodes, &save_plan . source_moves,
        config . clone(), &tantivy_index, fixture_graph ) . await?;

    { // FS: old file gone, new file present
      assert!( ! temp_fixtures . join ("owned/public/b.skg") . exists(),
               "b.skg should be deleted from public/");
      assert!( temp_fixtures . join ("owned/private/b.skg") . exists(),
               "b.skg should exist in private/"); }

    { // Graph source updated.
      let (_, source) =
        pid_and_source_from_id (
          &fixture_graph . load_full () . graph, &ID::new ("b") )
        . expect ("b should exist in the graph");
      assert_eq!(source . as_str(), "private"); }

    { // Tantivy: source updated
      let source : Option<String> =
        tantivy_source_for_id (&tantivy_index, "b", "b")?;
      assert_eq!(source . as_deref(), Some ("private")); }

    Ok (()) }
