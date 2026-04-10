// These tests have not been human-verified.
// cargo nextest run --test move_source

use futures::executor::block_on;
use indoc::indoc;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::one_node::skgnode_from_id;
use skg::dbs::init::{overwrite_new_empty_db, define_schema, create_empty_tantivy_index};
use skg::dbs::tantivy::search_index;
use skg::dbs::typedb::nodes::create_all_nodes;
use skg::dbs::typedb::relationships::create_all_relationships;
use skg::from_text::buffer_to_viewnode_forest_and_save_instructions;
use skg::save::update_graph_minus_merges;
use skg::test_utils::cleanup_test_tantivy_and_typedb_dbs;
use skg::types::errors::{SaveError, BufferValidationError};
use skg::types::memory::SkgNodeMap;
use skg::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use skg::types::skgnode::SkgNode;
use skg::types::save::DefineNode;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::fs;
use tantivy::DocAddress;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};

/// Set up a fresh test environment: copy fixtures to temp,
/// create TypeDB + Tantivy, return (config, driver, tantivy).
async fn setup (
  db_name     : &str,
  temp_prefix : &str,
) -> Result<(SkgConfig, TypeDBDriver, TantivyIndex, PathBuf), Box<dyn Error>> {
  let temp_fixtures : PathBuf =
    PathBuf::from ( format!("/tmp/{}-fixtures", temp_prefix) );
  if temp_fixtures . exists() {
    fs::remove_dir_all (&temp_fixtures)?; }
  copy_dir_all (
    &PathBuf::from ("tests/move_source/fixtures"),
    &temp_fixtures )?;
  let config : SkgConfig =
    load_config_with_overrides (
      temp_fixtures . join ("skgconfig.toml")
        . to_str() . unwrap(),
      Some (db_name),
      &[ ("public",  temp_fixtures . join ("public")),
         ("private", temp_fixtures . join ("private")),
         ("foreign", temp_fixtures . join ("foreign")) ] )?;
  let driver : TypeDBDriver =
    TypeDBDriver::new (
      "127.0.0.1:1729",
      Credentials::new ("admin", "password"),
      DriverOptions::new (false, None)? ) . await?;
  let nodes : Vec<SkgNode> =
    read_all_skg_files_from_sources (&config)?;
  overwrite_new_empty_db (db_name, &driver) . await?;
  define_schema (db_name, &driver) . await?;
  create_all_nodes (db_name, &driver, &nodes) . await?;
  create_all_relationships (db_name, &driver, &nodes) . await?;
  let tantivy_index : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder)?;
  Ok ((config, driver, tantivy_index, temp_fixtures)) }

async fn teardown (
  db_name        : &str,
  driver         : &TypeDBDriver,
  config         : &SkgConfig,
  temp_fixtures  : &Path,
) -> Result<(), Box<dyn Error>> {
  cleanup_test_tantivy_and_typedb_dbs (
    db_name, driver,
    Some (config . tantivy_folder . as_path()) ) . await?;
  if temp_fixtures . exists() {
    fs::remove_dir_all (temp_fixtures)?; }
  Ok (()) }

fn copy_dir_all (
  src : &PathBuf,
  dst : &PathBuf,
) -> std::io::Result<()> {
  fs::create_dir_all (dst)?;
  for entry in fs::read_dir (src)? {
    let entry = entry?;
    let ty = entry . file_type()?;
    if ty . is_dir() {
      copy_dir_all (
        &entry . path(),
        &dst . join (entry . file_name()) )?;
    } else {
      fs::copy (
        entry . path(),
        dst . join (entry . file_name()) )?; }}
  Ok (()) }

/// Query Tantivy for a node by title and return its source.
fn tantivy_source_for_id (
  tantivy_index : &TantivyIndex,
  query         : &str,
  expected_id   : &str,
) -> Result<Option<String>, Box<dyn Error>> {
  let (matches, searcher)
    : (Vec<(f32, DocAddress)>, tantivy::Searcher) =
    search_index (tantivy_index, query)?;
  for (_score, doc_address) in matches {
    let doc : tantivy::Document =
      searcher . doc (doc_address)?;
    let id_value : Option<String> =
      doc . get_first (tantivy_index . id_field)
      . and_then (|v| v . as_text() . map (String::from));
    if id_value . as_deref() == Some (expected_id) {
      let source_value : Option<String> =
        doc . get_first (tantivy_index . source_field)
        . and_then (|v| v . as_text() . map (String::from));
      return Ok (source_value); }}
  Ok (None) }


/////////////////
// Tests
/////////////////

/// Basic move: change b's source from public to private.
/// Verify FS (old file gone, new file present),
/// TypeDB (source attribute updated), and Tantivy (source field updated).
#[test]
fn test_move_node_to_another_owned_source (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str = "skg-test-move-source-1";
    let (config, driver, mut tantivy_index, temp_fixtures)
      : (SkgConfig, TypeDBDriver, TantivyIndex, PathBuf)
      = setup (db_name, "move-source-1") . await?;

    // a (public) contains b (public) contains c (public).
    // Edit b's source to private.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source private))) b
      *** (skg (node (id c) (source public))) c
    "};
    let (_forest, instructions, _merges, source_moves)
      = buffer_to_viewnode_forest_and_save_instructions (
          org_text, &config, &driver,
          &SkgNodeMap::new() ) . await?;
    assert_eq!(source_moves . len(), 1,
               "Expected exactly 1 source move");
    assert_eq!(source_moves[0] . pid . 0, "b");
    assert_eq!(source_moves[0] . old_source . as_str(), "public");
    assert_eq!(source_moves[0] . new_source . as_str(), "private");

    let replacement : Option<TantivyIndex> =
      update_graph_minus_merges (
        instructions, &source_moves,
        config . clone(), &tantivy_index, &driver ) . await?;
    if let Some (new_idx) = replacement {
      tantivy_index = new_idx; }

    { // FS: old file should be gone, new file should exist
      let old_path : PathBuf =
        temp_fixtures . join ("public/b.skg");
      let new_path : PathBuf =
        temp_fixtures . join ("private/b.skg");
      assert!( ! old_path . exists(),
               "b.skg should be deleted from public/");
      assert!( new_path . exists(),
               "b.skg should exist in private/"); }

    { // FS: read SkgNode back from disk via TypeDB lookup
      let node_b : SkgNode =
        skgnode_from_id (&config, &driver, &ID::new ("b"))
        . await?;
      assert_eq!(node_b . source, SourceName::from ("private"),
                 "SkgNode read from disk should have source=private"); }

    { // TypeDB: source should be updated
      let (pid, source) : (ID, SourceName) =
        skg::dbs::typedb::search::pid_and_source_from_id (
          &config . db_name, &driver, &ID::new ("b") ) . await?
        . expect ("b should exist in TypeDB");
      assert_eq!(pid . 0, "b");
      assert_eq!(source . as_str(), "private",
                 "TypeDB should show source=private for b"); }

    { // Tantivy: source should be updated
      let source : Option<String> =
        tantivy_source_for_id (&tantivy_index, "b", "b")?;
      assert_eq!(source . as_deref(), Some ("private"),
                 "Tantivy should show source=private for b"); }

    { // Other nodes unchanged
      let node_a : SkgNode =
        skgnode_from_id (&config, &driver, &ID::new ("a"))
        . await?;
      assert_eq!(node_a . source, SourceName::from ("public"));
      let node_c : SkgNode =
        skgnode_from_id (&config, &driver, &ID::new ("c"))
        . await?;
      assert_eq!(node_c . source, SourceName::from ("public")); }

    { // Containment relationships should be unchanged
      let node_a : SkgNode =
        skgnode_from_id (&config, &driver, &ID::new ("a"))
        . await?;
      assert!(node_a . contains . contains (&ID::new ("b")),
              "a should still contain b after move");
      let node_b : SkgNode =
        skgnode_from_id (&config, &driver, &ID::new ("b"))
        . await?;
      assert!(node_b . contains . contains (&ID::new ("c")),
              "b should still contain c after move"); }

    teardown (db_name, &driver, &config, &temp_fixtures) . await?;
    Ok (()) } ) }

/// Nodes referenced by extra_id in the buffer (not PID).
/// The save pipeline should resolve extra_ids to PIDs
/// and the move should still work correctly.
#[test]
fn test_move_node_referenced_by_extra_id (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str = "skg-test-move-source-2";
    let (config, driver, mut tantivy_index, temp_fixtures)
      : (SkgConfig, TypeDBDriver, TantivyIndex, PathBuf)
      = setup (db_name, "move-source-2") . await?;

    // Use extra_ids (a-alias, b-alias, c-alias) instead of PIDs.
    let org_text : &str = indoc! {"
      * (skg (node (id a-alias) (source public))) a
      ** (skg (node (id b-alias) (source private))) b
      *** (skg (node (id c-alias) (source public))) c
    "};
    let (_forest, instructions, _merges, source_moves)
      = buffer_to_viewnode_forest_and_save_instructions (
          org_text, &config, &driver,
          &SkgNodeMap::new() ) . await?;

    // source_moves should use the PID, not the extra_id
    assert_eq!(source_moves . len(), 1,
               "Expected exactly 1 source move");
    assert_eq!(source_moves[0] . pid . 0, "b",
               "SourceMove should use PID, not extra_id");

    let replacement : Option<TantivyIndex> =
      update_graph_minus_merges (
        instructions, &source_moves,
        config . clone(), &tantivy_index, &driver ) . await?;
    if let Some (new_idx) = replacement {
      tantivy_index = new_idx; }

    { // FS: old file gone, new file present
      let old_path : PathBuf =
        temp_fixtures . join ("public/b.skg");
      let new_path : PathBuf =
        temp_fixtures . join ("private/b.skg");
      assert!( ! old_path . exists(),
               "b.skg should be deleted from public/");
      assert!( new_path . exists(),
               "b.skg should exist in private/"); }

    { // TypeDB: source updated, extra_ids preserved
      let (pid, source) : (ID, SourceName) =
        skg::dbs::typedb::search::pid_and_source_from_id (
          &config . db_name, &driver, &ID::new ("b") ) . await?
        . expect ("b should exist in TypeDB");
      assert_eq!(pid . 0, "b");
      assert_eq!(source . as_str(), "private");
      let extra_ids : Vec<ID> =
        skg::test_utils::extra_ids_from_pid (
          &config . db_name, &driver, &ID::new ("b") ) . await?;
      assert!(extra_ids . contains (&ID::new ("b-alias")),
              "extra_id b-alias should be preserved after move"); }

    { // Tantivy: source updated
      let source : Option<String> =
        tantivy_source_for_id (&tantivy_index, "b", "b")?;
      assert_eq!(source . as_deref(), Some ("private")); }

    teardown (db_name, &driver, &config, &temp_fixtures) . await?;
    Ok (()) } ) }

/// Move two nodes in the same save.
#[test]
fn test_move_multiple_nodes (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str = "skg-test-move-source-3";
    let (config, driver, mut _tantivy_index, temp_fixtures)
      : (SkgConfig, TypeDBDriver, TantivyIndex, PathBuf)
      = setup (db_name, "move-source-3") . await?;

    // Move both b and c to private.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source private))) b
      *** (skg (node (id c) (source private))) c
    "};
    let (_forest, instructions, _merges, source_moves)
      = buffer_to_viewnode_forest_and_save_instructions (
          org_text, &config, &driver,
          &SkgNodeMap::new() ) . await?;
    assert_eq!(source_moves . len(), 2,
               "Expected 2 source moves");

    let move_pids : Vec<&str> =
      source_moves . iter() . map (|sm| sm . pid . 0 . as_str()) . collect();
    assert!(move_pids . contains (&"b"), "Should move b");
    assert!(move_pids . contains (&"c"), "Should move c");

    let replacement : Option<TantivyIndex> =
      update_graph_minus_merges (
        instructions, &source_moves,
        config . clone(), &_tantivy_index, &driver ) . await?;
    if let Some (new_idx) = replacement {
      _tantivy_index = new_idx; }

    { // FS
      assert!( ! temp_fixtures . join ("public/b.skg") . exists() );
      assert!( ! temp_fixtures . join ("public/c.skg") . exists() );
      assert!( temp_fixtures . join ("private/b.skg") . exists() );
      assert!( temp_fixtures . join ("private/c.skg") . exists() );
      // a stays in public
      assert!( temp_fixtures . join ("public/a.skg") . exists() ); }

    { // TypeDB
      let (_, source_b) =
        skg::dbs::typedb::search::pid_and_source_from_id (
          &config . db_name, &driver, &ID::new ("b") ) . await?
        . expect ("b should exist");
      let (_, source_c) =
        skg::dbs::typedb::search::pid_and_source_from_id (
          &config . db_name, &driver, &ID::new ("c") ) . await?
        . expect ("c should exist");
      assert_eq!(source_b . as_str(), "private");
      assert_eq!(source_c . as_str(), "private"); }

    teardown (db_name, &driver, &config, &temp_fixtures) . await?;
    Ok (()) } ) }

/// Moving to a foreign source should be rejected.
#[test]
fn test_move_to_foreign_source_rejected (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str = "skg-test-move-source-4";
    let (config, driver, _tantivy_index, temp_fixtures)
      : (SkgConfig, TypeDBDriver, TantivyIndex, PathBuf)
      = setup (db_name, "move-source-4") . await?;

    // Try to move b to foreign source.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source foreign))) b
      *** (skg (node (id c) (source public))) c
    "};
    let result =
      buffer_to_viewnode_forest_and_save_instructions (
        org_text, &config, &driver,
        &SkgNodeMap::new() ) . await;
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
      assert!( temp_fixtures . join ("public/b.skg") . exists(),
               "b.skg should still be in public/"); }

    teardown (db_name, &driver, &config, &temp_fixtures) . await?;
    Ok (()) } ) }

/// Moving from a foreign source should be rejected.
#[test]
fn test_move_from_foreign_source_rejected (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str = "skg-test-move-source-5";
    let (config, driver, _tantivy_index, temp_fixtures)
      : (SkgConfig, TypeDBDriver, TantivyIndex, PathBuf)
      = setup (db_name, "move-source-5") . await?;

    // Try to move foreign-node to public.
    let org_text : &str = indoc! {"
      * (skg (node (id foreign-node) (source public))) foreign-node
    "};
    let result =
      buffer_to_viewnode_forest_and_save_instructions (
        org_text, &config, &driver,
        &SkgNodeMap::new() ) . await;
    assert!(result . is_err(),
            "Moving from foreign source should be rejected");

    { // FS: nothing should have changed
      assert!( temp_fixtures . join ("foreign/foreign-node.skg") . exists(),
               "foreign-node.skg should still be in foreign/"); }

    teardown (db_name, &driver, &config, &temp_fixtures) . await?;
    Ok (()) } ) }

/// Moving and merging the same node simultaneously should be rejected.
#[test]
fn test_move_and_merge_simultaneously_rejected (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str = "skg-test-move-source-6";
    let (config, driver, _tantivy_index, temp_fixtures)
      : (SkgConfig, TypeDBDriver, TantivyIndex, PathBuf)
      = setup (db_name, "move-source-6") . await?;

    // Move b to private AND merge b into stay.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source private) (editRequest (merge stay)))) b
      *** (skg (node (id c) (source public))) c
      * (skg (node (id stay) (source public))) stay
    "};
    let result =
      buffer_to_viewnode_forest_and_save_instructions (
        org_text, &config, &driver,
        &SkgNodeMap::new() ) . await;
    assert!(result . is_err(),
            "Moving and merging same node should be rejected");
    match result {
      Err (SaveError::BufferValidationErrors (errors)) => {
        assert!(errors . iter() . any (|e| matches!(
          e, BufferValidationError::CannotMoveAndMergeSimultaneously (_))),
          "Expected CannotMoveAndMergeSimultaneously error, got: {:?}",
          errors); },
      Err (other) =>
        panic!("Expected BufferValidationErrors, got: {:?}", other),
      Ok (_) =>
        panic!("Expected error, got Ok"), }

    teardown (db_name, &driver, &config, &temp_fixtures) . await?;
    Ok (()) } ) }

/// No source change: no SourceMove should be produced.
#[test]
fn test_no_source_change_produces_no_moves (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str = "skg-test-move-source-7";
    let (config, driver, _tantivy_index, temp_fixtures)
      : (SkgConfig, TypeDBDriver, TantivyIndex, PathBuf)
      = setup (db_name, "move-source-7") . await?;

    // Save with same sources as on disk.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source public))) b
      *** (skg (node (id c) (source public))) c
    "};
    let (_forest, _instructions, _merges, source_moves)
      = buffer_to_viewnode_forest_and_save_instructions (
          org_text, &config, &driver,
          &SkgNodeMap::new() ) . await?;
    assert_eq!(source_moves . len(), 0,
               "No source changes => no source moves");

    teardown (db_name, &driver, &config, &temp_fixtures) . await?;
    Ok (()) } ) }

/// Reproduces the bug: changing only the source (nothing else)
/// with a populated pool caused the instruction to be filtered out
/// by filter_unchanged_save_instructions (which didn't compare source).
#[test]
fn test_source_only_change_with_populated_pool (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str = "skg-test-move-source-8";
    let (config, driver, mut tantivy_index, temp_fixtures)
      : (SkgConfig, TypeDBDriver, TantivyIndex, PathBuf)
      = setup (db_name, "move-source-8") . await?;

    // Populate pool with the nodes as they exist on disk (source=public).
    let pool : SkgNodeMap = {
      let nodes : Vec<SkgNode> =
        read_all_skg_files_from_sources (&config)?;
      let mut m : SkgNodeMap = SkgNodeMap::new();
      for node in nodes {
        m . insert (node . pid . clone(), node); }
      m };

    // Change only b's source to private.
    // Title, body, contains — all identical to disk.
    let org_text : &str = indoc! {"
      * (skg (node (id a) (source public))) a
      ** (skg (node (id b) (source private))) b
      *** (skg (node (id c) (source public))) c
    "};
    let (_forest, instructions, _merges, source_moves)
      = buffer_to_viewnode_forest_and_save_instructions (
          org_text, &config, &driver, &pool ) . await?;

    // The source move must be detected even with populated pool.
    assert_eq!(source_moves . len(), 1,
               "Source-only change should produce a SourceMove");
    assert_eq!(source_moves[0] . pid . 0, "b");

    // The save instruction for b must not have been filtered out.
    let b_in_instructions : bool =
      instructions . iter() . any (|i| match i {
        DefineNode::Save (skg::types::save::SaveNode (n)) =>
          n . pid . 0 == "b",
        _ => false });
    assert!(b_in_instructions,
            "b's save instruction must survive filtering");

    let replacement : Option<TantivyIndex> =
      update_graph_minus_merges (
        instructions, &source_moves,
        config . clone(), &tantivy_index, &driver ) . await?;
    if let Some (new_idx) = replacement {
      tantivy_index = new_idx; }

    { // FS: old file gone, new file present
      assert!( ! temp_fixtures . join ("public/b.skg") . exists(),
               "b.skg should be deleted from public/");
      assert!( temp_fixtures . join ("private/b.skg") . exists(),
               "b.skg should exist in private/"); }

    { // TypeDB: source updated
      let (_, source) =
        skg::dbs::typedb::search::pid_and_source_from_id (
          &config . db_name, &driver, &ID::new ("b") ) . await?
        . expect ("b should exist in TypeDB");
      assert_eq!(source . as_str(), "private"); }

    { // Tantivy: source updated
      let source : Option<String> =
        tantivy_source_for_id (&tantivy_index, "b", "b")?;
      assert_eq!(source . as_deref(), Some ("private")); }

    teardown (db_name, &driver, &config, &temp_fixtures) . await?;
    Ok (()) } ) }
