// cargo nextest run --test grouped_unit -E 'test(multi_source_loading::)'

use std::collections::HashMap;
use std::fs;
use std::io::{Result as IoResult, Error as IoError, ErrorKind as IoErrorKind};
use std::path::PathBuf;
use tempfile::{tempdir, TempDir};

use skg::dbs::filesystem::multiple_nodes::error_unless_each_id_names_one_node;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::one_node::write_nodecomplete_to_source;
use skg::test_utils::set_source_retagging_levels;
use skg::types::misc::{SkgfileSource, SkgConfig, ID, SourceName};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};

/// Helper to create a minimal SkgConfig for tests.
/// `data_root` should be each test's tempdir so that any
/// `initialization-error_*.org` reports land inside the tempdir
/// rather than polluting the project root.
fn test_config(
  sources   : HashMap<SourceName, SkgfileSource>,
  data_root : PathBuf,
) -> SkgConfig {
  let mut cfg : SkgConfig = SkgConfig::dummyFromSources (sources);
  cfg . data_root = data_root;
  cfg }

#[test]
fn test_load_from_single_source() {
  let temp_dir : TempDir = tempdir() . unwrap();
  let source_path : PathBuf = temp_dir . path() . join ("main");
  fs::create_dir_all (&source_path) . unwrap();

  let config : SkgConfig = { // Needed by write_nodecomplete_to_source
    let mut sources : HashMap<SourceName, SkgfileSource> =
      HashMap::new();
    sources . insert(
      SourceName::from ("main"),
      SkgfileSource {
        name: SourceName::from ("main"),
        abbreviation: None,
        path: source_path . clone(),
        user_owns_it: true, } );
    test_config (sources, temp_dir . path () . to_path_buf ()) };

  // Create a test node
  let mut node : NodeComplete = empty_node_complete();
  node . pid = ID::new ("test1");
  node . title = "Test Node 1" . to_string();
  set_source_retagging_levels ( &mut node, &SourceName::from ("main") );
  write_nodecomplete_to_source(&node, &config) . unwrap();

  let result : IoResult<Vec<NodeComplete>> =
    read_all_skg_files_from_sources (&config);
  assert!(result . is_ok(),
          "Should successfully load from single source");

  let nodes : Vec<NodeComplete> = result . unwrap();
  assert_eq!(nodes . len(), 1, "Should have loaded 1 node");
  assert_eq!(&*nodes[0] . source, "main", "Source should be 'main'");
  assert_eq!(nodes[0] . title, "Test Node 1");
}

#[test]
fn test_load_from_multiple_sources() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create two source directories
  let main_path : PathBuf = temp_dir . path() . join ("main");
  let shared_path : PathBuf = temp_dir . path() . join ("shared");
  fs::create_dir_all (&main_path) . unwrap();
  fs::create_dir_all (&shared_path) . unwrap();

  let config : SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> = HashMap::new();
    sources . insert(
      SourceName::from ("main"),
      SkgfileSource {
        name: SourceName::from ("main"),
        abbreviation: None,
        path: main_path,
        user_owns_it: true, } );
    sources . insert(
      SourceName::from ("shared"),
      SkgfileSource {
        name: SourceName::from ("shared"),
        abbreviation: None,
        path: shared_path,
        user_owns_it: false, } );
    test_config (sources, temp_dir . path () . to_path_buf ()) };

  // Create nodes in main source
  let mut node1 : NodeComplete = empty_node_complete();
  node1 . pid = ID::new ("main1");
  node1 . title = "Main Node 1" . to_string();
  set_source_retagging_levels ( &mut node1, &SourceName::from ("main") );
  write_nodecomplete_to_source(&node1, &config) . unwrap();

  let mut node2 : NodeComplete = empty_node_complete();
  node2 . pid = ID::new ("main2");
  node2 . title = "Main Node 2" . to_string();
  set_source_retagging_levels ( &mut node2, &SourceName::from ("main") );
  write_nodecomplete_to_source(&node2, &config) . unwrap();

  // Create a node in the shared source. Written RAW: 'shared' is
  // foreign, and the node writer is the SAVE path, which refuses a
  // foreign home rather than drop the title silently. Planting a
  // foreign fixture is a filesystem act, not a save.
  fs::write (
    config . sources . get (&SourceName::from ("shared"))
      . unwrap () . path . join ("shared1.skg"),
    "pid: shared1\ntitle: Shared Node 1\n" ) . unwrap();

  let result : IoResult<Vec<NodeComplete>> =
    read_all_skg_files_from_sources (&config);
  assert!(result . is_ok(), "Should successfully load from multiple sources");

  let nodes : Vec<NodeComplete> = result . unwrap();
  assert_eq!(nodes . len(), 3, "Should have loaded 3 nodes total");

  // Verify sources are set correctly
  let main_nodes: Vec<&NodeComplete> = nodes . iter()
    . filter(|n| &*n . source == "main")
    . collect();
  let shared_nodes: Vec<&NodeComplete> = nodes . iter()
    . filter(|n| &*n . source == "shared")
    . collect();

  assert_eq!(main_nodes . len(), 2, "Should have 2 nodes from main");
  assert_eq!(shared_nodes . len(), 1, "Should have 1 node from shared");
}

#[test]
fn test_telescope_is_not_a_conflict_but_two_pids_are() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create two source directories
  let main_path : PathBuf = temp_dir . path() . join ("main");
  let shared_path : PathBuf = temp_dir . path() . join ("shared");
  fs::create_dir_all (&main_path) . unwrap();
  fs::create_dir_all (&shared_path) . unwrap();

  let config : SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> =
      HashMap::new();
    sources . insert(
      SourceName::from ("main"),
      SkgfileSource {
        name: SourceName::from ("main"),
        abbreviation: None,
        path: main_path,
        user_owns_it: true, } );
    sources . insert(
      SourceName::from ("shared"),
      SkgfileSource {
        name: SourceName::from ("shared"),
        abbreviation: None,
        path: shared_path,
        user_owns_it: false, } );
    test_config (sources, temp_dir . path () . to_path_buf ()) };

  // Same pid in both sources: no longer a duplicate -- they are the
  // SECTIONS of one privacy telescope, folded into one node whose
  // home is the more public section (alphabetical fallback order for
  // this dummy config: "main" precedes "shared"). The stray second
  // title is a fold warning, not an error. The section files are
  // written RAW: a whole-node write would (correctly) sweep the
  // pid's sections at other levels, so two sequential
  // write_nodecomplete_to_source calls cannot build a telescope.
  fs::write (
    config . sources . get (&SourceName::from ("main"))
      . unwrap () . path . join ("duplicate_id.skg"),
    "pid: duplicate_id\ntitle: Node in Main\n" ) . unwrap ();
  fs::write (
    config . sources . get (&SourceName::from ("shared"))
      . unwrap () . path . join ("duplicate_id.skg"),
    "pid: duplicate_id\ntitle: Node in Shared\n" ) . unwrap ();

  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config) . unwrap();
  assert_eq!( nodes . len(), 1,
    "same-pid files across sources fold into one telescope" );
  assert_eq!( nodes[0] . title, "Node in Main",
    "the home (most public titled section) wins the title" );
  assert_eq!( nodes[0] . source, SourceName::from ("main") );
  error_unless_each_id_names_one_node (
    &nodes, &config . data_root)
    . expect ("a telescope is not an id conflict");

  { // What REMAINS a conflict: one id claimed by two distinct pids
    // (here via extra_ids).
    let mut node_a : NodeComplete = empty_node_complete();
    node_a . pid = ID::new ("pid-a");
    node_a . title = "A" . to_string();
    node_a . extra_ids = vec! [ ID::new ("contested") ];
    set_source_retagging_levels ( &mut node_a, &SourceName::from ("main") );
    let mut node_b : NodeComplete = empty_node_complete();
    node_b . pid = ID::new ("pid-b");
    node_b . title = "B" . to_string();
    node_b . extra_ids = vec! [ ID::new ("contested") ];
    set_source_retagging_levels ( &mut node_b, &SourceName::from ("shared") );
    let result : IoResult<()> =
      error_unless_each_id_names_one_node (
        & [ node_a, node_b ], &config . data_root);
    assert!(result . is_err(), "Should fail: two pids claim one id");
    let err : IoError = result . unwrap_err();
    assert_eq!(err . kind(), IoErrorKind::InvalidData);
    let err_msg = err . to_string();
    assert!(err_msg . contains ("claimed by more than one node"),
            "Error should say what the violation is: {}", err_msg);
    assert!(err_msg . contains ("contested"), "Error should include the ID"); }
}

#[test]
fn test_one_id_claimed_by_a_pid_and_anothers_extra_id() {
  let temp_dir : TempDir = tempdir() . unwrap();

  // Create two source directories
  let main_path : PathBuf = temp_dir . path() . join ("main");
  let shared_path : PathBuf = temp_dir . path() . join ("shared");
  fs::create_dir_all (&main_path) . unwrap();
  fs::create_dir_all (&shared_path) . unwrap();

  let config : SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> =
      HashMap::new();
    sources . insert(
      SourceName::from ("main"),
      SkgfileSource {
        name: SourceName::from ("main"),
        abbreviation: None,
        path: main_path,
        user_owns_it: true, } );
    sources . insert(
      SourceName::from ("shared"),
      SkgfileSource {
        name: SourceName::from ("shared"),
        abbreviation: None,
        path: shared_path,
        user_owns_it: false, } );
    test_config (sources, temp_dir . path () . to_path_buf ()) };

  // Create node in main with multiple IDs
  let mut node1 : NodeComplete = empty_node_complete();
  node1 . pid = ID::new ("id1");
  node1 . extra_ids = vec![ID::new ("id2")];
  node1 . title = "Node with Multiple IDs" . to_string();
  set_source_retagging_levels ( &mut node1, &SourceName::from ("main") );
  write_nodecomplete_to_source(&node1, &config) . unwrap();

  // Create node in shared that has one overlapping ID. Written RAW:
  // 'shared' is foreign, and the node writer refuses a foreign home.
  fs::write (
    config . sources . get (&SourceName::from ("shared"))
      . unwrap () . path . join ("id2.skg"),
    "pid: id2\ntitle: Another Node\nextra_ids:\n- id3\n" ) . unwrap();

  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config) . unwrap();
  let result : IoResult<()> =
    error_unless_each_id_names_one_node (
      &nodes, &config . data_root);
  assert!(result . is_err(), "Should fail due to overlapping ID");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::InvalidData);
  assert!(err . to_string() . contains ("id2"), "Error should mention the overlapping ID");
}

#[test]
fn test_load_from_empty_sources() {
  let temp_dir : TempDir = tempdir() . unwrap();
  let source_path : PathBuf = temp_dir . path() . join ("empty_source");
  fs::create_dir_all (&source_path) . unwrap();

  let result : IoResult<Vec<NodeComplete>> = {
    let mut sources : HashMap<SourceName, SkgfileSource> =
      HashMap::new();
    sources . insert(
      SourceName::from ("empty"),
      SkgfileSource {
        name: SourceName::from ("empty"),
        abbreviation: None,
        path: source_path,
        user_owns_it: true, } );
    read_all_skg_files_from_sources(
      &test_config (sources,
                    temp_dir . path () . to_path_buf () )) };
  assert!(result . is_ok(),
          "Should successfully handle empty source");

  let nodes : Vec<NodeComplete> = result . unwrap();
  assert_eq!(nodes . len(), 0,
             "Should have loaded 0 nodes from empty source");
}

#[test]
fn test_source_field_set_correctly() {
  let temp_dir : TempDir = tempdir() . unwrap();

  let source_a : PathBuf = temp_dir . path() . join ("source_a");
  let source_b : PathBuf = temp_dir . path() . join ("source_b");
  fs::create_dir_all (&source_a) . unwrap();
  fs::create_dir_all (&source_b) . unwrap();

  let config : SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> =
      HashMap::new();
    sources . insert(
      SourceName::from ("source_a"),
      SkgfileSource {
        name: SourceName::from ("source_a"),
        abbreviation: None,
        path: source_a,
        user_owns_it: true, } );
    sources . insert(
      SourceName::from ("source_b"),
      SkgfileSource {
        name: SourceName::from ("source_b"),
        abbreviation: None,
        path: source_b,
        user_owns_it: true, } );
    test_config (sources, temp_dir . path () . to_path_buf ()) };

  // Create nodes
  let mut node_a : NodeComplete = empty_node_complete();
  node_a . pid = ID::new ("node_a");
  node_a . title = "Node A" . to_string();
  set_source_retagging_levels ( &mut node_a, &SourceName::from ("source_a") );
  write_nodecomplete_to_source(&node_a, &config) . unwrap();

  let mut node_b : NodeComplete = empty_node_complete();
  node_b . pid = ID::new ("node_b");
  node_b . title = "Node B" . to_string();
  set_source_retagging_levels ( &mut node_b, &SourceName::from ("source_b") );
  write_nodecomplete_to_source(&node_b, &config) . unwrap();

  let result : IoResult<Vec<NodeComplete>> =
    read_all_skg_files_from_sources (&config);
  assert!(result . is_ok());

  let nodes : Vec<NodeComplete> = result . unwrap();
  assert_eq!(nodes . len(), 2);

  // Find each node and verify source
  let node_a_result : Option<&NodeComplete> =
    nodes . iter() . find(|n| n . pid . as_str() == "node_a");
  let node_b_result : Option<&NodeComplete> =
    nodes . iter() . find(|n| n . pid . as_str() == "node_b");

  assert!(node_a_result . is_some());
  assert!(node_b_result . is_some());

  assert_eq!(&*node_a_result . unwrap() . source, "source_a");
  assert_eq!(&*node_b_result . unwrap() . source, "source_b");
}

#[test]
fn test_many_id_conflicts_create_org_file() {
  // Test that >10 duplicates triggers org file creation
  let temp_dir : TempDir = tempdir() . unwrap();

  let source_a : PathBuf = temp_dir . path() . join ("source_a");
  let source_b : PathBuf = temp_dir . path() . join ("source_b");
  fs::create_dir_all (&source_a) . unwrap();
  fs::create_dir_all (&source_b) . unwrap();

  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  sources . insert(
    SourceName::from ("source_a"),
    SkgfileSource {
      name: SourceName::from ("source_a"),
        abbreviation: None,
      path: source_a,
      user_owns_it: true, } );
  sources . insert(
    SourceName::from ("source_b"),
    SkgfileSource {
      name: SourceName::from ("source_b"),
        abbreviation: None,
      path: source_b,
      user_owns_it: true,
    }
  );
  let config : SkgConfig = test_config (sources, temp_dir . path () . to_path_buf ());

  // Create 15 GENUINE id conflicts: same-pid files across sources
  // are telescope sections now, so a conflict means one id claimed
  // by two DISTINCT pids -- here via extra_ids. In-memory nodes
  // suffice; the check takes the folded node list.
  let mut nodes : Vec<NodeComplete> = Vec::new ();
  for i in 1..=15 {
    let id : String = format!("dup_id_{}", i);
    let mut node_a : NodeComplete = empty_node_complete();
    let mut node_b : NodeComplete = empty_node_complete();
    node_a . pid = ID::new (&format!("pid_a_{}", i));
    node_b . pid = ID::new (&format!("pid_b_{}", i));
    node_a . extra_ids = vec![ID::new (&id)];
    node_b . extra_ids = vec![ID::new (&id)];
    node_a . title = format!("Node A {}", i);
    node_b . title = format!("Node B {}", i);
    set_source_retagging_levels ( &mut node_a, &SourceName::from ("source_a") );
    set_source_retagging_levels ( &mut node_b, &SourceName::from ("source_b") );
    nodes . push (node_a);
    nodes . push (node_b); }

  let result : IoResult<()> =
    error_unless_each_id_names_one_node (
      &nodes, &config . data_root);
  assert!(result . is_err(), "Should fail: 15 ids claimed by two nodes each");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::InvalidData);
  let err_msg = err . to_string();
  assert!(err_msg . contains ("15")
          && err_msg . contains ("claimed by more than one node"),
          "Error should count the conflicts and name them: {}", err_msg);

  // Check that org file was created in the test's tempdir.
  let org_file_path : PathBuf =
    temp_dir . path () . join (
      "initialization-error_ids-claimed-by-two-nodes.org");
  assert!(org_file_path . exists(),
          "Org file should be created for >10 conflicts");

  // Generate expected content programmatically
  let mut expected : String = String::new();
  expected . push_str ("#+title: IDs claimed by more than one node\n");
  expected . push_str ("#+date: <generated at initialization>\n\n");
  expected . push_str ("15 id(s) claimed by more than one node. Same-id files ACROSS SOURCES are not this: those are the sections of one privacy telescope (docs/telescopes.md). Each id below is claimed, as a primary or extra id, by the distinct nodes listed under it.\n\n");

  // IDs are sorted alphabetically (lexicographic), not numerically
  // So: dup_id_1, dup_id_10, dup_id_11, ..., dup_id_2, ...
  let mut ids : Vec<String> =
    (1..=15) . map(|i| format!("dup_id_{}", i)) . collect();
  ids . sort();

  for id in ids {
    let n : &str = id . rsplit ('_') . next () . unwrap ();
    expected . push_str(&format!("* {}\n", id));
    expected . push_str(&format!("** pid_a_{} (source_a)\n", n));
    expected . push_str(&format!("** pid_b_{} (source_b)\n", n));
  }

  // Read and verify full org file content
  let org_content : String = fs::read_to_string (&org_file_path) . unwrap();
  assert_eq!(org_content, expected,
             "Org file content should match expected format exactly");
  // No explicit cleanup: temp_dir's Drop handles it.
}

#[test]
fn test_unreadable_files_creates_org_file() {
  // Test that unreadable files trigger org file creation
  let temp_dir : TempDir = tempdir() . unwrap();

  let source_good : PathBuf = temp_dir . path() . join ("source_good");
  let source_bad : PathBuf = temp_dir . path() . join ("source_bad");
  fs::create_dir_all (&source_good) . unwrap();
  // Don't create source_bad directory - it should cause an error

  // Create config with only the good source for writing
  let mut write_sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  write_sources . insert(
    SourceName::from ("source_good"),
    SkgfileSource {
      name: SourceName::from ("source_good"),
        abbreviation: None,
      path: source_good . clone(),
      user_owns_it: true, } );
  let write_config : SkgConfig =
    test_config (write_sources,
                 temp_dir . path () . to_path_buf ());

  // Create a valid node in the good source
  let mut node : NodeComplete = empty_node_complete();
  node . pid = ID::new ("test1");
  node . title = "Test Node" . to_string();
  set_source_retagging_levels ( &mut node, &SourceName::from ("source_good") );
  write_nodecomplete_to_source(&node, &write_config) . unwrap();

  // Create config with both sources for reading (including the bad one)
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new();
  sources . insert(
    SourceName::from ("source_good"),
    SkgfileSource {
      name: SourceName::from ("source_good"),
        abbreviation: None,
      path: source_good,
      user_owns_it: true, } );
  sources . insert(
    SourceName::from ("source_bad"),
    SkgfileSource {
      name: SourceName::from ("source_bad"),
        abbreviation: None,
      path: source_bad . clone(),
      user_owns_it: true, } );

  let result : IoResult<Vec<NodeComplete>> =
    read_all_skg_files_from_sources(
      &test_config (sources,
                    temp_dir . path () . to_path_buf () ));
  assert!(result . is_err(), "Should fail due to unreadable source");

  let err : IoError = result . unwrap_err();
  assert_eq!(err . kind(), IoErrorKind::InvalidData);
  let err_msg = err . to_string();
  assert!(err_msg . contains ("unreadable"),
          "Error should mention unreadable files: {}", err_msg);

  // Check that org file was created in the test's tempdir.
  let org_file_path : PathBuf =
    temp_dir . path ()
      . join ("initialization-error_unreadable-skg-files.org");
  assert!(org_file_path . exists(),
          "Org file should be created for unreadable files");

  // Read org file content
  let org_content : String =
    fs::read_to_string (&org_file_path) . unwrap();

  // Verify header and count
  assert!(org_content . starts_with ("#+title: Unreadable SKG Files\n"));
  assert!(org_content . contains ("#+date: <generated at initialization>\n\n"));
  assert!(org_content . contains ("Found 1 unreadable file(s).\n\n"));

  // Verify structure: should have path as level 1, source as level 2, error as level 3
  let bad_path_str : String = source_bad . display() . to_string();
  assert!(org_content . contains(&format!("* {}\n", bad_path_str)),
          "Should list the bad path at level 1");
  assert!(org_content . contains ("** source_bad\n"),
          "Should list source_bad at level 2");
  assert!(org_content . contains ("*** Error: "),
          "Should have error message at level 3");

  // Error message is OS-dependent, but should mention the path issue
  assert!(org_content . contains ("No such file or directory") ||
          org_content . contains ("cannot find the path") ||
          org_content . contains ("system cannot find"),
          "Error should mention file/directory not found");

  // No explicit cleanup: temp_dir's Drop handles it.
}

/// The two shapes 'write_nodecomplete_telescope' refuses. Neither
/// arises from a skg save (every level is clamped to at least the
/// owner's home); both arrive from hand-edited files or a pull.
/// Writing either would publish the node's text or lose it.
#[test]
fn a_write_refuses_a_foreign_home_and_a_title_hoist() {
  let temp_dir : TempDir = tempdir() . unwrap();
  let public_path  : PathBuf = temp_dir . path() . join ("public");
  let foreign_path : PathBuf = temp_dir . path() . join ("foreign");
  fs::create_dir_all (&public_path)  . unwrap();
  fs::create_dir_all (&foreign_path) . unwrap();
  let config : SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> =
      HashMap::new();
    for (name, path, owned) in
      [ ("public",  public_path  . clone(), true  ),
        ("foreign", foreign_path . clone(), false ) ] {
      sources . insert ( SourceName::from (name), SkgfileSource {
        name         : SourceName::from (name),
        abbreviation : None,
        path,
        user_owns_it : owned, } ); }
    let mut config : SkgConfig =
      test_config (sources, temp_dir . path () . to_path_buf ());
    config . source_order = // most public first
      vec! [ SourceName::from ("foreign"),
             SourceName::from ("public") ];
    config };
  { // FOREIGN HOME: refused, rather than silently dropping the title.
    let mut node : NodeComplete = empty_node_complete();
    node . pid   = ID::new ("F");
    node . title = "foreign-homed" . to_string();
    set_source_retagging_levels (
      &mut node, &SourceName::from ("foreign") );
    let err : IoError =
      write_nodecomplete_to_source (&node, &config)
      . expect_err ("a foreign home is not writable");
    assert!( err . to_string() . contains ("do not own"),
             "the refusal says why: {}", err ); }
  { // TITLE HOIST: the home exists and is titleless, so the text
    // lives more privately and this write would publish it.
    fs::write ( public_path . join ("H.skg"),
                "pid: H\ncontains:\n- C\n" ) . unwrap();
    let mut node : NodeComplete = empty_node_complete();
    node . pid   = ID::new ("H");
    node . title = "private text" . to_string();
    set_source_retagging_levels (
      &mut node, &SourceName::from ("public") );
    let err : IoError =
      write_nodecomplete_to_source (&node, &config)
      . expect_err ("hoisting a title into a titleless home is refused");
    assert!( err . to_string() . contains ("would publish it"),
             "the refusal says why: {}", err );
    assert_eq!( fs::read_to_string (
                  public_path . join ("H.skg") ) . unwrap(),
                "pid: H\ncontains:\n- C\n",
                "and nothing was written" ); }
  { // The ordinary shape still writes: an owned, titled home.
    let mut node : NodeComplete = empty_node_complete();
    node . pid   = ID::new ("N");
    node . title = "ordinary" . to_string();
    set_source_retagging_levels (
      &mut node, &SourceName::from ("public") );
    write_nodecomplete_to_source (&node, &config)
      . expect ("an owned titled home writes"); }
}
