// cargo test --test multi_source_loading

use std::collections::HashMap;
use std::fs;
use tempfile::tempdir;

use skg::media::file_io::multiple_nodes::read_all_skg_files_from_sources;
use skg::media::file_io::one_node::write_skgnode;
use skg::types::misc::SkgfileSource;
use skg::types::skgnode::{SkgNode, empty_skgnode};
use skg::types::misc::ID;

#[test]
fn test_load_from_single_source() {
  let temp_dir = tempdir().unwrap();
  let source_path = temp_dir.path().join("main");
  fs::create_dir_all(&source_path).unwrap();

  // Create a test node
  let mut node = empty_skgnode();
  node.ids = vec![ID::new("test1")];
  node.title = "Test Node 1".to_string();
  write_skgnode(&node, &source_path.join("test1.skg")).unwrap();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "main".to_string(),
    SkgfileSource {
      nickname: "main".to_string(),
      path: source_path.clone(),
      user_owns_it: true,
    }
  );

  let result = read_all_skg_files_from_sources(&sources);
  assert!(result.is_ok(), "Should successfully load from single source");

  let nodes = result.unwrap();
  assert_eq!(nodes.len(), 1, "Should have loaded 1 node");
  assert_eq!(nodes[0].source, "main", "Source should be 'main'");
  assert_eq!(nodes[0].title, "Test Node 1");
}

#[test]
fn test_load_from_multiple_sources() {
  let temp_dir = tempdir().unwrap();

  // Create two source directories
  let main_path = temp_dir.path().join("main");
  let shared_path = temp_dir.path().join("shared");
  fs::create_dir_all(&main_path).unwrap();
  fs::create_dir_all(&shared_path).unwrap();

  // Create nodes in main source
  let mut node1 = empty_skgnode();
  node1.ids = vec![ID::new("main1")];
  node1.title = "Main Node 1".to_string();
  write_skgnode(&node1, &main_path.join("main1.skg")).unwrap();

  let mut node2 = empty_skgnode();
  node2.ids = vec![ID::new("main2")];
  node2.title = "Main Node 2".to_string();
  write_skgnode(&node2, &main_path.join("main2.skg")).unwrap();

  // Create nodes in shared source
  let mut node3 = empty_skgnode();
  node3.ids = vec![ID::new("shared1")];
  node3.title = "Shared Node 1".to_string();
  write_skgnode(&node3, &shared_path.join("shared1.skg")).unwrap();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "main".to_string(),
    SkgfileSource {
      nickname: "main".to_string(),
      path: main_path,
      user_owns_it: true,
    }
  );
  sources.insert(
    "shared".to_string(),
    SkgfileSource {
      nickname: "shared".to_string(),
      path: shared_path,
      user_owns_it: false,
    }
  );

  let result = read_all_skg_files_from_sources(&sources);
  assert!(result.is_ok(), "Should successfully load from multiple sources");

  let nodes = result.unwrap();
  assert_eq!(nodes.len(), 3, "Should have loaded 3 nodes total");

  // Verify sources are set correctly
  let main_nodes: Vec<&SkgNode> = nodes.iter()
    .filter(|n| n.source == "main")
    .collect();
  let shared_nodes: Vec<&SkgNode> = nodes.iter()
    .filter(|n| n.source == "shared")
    .collect();

  assert_eq!(main_nodes.len(), 2, "Should have 2 nodes from main");
  assert_eq!(shared_nodes.len(), 1, "Should have 1 node from shared");
}

#[test]
fn test_duplicate_id_detection_across_sources() {
  let temp_dir = tempdir().unwrap();

  // Create two source directories
  let main_path = temp_dir.path().join("main");
  let shared_path = temp_dir.path().join("shared");
  fs::create_dir_all(&main_path).unwrap();
  fs::create_dir_all(&shared_path).unwrap();

  // Create node with same ID in both sources
  let mut node1 = empty_skgnode();
  node1.ids = vec![ID::new("duplicate_id")];
  node1.title = "Node in Main".to_string();
  write_skgnode(&node1, &main_path.join("duplicate_id.skg")).unwrap();

  let mut node2 = empty_skgnode();
  node2.ids = vec![ID::new("duplicate_id")];
  node2.title = "Node in Shared".to_string();
  write_skgnode(&node2, &shared_path.join("duplicate_id.skg")).unwrap();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "main".to_string(),
    SkgfileSource {
      nickname: "main".to_string(),
      path: main_path,
      user_owns_it: true,
    }
  );
  sources.insert(
    "shared".to_string(),
    SkgfileSource {
      nickname: "shared".to_string(),
      path: shared_path,
      user_owns_it: false,
    }
  );

  let result = read_all_skg_files_from_sources(&sources);
  assert!(result.is_err(), "Should fail due to duplicate ID");

  let err = result.unwrap_err();
  assert_eq!(err.kind(), std::io::ErrorKind::InvalidData);
  let err_msg = err.to_string();
  assert!(err_msg.contains("Duplicate ID"), "Error should mention duplicate ID");
  assert!(err_msg.contains("duplicate_id"), "Error should include the ID");
}

#[test]
fn test_node_with_multiple_ids_duplicate_detection() {
  let temp_dir = tempdir().unwrap();

  // Create two source directories
  let main_path = temp_dir.path().join("main");
  let shared_path = temp_dir.path().join("shared");
  fs::create_dir_all(&main_path).unwrap();
  fs::create_dir_all(&shared_path).unwrap();

  // Create node in main with multiple IDs
  let mut node1 = empty_skgnode();
  node1.ids = vec![ID::new("id1"), ID::new("id2")];
  node1.title = "Node with Multiple IDs".to_string();
  write_skgnode(&node1, &main_path.join("id1.skg")).unwrap();

  // Create node in shared that has one overlapping ID
  let mut node2 = empty_skgnode();
  node2.ids = vec![ID::new("id2"), ID::new("id3")];
  node2.title = "Another Node".to_string();
  write_skgnode(&node2, &shared_path.join("id2.skg")).unwrap();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "main".to_string(),
    SkgfileSource {
      nickname: "main".to_string(),
      path: main_path,
      user_owns_it: true,
    }
  );
  sources.insert(
    "shared".to_string(),
    SkgfileSource {
      nickname: "shared".to_string(),
      path: shared_path,
      user_owns_it: false,
    }
  );

  let result = read_all_skg_files_from_sources(&sources);
  assert!(result.is_err(), "Should fail due to overlapping ID");

  let err = result.unwrap_err();
  assert_eq!(err.kind(), std::io::ErrorKind::InvalidData);
  assert!(err.to_string().contains("id2"), "Error should mention the overlapping ID");
}

#[test]
fn test_load_from_empty_sources() {
  let temp_dir = tempdir().unwrap();
  let source_path = temp_dir.path().join("empty_source");
  fs::create_dir_all(&source_path).unwrap();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "empty".to_string(),
    SkgfileSource {
      nickname: "empty".to_string(),
      path: source_path,
      user_owns_it: true,
    }
  );

  let result = read_all_skg_files_from_sources(&sources);
  assert!(result.is_ok(), "Should successfully handle empty source");

  let nodes = result.unwrap();
  assert_eq!(nodes.len(), 0, "Should have loaded 0 nodes from empty source");
}

#[test]
fn test_source_field_set_correctly() {
  let temp_dir = tempdir().unwrap();

  let source_a = temp_dir.path().join("source_a");
  let source_b = temp_dir.path().join("source_b");
  fs::create_dir_all(&source_a).unwrap();
  fs::create_dir_all(&source_b).unwrap();

  // Create nodes
  let mut node_a = empty_skgnode();
  node_a.ids = vec![ID::new("node_a")];
  node_a.title = "Node A".to_string();
  write_skgnode(&node_a, &source_a.join("node_a.skg")).unwrap();

  let mut node_b = empty_skgnode();
  node_b.ids = vec![ID::new("node_b")];
  node_b.title = "Node B".to_string();
  write_skgnode(&node_b, &source_b.join("node_b.skg")).unwrap();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "source_a".to_string(),
    SkgfileSource {
      nickname: "source_a".to_string(),
      path: source_a,
      user_owns_it: true,
    }
  );
  sources.insert(
    "source_b".to_string(),
    SkgfileSource {
      nickname: "source_b".to_string(),
      path: source_b,
      user_owns_it: true,
    }
  );

  let result = read_all_skg_files_from_sources(&sources);
  assert!(result.is_ok());

  let nodes = result.unwrap();
  assert_eq!(nodes.len(), 2);

  // Find each node and verify source
  let node_a_result = nodes.iter().find(|n| n.ids[0].as_str() == "node_a");
  let node_b_result = nodes.iter().find(|n| n.ids[0].as_str() == "node_b");

  assert!(node_a_result.is_some());
  assert!(node_b_result.is_some());

  assert_eq!(node_a_result.unwrap().source, "source_a");
  assert_eq!(node_b_result.unwrap().source, "source_b");
}

#[test]
fn test_many_duplicate_ids_creates_org_file() {
  // Test that >10 duplicates triggers org file creation
  let temp_dir = tempdir().unwrap();

  let source_a = temp_dir.path().join("source_a");
  let source_b = temp_dir.path().join("source_b");
  fs::create_dir_all(&source_a).unwrap();
  fs::create_dir_all(&source_b).unwrap();

  // Create 15 nodes with duplicate IDs
  for i in 1..=15 {
    let id = format!("dup_id_{}", i);
    let mut node_a = empty_skgnode();
    let mut node_b = empty_skgnode();
    node_a.ids = vec![ID::new(&id)];
    node_b.ids = vec![ID::new(&id)];
    node_a.title = format!("Node A {}", i);
    node_b.title = format!("Node B {}", i);
    write_skgnode( &node_a,
                &source_a.join(format!("{}.skg", id))) . unwrap();
    write_skgnode( &node_b,
                &source_b.join(format!("{}.skg", id))) . unwrap(); }

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "source_a".to_string(),
    SkgfileSource {
      nickname: "source_a".to_string(),
      path: source_a,
      user_owns_it: true,
    }
  );
  sources.insert(
    "source_b".to_string(),
    SkgfileSource {
      nickname: "source_b".to_string(),
      path: source_b,
      user_owns_it: true,
    }
  );

  let result = read_all_skg_files_from_sources(&sources);
  assert!(result.is_err(), "Should fail due to duplicate IDs");

  let err = result.unwrap_err();
  assert_eq!(err.kind(), std::io::ErrorKind::InvalidData);
  let err_msg = err.to_string();
  assert!(err_msg.contains("15") || err_msg.contains("duplicate"),
          "Error should mention duplicates: {}", err_msg);

  // Check that org file was created
  let org_file_path = "initialization-error_duplicate-ids.org";
  assert!(std::path::Path::new(org_file_path).exists(),
          "Org file should be created for >10 duplicates");

  // Generate expected content programmatically
  let mut expected = String::new();
  expected.push_str("#+title: Duplicate IDs Across Sources\n");
  expected.push_str("#+date: <generated at initialization>\n\n");
  expected.push_str("Found 15 duplicate IDs across sources.\n\n");

  // IDs are sorted alphabetically (lexicographic), not numerically
  // So: dup_id_1, dup_id_10, dup_id_11, ..., dup_id_2, ...
  let mut ids: Vec<String> = (1..=15).map(|i| format!("dup_id_{}", i)).collect();
  ids.sort();

  for id in ids {
    expected.push_str(&format!("* {}\n", id));
    expected.push_str("** source_a\n");
    expected.push_str("** source_b\n");
  }

  // Read and verify full org file content
  let org_content = fs::read_to_string(org_file_path).unwrap();
  assert_eq!(org_content, expected,
             "Org file content should match expected format exactly");

  // Clean up
  fs::remove_file(org_file_path).unwrap();
}

#[test]
fn test_unreadable_files_creates_org_file() {
  // Test that unreadable files trigger org file creation
  let temp_dir = tempdir().unwrap();

  let source_good = temp_dir.path().join("source_good");
  let source_bad = temp_dir.path().join("source_bad");
  fs::create_dir_all(&source_good).unwrap();
  // Don't create source_bad directory - it should cause an error

  // Create a valid node in the good source
  let mut node = empty_skgnode();
  node.ids = vec![ID::new("test1")];
  node.title = "Test Node".to_string();
  write_skgnode(&node, &source_good.join("test1.skg")).unwrap();

  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "source_good".to_string(),
    SkgfileSource {
      nickname: "source_good".to_string(),
      path: source_good,
      user_owns_it: true, } );
  sources.insert(
    "source_bad".to_string(),
    SkgfileSource {
      nickname: "source_bad".to_string(),
      path: source_bad.clone(),
      user_owns_it: true, } );

  let result = read_all_skg_files_from_sources(&sources);
  assert!(result.is_err(), "Should fail due to unreadable source");

  let err = result.unwrap_err();
  assert_eq!(err.kind(), std::io::ErrorKind::InvalidData);
  let err_msg = err.to_string();
  assert!(err_msg.contains("unreadable"),
          "Error should mention unreadable files: {}", err_msg);

  // Check that org file was created
  let org_file_path = "initialization-error_unreadable-skg-files.org";
  assert!(std::path::Path::new(org_file_path).exists(),
          "Org file should be created for unreadable files");

  // Read org file content
  let org_content = fs::read_to_string(org_file_path).unwrap();

  // Verify header and count
  assert!(org_content.starts_with("#+title: Unreadable SKG Files\n"));
  assert!(org_content.contains("#+date: <generated at initialization>\n\n"));
  assert!(org_content.contains("Found 1 unreadable file(s).\n\n"));

  // Verify structure: should have path as level 1, source as level 2, error as level 3
  let bad_path_str = source_bad.display().to_string();
  assert!(org_content.contains(&format!("* {}\n", bad_path_str)),
          "Should list the bad path at level 1");
  assert!(org_content.contains("** source_bad\n"),
          "Should list source_bad at level 2");
  assert!(org_content.contains("*** Error: "),
          "Should have error message at level 3");

  // Error message is OS-dependent, but should mention the path issue
  assert!(org_content.contains("No such file or directory") ||
          org_content.contains("cannot find the path") ||
          org_content.contains("system cannot find"),
          "Error should mention file/directory not found");

  // Clean up
  fs::remove_file(org_file_path).unwrap();
}
