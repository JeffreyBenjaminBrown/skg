//
// Tests for new map-based helpers
//

use skg::types::skgnode::{empty_skgnode, SkgNode};
use skg::types::skgnodemap::{skgnode_for_viewnode, skgnode_map_from_save_instructions, SkgNodeMap};
use skg::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, default_truenode};
use skg::types::save::{DefineNode, SaveNode, DeleteNode};
use skg::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};

use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;

#[test]
fn test_skgnode_for_viewnode_with_id_in_map() {
  // TrueNode with ID that exists in map → returns Some
  let id : ID =
    ID::new("test-id-123");
  let skgnode : SkgNode =
    SkgNode {
      title : "Test Node".to_string(),
      ids : vec![id.clone()],
      source : SourceName::from("main"),
      .. empty_skgnode()
    };
  let mut map : SkgNodeMap = SkgNodeMap::new();
  map.insert(id.clone(), skgnode.clone());
  let viewnode : ViewNode = ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::True (
      default_truenode ( id.clone(),
                         SourceName::from("main"),
                         "Test".to_string() )) };
  let config : SkgConfig = SkgConfig {
    db_name : "test-db".to_string(),
    tantivy_folder : PathBuf::from("/tmp/tantivy"),
    sources : HashMap::new(),
    port : 3000,
    delete_on_quit : false,
    neo4j_uri      : "bolt://localhost:7687".to_string(),
    neo4j_user     : "neo4j".to_string(),
    neo4j_password : "password".to_string(),
    initial_node_limit : 100,
    timing_log : false,
    config_dir : PathBuf::from("."),
  };

  let result :
    Option<&SkgNode> =
    skgnode_for_viewnode(&viewnode, &mut map, &config).unwrap();
  assert!(result.is_some(), "Should find SkgNode for TrueNode with ID in map");
  assert_eq!(result.unwrap().title, "Test Node");
}

#[test]
fn test_skgnode_for_viewnode_with_id_not_in_map() {
  // TrueNode with ID not in map and not on disk → returns error
  let id :
    ID =
    ID::new("test-id-456");
  let mut map :
    SkgNodeMap =
    SkgNodeMap::new(); // empty map
  // ID is not in map, will try to fetch from disk and fail
  let viewnode : ViewNode = ViewNode {
    focused : false,
    folded  : false,
    kind    : ViewNodeKind::True (
      default_truenode ( id,
                         SourceName::from("main"),
                         "Test".to_string() )) };
  let sources : HashMap<SourceName, SkgfileSource> = {
    // Config with "main" source pointing to a temp directory
    let mut s : HashMap<SourceName, SkgfileSource> = HashMap::new();
    s.insert(
      SourceName::from("main"),
      SkgfileSource {
        nickname : SourceName::from("main"),
        path     : PathBuf::from("/tmp/nonexistent-skg-dir"),
        user_owns_it : true,
      });
    s };
  let config : SkgConfig = SkgConfig {
    db_name : "test-db".to_string(),
    tantivy_folder : PathBuf::from("/tmp/tantivy"),
    sources,
    port : 3000,
    delete_on_quit : false,
    neo4j_uri      : "bolt://localhost:7687".to_string(),
    neo4j_user     : "neo4j".to_string(),
    neo4j_password : "password".to_string(),
    initial_node_limit : 100,
    timing_log : false,
    config_dir : PathBuf::from("."),
  };
  let result : Result<Option<&SkgNode>, Box<dyn Error>> =
    // Should error because the file doesn't exist on disk
    skgnode_for_viewnode(&viewnode, &mut map, &config);
  assert!(result.is_err(), "Should return error when ID not in map and file not on disk");
}

#[test]
fn test_skgnode_for_viewnode_scaffold() {
  // Scaffold → returns None
  let skgnode :
    SkgNode =
    SkgNode {
      title : "Test Node".to_string(),
      ids : vec![ID::new("test-id-789")],
      .. empty_skgnode()
    };

  let mut map :
    SkgNodeMap =
    SkgNodeMap::new();
  map.insert(ID::new("test-id-789"), skgnode);

  let viewnode :
    ViewNode =
    ViewNode {
      focused : false,
      folded : false,
      kind : ViewNodeKind::Scaff(Scaffold::AliasCol),
    };

  let config : SkgConfig = SkgConfig {
    db_name : "test-db".to_string(),
    tantivy_folder : PathBuf::from("/tmp/tantivy"),
    sources : HashMap::new(),
    port : 3000,
    delete_on_quit : false,
    neo4j_uri      : "bolt://localhost:7687".to_string(),
    neo4j_user     : "neo4j".to_string(),
    neo4j_password : "password".to_string(),
    initial_node_limit : 100,
    timing_log : false,
    config_dir : PathBuf::from("."),
  };

  let result :
    Option<&SkgNode> =
    skgnode_for_viewnode(&viewnode, &mut map, &config).unwrap();
  assert!(result.is_none(), "Should return None for Scaffold nodes");
}

#[test]
fn test_skgnode_map_from_save_instructions() {
  // Build map from SaveInstructions
  let id1 :
    ID =
    ID::new("id-001");
  let id2 :
    ID =
    ID::new("id-002");
  let id3 :
    ID =
    ID::new("id-003");

  let skgnode1 :
    SkgNode =
    SkgNode {
      title : "Node 1".to_string(),
      ids : vec![id1.clone()],
      .. empty_skgnode()
    };

  let skgnode2 :
    SkgNode =
    SkgNode {
      title : "Node 2".to_string(),
      ids : vec![id2.clone(), ID::new("extra-id")], // multiple IDs, should use first
      .. empty_skgnode()
    };

  let instructions : Vec<DefineNode> =
    vec![ DefineNode::Save(SaveNode(skgnode1.clone())),
          DefineNode::Save(SaveNode(skgnode2.clone())),
          DefineNode::Delete(
            DeleteNode { id: id3.clone(),
                            source: SourceName::from("main") }), ];

  let map :
    SkgNodeMap =
    skgnode_map_from_save_instructions(&instructions);

  // Delete instructions don't contribute to the map (they carry no SkgNode data)
  assert_eq!(map.len(), 2, "Map should contain 2 entries (Delete doesn't contribute)");
  assert_eq!(map.get(&id1).unwrap().title, "Node 1");
  assert_eq!(map.get(&id2).unwrap().title, "Node 2");
  assert!(map.get(&id3).is_none(), "Delete instruction should not be in map");
}

#[test]
fn test_skgnode_map_from_save_instructions_empty() {
  // Empty instructions → empty map
  let instructions :
    Vec<DefineNode> =
    vec![];

  let map :
    SkgNodeMap =
    skgnode_map_from_save_instructions(&instructions);

  assert_eq!(map.len(), 0, "Map should be empty");
}
