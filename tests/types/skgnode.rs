//
// Tests for new map-based helpers
//

use skg::types::skgnode::{skgnode_for_orgnode, skgnode_map_from_save_instructions, empty_skgnode, SkgNode, SkgNodeMap};
use skg::types::orgnode::{OrgNode, OrgNodeKind, TrueNode, Scaffold};
use skg::types::save::NonMerge_NodeAction;
use skg::types::misc::ID;

#[test]
fn test_skgnode_for_orgnode_with_id_in_map() {
  // TrueNode with ID that exists in map → returns Some
  let id :
    ID =
    ID::new("test-id-123");
  let skgnode :
    SkgNode =
    SkgNode {
      title : "Test Node".to_string(),
      ids : vec![id.clone()],
      source : "main".to_string(),
      .. empty_skgnode()
    };

  let mut map :
    SkgNodeMap =
    SkgNodeMap::new();
  map.insert(id.clone(), skgnode.clone());

  let orgnode :
    OrgNode =
    OrgNode {
      focused : false,
      folded : false,
      kind : OrgNodeKind::True(TrueNode {
        title : "Test".to_string(),
        body : None,
        id_opt : Some(id.clone()),
        source_opt : Some("main".to_string()),
        parent_ignores : false,
        indefinitive : false,
        cycle : false,
        stats : Default::default(),
        edit_request : None,
        view_requests : Default::default(),
      }),
    };

  let config = skg::types::misc::SkgConfig {
    db_name : "test-db".to_string(),
    tantivy_folder : std::path::PathBuf::from("/tmp/tantivy"),
    sources : std::collections::HashMap::new(),
    port : 3000,
    delete_on_quit : false,
    initial_node_limit : 100,
  };

  let result :
    Option<&SkgNode> =
    skgnode_for_orgnode(&orgnode, &mut map, &config).unwrap();
  assert!(result.is_some(), "Should find SkgNode for TrueNode with ID in map");
  assert_eq!(result.unwrap().title, "Test Node");
}

#[test]
fn test_skgnode_for_orgnode_with_id_not_in_map() {
  // TrueNode without source → returns None
  let id :
    ID =
    ID::new("test-id-456");
  let mut map :
    SkgNodeMap =
    SkgNodeMap::new(); // empty map

  let orgnode :
    OrgNode =
    OrgNode {
      focused : false,
      folded : false,
      kind : OrgNodeKind::True(TrueNode {
        title : "Test".to_string(),
        body : None,
        id_opt : Some(id),
        source_opt : None,  // No source, so can't fetch from disk
        parent_ignores : false,
        indefinitive : false,
        cycle : false,
        stats : Default::default(),
        edit_request : None,
        view_requests : Default::default(),
      }),
    };

  let config = skg::types::misc::SkgConfig {
    db_name : "test-db".to_string(),
    tantivy_folder : std::path::PathBuf::from("/tmp/tantivy"),
    sources : std::collections::HashMap::new(),
    port : 3000,
    delete_on_quit : false,
    initial_node_limit : 100,
  };

  let result :
    Option<&SkgNode> =
    skgnode_for_orgnode(&orgnode, &mut map, &config).unwrap();
  assert!(result.is_none(), "Should return None when no source");
}

#[test]
fn test_skgnode_for_orgnode_scaffold() {
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

  let orgnode :
    OrgNode =
    OrgNode {
      focused : false,
      folded : false,
      kind : OrgNodeKind::Scaff(Scaffold::AliasCol),
    };

  let config = skg::types::misc::SkgConfig {
    db_name : "test-db".to_string(),
    tantivy_folder : std::path::PathBuf::from("/tmp/tantivy"),
    sources : std::collections::HashMap::new(),
    port : 3000,
    delete_on_quit : false,
    initial_node_limit : 100,
  };

  let result :
    Option<&SkgNode> =
    skgnode_for_orgnode(&orgnode, &mut map, &config).unwrap();
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

  let skgnode3 :
    SkgNode =
    SkgNode {
      title : "Node 3".to_string(),
      ids : vec![id3.clone()],
      .. empty_skgnode()
    };

  let instructions :
    Vec<(SkgNode, NonMerge_NodeAction)> =
    vec![
      (skgnode1.clone(), NonMerge_NodeAction::Save),
      (skgnode2.clone(), NonMerge_NodeAction::Save),
      (skgnode3.clone(), NonMerge_NodeAction::Delete), // action shouldn't matter
    ];

  let map :
    SkgNodeMap =
    skgnode_map_from_save_instructions(&instructions);

  assert_eq!(map.len(), 3, "Map should contain 3 entries");
  assert_eq!(map.get(&id1).unwrap().title, "Node 1");
  assert_eq!(map.get(&id2).unwrap().title, "Node 2");
  assert_eq!(map.get(&id3).unwrap().title, "Node 3");
}

#[test]
fn test_skgnode_map_from_save_instructions_empty() {
  // Empty instructions → empty map
  let instructions :
    Vec<(SkgNode, NonMerge_NodeAction)> =
    vec![];

  let map :
    SkgNodeMap =
    skgnode_map_from_save_instructions(&instructions);

  assert_eq!(map.len(), 0, "Map should be empty");
}
