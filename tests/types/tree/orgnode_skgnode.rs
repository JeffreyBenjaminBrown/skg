// Tests for tree conversion helpers

use skg::types::skgnode::{empty_skgnode, SkgNode, SkgNodeMap};
use skg::types::orgnode::{OrgNode, OrgNodeKind, TrueNode};
use skg::types::misc::ID;
use skg::types::tree::orgnode_skgnode::{
  pairtree_from_tree_and_map,
  tree_and_map_from_pairtree,
};
use skg::types::tree::NodePair;

use ego_tree::Tree;

#[test]
fn test_pairtree_from_tree_and_map_single_node() {
  // Single node with SkgNode in map
  let id = ID::new("test-id-123");
  let skgnode = SkgNode {
    title : "Test Node".to_string(),
    ids : vec![id.clone()],
    .. empty_skgnode()
  };

  let mut map = SkgNodeMap::new();
  map.insert(id.clone(), skgnode.clone());

  let orgnode = OrgNode {
    focused : false,
    folded : false,
    kind : OrgNodeKind::True(TrueNode {
      title : "Test".to_string(),
      body : None,
      id_opt : Some(id.clone()),
      source_opt : None,
      parent_ignores : false,
      indefinitive : false,
      cycle : false,
      stats : Default::default(),
      edit_request : None,
      view_requests : Default::default(),
    }),
  };

  let tree = Tree::new(orgnode.clone());

  let pairtree = pairtree_from_tree_and_map(&tree, &map);

  let root = pairtree.root().value();
  assert_eq!(root.orgnode.title(), orgnode.title());
  assert!(root.mskgnode.is_some());
  assert_eq!(root.mskgnode.as_ref().unwrap().title, "Test Node");
}

#[test]
fn test_pairtree_from_tree_and_map_node_not_in_map() {
  // Node with ID not in map
  let id = ID::new("test-id-456");

  let map = SkgNodeMap::new(); // empty

  let orgnode = OrgNode {
    focused : false,
    folded : false,
    kind : OrgNodeKind::True(TrueNode {
      title : "Test".to_string(),
      body : None,
      id_opt : Some(id.clone()),
      source_opt : None,
      parent_ignores : false,
      indefinitive : false,
      cycle : false,
      stats : Default::default(),
      edit_request : None,
      view_requests : Default::default(),
    }),
  };

  let tree = Tree::new(orgnode.clone());

  let pairtree = pairtree_from_tree_and_map(&tree, &map);

  let root = pairtree.root().value();
  assert!(root.mskgnode.is_none(), "SkgNode should be None when not in map");
}

#[test]
fn test_tree_and_map_from_pairtree_single_node() {
  // Build PairTree and extract tree + map
  let id = ID::new("test-id-789");
  let skgnode = SkgNode {
    title : "Test Node".to_string(),
    ids : vec![id.clone()],
    .. empty_skgnode()
  };

  let orgnode = OrgNode {
    focused : false,
    folded : false,
    kind : OrgNodeKind::True(TrueNode {
      title : "Test".to_string(),
      body : None,
      id_opt : Some(id.clone()),
      source_opt : None,
      parent_ignores : false,
      indefinitive : false,
      cycle : false,
      stats : Default::default(),
      edit_request : None,
      view_requests : Default::default(),
    }),
  };

  let pair = NodePair {
    mskgnode : Some(skgnode.clone()),
    orgnode : orgnode.clone(),
  };

  let pairtree = Tree::new(pair);

  let (tree, map) = tree_and_map_from_pairtree(&pairtree);

  assert_eq!(tree.root().value().title(), orgnode.title());
  assert_eq!(map.len(), 1);
  assert_eq!(map.get(&id).unwrap().title, "Test Node");
}

#[test]
fn test_roundtrip_conversion() {
  // Test that converting tree+map -> pairtree -> tree+map preserves data
  let id1 = ID::new("id-001");
  let id2 = ID::new("id-002");

  let skgnode1 = SkgNode {
    title : "Node 1".to_string(),
    ids : vec![id1.clone()],
    .. empty_skgnode()
  };

  let skgnode2 = SkgNode {
    title : "Node 2".to_string(),
    ids : vec![id2.clone()],
    .. empty_skgnode()
  };

  let mut map = SkgNodeMap::new();
  map.insert(id1.clone(), skgnode1.clone());
  map.insert(id2.clone(), skgnode2.clone());

  let orgnode1 = OrgNode {
    focused : false,
    folded : false,
    kind : OrgNodeKind::True(TrueNode {
      title : "Root".to_string(),
      body : None,
      id_opt : Some(id1.clone()),
      source_opt : None,
      parent_ignores : false,
      indefinitive : false,
      cycle : false,
      stats : Default::default(),
      edit_request : None,
      view_requests : Default::default(),
    }),
  };

  let orgnode2 = OrgNode {
    focused : false,
    folded : false,
    kind : OrgNodeKind::True(TrueNode {
      title : "Child".to_string(),
      body : None,
      id_opt : Some(id2.clone()),
      source_opt : None,
      parent_ignores : false,
      indefinitive : false,
      cycle : false,
      stats : Default::default(),
      edit_request : None,
      view_requests : Default::default(),
    }),
  };

  let mut tree = Tree::new(orgnode1.clone());
  tree.root_mut().append(orgnode2.clone());

  // Convert to PairTree
  let pairtree = pairtree_from_tree_and_map(&tree, &map);

  // Convert back
  let (new_tree, new_map) = tree_and_map_from_pairtree(&pairtree);

  // Verify tree structure preserved
  assert_eq!(new_tree.root().value().title(), "Root");
  assert_eq!(new_tree.root().children().count(), 1);
  assert_eq!(
    new_tree.root().first_child().unwrap().value().title(),
    "Child"
  );

  // Verify map preserved
  assert_eq!(new_map.len(), 2);
  assert_eq!(new_map.get(&id1).unwrap().title, "Node 1");
  assert_eq!(new_map.get(&id2).unwrap().title, "Node 2");
}
