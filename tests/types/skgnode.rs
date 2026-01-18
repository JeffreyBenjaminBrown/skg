//
// Tests for new map-based helpers
//

use skg::types::skgnode::{skgnode_for_orgnode, SkgNode, SkgNodeMap};
use skg::types::orgnode::{OrgNode, OrgNodeKind, TrueNode, Scaffold};
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
      aliases : None,
      source : "test-source".to_string(),
      ids : vec![id.clone()],
      body : None,
      contains : None,
      subscribes_to : None,
      hides_from_its_subscriptions : None,
      overrides_view_of : None,
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
        source_opt : None,
        parent_ignores : false,
        indefinitive : false,
        cycle : false,
        stats : Default::default(),
        edit_request : None,
        view_requests : Default::default(),
      }),
    };

  let result :
    Option<&SkgNode> =
    skgnode_for_orgnode(&orgnode, &map);
  assert!(result.is_some(), "Should find SkgNode for TrueNode with ID in map");
  assert_eq!(result.unwrap().title, "Test Node");
}

#[test]
fn test_skgnode_for_orgnode_with_id_not_in_map() {
  // TrueNode with ID that doesn't exist in map → returns None
  let id :
    ID =
    ID::new("test-id-456");
  let map :
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
        source_opt : None,
        parent_ignores : false,
        indefinitive : false,
        cycle : false,
        stats : Default::default(),
        edit_request : None,
        view_requests : Default::default(),
      }),
    };

  let result :
    Option<&SkgNode> =
    skgnode_for_orgnode(&orgnode, &map);
  assert!(result.is_none(), "Should return None when ID not in map");
}

#[test]
fn test_skgnode_for_orgnode_scaffold() {
  // Scaffold → returns None
  let skgnode :
    SkgNode =
    SkgNode {
      title : "Test Node".to_string(),
      aliases : None,
      source : "test-source".to_string(),
      ids : vec![ID::new("test-id-789")],
      body : None,
      contains : None,
      subscribes_to : None,
      hides_from_its_subscriptions : None,
      overrides_view_of : None,
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

  let result :
    Option<&SkgNode> =
    skgnode_for_orgnode(&orgnode, &map);
  assert!(result.is_none(), "Should return None for Scaffold nodes");
}
