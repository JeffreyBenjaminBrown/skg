// Tests for to_org complete contents functions

use skg::types::maps::add_v_to_map_if_absent;
use skg::types::skgnode::{empty_skgnode, SkgNode, SkgNodeMap};
use skg::types::misc::ID;

#[tokio::test]
async fn test_add_v_to_map_if_absent_already_present() {
  // If SkgNode already in map, should not call fetch function
  let id :
    ID =
    ID::new("test-id-123");
  let skgnode :
    SkgNode =
    SkgNode {
      title : "Cached Node".to_string(),
      ids : vec![id.clone()],
      .. empty_skgnode()
    };

  let mut map :
    SkgNodeMap =
    SkgNodeMap::new();
  map.insert(id.clone(), skgnode.clone());

  // Fetch function that would load from disk (not called since already cached)
  let fetch_fn = |_key: &ID| async {
    panic!("Should not be called - value already in map")
  };

  let result =
    add_v_to_map_if_absent(&id, &mut map, fetch_fn).await;

  assert!(result.is_ok(), "Should succeed without fetching");
  assert_eq!(map.len(), 1, "Map should still have 1 entry");
  assert_eq!(map.get(&id).unwrap().title, "Cached Node",
             "Should return cached node");
}
