// Tests for to_org complete contents functions

use skg::types::maps::add_v_to_map_if_absent;
use skg::types::nodes::complete::{empty_graphnode, Graphnode};

use skg::types::misc::ID;
use std::collections::HashMap;

#[tokio::test]
async fn test_add_v_to_map_if_absent_already_present() {
  // If Graphnode already in map, should not call fetch function
  let skgid :
    ID =
    ID::new ("test-id-123");
  let graphnode :
    Graphnode =
    Graphnode {
      title : "Cached Node" . to_string(),
      pid : skgid . clone(),
      .. empty_graphnode()
    };

  let mut map : HashMap<ID, Graphnode> = HashMap::new();
  map . insert(skgid . clone(), graphnode . clone());

  // Fetch function that would load from disk (not called since already cached)
  let fetch_fn = |_key: &ID| async {
    panic!("Should not be called - value already in map")
  };

  let result =
    add_v_to_map_if_absent(&skgid, &mut map, fetch_fn) . await;

  assert!(result . is_ok(), "Should succeed without fetching");
  assert_eq!(map . len(), 1, "Map should still have 1 entry");
  assert_eq!(map . get (&skgid) . unwrap() . title, "Cached Node",
             "Should return cached node");
}
