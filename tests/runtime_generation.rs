use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::init::{
  create_empty_tantivy_index, empty_in_ram_tantivy_index,
  rebuild_tantivy_as_generation,
};
use skg::types::env::SharedRuntime;
use skg::types::misc::{ID, SkgConfig};
use skg::types::nodes::complete::empty_node_complete;

use std::sync::Arc;
use std::collections::HashMap;

fn config () -> SkgConfig {
  SkgConfig::dummyFromSources (HashMap::new ())
}

fn graph_with (pid : &str) -> InRustGraph {
  let mut node = empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  InRustGraph::from_nodecompletes (&[node])
}

#[test]
fn cloned_runtime_observes_atomic_generations_while_old_snapshot_stays_stable () {
  let mut old_config = config ();
  old_config . owned_folder = "old-config" . to_string ();
  let old_index = empty_in_ram_tantivy_index () . unwrap ();
  let runtime = Arc::new (SharedRuntime::new (
    old_config, Arc::new (graph_with ("old-node")), old_index . clone ()));
  let other_connection = runtime . clone ();
  let captured_old = runtime . snapshot ();

  let mut new_config = (*captured_old . config) . clone ();
  new_config . owned_folder = "new-config" . to_string ();
  let new_index = empty_in_ram_tantivy_index () . unwrap ();
  let published = runtime . publish (
    Arc::new (new_config), Arc::new (graph_with ("new-node")),
    new_index . clone ());

  assert_eq! (published . generation, 1);
  let observed = other_connection . snapshot ();
  assert_eq! (observed . generation, 1);
  assert_eq! (observed . config . owned_folder, "new-config");
  assert! (observed . graph . nodes . contains_key (&ID::from ("new-node")));
  assert! (Arc::ptr_eq (&observed . tantivy_index . index, &new_index . index));

  assert_eq! (captured_old . generation, 0);
  assert_eq! (captured_old . config . owned_folder, "old-config");
  assert! (captured_old . graph . nodes . contains_key (&ID::from ("old-node")));
  assert! (Arc::ptr_eq (
    &captured_old . tantivy_index . index, &old_index . index));
}

#[test]
fn unpublished_candidate_cannot_change_the_visible_generation () {
  let runtime = SharedRuntime::new (
    config (), Arc::new (graph_with ("published")),
    empty_in_ram_tantivy_index () . unwrap ());
  let before = runtime . snapshot ();
  let _failed_candidate = graph_with ("never-published");
  let after = runtime . snapshot ();
  assert! (Arc::ptr_eq (&before, &after));
  assert_eq! (after . generation, 0);
  assert! (! after . graph . nodes . contains_key (&ID::from ("never-published")));
}

#[test]
fn rebuild_candidate_uses_a_sibling_directory_without_wiping_live_index () {
  let temp = tempfile::tempdir () . unwrap ();
  let live_path = temp . path () . join ("tantivy");
  let live_index = create_empty_tantivy_index (&live_path) . unwrap ();
  let mut candidate_config = config ();
  candidate_config . tantivy_folder = live_path . clone ();
  let (generation_config, generation_index) = rebuild_tantivy_as_generation (
    &candidate_config, &[node_for_generation ("candidate")], 7) . unwrap ();
  assert_ne! (generation_config . tantivy_folder, live_path);
  assert! (live_path . is_dir (), "the published index directory survives");
  assert! (generation_config . tantivy_folder . is_dir ());
  assert! (! Arc::ptr_eq (&live_index . index, &generation_index . index));
}

fn node_for_generation (pid : &str) -> skg::types::nodes::complete::NodeComplete {
  let mut node = empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node
}
