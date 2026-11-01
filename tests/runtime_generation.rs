use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::init::{
  create_empty_tantivy_index, empty_in_ram_tantivy_index,
  rebuild_tantivy_as_generation,
};
use skg::types::env::SharedRuntime;
use skg::types::misc::{ID, SkgConfig};
use skg::types::nodes::complete::empty_graphnode;

use std::sync::Arc;
use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  SkgConfig::dummyFromSkgrepos (HashMap::new ())
}

fn graph_with (pid : &str) -> InRustGraph {
  let mut node = empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  InRustGraph::from_graphnodes (&[node])
}

#[test]
fn cloned_runtime_observes_atomic_generations_while_old_snapshot_stays_stable () {
  let mut old_config = config ();
  old_config . tantivy_folder = PathBuf::from ("old-config");
  let old_index = empty_in_ram_tantivy_index () . unwrap ();
  let runtime = Arc::new (SharedRuntime::new (
    old_config, Arc::new (graph_with ("old-node")), old_index . clone ()));
  let other_connection = runtime . clone ();
  let captured_old = runtime . graph_snapshot ();

  let mut new_config = (*captured_old . config) . clone ();
  new_config . tantivy_folder = PathBuf::from ("new-config");
  let new_index = empty_in_ram_tantivy_index () . unwrap ();
  let swapped_in = runtime . swap_in (
    Arc::new (new_config), Arc::new (graph_with ("new-node")),
    new_index . clone ());

  assert_eq! (swapped_in . generation, captured_old . generation + 1);
  let observed = other_connection . graph_snapshot ();
  assert_eq! (observed . generation, captured_old . generation + 1);
  assert_eq! (observed . config . tantivy_folder, PathBuf::from ("new-config"));
  assert! (observed . graph . nodes . contains_key (&ID::from ("new-node")));
  assert! (Arc::ptr_eq (&observed . tantivy_index . index, &new_index . index));

  assert_eq! (captured_old . config . tantivy_folder, PathBuf::from ("old-config"));
  assert! (captured_old . graph . nodes . contains_key (&ID::from ("old-node")));
  assert! (Arc::ptr_eq (
    &captured_old . tantivy_index . index, &old_index . index));
}

#[test]
fn a_candidate_not_swapped_in_cannot_change_the_visible_generation () {
  let runtime = SharedRuntime::new (
    config (), Arc::new (graph_with ("swapped-in")),
    empty_in_ram_tantivy_index () . unwrap ());
  let before = runtime . graph_snapshot ();
  let _failed_candidate = graph_with ("never-swapped-in");
  let after = runtime . graph_snapshot ();
  assert! (Arc::ptr_eq (&before, &after));
  assert_eq! (after . generation, before . generation);
  assert! (! after . graph . nodes . contains_key (&ID::from ("never-swapped-in")));
}

#[test]
fn a_runtime_starts_after_every_earlier_process_and_stamps_what_it_read () {
  // A buffer an earlier server process rendered must count as older
  // than every node a later one read: startup generations follow the clock.
  let earlier = SharedRuntime::new (
    config (), Arc::new (graph_with ("read")),
    empty_in_ram_tantivy_index () . unwrap ()) . graph_snapshot ();
  std::thread::sleep (std::time::Duration::from_millis (1));
  let later = SharedRuntime::new (
    config (), Arc::new (graph_with ("read")),
    empty_in_ram_tantivy_index () . unwrap ()) . graph_snapshot ();
  assert! (later . generation > earlier . generation);
  assert_eq! (later . graph . last_change_of (&ID::from ("read")),
              Some (later . generation));
  assert_eq! (later . graph . last_change_of (&ID::from ("never-read")), None);
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
  assert! (live_path . is_dir (), "the swapped-in search-index directory survives");
  assert! (generation_config . tantivy_folder . is_dir ());
  assert! (! Arc::ptr_eq (&live_index . index, &generation_index . index));
}

fn node_for_generation (pid : &str) -> skg::types::nodes::complete::Graphnode {
  let mut node = empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node
}
