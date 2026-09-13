use skg::dbs::filesystem::one_node::nodecomplete_from_id;
use skg::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use skg::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle;
use skg::save::update_graph_minus_nodeMerges;
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores,
                      tantivy_contains_id};
use skg::types::env::new_mutation_gate;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, SourceName};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode};

use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

#[test]
fn rejected_revocation_does_not_mutate_any_store () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-extra-id-revocation",
    |stores| Box::pin (async move {
      stores . reset (
        "rejected_revocation_does_not_mutate_any_store",
        "tests/save/extra_id_revocation/fixtures") ?;
      exercise_rejected_revocation (&stores . config, &stores . tantivy) . await
    }))
}

async fn exercise_rejected_revocation (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let before_graph : Arc<InRustGraph> = graph . load_full ();
  let path : PathBuf = config . sources [&SourceName::from ("main")] . path
    . join ("P.skg");
  let before_bytes : Vec<u8> = fs::read (&path) ?;
  let before_search : bool = tantivy_contains_id (tantivy, "P", "P") ?;
  let mut saved : NodeComplete = nodecomplete_from_id (config, &ID::from ("P")) ?;
  saved . extra_ids . clear ();

  let result : Result<Option<TantivyIndex>, Box<dyn Error>> =
    update_graph_minus_nodeMerges (
    vec![DefineNode::Save (SaveNode (saved))], &[], config . clone (),
    tantivy, &graph, &new_mutation_gate ()) . await;
  let error : Box<dyn Error> = match result {
    Err (error) => error,
    Ok (_) => panic! ("managed Save must reject an extra-ID revocation"),
  };
  assert! (error . to_string () . contains ("revoke existing extra ID"));

  wait_for_tantivy_writes_idle ();
  assert! (Arc::ptr_eq (&before_graph, &graph . load_full ()));
  assert_eq! (before_bytes, fs::read (&path) ?);
  assert_eq! (
    before_search,
    tantivy_contains_id (tantivy, "P", "P") ?);
  Ok (( ))
}
