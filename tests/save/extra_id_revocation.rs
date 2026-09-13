use skg::dbs::filesystem::one_node::nodecomplete_from_id;
use skg::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use skg::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle;
use skg::nodeMerge::merge_nodes;
use skg::save::update_graph_minus_nodeMerges;
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores,
                      tantivy_contains_id};
use skg::types::env::new_mutation_gate;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, SourceName};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, DeleteNode, NodeMerge, SaveNode};

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
      exercise_rejected_revocation (&stores . config, &stores . tantivy) . await ?;
      stores . reset (
        "rejected_merge_override_does_not_mutate_any_store",
        "tests/save/override_collision/fixtures") ?;
      exercise_rejected_merge_override (
        &stores . config, &stores . tantivy) . await
    }))
}

async fn exercise_rejected_merge_override (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let before_graph : Arc<InRustGraph> = graph . load_full ();
  let source_path : &PathBuf = &config . sources [&SourceName::from ("main")] . path;
  let paths : Vec<PathBuf> = ["N1.skg", "N2.skg", "R1.skg", "R2.skg"]
    . iter () . map (|name| source_path . join (name)) . collect ();
  let before_bytes : Vec<Vec<u8>> = paths . iter ()
    . map (fs::read) . collect::<Result<Vec<Vec<u8>>, _>> () ?;
  let mut n1 : NodeComplete = nodecomplete_from_id (config, &ID::from ("N1")) ?;
  n1 . extra_ids = vec![ID::from ("N2")];
  let mut preserver : NodeComplete =
    nodecomplete_from_id (config, &ID::from ("N2")) ?;
  preserver . pid = ID::from ("preserver");
  preserver . extra_ids . clear ();
  preserver . overrides_view_of = skg::types::misc::MSV::Unspecified;
  let merge : NodeMerge = NodeMerge {
    acquiree_text_preserver : SaveNode (preserver),
    updated_acquirer        : SaveNode (n1),
    acquiree_to_delete      : DeleteNode {
      id : ID::from ("N2"), source : SourceName::from ("main"),
    },
  };
  let result : Result<Option<TantivyIndex>, Box<dyn Error>> = merge_nodes (
    &[merge], config . clone (), tantivy, &graph, &new_mutation_gate ()) . await;
  let error : Box<dyn Error> = match result {
    Err (error) => error,
    Ok (_) => panic! ("merge-created override collision must be rejected"),
  };
  let message : String = error . to_string ();
  for expected in ["N1", "N2", "R1", "R2", "canonicalizing"] {
    assert! (message . contains (expected), "missing {expected}: {message}"); }

  wait_for_tantivy_writes_idle ();
  assert! (Arc::ptr_eq (&before_graph, &graph . load_full ()));
  let after_bytes : Vec<Vec<u8>> = paths . iter ()
    . map (fs::read) . collect::<Result<Vec<Vec<u8>>, _>> () ?;
  assert_eq! (before_bytes, after_bytes);
  Ok (( ))
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
