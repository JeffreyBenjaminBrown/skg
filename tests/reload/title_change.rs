// Partial-reload parity tests. An out-of-band edit to a .skg file on
// disk, followed by a reload of that path, must leave the derived
// stores in the same state as a full rebuild from disk -- and must NOT
// disturb untouched telescopes.
//
// cargo nextest run --test grouped_saves -E 'test(reload::title_change::)'

use skg::dbs::in_rust_graph::InRustGraph;
use skg::serve::handlers::reload_paths::{
  classify_touched_telescopes,
  reload_touched_telescopes,
};
use skg::test_utils::{
  graph_handle_from_config,
  run_with_test_db,
  skg_env_from_parts,
};
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, SkgConfig, SourceName};
use skg::types::store_state::{
  PathDigest,
  PathIndexState,
  SelectedPathValue,
};

use std::error::Error;
use std::path::PathBuf;

fn main_source_dir (config : &SkgConfig) -> PathBuf {
  config . sources
    . get ( &SourceName::from ("main") )
    . expect ("single-source test config names its source 'main'")
    . path . clone () }

fn title_in (graph : &InRustGraph, pid : &str) -> Option<String> {
  graph . get ( &ID (pid . to_string ()) )
    . map ( |n| n . title . clone () ) }

#[test]
fn reload_reflects_an_edited_title (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-reload-title",
    "tests/reload/fixtures-title-change",
    "/tmp/tantivy-test-reload-title",
    |config, driver, tantivy| Box::pin ( async move {
      let graph = graph_handle_from_config (config) ?;
      let mut env : SkgEnv =
        skg_env_from_parts (config, driver . clone (), tantivy, &graph);

      assert_eq! ( title_in (& env . in_rust_graph . load_full (), "n1"),
                   Some ("n1 old" . to_string ()),
                   "precondition: n1 loaded with its original title" );

      // Out-of-band edit on disk (as a magit discard / external edit would).
      let n1_path : PathBuf =
        main_source_dir (config) . join ("n1.skg");
      std::fs::write ( &n1_path, "pid: n1\ntitle: n1 new\n" ) ?;

      let touched =
        classify_touched_telescopes (
          & env . config, &[ n1_path . clone () ] );
      reload_touched_telescopes ( &mut env, touched ) . await
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;

      let after = env . in_rust_graph . load_full ();
      assert_eq! ( title_in (&after, "n1"), Some ("n1 new" . to_string ()),
                   "reload applied the edited title" );
      assert_eq! ( title_in (&after, "n2"), Some ("n2" . to_string ()),
                   "reload left the untouched telescope n2 alone" );

      // Parity with a full rebuild from the same disk state.
      let oracle = graph_handle_from_config (config) ? . load_full ();
      assert_eq! ( title_in (&after, "n1"), title_in (&oracle, "n1") );
      assert_eq! ( after . len (), oracle . len () );
      let expected_digest = PathDigest::of_bytes (&std::fs::read (&n1_path) ?);
      assert_eq! (after . manifest . get (&n1_path), Some (&expected_digest));
      let selected = after . path_outcomes . get (&n1_path)
        . expect ("reload records the exact selected path");
      assert_eq! (selected . value, SelectedPathValue::Present (expected_digest));
      assert! (matches! (
        selected . index_state,
        PathIndexState::Acknowledged { tantivy_generation: Some (_) }));
      Ok (( )) } )) }

#[test]
fn reload_removes_a_deleted_node (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-reload-delete",
    "tests/reload/fixtures-title-change",
    "/tmp/tantivy-test-reload-delete",
    |config, driver, tantivy| Box::pin ( async move {
      let graph = graph_handle_from_config (config) ?;
      let mut env : SkgEnv =
        skg_env_from_parts (config, driver . clone (), tantivy, &graph);
      assert! ( title_in (& env . in_rust_graph . load_full (), "n2")
                . is_some (),
                "precondition: n2 present" );

      // Remove n2's only section from disk, then reload its path.
      let n2_path : PathBuf =
        main_source_dir (config) . join ("n2.skg");
      std::fs::remove_file ( &n2_path ) ?;

      let touched =
        classify_touched_telescopes (
          & env . config, &[ n2_path . clone () ] );
      reload_touched_telescopes ( &mut env, touched ) . await
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;

      let after = env . in_rust_graph . load_full ();
      assert! ( title_in (&after, "n2") . is_none (),
                "reload removed the deleted node from the graph" );
      assert! ( title_in (&after, "n1") . is_some (),
                "n1 survived" );

      let oracle = graph_handle_from_config (config) ? . load_full ();
      assert_eq! ( after . len (), oracle . len (),
                   "graph parity with a full rebuild from disk" );
      assert! (!after . manifest . contains_key (&n2_path));
      let selected = after . path_outcomes . get (&n2_path)
        . expect ("reload records selected absence for a deletion");
      assert_eq! (selected . value, SelectedPathValue::Absent);
      assert! (matches! (
        selected . index_state,
        PathIndexState::Acknowledged { tantivy_generation: Some (_) }));
      Ok (( )) } )) }

#[test]
fn reload_rejects_a_new_extra_id_claimed_by_an_untouched_node (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-reload-id-conflict",
    "tests/reload/fixtures-title-change",
    "/tmp/tantivy-test-reload-id-conflict",
    |config, driver, tantivy| Box::pin ( async move {
      let graph = graph_handle_from_config (config) ?;
      let mut env = skg_env_from_parts (
        config, driver . clone (), tantivy, &graph);
      let n1_path = main_source_dir (config) . join ("n1.skg");
      let selected_before = env . in_rust_graph . load_full ();
      std::fs::write (
        &n1_path,
        "pid: n1\nextra_ids:\n- n2\ntitle: n1 conflicting\n") ?;
      let touched = classify_touched_telescopes (
        &env . config, &[n1_path . clone ()]);
      let error = reload_touched_telescopes (&mut env, touched) . await
        . expect_err ("n2 cannot name both n1 and n2");
      assert! (error . contains ("multiple nodes"), "{}", error);
      let selected_after = env . in_rust_graph . load_full ();
      assert_eq! (
        title_in (&selected_after, "n1"), Some ("n1 old" . into ()),
        "joint ID-claim failure keeps the last-good graph");
      assert_eq! (
        selected_after . graph_generation,
        selected_before . graph_generation);
      assert_eq! (selected_after . manifest, selected_before . manifest);
      Ok (( )) } )) }
