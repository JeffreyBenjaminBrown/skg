// cargo nextest run --test grouped_saves -E 'test(save::id_folder_outside_diff_view::)'
//
// Outside the diff view, an ID folder stays as the user put it, and
// the refresh keeps it up to date. The parser lets an occurrence sit
// under an ID folder; it claims nothing there (the folder is
// write-protected), and the refresh keeps it, unwarranted.
//
// Fixture: two unrelated nodes, n and x.

use std::error::Error;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{save_ignoring_streamed_messages, empty_views_state, run_with_shared_test_stores, graph_handle_from_config};
use skg::types::config::SkgConfig;
use skg::types::misc::TantivyIndex;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/save/id_folder_outside_diff_view/fixtures";
  run_with_shared_test_stores (
    "skg-test-save-id-folder-outside-diff-view",
    |s| Box::pin ( async move {
      s . reset ("a_noClaim_child_under_an_id_folder_survives_the_rerender", fixtures) ?;
      a_noClaim_child_under_an_id_folder_survives_the_rerender (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// Save 'buffer', asserting that it saves; return the rerendered view.
async fn save_buffer (
  buffer      : &str,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<String, Box<dyn Error>> {
  let response : SaveResponse = save_ignoring_streamed_messages (
    buffer, config, tantivy, graph,
    &Err ( String::new () ), views_state ) . await ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save, got {:?}\n\nbuffer:\n{}",
            response . errors, buffer );
  Ok (response . saved_view) }

async fn a_noClaim_child_under_an_id_folder_survives_the_rerender (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let buffer : &str = "\
* (skg node (gnid n) (skgRepos (nodeRepo main))) n
** (skg idFolder)
*** (skg id) n
*** (skg node (gnid x) (skgRepos (nodeRepo main)) noClaim) x
";
  let saved : String = save_buffer (
    buffer, config, tantivy, &graph, &mut views_state ) . await ?;
  println! ("Result of saving a child claiming nothing under an ID folder:\n{}", saved);
  let lines : Vec<String> = saved . lines () . map ( |line|
    skg::metadata_canon::without_viewnode_ids (line) ) . collect ();
  assert_eq! ( lines, vec! [
    "* (skg node (gnid n) (skgRepos (nodeRepo main))) n",
    "** (skg idFolder)",
    "*** (skg id) n",
    "*** (skg node (gnid x) (skgRepos (nodeRepo main)) (view noWarrant)) x" ],
    "the ID folder stays, holding x unwarranted:\n{}", saved );
  let again : String = save_buffer (
    &saved, config, tantivy, &graph, &mut views_state ) . await ?;
  assert_eq! ( again, saved, "a further save changes nothing" );
  Ok (( )) }
