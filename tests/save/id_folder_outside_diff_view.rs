// cargo nextest run --test grouped_saves -E 'test(save::id_folder_outside_diff_view::)'
//
// Outside the diff view, every rerender removes the ID folders
// ('remove_id_folders'). The parser lets a non-member sit under an
// ID folder; the rerender keeps it, under the folder entombstoned as an
// orphaned folder is, and drops the folder's entries.
//
// Fixture: two unrelated nodes, n and x.

use crate::git_fixture_helpers::mk_test_tcp_stream_pair;
use std::error::Error;
use std::net::TcpStream;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::types::misc::{SkgConfig, TantivyIndex};
use skg::types::views_state::OpenViews;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/save/id_folder_outside_diff_view/fixtures";
  run_with_shared_test_stores (
    "skg-test-save-id-folder-outside-diff-view",
    |s| Box::pin ( async move {
      s . reset ("a_nonMember_under_an_id_folder_survives_the_rerender", fixtures) ?;
      a_nonMember_under_an_id_folder_survives_the_rerender (
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
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Err ( String::new () ), views_state ) . await ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save, got {:?}\n\nbuffer:\n{}",
            response . errors, buffer );
  Ok (response . saved_view) }

async fn a_nonMember_under_an_id_folder_survives_the_rerender (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let buffer : &str = "\
* (skg (node (id n) (repo main))) n
** (skg idFolder)
*** (skg id) n
*** (skg (node (id x) (repo main) noClaim)) x
";
  let saved : String = save_buffer (
    buffer, config, tantivy, &graph, &mut views_state ) . await ?;
  println! ("Result of saving a non-member under an ID folder:\n{}", saved);
  assert! ( ! saved . contains ("idFolder") && ! saved . contains ("(skg id)"),
            "the rerender removes the ID folder and its entries:\n{}", saved );
  let x_line : &str = saved . lines ()
    . find ( |line| line . contains ("(id x)") )
    . unwrap_or_else ( || panic! ("x must survive the rerender:\n{}", saved) );
  assert! ( x_line . starts_with ("*** ") && x_line . contains ("noClaim"),
            "x stays a non-member at its depth:\n{}", saved );
  assert! ( saved . lines () . any ( |line|
              line . starts_with ("** ") && line . contains ("tombstone") ),
            "the folder that held x is a tombstone:\n{}", saved );
  let again : String = save_buffer (
    &saved, config, tantivy, &graph, &mut views_state ) . await ?;
  assert_eq! ( again, saved, "a further save changes nothing" );
  Ok (( )) }
