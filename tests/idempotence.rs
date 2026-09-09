// cargo nextest run --test grouped_overrides -E 'test(idempotence::)'
//
// plan_v2 §15: completion is a fixpoint. Re-rendering an already-rendered buffer,
// with no edits and an unchanged graph, must yield byte-identical output. This is
// the property the whole local-update refactor rests on (order-robust, no
// accumulating drift across saves), so it deserves a regression guard.
//
// Method: render the view de-novo to get a complete buffer, then run it through
// the save+rerender pipeline twice. The two rerenders must be identical. (We do
// not assert de-novo == first-rerender -- those can legitimately differ in
// metadata; idempotence is specifically that a second rerender changes nothing.)
//
// Fixture exercises several ViewNode kinds in one view: Active content (a -> b,
// c), an AliasCol + Alias (b has alias "bee"), the IDCols, a SubscribeeCol +
// subscribee-as-such (c subscribes to d), the HiddenOutsideOfSubscribeeCol, and
// a PartnerCol -- e subscribes to b, so b shows a SubscriberCol listing e.
// (The death-cleanup / generalized-orphan path -- a col whose owning node was
// deleted -- is covered separately by the save_collateral_delete_then_edit_under_it
// integration test, which only a delete can trigger, not an unchanged-graph rerender.)

use std::error::Error;
use std::net::TcpStream;

use skg::test_utils::{run_with_test_graph, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

use skg::dbs::in_rust_graph::InRustGraphHandle;

#[test]
fn rerender_is_idempotent
  () -> Result<(), Box<dyn Error>> {
  run_with_test_graph (
    "skg-test-idempotence",
    "tests/idempotence/fixtures",
    "/tmp/tantivy-test-idempotence",
    |config, fixture_graph, tantivy| Box::pin ( async move {
      rerender_is_idempotent_impl (
        config, fixture_graph, tantivy ) . await
    } )) }

/// Run one save+rerender of `buf` through the full pipeline, returning the
/// rerendered saved view.
async fn rerender_once (
  buf     : &str,
  config  : &SkgConfig,
  fixture_graph  : &InRustGraphHandle,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (),
  };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    buf, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "rerender must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

async fn rerender_is_idempotent_impl (
  config  : &SkgConfig,
  fixture_graph  : &InRustGraphHandle,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // The complete de-novo view of root `a`, which we then feed back through the
  // pipeline as if the user saved it unchanged.
  let (complete_buffer, _pids, _tree)
    : (String, Vec<ID>, _) =
    multi_root_view (
      config, Some (tantivy),
      &[ ID ("a" . to_string ()) ], false ) . await ?;

  let first  : String =
    rerender_once (&complete_buffer, config, fixture_graph, tantivy) . await ?;
  let second : String =
    rerender_once (&first, config, fixture_graph, tantivy) . await ?;

  assert_eq! ( first, second,
    "completion is not a fixpoint: a second rerender of an unchanged buffer \
     changed it.\n--- first rerender ---\n{}\n--- second rerender ---\n{}",
    first, second );
  Ok (( )) }
