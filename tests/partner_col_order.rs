// cargo test --test partner_col_order -- --nocapture
//
// A ColPolicy::ReadOnlySet col (here a SubscriberCol) respects the
// user's view-local member order across save and rerender
// (TODO/full-schema/8_readonly-set-ergonomics.org): present members
// keep their buffer order, and the order survives a further
// unchanged save. The order never reaches disk; it is view-local.
//
// Fixture: r and t both subscribe to n, so a view of n shows a
// SubscriberCol whose de-novo member order is sorted by ID (r, t).
// The test swaps them in the buffer and saves.

use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::test_utils::{run_with_test_db, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

use skg::dbs::in_rust_graph::InRustGraphHandle;
use typedb_driver::TypeDBDriver;

#[test]
fn readonly_col_order_is_preserved
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-partner-col-order",
    "tests/partner_col_order/fixtures",
    "/tmp/tantivy-test-partner-col-order",
    |config, driver, tantivy| Box::pin ( async move {
      readonly_col_order_is_preserved_impl (
        config, driver, tantivy ) . await
    } )) }

async fn save_and_rerender (
  buf     : &str,
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
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
    buf, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}", response . errors );
  Ok ( response . saved_view ) }

/// The whole line containing the given metadata fragment.
fn line_containing<'a> (
  buf      : &'a str,
  fragment : &str,
) -> &'a str {
  buf . lines ()
    . find ( |l| l . contains (fragment) )
    . unwrap_or_else (
      || panic! ( "no line contains {:?} in:\n{}", fragment, buf )) }

fn assert_member_order (
  buf    : &str,
  first  : &str, // a metadata fragment, e.g. "(id t)"
  second : &str,
  label  : &str,
) {
  let i : usize = buf . find (first) . unwrap_or_else (
    || panic! ( "{}: {:?} not found in:\n{}", label, first, buf ));
  let j : usize = buf . find (second) . unwrap_or_else (
    || panic! ( "{}: {:?} not found in:\n{}", label, second, buf ));
  assert! ( i < j,
    "{}: expected {:?} before {:?} in:\n{}", label, first, second, buf ); }

async fn readonly_col_order_is_preserved_impl (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  skg::dbs::in_rust_graph::init_global_handle_for_first_time_or_panic (
    graph_handle_from_config (config) ? );
  let (complete_buffer, _pids, _tree)
    : (String, Vec<ID>, _) =
    multi_root_view (
      driver, config, Some (tantivy),
      &[ ID ("n" . to_string ()) ], false ) . await ?;
  assert_member_order (
    &complete_buffer, "(id r)", "(id t)", "de novo (sorted)" );
  let swapped : String = {
    // Swap the two member lines, putting t before r.
    let r_line : &str = line_containing (&complete_buffer, "(id r)");
    let t_line : &str = line_containing (&complete_buffer, "(id t)");
    complete_buffer
      . replace ( r_line, "SWAP_PLACEHOLDER" )
      . replace ( t_line, r_line )
      . replace ( "SWAP_PLACEHOLDER", t_line ) };
  let first_save : String =
    save_and_rerender (&swapped, config, driver, tantivy) . await ?;
  assert_member_order (
    &first_save, "(id t)", "(id r)", "after reorder + save" );
  let second_save : String =
    save_and_rerender (&first_save, config, driver, tantivy) . await ?;
  assert_member_order (
    &second_save, "(id t)", "(id r)", "after second (unchanged) save" );
  assert_eq! ( first_save, second_save,
    "the reordered view must be a completion fixpoint" );
  Ok (( )) }
