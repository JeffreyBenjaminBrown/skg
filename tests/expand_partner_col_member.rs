// cargo nextest run --test expand_partner_col_member
//
// Definitive expansion of a read-only PartnerCol member
// (TODO/full-schema/13_test-rel-matrix.org). Confirmed 2026-06-12:
// expanding a subscriberCol member behaves like expanding any
// indefinitive node -- the member line stays a raw member, its own
// content appears, and NO subscription-hides apply (those are scoped
// to subscribees-as-such, the other direction).
//
// Its own target because it installs the process-global graph handle
// (the inbound subscriberCol is created from snapshot_global).

use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::dbs::in_rust_graph::{
  InRustGraphHandle, init_global_handle_for_first_time_or_panic};
use skg::test_utils::{run_with_test_db, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use ego_tree::Tree;
use skg::types::viewnode::ViewNode;
use typedb_driver::TypeDBDriver;

async fn save (
  buf     : &str,
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (), };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  update_from_and_rerender_buffer (
    &mut stream, buf, driver, config, tantivy, graph, false,
    &Err ( String::new () ), &mut views_state ) . await }

/// Line in 'buf' that mentions 'fragment'.
fn line_containing<'a> ( buf : &'a str, fragment : &str ) -> &'a str {
  buf . lines ()
    . find ( |l| l . contains (fragment) )
    . unwrap_or_else (
      || panic! ("no line contains {:?} in:\n{}", fragment, buf )) }

#[test]
fn expanding_subscriberCol_member_is_plain_expansion
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-expand-partner-col-member",
    "tests/expand_partner_col_member/fixtures",
    "/tmp/tantivy-test-expand-partner-col-member",
    |config, driver, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      init_global_handle_for_first_time_or_panic ( graph . clone () );
      let (n_view, _pids, _tree)
        : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view (
          driver, config, Some (tantivy), &[ID::from ("N")], false )
        . await ?;
      assert! ( n_view . contains ("subscriberCol")
                && line_containing (&n_view, "(id S)") . contains (" indef"),
        "N's view should show S as an indefinitive subscriberCol \
         member:\n{}", n_view );
      // Request definitive expansion of the subscriberCol member S.
      let expanded_request : String = n_view
        . replace ("indef (graphStats",
                   "indef (viewRequests definitiveView) (graphStats");
      let saved : String =
        save (&expanded_request, config, driver, tantivy, &graph)
        . await ? . saved_view;
      // S stays a raw subscriberCol member, now definitive...
      assert! ( saved . contains ("subscriberCol"),
        "S should remain under the subscriberCol:\n{}", saved );
      assert! ( ! line_containing (&saved, "(id S)") . contains (" indef"),
        "S should be definitive after expansion:\n{}", saved );
      // ...its own content appears...
      assert! ( saved . contains ("(id C)") && saved . contains ("content of S"),
        "S's own content (C) should appear on expansion:\n{}", saved );
      // ...and NO subscription-hides apply (S is a subscriber here, not
      // a subscribee-as-such): no hidden-subscription scaffolds.
      assert! ( ! saved . contains ("hiddenInSubscribeeCol")
                && ! saved . contains ("hiddenOutsideOfSubscribeeCol"),
        "expanding a subscriberCol member must not apply hides:\n{}",
        saved );
      Ok (( )) } )) }
