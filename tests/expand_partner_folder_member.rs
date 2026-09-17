// cargo nextest run --test grouped_overrides -E 'test(expand_partner_folder_member::)'
//
// Definitive expansion of a read-only PartnerFolder member
// (TODO/full-schema/13_test-rel-matrix.org). Confirmed 2026-06-12:
// expanding a subscriberFolder member behaves like expanding any
// write-protected node -- the member line stays a raw member, its own
// content appears, and NO subscription-hides apply (those are scoped
// to subscribees-as-such, the other direction).

use std::error::Error;
use std::net::TcpStream;

use skg::dbs::in_rust_graph::{
  InRustGraphHandle};
use skg::test_utils::{run_with_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use ego_tree::Tree;
use skg::types::viewnode::ViewNode;

async fn save (
  buf     : &str,
  config : &SkgConfig,
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
    &mut stream, buf, config, tantivy, graph, false,
    &Err ( String::new () ), &mut views_state ) . await }

/// Line in 'buf' that mentions 'fragment'.
fn line_containing<'a> ( buf : &'a str, fragment : &str ) -> &'a str {
  buf . lines ()
    . find ( |l| l . contains (fragment) )
    . unwrap_or_else (
      || panic! ("no line contains {:?} in:\n{}", fragment, buf )) }

#[test]
fn expanding_subscriberFolder_member_is_plain_expansion
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-expand-partner-folder-member",
    "tests/expand_partner_folder_member/fixtures",
    "/tmp/tantivy-test-expand-partner-folder-member",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let (n_view, _pids, _tree)
        : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view (
          config, Some (tantivy), &[ID::from ("N")], false )
 ?;
      assert! ( n_view . contains ("subscriberFolder")
                && line_containing (&n_view, "(id S)") . contains (" writeProtected"),
        "N's view should show S as a write-protected subscriberFolder \
         member:\n{}", n_view );
      // Request definitive expansion of the subscriberFolder member S.
      // Since uniform-heralds the write-protected member line carries
      // 'write-protected (birthHerald ...)' (the old graphStats atom is gone), so
      // we inject the definitiveView request right after 'writeProtected'. S is
      // the only write-protected node in this view.
      let s_line : String =
        line_containing (&n_view, "(id S)") . to_string ();
      let s_line_expanded : String = s_line . replace (
        " writeProtected ", " writeProtected (viewRequests definitiveView) " );
      let expanded_request : String =
        n_view . replace (&s_line, &s_line_expanded);
      let saved : String =
        save (&expanded_request, config, tantivy, &graph)
        . await ? . saved_view;
      // S stays a raw subscriberFolder member, now definitive...
      assert! ( saved . contains ("subscriberFolder"),
        "S should remain under the subscriberFolder:\n{}", saved );
      assert! ( ! line_containing (&saved, "(id S)") . contains (" writeProtected"),
        "S should be definitive after expansion:\n{}", saved );
      // ...its own content appears...
      assert! ( saved . contains ("(id C)") && saved . contains ("content of S"),
        "S's own content (C) should appear on expansion:\n{}", saved );
      // ...and NO subscription-hides apply (S is a subscriber here, not
      // a subscribee-as-such): no hidden-subscription scaffolds.
      assert! ( ! saved . contains ("hiddenInSubscribeeFolder")
                && ! saved . contains ("hiddenOutsideOfSubscribeeFolder"),
        "expanding a subscriberFolder member must not apply hides:\n{}",
        saved );
      Ok (( )) } )) }
