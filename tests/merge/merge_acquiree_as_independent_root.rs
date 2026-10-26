// cargo test --test merge merge_acquiree_as_independent_root -- --nocapture
//
// Regression test: after a nodeMerge, a viewnode whose pid is the
// acquiree's stands for the acquirer when the view is refreshed.
//
// Graph fixture:
//   b and d, both leaves, both at the main skgrepo.
//
// The saved buffer has both b and d as top-level roots (i.e. not
// content-children of anything), and puts (editRequest (merge b))
// on d. So d is the acquirer and b is the acquiree.
//
// Before this fix, the rerender of the saved buffer would show b
// as a PhantomDeleted at the top level (b hit
// deleted_by_this_save_pids in extend_true_content_at_unrestrictedVognode and
// short-circuited to (deleted ...)). The goal-list-resolution fix
// in reconcile_content_children only helped for acquirees that
// were content-children of some other node in the view -- it did
// nothing for top-level/non-member acquirees because reconcile
// never runs on them.
//
// Correct behaviour: any viewnode with pid = acquiree stands for the
// acquirer after the save: the refresh re-kinds it in place. The two
// roots then stand for one node, siblings under the buffer root, and
// the refresh merges them.
//
// Expected result:
//   * d            <-- was b, now d (first in doc order -> editable)
//   ** MERGED: b
// (No tombstone anywhere.)

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;

use skg::test_utils::{run_with_test_stores, graph_handle_from_config, audit_inrustgraph_or_panic};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::types::misc::{SkgConfig, TantivyIndex};


#[test]
fn test_merge_acquiree_as_independent_root
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-merge-acquiree-as-independent-root",
    "tests/merge/merge_acquiree_as_independent_root/fixtures",
    "/tmp/tantivy-test-merge-acquiree-as-independent-root",
    |config, tantivy| Box::pin ( async move {
      merge_acquiree_as_independent_root_impl(
        config, tantivy ) . await
    } )) }

async fn merge_acquiree_as_independent_root_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg node (gnid b) (skgRepos (nodeRepo main))) b
    * (skg node (gnid d) (skgRepos (nodeRepo main)) (editRequest (merge b))) d
  "};

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
        open_views            : OpenViews::new (),
        graph_straddle        : None,
        };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer after merge:\n{}", response . saved_view);
  if !response . errors . is_empty() {
    println!("Errors: {:?}", response . errors); }

  let mut failures : Vec<String> = Vec::new();
  let view : &str = &response . saved_view;
  let lines : Vec<&str> = view . lines() . collect();

  // 1. No tombstone anywhere: acquiree b should have been
  //    re-kinded as d by the refresh.
  if view . contains ("tombstone") {
    failures . push (
      "saved view contains a tombstone -- the top-level \
       acquiree b was entombstoned as deleted instead of being swapped \
       to its acquirer d by the refresh"
      . to_string() ); }

  // 2. The first top-level headline should reference (gnid d).
  //    (It was originally pid=b; the refresh re-kinded it.)
  let first_root : Option<&&str> =
    lines . iter () . find ( |l| l . starts_with ("* ") );
  match first_root {
    None => failures . push (
      "no top-level headline found in rendered view" . to_string() ),
    Some (l) => {
      if ! l . contains ("(gnid d)") {
        failures . push ( format! (
          "first top-level headline does not reference (gnid d); \
           got: {}", l ) ); } } }

  // 3. One top-level headline references (gnid d): the b-root and
  //    the original d-root, siblings standing for one node, merge.
  let d_root_count : usize =
    lines . iter ()
    . filter ( |l| l . starts_with ("* ") && l . contains ("(gnid d)") )
    . count ();
  if d_root_count != 1 {
    failures . push ( format! (
      "expected 1 top-level (gnid d) headline (the b-root and the \
       d-root merged); found {}", d_root_count )); }

  // 4. The MERGED: b preserver should appear in the view, under d.
  if ! view . contains ("MERGED: b") {
    failures . push (
      "text preserver 'MERGED: b' not found in rendered view"
      . to_string() ); }

  if !failures . is_empty() {
    let msg : String = format!(
      "\n{} assertion(s) failed:\n  - {}",
      failures . len(),
      failures . join ("\n  - ") );
    panic!("{}", msg); }

  audit_inrustgraph_or_panic (&graph)?;
  Ok (( )) }
