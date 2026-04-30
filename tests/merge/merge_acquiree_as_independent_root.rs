// cargo test --test merge merge_acquiree_as_independent_root -- --nocapture
//
// Regression test for the preprocessing pass introduced to swap
// acquiree pids to acquirer pids in the viewforest before rerender
// walks it.
//
// Graph fixture:
//   b and d, both leaves, both at the main source.
//
// The saved buffer has both b and d as top-level roots (i.e. not
// content-children of anything), and puts (editRequest (merge b))
// on d. So d is the acquirer and b is the acquiree.
//
// Before this fix, the rerender of the saved buffer would show b
// as a DeletedNode at the top level (b hit
// deleted_by_this_save_pids in resolve_truenode_kind and
// short-circuited to (deleted ...)). The goal-list-resolution fix
// in reconcile_content_children only helped for acquirees that
// were content-children of some other node in the view -- it did
// nothing for top-level/Independent acquirees because reconcile
// never runs on them.
//
// Correct behaviour: any viewnode with pid = acquiree should be
// swapped to pid = acquirer *before* rerender. The preprocessing
// pass resolve_extra_ids_in_viewforest does that.
//
// Expected result:
//   * d            <-- was b, rewritten to d (first in doc order -> definitive)
//   ** MERGED: b
//   * d (indef)    <-- the original d root
// (No (deleted ...) anywhere.)

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::test_utils::{run_with_test_db, graph_handle_from_config, audit_memory_or_panic};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::memory::OpenViews;

use skg::dbs::memory::InRustGraphHandle;
use skg::types::misc::{SkgConfig, TantivyIndex};

use typedb_driver::TypeDBDriver;

#[test]
fn test_merge_acquiree_as_independent_root
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-merge-acquiree-as-independent-root",
    "tests/merge/merge_acquiree_as_independent_root/fixtures",
    "/tmp/tantivy-test-merge-acquiree-as-independent-root",
    |config, driver, tantivy| Box::pin ( async move {
      merge_acquiree_as_independent_root_impl(
        config, driver, tantivy ) . await
    } )) }

async fn merge_acquiree_as_independent_root_impl (
  config  : &SkgConfig,
  driver: &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg (node (id b) (source main))) b
    * (skg (node (id d) (source main) (editRequest (merge b)))) d
  "};

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),
        };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer after merge:\n{}", response . saved_view);
  if !response . errors . is_empty() {
    println!("Errors: {:?}", response . errors); }

  let mut failures : Vec<String> = Vec::new();
  let view : &str = &response . saved_view;
  let lines : Vec<&str> = view . lines() . collect();

  // 1. No (deleted ...) anywhere: acquiree b should have been
  //    swapped to d by resolve_extra_ids_in_viewforest.
  if view . contains ("(deleted") {
    failures . push (
      "saved view contains a (deleted ...) atom -- the top-level \
       acquiree b was rendered as deleted instead of being swapped \
       to its acquirer d by the preprocessing pass"
      . to_string() ); }

  // 2. The first top-level headline should reference (id d).
  //    (It was originally pid=b; preprocessing rewrote it.)
  let first_root : Option<&&str> =
    lines . iter () . find ( |l| l . starts_with ("* ") );
  match first_root {
    None => failures . push (
      "no top-level headline found in rendered view" . to_string() ),
    Some (l) => {
      if ! l . contains ("(id d)") {
        failures . push ( format! (
          "first top-level headline does not reference (id d); \
           got: {}", l ) ); } } }

  // 3. Both top-level headlines should reference (id d): the
  //    rewritten b-root and the original d-root.
  let d_root_count : usize =
    lines . iter ()
    . filter ( |l| l . starts_with ("* ") && l . contains ("(id d)") )
    . count ();
  if d_root_count != 2 {
    failures . push ( format! (
      "expected 2 top-level (id d) headlines (rewritten b + \
       original d); found {}", d_root_count )); }

  // 4. The MERGED: b preserver should appear in the view (under
  //    whichever d root is definitive -- the first one in doc
  //    order, i.e. the rewritten-from-b root).
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

  audit_memory_or_panic (&graph, &config . db_name, driver) . await?;
  Ok (( )) }
