// cargo test --test merge merge_acquiree_in_sibling_subtree -- --nocapture
//
// Regression test for a rerender-path bug in the merge pipeline.
//
// Graph fixture:
//   a contains [b]
//   c contains [d]
//
// The saved buffer has a and c as top-level roots, and puts
// (editRequest (merge b)) on d -- i.e. d is the acquirer, b is the
// acquiree.
//
// After the save, a.skg still lists b in its contains, and b has
// become an extra_id of d. The fresh-view path resolves that
// extra_id transparently (pid_and_source_from_id does a primary-
// or-extra lookup), so opening a afresh shows a -> d correctly.
//
// The rerender path (rerender_view -> complete_viewtree ->
// reconcile_content_children -> content_goal_list) does NOT
// resolve extra_ids. It walks a.contains = [b] verbatim, matches
// the existing pid=b child in a's viewforest, and lets the
// deleted_by_this_save_pids check mutate it into a DeletedNode.
// Result: the saved-view rerender shows a -> (deleted b) instead
// of a -> d.
//
// This test asserts the correct post-merge rendering:
//   * a
//   ** d           <-- (previously: ** (deleted b), which is wrong)
//   * c
//   ** d
//   *** MERGED: b
// (The first d is expected to be definitive since a's subtree is
// visited first in document order; the second d indefinitive.)

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::test_utils::{run_with_test_db, graph_handle_from_config, audit_in_rust_graph_or_panic};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::types::misc::{SkgConfig, TantivyIndex};

use typedb_driver::TypeDBDriver;

#[test]
fn test_merge_acquiree_in_sibling_subtree
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-merge-acquiree-in-sibling-subtree",
    "tests/merge/merge_acquiree_in_sibling_subtree/fixtures",
    "/tmp/tantivy-test-merge-acquiree-in-sibling-subtree",
    |config, driver, tantivy| Box::pin ( async move {
      merge_acquiree_in_sibling_subtree_impl(
        config, driver, tantivy ) . await
    } )) }

async fn merge_acquiree_in_sibling_subtree_impl (
  config  : &SkgConfig,
  driver: &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg (node (id a) (source main))) a
    ** (skg (node (id b) (source main))) b
    * (skg (node (id c) (source main))) c
    ** (skg (node (id d) (source main) (editRequest (merge b)))) d
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

  // 1. No (deleted ...) anywhere: the acquiree b should have
  //    been substituted by the acquirer d, not rendered as deleted.
  if view . contains ("(deleted") {
    failures . push (
      "saved view contains a (deleted ...) atom -- the acquiree \
       b was rendered as deleted instead of being resolved to its \
       acquirer d via extra_id"
      . to_string() ); }

  // 2. a's first (and only) content child should be d, not b.
  let a_idx : Option<usize> =
    lines . iter() . position ( |l| l . contains ("(id a)") );
  if let Some (a_i) = a_idx {
    let a_child_line : Option<&&str> =
      lines[a_i + 1 ..] . iter()
      . take_while ( |l| l . starts_with ("** ") )
      . next ();
    match a_child_line {
      None => failures . push (
        "a has no ** child in the rendered view" . to_string() ),
      Some (l) => {
        if ! l . contains ("(id d)") {
          failures . push ( format! (
            "a's first ** child does not reference (id d); got: {}",
            l ) ); } } }
  } else {
    failures . push (
      "a not found in rendered view" . to_string() ); }

  // 3. d should also appear under c (same pid, definitive under
  //    whichever root comes first in document order).
  let c_idx : Option<usize> =
    lines . iter() . position ( |l| l . contains ("(id c)") );
  if let Some (c_i) = c_idx {
    let under_c : Vec<&&str> =
      lines[c_i + 1 ..] . iter()
      . take_while ( |l| ! l . starts_with ("* ") )
      . collect();
    let d_under_c : bool =
      under_c . iter() . any ( |l| l . contains ("(id d)") );
    if ! d_under_c {
      failures . push (
        "d not found under c in rendered view" . to_string() ); }
  } else {
    failures . push (
      "c not found in rendered view" . to_string() ); }

  // 4. The MERGED: b preserver should appear somewhere in the view.
  //    Exactly one of the two d occurrences (a's and c's) is
  //    definitive; the other is indefinitive and won't expand its
  //    contents. DefinitiveMap picks the first in document order.
  if ! view . contains ("MERGED: b") {
    failures . push (
      "text preserver 'MERGED: b' not found anywhere in rendered \
       view"
      . to_string() ); }

  if !failures . is_empty() {
    let msg : String = format!(
      "\n{} assertion(s) failed:\n  - {}",
      failures . len(),
      failures . join ("\n  - ") );
    panic!("{}", msg); }

  audit_in_rust_graph_or_panic (&graph, &config . db_name, driver) . await?;
  Ok (( )) }
