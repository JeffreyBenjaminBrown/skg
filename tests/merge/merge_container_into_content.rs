// cargo test --test merge merge_container_into_content -- --nocapture
//
// Tests the full save-and-rerender pipeline when merging
// a container into one of its own content nodes.
//
// Fixture graph:
//   a contains [aa, b, c]
//   aa contains [x]
//
// Buffer merges a into aa.
// Expected: a becomes a DeletedNode, aa gains a's children (b, c)
// plus a text preserver ("MERGED: a"),
// and the editRequest is stripped from aa's metadata.

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::path::Path;
use std::sync::Arc;

use skg::dbs::filesystem::one_node::nodecomplete_from_pid_and_source;
use skg::test_utils::{run_with_test_db, graph_handle_from_config, audit_inrustgraph_or_panic};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, SourceName};

use typedb_driver::TypeDBDriver;

#[test]
fn test_merge_container_into_content
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-merge-container-into-content",
    "tests/merge/merge_container_into_content/fixtures",
    "/tmp/tantivy-test-merge-container-into-content",
    |config, driver, tantivy| Box::pin ( async move {
      merge_container_into_content_impl(
        config, driver, tantivy ) . await
    } )) }

async fn merge_container_into_content_impl (
  config  : &SkgConfig,
  driver: &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // The input buffer: a content view of 'a',
  // with aa requesting to merge a into itself.
  let input_org_text : &str = indoc! {"
    * (skg (node (id a) (source main))) a
    ** (skg (node (id aa) (source main) (editRequest (merge a)))) aa
    *** (skg (node (id x) (source main))) x
    ** (skg (node (id b) (source main))) b
    ** (skg (node (id c) (source main))) c
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

  // -- Filesystem assertions (merge mechanics) --

  // a.skg should be deleted from disk.
  let a_path : String =
    skg::util::path_from_pid_and_source (
      config, &SourceName::from ("main"), ID::from ("a") ) ?;
  if Path::new (&a_path) . exists() {
    failures . push (
      "a.skg should be deleted after merge" . to_string() ); }

  // aa.skg should exist and have the merged content.
  let aa_nodecomplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("aa"), &SourceName::from ("main") ) ?;
  if !aa_nodecomplete . extra_ids . contains (&ID::from ("a")) {
    failures . push (
      "aa should have 'a' as an extra_id" . to_string() ); }
  if !aa_nodecomplete . contains . contains (&ID::from ("x")) {
    failures . push (
      "aa should still contain x" . to_string() ); }
  if !aa_nodecomplete . contains . contains (&ID::from ("b")) {
    failures . push (
      "aa should now contain b (adopted from a)" . to_string() ); }
  if !aa_nodecomplete . contains . contains (&ID::from ("c")) {
    failures . push (
      "aa should now contain c (adopted from a)" . to_string() ); }

  // A text preserver should exist.
  // Its PID is a new UUID; find it: in aa's contains
  // but not x, b, c, or aa.
  let known_ids : Vec<&str> = vec!["x", "b", "c", "aa"];
  let preserver_candidates : Vec<&ID> =
    aa_nodecomplete . contains . iter()
    . filter ( |id| !known_ids . contains (& id . 0 . as_str()) )
    . collect();
  if preserver_candidates . len() != 1 {
    failures . push ( format!(
      "Expected 1 text preserver in aa's contains, found {}: {:?}",
      preserver_candidates . len(), preserver_candidates ) );
  } else {
    let preserver_pid : &ID = preserver_candidates[0];
    let preserver_nodecomplete =
      nodecomplete_from_pid_and_source (
        config, preserver_pid . clone(),
        &SourceName::from ("main") ) ?;
    if preserver_nodecomplete . title != "MERGED: a" {
      failures . push ( format!(
        "Text preserver title should be 'MERGED: a', got '{}'",
        preserver_nodecomplete . title )); }
    if preserver_nodecomplete . source != SourceName::from ("main") {
      failures . push ( format!(
        "Text preserver source should be 'main', got '{:?}'",
        preserver_nodecomplete . source )); }
    if !preserver_nodecomplete . contains . is_empty() {
      failures . push ( format!(
        "Text preserver should have no contents, got {:?}",
        preserver_nodecomplete . contains )); }
    if !preserver_nodecomplete . extra_ids . is_empty() {
      failures . push ( format!(
        "Text preserver should have no extra_ids, got {:?}",
        preserver_nodecomplete . extra_ids )); } }

  // aa must not contain itself in the filesystem.
  if aa_nodecomplete . contains . contains (&ID::from ("aa")) {
    failures . push (
      "aa contains itself on filesystem (self-containment after merge)"
      . to_string() ); }

  // aa must not contain itself in TypeDB.
  { let ( container_to_contents, _ ) =
      skg::dbs::typedb::search::contains_from_pids::contains_from_pids (
        &config . db_name, driver,
        &[ID::from ("aa")] ) . await ?;
    if let Some (aa_contents) = container_to_contents . get (&ID::from ("aa")) {
      if aa_contents . contains (&ID::from ("aa")) {
        failures . push (
          "aa contains itself in TypeDB (self-containment after merge)"
          . to_string() ); } } }

  let view : &str = &response . saved_view;

  // editRequest should NOT persist in the rendered buffer.
  if view . contains ("editRequest") {
    failures . push (
      "editRequest persists after merge execution"
      . to_string() ); }

  // a should NOT appear as a DeletedNode: the preprocessing pass
  // 'rewriteInPlace_viewnodes_whose_id_is_newly_extra' rewrites viewnodes whose pid
  // became an extra-id (here, a is an extra-id of aa after the
  // merge) to carry the acquirer's pid instead. The acquiree line
  // is displayed as a view of the acquirer, not as a tombstone.
  if view . contains ("(deleted") {
    failures . push (
      "acquiree 'a' was rendered as a DeletedNode; expected the \
       viewnode to have been rewritten to (id aa)"
      . to_string() ); }

  // With the preprocessing pass, the root viewnode (originally
  // pid=a) is rewritten to pid=aa and becomes the buffer's root
  // headline at '* ' level. Its content children (x, b, c, plus
  // the MERGED: a preserver) sit at '** '.
  let lines : Vec<&str> = view . lines() . collect();
  let aa_line_idx : Option<usize> =
    lines . iter() . position (
      |line| line . starts_with ("* ") && line . contains ("(id aa)") );
  if aa_line_idx . is_none() {
    failures . push (
      "aa should be the top-level headline in the rendered buffer"
      . to_string() );
  } else {
    let aa_idx : usize = aa_line_idx . unwrap();
    let mut aa_children_lines : Vec<&str> = Vec::new();
    for line in &lines[aa_idx + 1 ..] {
      if line . starts_with ("** ") {
        aa_children_lines . push (line);
      } else if line . starts_with ("* ") {
        break; } }

    let b_under_aa : Option<&&str> =
      aa_children_lines . iter()
      . find ( |line| line . contains ("(id b)") );
    let c_under_aa : Option<&&str> =
      aa_children_lines . iter()
      . find ( |line| line . contains ("(id c)") );
    if b_under_aa . is_none() {
      failures . push (
        "b does not appear as a child of aa"
        . to_string() );
    } else if b_under_aa . unwrap() . contains ("parentIs") {
      failures . push (
        "b under aa is marked parentIs=Independent"
        . to_string() ); }
    if c_under_aa . is_none() {
      failures . push (
        "c does not appear as a child of aa"
        . to_string() );
    } else if c_under_aa . unwrap() . contains ("parentIs") {
      failures . push (
        "c under aa is marked parentIs=Independent"
        . to_string() ); }

    // Text preserver should appear under aa.
    let preserver_under_aa : bool =
      aa_children_lines . iter()
      . any ( |line| line . contains ("MERGED: a") );
    if !preserver_under_aa {
      failures . push (
        "text preserver 'MERGED: a' not under aa in buffer"
        . to_string() ); } }

  if !failures . is_empty() {
    let msg : String = format!(
      "\n{} assertion(s) failed:\n  - {}",
      failures . len(),
      failures . join ("\n  - ") );
    panic!("{}", msg); }

  audit_inrustgraph_or_panic (&graph, &config . db_name, driver) . await?;
  Ok (( )) }
