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
use std::path::Path;

use skg::dbs::filesystem::one_node::skgnode_from_pid_and_source;
use skg::test_utils::run_with_test_db;
use skg::serve::handlers::save_buffer::update_from_and_rerender_buffer;
use skg::serve::ConnectionState;
use skg::types::memory::SkgnodesInMemory;
use skg::types::memory::SkgNodeMap;
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
  driver  : &TypeDBDriver,
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

  let mut conn_state : ConnectionState = ConnectionState {
    diff_mode_enabled : false,
    memory            : SkgnodesInMemory::new () };
  let response = update_from_and_rerender_buffer (
    input_org_text, driver, config, tantivy, false,
    SkgNodeMap::new(),
    &Err ( String::new () ), &mut conn_state ) . await ?;

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
  let aa_skgnode =
    skgnode_from_pid_and_source (
      config, ID::from ("aa"), &SourceName::from ("main") ) ?;
  if !aa_skgnode . extra_ids . contains (&ID::from ("a")) {
    failures . push (
      "aa should have 'a' as an extra_id" . to_string() ); }
  if !aa_skgnode . contains . contains (&ID::from ("x")) {
    failures . push (
      "aa should still contain x" . to_string() ); }
  if !aa_skgnode . contains . contains (&ID::from ("b")) {
    failures . push (
      "aa should now contain b (adopted from a)" . to_string() ); }
  if !aa_skgnode . contains . contains (&ID::from ("c")) {
    failures . push (
      "aa should now contain c (adopted from a)" . to_string() ); }

  // A text preserver should exist.
  // Its PID is a new UUID; find it: in aa's contains
  // but not x, b, c, or aa.
  let known_ids : Vec<&str> = vec!["x", "b", "c", "aa"];
  let preserver_candidates : Vec<&ID> =
    aa_skgnode . contains . iter()
    . filter ( |id| !known_ids . contains (& id . 0 . as_str()) )
    . collect();
  if preserver_candidates . len() != 1 {
    failures . push ( format!(
      "Expected 1 text preserver in aa's contains, found {}: {:?}",
      preserver_candidates . len(), preserver_candidates ) );
  } else {
    let preserver_pid : &ID = preserver_candidates[0];
    let preserver_skgnode =
      skgnode_from_pid_and_source (
        config, preserver_pid . clone(),
        &SourceName::from ("main") ) ?;
    if preserver_skgnode . title != "MERGED: a" {
      failures . push ( format!(
        "Text preserver title should be 'MERGED: a', got '{}'",
        preserver_skgnode . title )); } }

  // aa must NOT contain itself.
  if aa_skgnode . contains . contains (&ID::from ("aa")) {
    failures . push (
      "KNOWN BUG: aa contains itself (self-containment after merge)"
      . to_string() ); }

  let view : &str = &response . saved_view;

  // editRequest should NOT persist in the rendered buffer.
  if view . contains ("editRequest") {
    failures . push (
      "KNOWN BUG 1: editRequest persists after merge execution"
      . to_string() ); }

  // a should appear as a DeletedNode, not a normal TrueNode.
  if !view . contains ("(deleted") {
    failures . push (
      "KNOWN BUG 3: acquiree 'a' not rendered as DeletedNode"
      . to_string() ); }

  // b and c under aa should NOT be parentIgnores.
  let lines : Vec<&str> = view . lines() . collect();
  let aa_line_idx : Option<usize> =
    lines . iter() . position (
      |line| line . contains ("(id aa)") );
  if aa_line_idx . is_none() {
    failures . push (
      "aa should be in the rendered buffer" . to_string() );
  } else {
    let aa_idx : usize = aa_line_idx . unwrap();
    // Collect aa's direct children (*** level)
    // until we hit ** or * or end.
    let mut aa_children_lines : Vec<&str> = Vec::new();
    for line in &lines[aa_idx + 1 ..] {
      if line . starts_with ("*** ") {
        aa_children_lines . push (line);
      } else if line . starts_with ("** ") || line . starts_with ("* ") {
        break; } }

    let b_under_aa : Option<&&str> =
      aa_children_lines . iter()
      . find ( |line| line . contains ("(id b)") );
    let c_under_aa : Option<&&str> =
      aa_children_lines . iter()
      . find ( |line| line . contains ("(id c)") );
    if b_under_aa . is_none() {
      failures . push (
        "KNOWN BUG 2: b does not appear as a child of aa"
        . to_string() );
    } else if b_under_aa . unwrap() . contains ("parentIgnores") {
      failures . push (
        "KNOWN BUG 2: b under aa is marked parentIgnores"
        . to_string() ); }
    if c_under_aa . is_none() {
      failures . push (
        "KNOWN BUG 2: c does not appear as a child of aa"
        . to_string() );
    } else if c_under_aa . unwrap() . contains ("parentIgnores") {
      failures . push (
        "KNOWN BUG 2: c under aa is marked parentIgnores"
        . to_string() ); }

    // Text preserver should appear under aa.
    let preserver_under_aa : bool =
      aa_children_lines . iter()
      . any ( |line| line . contains ("MERGED: a") );
    if !preserver_under_aa {
      failures . push (
        "KNOWN BUG: text preserver 'MERGED: a' not under aa in buffer"
        . to_string() ); } }

  if !failures . is_empty() {
    let msg : String = format!(
      "\n{} assertion(s) failed:\n  - {}",
      failures . len(),
      failures . join ("\n  - ") );
    panic!("{}", msg); }

  Ok (( )) }
