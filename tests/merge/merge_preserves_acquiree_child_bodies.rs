// cargo test --test merge merge_preserves_acquiree_child_bodies -- --nocapture

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::dbs::filesystem::one_node::nodecomplete_from_pid_and_source;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::test_utils::{
  audit_inrustgraph_or_panic, graph_handle_from_config, run_with_test_db,
};
use skg::test_utils::update_from_and_rerender_buffer_test
  as update_from_and_rerender_buffer;
use skg::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use skg::types::views_state::OpenViews;

use typedb_driver::TypeDBDriver;

#[test]
fn test_merge_preserves_acquiree_child_bodies
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-merge-preserves-acquiree-child-bodies",
    "tests/merge/merge_preserves_acquiree_child_bodies/fixtures",
    "/tmp/tantivy-test-merge-preserves-acquiree-child-bodies",
    |config, driver, tantivy| Box::pin ( async move {
      merge_preserves_acquiree_child_bodies_impl (
        config, driver, tantivy ) . await
    } )) }

async fn merge_preserves_acquiree_child_bodies_impl (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg (node (id a) (source main) (editRequest (merge b)))) a
    a text
    ** (skg (node (id a1) (source main))) a1
    a1 text
    ** (skg (node (id a2) (source main))) a2
    a2 text
    * (skg (node (id b) (source main))) b
    b text
    ** (skg (node (id b1) (source main))) b1
    b1 text
    ** (skg (node (id b2) (source main))) b2
    b2 text
  "};

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
    input_org_text, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer after merge:\n{}", response . saved_view);
  if !response . errors . is_empty() {
    println!("Errors: {:?}", response . errors); }

  let mut failures : Vec<String> = Vec::new();
  let b1 : skg::types::nodes::complete::NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("b1"), &SourceName::from ("main") ) ?;
  let b2 : skg::types::nodes::complete::NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("b2"), &SourceName::from ("main") ) ?;
  if b1 . body . as_deref () != Some ("b1 text") {
    failures . push (format!(
      "b1 body on disk should be 'b1 text', got {:?}",
      b1 . body)); }
  if b2 . body . as_deref () != Some ("b2 text") {
    failures . push (format!(
      "b2 body on disk should be 'b2 text', got {:?}",
      b2 . body)); }
  if !response . saved_view . contains ("b1 text") {
    failures . push (
      "rerendered buffer does not contain b1 body text"
      . to_string ()); }
  if !response . saved_view . contains ("b2 text") {
    failures . push (
      "rerendered buffer does not contain b2 body text"
      . to_string ()); }

  if !failures . is_empty() {
    let msg : String = format!(
      "\n{} assertion(s) failed:\n  - {}",
      failures . len(),
      failures . join ("\n  - ") );
    panic!("{}", msg); }

  audit_inrustgraph_or_panic (&graph, &config . db_name, driver) . await?;
  Ok (( )) }
