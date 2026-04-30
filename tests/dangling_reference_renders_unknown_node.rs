// cargo test --test dangling_reference_renders_unknown_node -- --nocapture
//
// Regression: a single dangling reference deep in a viewforest used
// to abort the whole single_root_view call with
// "Error generating document: ID '...' not found in database".
//
// Fixture: parent.skg has contains: [ghost], but no ghost.skg exists
// (the referent was deleted directly without rewriting parent's
// contains list -- the canonical way for a dangling reference to
// arise in real use).
//
// Expected: rendering the view of `parent` succeeds, with `ghost`
// represented as an UnknownNode (skg metadata `(unknownNode (id ghost))`)
// rather than killing the view.

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::to_org::render::content_view::single_root_view;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::memory::OpenViews;
use typedb_driver::TypeDBDriver;

#[test]
fn test_dangling_reference_renders_unknown_node
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-dangling-reference-renders-unknown-node",
    "tests/dangling_reference_renders_unknown_node/fixtures",
    "/tmp/tantivy-test-dangling-reference-renders-unknown-node",
    |config, driver, _tantivy| Box::pin ( async move {
      let (rendered, _pids, _) =
        single_root_view (
          driver, config, None,
          &ID ( "parent" . to_string () ),
          false ) . await ?;
      println!("Rendered:\n{}", rendered);
      // graphStats is empty because contents/containers counters
      // count only resolvable nodes (UnknownNode is a placeholder,
      // not an actual contained node), and the per-birth filter
      // hides (containers 0) for independent-birth.
      // The unknown line has no headline title -- the id already
      // appears in the metadata, no need to duplicate it.
      let expected = indoc! {"* (skg (node (id parent) (source main) (birth independent))) parent
                              ** (skg (unknownNode (id ghost)))
                              "};
      assert_eq! (rendered, expected,
                  "view should render the dangling child as an \
                   UnknownNode rather than aborting");
      Ok (( ))
    } )) }

// A view that the server rendered with an UnknownNode line in it
// must round-trip through save without tripping the local-structure
// validator. Previously, TrueNode parent + UnknownNode child
// triggered LocalStructureViolation.
#[test]
fn test_buffer_with_unknownnode_child_saves_cleanly
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-unknownnode-save-roundtrip",
    "tests/dangling_reference_renders_unknown_node/fixtures-save-roundtrip",
    "/tmp/tantivy-test-unknownnode-save-roundtrip",
    |config, driver, tantivy| Box::pin ( async move {
      buffer_with_unknownnode_child_saves_cleanly_impl (
        config, driver, tantivy ) . await
    } )) }

async fn buffer_with_unknownnode_child_saves_cleanly_impl (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg (node (id parent) (source main))) parent
    ** (skg (unknownNode (id ghost)))
  "};
  let graph : skg::dbs::memory::InRustGraphHandle =
    skg::dbs::memory::new_handle (skg::dbs::memory::InRustGraph::new ());
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
  if ! response . errors . is_empty () {
    panic! ("save returned errors instead of completing: {:?}",
            response . errors); }
  Ok (( )) }
