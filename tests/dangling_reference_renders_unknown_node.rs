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

use skg::to_org::render::content_view::single_root_view;
use skg::test_utils::run_with_test_db;
use skg::types::misc::ID;

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
          driver, config,
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
