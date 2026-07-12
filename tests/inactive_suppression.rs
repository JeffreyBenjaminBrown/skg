// cargo nextest run --test grouped_overrides -E 'test(inactive_suppression::)'
//
// TODO/full-schema/9-2_source-set-safety.org, inactive-node rewrite
// suppression: under a restricted source-set, any instruction that
// would modify an inactive node is dropped, not executed and not
// fatal, with the warning "Inactive nodes present in saved buffer
// remain unchanged in graph." -- and only when something was
// actually suppressed: an untouched stale node's identical-to-disk
// instruction is discarded by the noop filter first, so it saves
// silently.
//
// This lives in its own test target because the silent-untouched
// case needs the process-global in-Rust graph installed (the noop
// filter reads it), and installing it inside a shared-process
// target would couple unrelated tests.

use indoc::indoc;

use skg::from_text::buffer_to_validated_saveplan;
use skg::source_sets::{ActiveSourceSet, SourceSetName, run_with_source_set_test_db};
use skg::types::misc::{ID, members_of};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode};

use std::error::Error;

fn save_ids (instructions : &[DefineNode]) -> Vec<ID> {
  instructions . iter ()
    . filter_map ( |i| match i {
        DefineNode::Save (SaveNode (node)) => Some (node . pid . clone ()),
        _ => None } )
    . collect () }

fn saved_node_by_id<'a> (
  instructions : &'a [DefineNode],
  id           : &str,
) -> &'a NodeComplete {
  instructions . iter ()
    . find_map ( |i| match i {
        DefineNode::Save (SaveNode (node))
          if node . pid == ID::from (id) => Some (node),
        _ => None } )
    . unwrap_or_else ( || panic! ("no SaveNode for {}", id) ) }

#[test]
fn writes_to_inactive_nodes_are_suppressed_with_warning (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-inactive-suppression",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-inactive-suppression",
    |config, driver, _tantivy| Box::pin ( async move {
      skg::dbs::in_rust_graph::try_init_global_handle (
        skg::test_utils::graph_handle_from_config (config) ? );
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName ("public" . to_string ())) ?;
      { // An EDITED now-inactive definitive node: write suppressed,
        // warning attached, containment preserved.
        let buffer = indoc! {"
          * (skg (node (id root) (source public))) root
          ** (skg (node (id active-b) (source public) indef)) active-b
          ** (skg (node (id private-a) (source private))) edited private title
        "};
        let (_viewforest, plan, warnings) =
          buffer_to_validated_saveplan (
            buffer, config, driver, Some (&active) ) . await ?;
        assert! (
          ! save_ids (&plan . define_nodes)
            . contains (&ID::from ("private-a")),
          "the edit to the inactive node must be suppressed" );
        assert! (
          warnings . iter () . any ( |w| w . contains (
            "Inactive nodes present in saved buffer remain unchanged in graph")),
          "suppression must warn: {:?}", warnings );
        assert_eq! (
          members_of (&saved_node_by_id (&plan . define_nodes, "root") . contains),
          vec![ ID::from ("active-b"), ID::from ("private-a") ],
          "the active parent keeps containing the inactive child" ); }
      { // The same stale node UNTOUCHED: the noop filter drops its
        // instruction before suppression looks, so no warning.
        let buffer = indoc! {"
          * (skg (node (id root) (source public))) root
          ** (skg (node (id active-b) (source public) indef)) active-b
          ** (skg (node (id private-a) (source private))) private title must not leak
          private body must not leak
        "};
        let (_viewforest, plan, warnings) =
          buffer_to_validated_saveplan (
            buffer, config, driver, Some (&active) ) . await ?;
        assert! (
          ! save_ids (&plan . define_nodes)
            . contains (&ID::from ("private-a")),
          "an untouched stale node writes nothing" );
        assert! (
          ! warnings . iter () . any ( |w| w . contains (
            "remain unchanged in graph")),
          "an untouched stale buffer saves without the suppression \
           warning: {:?}", warnings ); }
      { // Moving a node into an inactive source: move suppressed,
        // warning attached, node unmoved.
        let buffer = indoc! {"
          * (skg (node (id root) (source public))) root
          ** (skg (node (id active-b) (source private))) active-b
        "};
        let (_viewforest, plan, warnings) =
          buffer_to_validated_saveplan (
            buffer, config, driver, Some (&active) ) . await ?;
        assert! (
          plan . source_moves . is_empty (),
          "a move into an inactive source must be suppressed" );
        assert! (
          ! save_ids (&plan . define_nodes)
            . contains (&ID::from ("active-b")),
          "the write claiming the inactive source must be suppressed" );
        assert! (
          warnings . iter () . any ( |w| w . contains (
            "remain unchanged in graph")),
          "suppression must warn: {:?}", warnings ); }
      Ok (( )) } )) }
