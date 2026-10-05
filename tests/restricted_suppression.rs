// cargo nextest run --test grouped_overrides -E 'test(restricted_suppression::)'
//
// TODO/DONE/full-schema/DONE/9-2_source-set-safety.org, restricted-node rewrite
// suppression: under a restricted skgrepo-set, any nodeInstruction that
// would modify a restricted node is dropped, not executed and not
// fatal, with the warning "Restricted nodes present in saved buffer
// remain unchanged in graph." -- and only when something was
// actually suppressed: an untouched stale node's identical-to-disk
// nodeInstruction is discarded by the noop filter first, so it saves
// silently.
//
// This lives in its own test target because the silent-untouched
// case needs the explicit in-Rust graph installed (the noop
// filter reads it), and installing it inside a shared-process
// target would couple unrelated tests.

use indoc::indoc;

use skg::from_text::buffer_to_validated_saveplan;
use skg::skgrepo_sets::{SkgrepoRestriction, SkgRepoSetName, run_with_skgrepo_set_test_db};
use skg::types::misc::{ID, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, SaveNode};

use std::error::Error;

fn save_skgids (instructions : &[NodeInstruction]) -> Vec<ID> {
  instructions . iter ()
    . filter_map ( |i| match i {
        NodeInstruction::Save (SaveNode (node)) => Some (node . pid . clone ()),
        _ => None } )
    . collect () }

fn saved_node_by_skgid<'a> (
  instructions : &'a [NodeInstruction],
  skgid           : &str,
) -> &'a Graphnode {
  instructions . iter ()
    . find_map ( |i| match i {
        NodeInstruction::Save (SaveNode (node))
          if node . pid == ID::from (skgid) => Some (node),
        _ => None } )
    . unwrap_or_else ( || panic! ("no SaveNode for {}", skgid) ) }

#[test]
fn writes_to_restricted_nodes_are_suppressed_with_warning (
) -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-restricted-suppression",
    "tests/repo_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-restricted-suppression",
    |config, _tantivy| Box::pin ( async move {
      (
        skg::test_utils::graph_handle_from_config (config) ? );
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgRepoSetName ("public" . to_string ())) ?;
      { // An EDITED now-restricted editable node: write suppressed,
        // warning attached, containment preserved.
        let buffer = indoc! {"
          * (skg (node (id root) (repo public))) root
          ** (skg (node (id active-b) (repo public) writeProtected)) active-b
          ** (skg (node (id private-a) (repo private))) edited private title
        "};
        let (_viewforest, plan, warnings) =
          buffer_to_validated_saveplan (
            buffer, config, Some (&restriction) )  ?;
        assert! (
          ! save_skgids (&plan . node_instructions)
            . contains (&ID::from ("private-a")),
          "the edit to the restricted node must be suppressed" );
        assert! (
          warnings . iter () . any ( |w| w . contains (
            "Restricted nodes present in saved buffer remain unchanged in graph")),
          "suppression must warn: {:?}", warnings );
        assert_eq! (
          members_of (&saved_node_by_skgid (&plan . node_instructions, "root") . contains),
          vec![ ID::from ("active-b"), ID::from ("private-a") ],
          "the unrestricted parent keeps containing the restricted child" ); }
      { // The same stale node UNTOUCHED: the noop filter drops its
        // nodeInstruction before suppression looks, so no warning.
        let buffer = indoc! {"
          * (skg (node (id root) (repo public))) root
          ** (skg (node (id active-b) (repo public) writeProtected)) active-b
          ** (skg (node (id private-a) (repo private))) private title must not leak
          private body must not leak
        "};
        let (_viewforest, plan, warnings) =
          buffer_to_validated_saveplan (
            buffer, config, Some (&restriction) )  ?;
        assert! (
          ! save_skgids (&plan . node_instructions)
            . contains (&ID::from ("private-a")),
          "an untouched stale node writes nothing" );
        assert! (
          ! warnings . iter () . any ( |w| w . contains (
            "remain unchanged in graph")),
          "an untouched stale buffer saves without the suppression \
           warning: {:?}", warnings ); }
      { // Moving a node into a restricted skgrepo: move suppressed,
        // warning attached, node unmoved.
        let buffer = indoc! {"
          * (skg (node (id root) (repo public))) root
          ** (skg (node (id active-b) (repo private))) active-b
        "};
        let (_viewforest, plan, warnings) =
          buffer_to_validated_saveplan (
            buffer, config, Some (&restriction) )  ?;
        assert! (
          plan . skgrepo_moves . is_empty (),
          "a move into a restricted repo must be suppressed" );
        assert! (
          ! save_skgids (&plan . node_instructions)
            . contains (&ID::from ("active-b")),
          "the write claiming the restricted repo must be suppressed" );
        assert! (
          warnings . iter () . any ( |w| w . contains (
            "remain unchanged in graph")),
          "suppression must warn: {:?}", warnings ); }
      Ok (( )) } )) }
