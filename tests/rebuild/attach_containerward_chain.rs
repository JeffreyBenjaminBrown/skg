// cargo nextest run --test grouped_saves -E 'test(attach_containerward_chain::)'

use indoc::indoc;
use skg::to_org::extend::containerward_chain::{
  ContainerwardChainPartners, attach_containerward_chains_under_members};
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores};
use skg::types::misc::SkgConfig;
use skg::types::viewnode::Viewnode;

use skg::org_to_text::viewforest_to_string;
use skg::assert_metadata_eq;

use ego_tree::{NodeId, Tree};
use std::error::Error;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-rebuild-attach-containerward-chain",
    |s| Box::pin ( async move {
      s . reset ("a_chain_branches_and_shows_a_repeat_as_a_leaf",
                 "tests/rebuild/fixtures-fork-extension") ?;
      a_chain_branches_and_shows_a_repeat_as_a_leaf (&s . config) ?;
      Ok (( )) } )) }

/// a11's containers are a1 and a2; a1's are a and a1 itself, a2's are
/// a and b. The chain under a11 shows every container, in ID order,
/// each node the walk met before as a leaf: a1 under a1, and the
/// second a.
fn a_chain_branches_and_shows_a_repeat_as_a_leaf (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let input : &str = indoc! {"
    * (skg (node (id a11) (repo main))) a11
  "};
  let mut viewforest : Tree<Viewnode> =
    maybePlaced_to_placed_tree ( org_to_uninterpreted_nodes (input) ? . 0 ) ?;
  let a11 : NodeId =
    viewforest . root () . first_child () . unwrap () . id ();
  attach_containerward_chains_under_members (
    &mut viewforest, &[a11],
    &graph_handle_from_config (config) ? . load_full (), config, None,
    ContainerwardChainPartners::LiveGraph ) ?;
  let expected : &str = indoc! {"
    * (skg (node (id a11) (repo main))) a11
    ** (skg (node (id a1) (repo main) nonMember writeProtected)) a1
    *** (skg (node (id a) (repo main) nonMember writeProtected)) a
    *** (skg (node (id a1) (repo main) nonMember writeProtected)) a1
    ** (skg (node (id a2) (repo main) nonMember writeProtected)) a2
    *** (skg (node (id a) (repo main) nonMember writeProtected)) a
    *** (skg (node (id b) (repo main) nonMember writeProtected)) b
  "};
  let expected_trees : Tree<Viewnode> =
    maybePlaced_to_placed_tree ( org_to_uninterpreted_nodes (expected) ? . 0 ) ?;
  assert_metadata_eq! (
    viewforest_to_string (&viewforest, config) ?,
    viewforest_to_string (&expected_trees, config) ?,
    "the chain under a11" );
  Ok (( )) }
