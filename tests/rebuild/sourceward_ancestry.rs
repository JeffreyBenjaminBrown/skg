// cargo test --test rebuild -- sourceward_ancestry
//
// Tests that sourceward view expansion inserts containerward
// ancestry beneath each ParentIs::LinkTarget source node.
//
// Graph (see fixtures-sourceward-ancestry/):
//   Links:        b -> a,  c -> b,  d -> a
//     PITFALL: All IDs are single-character,
//     but the titles for b, c and d must be longer than that,
//     to accommodate the link.
//     (I try to make ID=title in most test nodes,
//     but that's not possible when the link includes a title.)
//   Containment:  bb > b,  ccc > cc,  cc > c,  d > cc
//
// After sourceward expansion from "a", the expected tree is:
//   a
//   +-- b  (LinkTarget)         — links to a
//   |   +-- bb (Content) — b's container
//   |   +-- c  (LinkTarget)     — links to b (sourceward chain)
//   |       +-- cc (Content)  — c's container
//   |           +-- ccc (Content) — cc's container
//   |           +-- d   (Content) — cc's other container
//   +-- d  (LinkTarget)         — links to a
//
// Sibling order is nondeterministic (containers come from
// a HashSet), so assertions check parent-child relationships
// and child counts without depending on order.

use indoc::indoc;
use skg::to_org::expand::backpath::build_and_integrate_sourceward_path;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::test_utils::run_with_test_db;
use skg::types::misc::SkgConfig;
use skg::types::viewnode::{ViewNode, ViewNodeKind, ParentIs};


use ego_tree::{NodeId, Tree};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Collect (pid, parentIs) pairs for all TrueNode children of a node.
fn children_info (
  tree : &Tree<ViewNode>,
  node_id : NodeId,
) -> Vec<(String, ParentIs)> {
  tree . get (node_id) . unwrap () . children ()
    . filter_map ( |child| {
      if let ViewNodeKind::True (t) = &child . value () . kind {
        Some (( t.id.0 . clone (), t.parentIs ))
      } else { None } } )
    . collect () }

/// Find a child TrueNode by pid, returning its NodeId.
fn find_child (
  tree : &Tree<ViewNode>,
  parent : NodeId,
  pid : &str,
) -> Option<NodeId> {
  tree . get (parent) . unwrap () . children ()
    . find ( |child| {
      if let ViewNodeKind::True (t) = &child . value () . kind {
        t.id.0 == pid
      } else { false } } )
    . map ( |n| n . id () ) }

#[test]
fn test_sourceward_ancestry (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-sourceward-ancestry",
    "tests/rebuild/fixtures-sourceward-ancestry",
    "/tmp/tantivy-test-sourceward-ancestry",
    |config, driver, _tantivy| Box::pin ( async move {
      test_sourceward_ancestry_impl (config, driver) . await
    } )) }

async fn test_sourceward_ancestry_impl (
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Start with a minimal tree containing just node "a".
  let input : &str = indoc! {"
    * (skg (node (id a) (source main))) a
  "};
  let unchecked_viewforest =
    org_to_uninterpreted_nodes (input) ? . 0;
  let mut viewforest : Tree<ViewNode> =
    maybePlaced_to_placed_tree (unchecked_viewforest) ?;
  let node_a : NodeId =
    viewforest . root () . first_child () . unwrap () . id ();

  // Request sourceward expansion from "a".
  build_and_integrate_sourceward_path (
    &mut viewforest, node_a, config, driver
  ) . await ?;

  // --- a should have exactly 2 LinkTarget children: b and d ---
  let a_children = children_info (&viewforest, node_a);
  assert! ( a_children . contains (&("b" . into (),
                                     ParentIs::LinkTarget)),
            "a should have LinkTarget child b" );
  assert! ( a_children . contains (&("d" . into (),
                                     ParentIs::LinkTarget)),
            "a should have LinkTarget child d" );
  assert_eq! ( a_children . len (), 2,
               "a should have exactly 2 children" );

  // --- b should have 2 children: bb (Content) and c (LinkTarget) ---
  let node_b = find_child (&viewforest, node_a, "b")
    . expect ("a should have child b");
  let b_children = children_info (&viewforest, node_b);
  assert! ( b_children . contains (&("bb" . into (),
                                     ParentIs::Content)),
            "b should have Content child bb" );
  assert! ( b_children . contains (&("c" . into (),
                                     ParentIs::LinkTarget)),
            "b should have LinkTarget child c" );
  assert_eq! ( b_children . len (), 2,
               "b should have exactly 2 children" );

  // --- bb should have no children ---
  let node_bb = find_child (&viewforest, node_b, "bb")
    . expect ("b should have child bb");
  assert_eq! ( children_info (&viewforest, node_bb) . len (), 0,
               "bb should have no children" );

  // --- c should have 1 child: cc (Content) ---
  let node_c = find_child (&viewforest, node_b, "c")
    . expect ("b should have child c");
  let c_children = children_info (&viewforest, node_c);
  assert! ( c_children . contains (&("cc" . into (),
                                 ParentIs::Content)),
            "c should have Content child cc" );
  assert_eq! ( c_children . len (), 1,
               "c should have exactly 1 child" );

  // --- cc should have 2 children: ccc and d (both Content) ---
  let node_cc = find_child (&viewforest, node_c, "cc")
    . expect ("c should have child cc");
  let cc_children = children_info (&viewforest, node_cc);
  assert! ( cc_children . contains (&("ccc" . into (),
                                      ParentIs::Content)),
            "cc should have child ccc (Content)" );
  assert! ( cc_children . contains (&("d" . into (),
                                      ParentIs::Content)),
            "cc should have child d (Content)" );
  assert_eq! ( cc_children . len (), 2,
               "cc should have exactly 2 children" );

  // --- ccc and d (under cc) should have no children ---
  let node_ccc = find_child (&viewforest, node_cc, "ccc")
    . expect ("cc should have child ccc");
  assert_eq! ( children_info (&viewforest, node_ccc) . len (), 0,
               "ccc should have no children" );
  let node_d_ancestry = find_child (&viewforest, node_cc, "d")
    . expect ("cc should have child d");
  assert_eq! ( children_info (&viewforest, node_d_ancestry) . len (), 0,
               "d (ancestry) should have no children" );

  // --- d (LinkTarget, under a) should have no children ---
  let node_d = find_child (&viewforest, node_a, "d")
    . expect ("a should have child d");
  assert_eq! ( children_info (&viewforest, node_d) . len (), 0,
               "d (LinkTarget) should have no children" );

  Ok (( )) }
