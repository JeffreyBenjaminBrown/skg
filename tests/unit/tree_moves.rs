use super::*;

/// A root with children 1, 2, 3; returns the tree and their ids.
fn three_children (
) -> (Tree<u32>, NodeId, [NodeId; 3]) {
  let mut tree : Tree<u32> = Tree::new (0);
  let root : NodeId = tree . root () . id ();
  let a : NodeId = tree . root_mut () . append (1) . id ();
  let b : NodeId = tree . root_mut () . append (2) . id ();
  let c : NodeId = tree . root_mut () . append (3) . id ();
  (tree, root, [a, b, c]) }

fn values (
  tree   : &Tree<u32>,
  parent : NodeId,
) -> Vec<u32> {
  tree . get (parent) . unwrap () . children ()
    . map ( |child| *child . value () ) . collect () }

#[test]
fn moving_a_node_already_in_place_changes_nothing () {
  let (mut tree, root, [a, b, c]) = three_children ();
  move_to_first_child (&mut tree, root, a) . unwrap ();
  move_to_last_child (&mut tree, root, c) . unwrap ();
  move_before (&mut tree, b, a) . unwrap ();
  move_after (&mut tree, b, c) . unwrap ();
  move_before (&mut tree, b, b) . unwrap ();
  assert_eq! ( values (&tree, root), vec! [1, 2, 3] ); }

#[test]
fn each_move_places_the_node () {
  let (mut tree, root, [a, b, c]) = three_children ();
  move_to_first_child (&mut tree, root, c) . unwrap ();
  assert_eq! ( values (&tree, root), vec! [3, 1, 2] );
  move_to_last_child (&mut tree, root, c) . unwrap ();
  assert_eq! ( values (&tree, root), vec! [1, 2, 3] );
  move_before (&mut tree, a, b) . unwrap ();
  assert_eq! ( values (&tree, root), vec! [2, 1, 3] );
  move_after (&mut tree, c, b) . unwrap ();
  assert_eq! ( values (&tree, root), vec! [1, 3, 2] ); }

#[test]
fn arranging_orders_the_ends () {
  let (mut tree, root, [a, b, c]) = three_children ();
  arrange_first_children (&mut tree, root, &[c, b]) . unwrap ();
  assert_eq! ( values (&tree, root), vec! [3, 2, 1] );
  arrange_last_children (&mut tree, root, &[c, a]) . unwrap ();
  assert_eq! ( values (&tree, root), vec! [2, 3, 1] ); }
