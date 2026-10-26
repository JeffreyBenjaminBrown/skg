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

/// Walks the children of 'parent' forward and backward, and the whole
/// tree from its root, each walk stopping after more steps than the
/// tree has nodes; a node linked to itself would exceed that.
fn assert_every_traversal_is_finite (
  tree   : &Tree<u32>,
  parent : NodeId,
) {
  let bound : usize = tree . values () . count () + 1;
  let parent_ref : NodeRef<u32> = tree . get (parent) . unwrap ();
  assert! ( parent_ref . children () . take (bound) . count () < bound );
  assert! ( parent_ref . last_child () . unwrap ()
            . prev_siblings () . take (bound) . count () < bound );
  assert! ( tree . root () . descendants () . take (bound) . count ()
            < bound ); }

#[test]
fn moving_a_node_already_in_place_changes_nothing () {
  let (mut tree, root, [a, b, c]) = three_children ();
  move_to_first_child (&mut tree, root, a) . unwrap ();
  move_to_last_child (&mut tree, root, c) . unwrap ();
  move_before (&mut tree, b, a) . unwrap ();
  move_after (&mut tree, b, c) . unwrap ();
  assert_every_traversal_is_finite (&tree, root);
  assert_eq! ( values (&tree, root), vec! [1, 2, 3] ); }

#[test]
fn moving_a_node_next_to_itself_changes_nothing () {
  let (mut tree, root, [a, b, c]) = three_children ();
  move_before (&mut tree, b, b) . unwrap ();
  move_after (&mut tree, b, b) . unwrap ();
  move_before (&mut tree, a, a) . unwrap ();
  move_after (&mut tree, c, c) . unwrap ();
  assert_every_traversal_is_finite (&tree, root);
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

/// Created children go after their predecessors in the model, or
/// first; the kept ones stay where they stand, whatever the model's
/// order.
#[test]
fn placing_created_children_moves_no_kept_one () {
  let (mut tree, root, [a, _b, c]) = three_children ();
  let d : NodeId = tree . root_mut () . append (4) . id ();
  let e : NodeId = tree . root_mut () . append (5) . id ();
  // The model orders them e, c, a, d; b is not the refresh's.
  place_created_children_after_their_model_predecessors (
    &mut tree, root, &[e, c, a, d], &HashSet::from ([d, e]) ) . unwrap ();
  assert_eq! ( values (&tree, root), vec! [5, 1, 4, 2, 3] );
  assert_every_traversal_is_finite (&tree, root); }
