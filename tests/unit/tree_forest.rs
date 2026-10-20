use super::*;

/// Two roots, each with children, one child with a child of its own.
#[test]
fn nodes_yields_every_viewnode_of_a_forest_exactly_once () {
  let mut forest : Forest<u32> =
    Forest::from_internal_tree ( Tree::new (0) );
  let first_root : NodeId = forest . append_root (1);
  let second_root : NodeId = forest . append_root (2);
  { let mut first_root_mut : NodeMut<u32> =
      forest . get_mut (first_root) . unwrap ();
    first_root_mut . append (11) . append (111);
    first_root_mut . append (12); }
  forest . get_mut (second_root) . unwrap () . append (21);
  let mut yielded : Vec<u32> =
    forest . nodes () . map ( |node| *node . value () ) . collect ();
  yielded . sort ();
  assert_eq! ( yielded, vec! [1, 2, 11, 12, 21, 111] ); }
