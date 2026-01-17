use skg::types::tree::generic::{do_everywhere_in_tree_dfs, eq_trees};
use skg::types::tree::from_org_text::tree_from_org_text;

#[test]
fn test_do_everywhere_adding_parent() {
  let initial_text = include_str!("generic/fixtures/initial.txt");
  let mut tree = tree_from_org_text(initial_text).unwrap();
  let expected_text = include_str!("generic/fixtures/after-adding-parent.txt");
  let expected_tree = tree_from_org_text(expected_text).unwrap();
  let root_id = tree.root().id();
  do_everywhere_in_tree_dfs(
    &mut tree, root_id,
    &mut |mut node| { // Have a parent? Then append its title to yours.
      let current_title = node.value().clone();
      let parent_title: Option<String> =
        node.parent().map(|mut p| p.value().clone());
      let mut new_title = current_title.clone();
      if let Some(p_title) = parent_title {
        new_title.push_str(&p_title); }
      *node.value() = new_title;
      Ok(()) }
  ).unwrap();
  assert!(eq_trees(tree.root(), expected_tree.root()));
}

#[test]
fn test_do_everywhere_adding_grandchild() {
  let initial_text = include_str!("generic/fixtures/initial.txt");
  let mut tree = tree_from_org_text(initial_text).unwrap();
  let expected_text = include_str!("generic/fixtures/after-adding-grandchild.txt");
  let expected_tree = tree_from_org_text(expected_text).unwrap();
  let root_id = tree.root().id();
  do_everywhere_in_tree_dfs(
    &mut tree, root_id,
    &mut |mut node| { // Have a grandchild? Then append your first one's title to yours.
      let current_title = node.value().clone();
      let node_id = node.id();
      let first_grandchild_title: Option<String> = {
        let mut result = None;
        let tree_ref = node.tree();
        for child in tree_ref.get(node_id).unwrap().children() {
          if let Some(grandchild) = child.children().next() {
            result = Some(grandchild.value().clone());
            break; }}
        result };
      let mut new_title = current_title.clone();
      if let Some(gc_title) = first_grandchild_title {
        new_title.push_str(&gc_title); }
      *node.value() = new_title;
      Ok(()) }
  ).unwrap();
  assert!(eq_trees(tree.root(), expected_tree.root()));
}
