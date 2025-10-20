// cargo test uninterpreted2

use indoc::indoc;
use skg::save::org_to_uninterpreted_nodes;
use skg::types::{ID, OrgNode, RelToParent};
use ego_tree::Tree;

#[test]
fn test_org_to_uninterpreted_nodes2() {
  let input: &str =
    indoc! {"
            Ignored text.
            Ignored text.
            * a
            ** aa
               body of aa
            ** ab
            * b
              body of b
            ** ba
            *** baa
                body of baa
            ** bb
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input).unwrap();

  assert_eq!(trees.len(), 2, "Should have exactly 2 trees");

  // Test first tree (node 'a' and its children)
  let tree_a = &trees[0];
  let node_a = tree_a.root().value();
  assert_eq!(node_a.title, "a");
  assert_eq!(node_a.body, None);

  // Test node 'a' has children in the tree
  let node_a_children: Vec<_> = tree_a.root().children().collect();
  assert_eq!(node_a_children.len(), 2, "Node 'a' should have 2 children");
  assert_eq!(node_a_children[0].value().title, "aa");
  assert_eq!(node_a_children[0].value().body, Some("   body of aa".to_string()));
  assert_eq!(node_a_children[1].value().title, "ab");

  // Test second tree (node 'b' and its children)
  let tree_b = &trees[1];
  let node_b = tree_b.root().value();
  assert_eq!(node_b.title, "b");
  assert_eq!(node_b.body, Some("  body of b".to_string()));

  // Test node 'b' has children in the tree
  let node_b_children: Vec<_> = tree_b.root().children().collect();
  assert_eq!(node_b_children.len(), 2, "Node 'b' should have 2 children");
  assert_eq!(node_b_children[0].value().title, "ba");
  assert_eq!(node_b_children[1].value().title, "bb");

  // Test deeper nesting - node 'ba' should have child 'baa'
  let node_ba_children: Vec<_> = node_b_children[0].children().collect();
  assert_eq!(node_ba_children.len(), 1, "Node 'ba' should have 1 child");
  assert_eq!(node_ba_children[0].value().title, "baa");
  assert_eq!(node_ba_children[0].value().body, Some("    body of baa".to_string()));
}

#[test]
fn test_org_to_uninterpreted_nodes2_with_metadata() {
  let input: &str =
    indoc! {"
            * (skg (id root) (view focused)) root
            Root body content
            ** (skg (id child1) (view folded)) child1
            Child1 body
            * (skg (code (relToParent parentIgnores) indefinitive)) parentIgnores node
            ParentIgnores body
            * (skg (view cycle) (code repeated)) cycling node
            This node has cycle and repeated flags
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input).unwrap();

  assert_eq!(trees.len(), 3, "Should have exactly 3 trees");

  // Test root node with metadata
  let root_node = trees[0].root().value();
  assert_eq!(root_node.title, "root");
  assert_eq!(root_node.metadata.id, Some(ID::from("root")));
  assert_eq!(root_node.metadata.viewData.focused, true);
  assert_eq!(root_node.metadata.viewData.folded, false);
  assert_eq!(root_node.body, Some("Root body content".to_string()));

  // Test parentIgnores node
  let parentIgnores_node = trees[1].root().value();
  assert_eq!(parentIgnores_node.title, "parentIgnores node");
  assert_eq!(parentIgnores_node.metadata.code.relToParent, RelToParent::ParentIgnores);
  assert_eq!(parentIgnores_node.metadata.code.indefinitive, true);
  assert_eq!(parentIgnores_node.body, Some("ParentIgnores body".to_string()));

  // Test cycling node
  let cycle_node = trees[2].root().value();
  assert_eq!(cycle_node.title, "cycling node");
  assert_eq!(cycle_node.metadata.viewData.cycle, true);
  assert_eq!(cycle_node.metadata.code.repeat, true);
  assert_eq!(cycle_node.body, Some("This node has cycle and repeated flags".to_string()));
}

#[test]
fn test_org_to_uninterpreted_nodes2_default_values() {
  let input: &str =
    indoc! {"
            * simple node
            Simple body
            * another node
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input).unwrap();

  assert_eq!(trees.len(), 2);

  // Test first node - should have all default values except title and body
  let first_node = trees[0].root().value();
  assert_eq!(first_node.title, "simple node");
  assert_eq!(first_node.body, Some("Simple body".to_string()));
  assert_eq!(first_node.metadata.id, None);
  assert_eq!(first_node.metadata.code.relToParent, RelToParent::Content);
  assert_eq!(first_node.metadata.viewData.cycle, false);
  assert_eq!(first_node.metadata.viewData.focused, false);
  assert_eq!(first_node.metadata.viewData.folded, false);
  assert_eq!(first_node.metadata.code.indefinitive, false);
  assert_eq!(first_node.metadata.code.repeat, false);
  assert_eq!(first_node.metadata.code.toDelete, false);

  // Test second node - should have no body
  let second_node = trees[1].root().value();
  assert_eq!(second_node.title, "another node");
  assert_eq!(second_node.body, None);
}

#[test]
fn test_org_to_uninterpreted_nodes2_body_spacing() {
  let input: &str =
    indoc! {"
            * node with spaced body
              body with 2 spaces
            ** child
               child body with 3 spaces
            * node with complex body
            line 1
              line 2 with 2 spaces
                line 3 with 4 spaces
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input).unwrap();

  assert_eq!(trees.len(), 2);

  // Test first node body spacing preservation
  let first_node = trees[0].root().value();
  assert_eq!(first_node.title, "node with spaced body");
  assert_eq!(first_node.body, Some("  body with 2 spaces".to_string()));

  // Test second node complex body
  let second_node = trees[1].root().value();
  assert_eq!(second_node.title, "node with complex body");
  let expected_body = "line 1\n  line 2 with 2 spaces\n    line 3 with 4 spaces";
  assert_eq!(second_node.body, Some(expected_body.to_string()));
}

#[test]
fn test_org_to_uninterpreted_nodes2_basic_metadata() {
  let input: &str =
    indoc! {"
            * (skg (id test) (view folded)) simple node with metadata
            Node body
            * regular node without metadata
            Regular body
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input).unwrap();

  assert_eq!(trees.len(), 2);

  // Test node with metadata
  let meta_node = trees[0].root().value();
  assert_eq!(meta_node.title, "simple node with metadata");
  assert_eq!(meta_node.metadata.id, Some(ID::from("test")));
  assert_eq!(meta_node.metadata.viewData.folded, true);
  assert_eq!(meta_node.body, Some("Node body".to_string()));

  // Test node without metadata (should have defaults)
  let regular_node = trees[1].root().value();
  assert_eq!(regular_node.title, "regular node without metadata");
  assert_eq!(regular_node.metadata.id, None);
  assert_eq!(regular_node.metadata.viewData.folded, false);
  assert_eq!(regular_node.body, Some("Regular body".to_string()));
}

#[test]
fn test_org_to_uninterpreted_nodes2_empty_input() {
  let input = "";
  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input).unwrap();
  assert_eq!(trees.len(), 0);

  let input2 = "   \n  \n  ";
  let trees2: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input2).unwrap();
  assert_eq!(trees2.len(), 0);
}

#[test]
fn test_org_to_uninterpreted_nodes2_only_text() {
  let input: &str =
    indoc! {"
            This is just text
            with no headlines
            at all
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input).unwrap();
  assert_eq!(trees.len(), 0,
             "Should have no trees when there are no headlines");
}

#[test]
fn test_org_to_uninterpreted_nodes2_invalid_metadata() {
  let _input: &str =
    indoc! {"
            * (skg invalidKey:value) invalid key
            * (skg (code (relToParent invalidValue)) invalid value
            * (skg unknownFlag) unknown flag
        "};

  // Test invalid key
  let input_invalid_key = "* (skg (invalidKey value)) invalid key";
  let result = org_to_uninterpreted_nodes(input_invalid_key);
  assert!(result.is_err());
  assert!(result.unwrap_err().contains("Unknown metadata key: invalidKey"));

  // Test invalid relToParent value
  let input_invalid_value = "* (skg (code (relToParent invalidValue))) invalid value";
  let result = org_to_uninterpreted_nodes(input_invalid_value);
  assert!(result.is_err());
  assert!(result.unwrap_err().contains("Unknown relToParent value: invalidValue"));

  // Test unknown flag
  let input_unknown_flag = "* (skg unknownFlag) unknown flag";
  let result = org_to_uninterpreted_nodes(input_unknown_flag);
  assert!(result.is_err());
  assert!(result.unwrap_err().contains("Unexpected element in metadata"));
}

#[test]
fn test_org_to_uninterpreted_nodes2_orphaned_nodes() {
  // Test that nested nodes without a level 1 parent cause an error
  let input_orphaned: &str =
    indoc! {"
            ** orphaned level 2 node
            This should cause an error
        "};

  let result = org_to_uninterpreted_nodes(input_orphaned);
  assert!(result.is_err());
  let error_msg = result.unwrap_err();
  assert!(error_msg.contains("jumps too far between levels"));
  assert!(error_msg.contains("level 2"));

  // Test that level 3 without proper parents also causes an error
  let input_level3_orphaned: &str =
    indoc! {"
            *** deeply orphaned level 3 node
        "};

  let result = org_to_uninterpreted_nodes(input_level3_orphaned);
  assert!(result.is_err());
  let error_msg = result.unwrap_err();
  assert!(error_msg.contains("jumps too far between levels"));
  assert!(error_msg.contains("level 3"));
}
