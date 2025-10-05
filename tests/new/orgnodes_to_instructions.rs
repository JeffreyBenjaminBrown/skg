// TODO ? These tests are AI-generated,
// and no human has verified (most of) them.
// (The library code looks bulletproof to me,
// so the quality of these tests feels low-stakes.)

use indoc::indoc;
use skg::new::{org_to_uninterpreted_nodes2, orgnodes_to_save_instructions};
use skg::types::{OrgNode2, ID, SkgNode, NodeSaveAction};
use ego_tree::Tree;

#[test]
fn test_orgnodes_to_save_instructions_basic() {
  let input: &str =
    indoc! {"
            * <skg<id:root1>> root node 1
            Root body content
            ** <skg<id:child1>> child 1
            Child body
            * <skg<id:root2,mightContainMore,toDelete>> root node 2
            Root 2 body
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  assert_eq!(instructions.len(), 3, "Should have 3 instructions");

  // Test root1
  let (root1_skg, root1_action) = &instructions[0];
  assert_eq!(root1_skg.title, "root node 1");
  assert_eq!(root1_skg.body, Some("Root body content".to_string()));
  assert_eq!(root1_skg.ids, vec![ID::from("root1")]);
  assert_eq!(root1_skg.contains, vec![ID::from("child1")]);
  assert_eq!(root1_action.mightContainMore, false);
  assert_eq!(root1_action.toDelete, false);

  // Test child1
  let (child1_skg, child1_action) = &instructions[1];
  assert_eq!(child1_skg.title, "child 1");
  assert_eq!(child1_skg.body, Some("Child body".to_string()));
  assert_eq!(child1_skg.ids, vec![ID::from("child1")]);
  assert_eq!(child1_skg.contains, vec![]); // No children
  assert_eq!(child1_action.mightContainMore, false);
  assert_eq!(child1_action.toDelete, false);

  // Test root2 with metadata flags
  let (root2_skg, root2_action) = &instructions[2];
  assert_eq!(root2_skg.title, "root node 2");
  assert_eq!(root2_skg.body, Some("Root 2 body".to_string()));
  assert_eq!(root2_skg.ids, vec![ID::from("root2")]);
  assert_eq!(root2_action.mightContainMore, true);
  assert_eq!(root2_action.toDelete, true);
}

#[test]
fn test_orgnodes_to_save_instructions_with_aliases() {
  let input: &str =
    indoc! {"
            * <skg<id:main>> main node
            Main body
            ** <skg<id:alias_col,relToOrgParent:aliasCol>> aliases
            *** <skg<id:alias1,relToOrgParent:alias>> first alias
            *** <skg<id:alias2,relToOrgParent:alias>> second alias
            ** <skg<id:content_child>> content child
            Content body
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  // Should have 2 instructions: main node and content_child
  // AliasCol and Alias nodes should not appear in output
  assert_eq!(instructions.len(), 2, "Should have 2 instructions");

  // Test main node
  let (main_skg, _) = &instructions[0];
  assert_eq!(main_skg.title, "main node");
  assert_eq!(main_skg.ids, vec![ID::from("main")]);
  assert_eq!(main_skg.contains, vec![ID::from("content_child")]);

  // Test aliases collection
  assert_eq!(main_skg.aliases, Some(vec!["first alias".to_string(), "second alias".to_string()]));

  // Test content child
  let (content_skg, _) = &instructions[1];
  assert_eq!(content_skg.title, "content child");
  assert_eq!(content_skg.ids, vec![ID::from("content_child")]);
  assert_eq!(content_skg.aliases, None); // No aliases
}

#[test]
fn test_orgnodes_to_save_instructions_no_aliases() {
  let input: &str =
    indoc! {"
            * <skg<id:node1>> node without aliases
            Body content
            ** <skg<id:child1>> regular child
            Child body
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  assert_eq!(instructions.len(), 2);

  let (node1_skg, _) = &instructions[0];
  assert_eq!(node1_skg.aliases, None, "Should have no aliases");
  assert_eq!(node1_skg.contains, vec![ID::from("child1")]);
}

#[test]
fn test_orgnodes_to_save_instructions_multiple_alias_cols() {
  let input: &str =
    indoc! {"
            * <skg<id:main>> main node
            ** <skg<id:alias_col1,relToOrgParent:aliasCol>> first alias collection
            *** <skg<id:alias1,relToOrgParent:alias>> alias one
            *** <skg<id:alias2,relToOrgParent:alias>> alias two
            ** <skg<id:alias_col2,relToOrgParent:aliasCol>> second alias collection
            *** <skg<id:alias3,relToOrgParent:alias>> alias three
            ** <skg<id:content1>> content node
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  assert_eq!(instructions.len(), 2); // main and content1

  let (main_skg, _) = &instructions[0];
  assert_eq!(main_skg.aliases, Some(vec!["alias one".to_string(), "alias two".to_string(), "alias three".to_string()]));
  assert_eq!(main_skg.contains, vec![ID::from("content1")]);
}

#[test]
fn test_orgnodes_to_save_instructions_mixed_relations() {
  let input: &str =
    indoc! {"
            * <skg<id:root>> root node
            ** <skg<id:container,relToOrgParent:container>> container child
            ** <skg<id:content1>> content child 1
            ** <skg<id:alias_col,relToOrgParent:aliasCol>> aliases
            *** <skg<id:alias1,relToOrgParent:alias>> my alias
            ** <skg<id:content2>> content child 2
            ** <skg<id:none_rel,relToOrgParent:none>> none relation child
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  // Should have instructions for: root, container, content1, content2, none_rel
  // AliasCol and Alias should be skipped
  assert_eq!(instructions.len(), 5);

  let (root_skg, _) = &instructions[0];
  assert_eq!(root_skg.title, "root node");
  assert_eq!(root_skg.aliases, Some(vec!["my alias".to_string()]));
  assert_eq!(root_skg.contains, vec![ID::from("content1"), ID::from("content2")]); // Only Content relations
}

#[test]
fn test_orgnodes_to_save_instructions_deep_nesting() {
  let input: &str =
    indoc! {"
            * <skg<id:level1>> level 1
            ** <skg<id:level2a>> level 2a
            *** <skg<id:level3a>> level 3a
            **** <skg<id:level4>> level 4
            ** <skg<id:level2b>> level 2b
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  assert_eq!(instructions.len(), 5);

  // Check contains relationships
  let (level1_skg, _) = &instructions[0];
  assert_eq!(level1_skg.contains, vec![ID::from("level2a"), ID::from("level2b")]);

  let (level2a_skg, _) = &instructions[1];
  assert_eq!(level2a_skg.contains, vec![ID::from("level3a")]);

  let (level3a_skg, _) = &instructions[2];
  assert_eq!(level3a_skg.contains, vec![ID::from("level4")]);

  let (level4_skg, _) = &instructions[3];
  assert_eq!(level4_skg.contains, vec![]); // Leaf node

  let (level2b_skg, _) = &instructions[4];
  assert_eq!(level2b_skg.contains, vec![]); // Leaf node
}

#[test]
fn test_orgnodes_to_save_instructions_error_missing_id() {
  let input: &str =
    indoc! {"
            * <skg<id:good_node>> good node
            * node without ID
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let result = orgnodes_to_save_instructions(trees);

  assert!(result.is_err(), "Should return error for missing ID");
  let error_msg = result.unwrap_err();
  assert!(error_msg.contains("node without ID"));
  assert!(error_msg.contains("has no ID"));
}

#[test]
fn test_orgnodes_to_save_instructions_empty_input() {
  let trees: Vec<Tree<OrgNode2>> = vec![];
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  assert_eq!(instructions.len(), 0, "Empty input should produce empty output");
}

#[test]
fn test_orgnodes_to_save_instructions_only_aliases() {
  let input: &str =
    indoc! {"
            * <skg<id:main>> main node
            ** <skg<id:alias_col,relToOrgParent:aliasCol>> aliases only
            *** <skg<id:alias1,relToOrgParent:alias>> alias one
            *** <skg<id:alias2,relToOrgParent:alias>> alias two
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  assert_eq!(instructions.len(), 1); // Only main node

  let (main_skg, _) = &instructions[0];
  assert_eq!(main_skg.aliases, Some(vec!["alias one".to_string(), "alias two".to_string()]));
  assert_eq!(main_skg.contains, vec![]); // No content children
}

#[test]
fn test_orgnodes_to_save_instructions_complex_scenario() {
  let input: &str =
    indoc! {"
            * <skg<id:doc1,mightContainMore>> Document 1
            Document body
            ** <skg<id:aliases1,relToOrgParent:aliasCol>> Doc1 Aliases
            *** <skg<id:alias_a,relToOrgParent:alias>> First Document
            *** <skg<id:alias_b,relToOrgParent:alias>> Primary Doc
            ** <skg<id:section1>> Section 1
            Section 1 body
            *** <skg<id:subsection1a>> Subsection 1a
            ** <skg<id:section2,toDelete>> Section 2
            ** <skg<id:section3>> Section 3
            * <skg<id:doc2>> Document 2
            ** <skg<id:ref_section,relToOrgParent:container>> Reference Section
        "};

  let trees: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input).unwrap();
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    orgnodes_to_save_instructions(trees).unwrap();

  assert_eq!(instructions.len(), 7); // doc1, section1, subsection1a, section2, section3, doc2, ref_section

  // Test doc1
  let (doc1_skg, doc1_action) = &instructions[0];
  assert_eq!(doc1_skg.title, "Document 1");
  assert_eq!(doc1_skg.aliases, Some(vec!["First Document".to_string(), "Primary Doc".to_string()]));
  assert_eq!(doc1_skg.contains,
             vec![ID::from("section1"), ID::from("section3")]);
  assert_eq!(doc1_action.mightContainMore, true);
  assert_eq!(doc1_action.toDelete, false);

  // Test section2 with toDelete
  let (section2_skg, section2_action) = &instructions[3];
  assert_eq!(section2_skg.title, "Section 2");
  assert_eq!(section2_action.toDelete, true);
  assert_eq!(section2_action.mightContainMore, false);

  // Test that subsection1a is child of section1
  let (section1_skg, _) = &instructions[1];
  assert_eq!(section1_skg.contains, vec![ID::from("subsection1a")]);
}
