use indoc::indoc;
use skg::new::{org_to_uninterpreted_nodes2, add_missing_info_to_trees};
use skg::types::OrgNode2;
use ego_tree::Tree;

// Import test utilities
use skg::test_utils::compare_trees_modulo_id;

#[test]
fn test_add_missing_info_comprehensive() {
  // Applying 'add_missing_info_to_trees' should make
  // 'with_missing_info' equivalent to 'without_missing_info',
  // modulo the specific ID values added.
  let with_missing_info: &str =
    indoc! {"
            * <skg<id:root>> root
            ** <skg<relToOrgParent:aliasCol>> aliases
            *** new alias
            *** <skg<relToOrgParent:alias>> preexisting alias
            ** no id
            *** also no id
        "};
  let without_missing_info: &str =
    indoc! {"
            * <skg<id:root>> root
            ** <skg<relToOrgParent:aliasCol>> aliases
            *** <skg<relToOrgParent:alias>> new alias
            *** <skg<relToOrgParent:alias>> preexisting alias
            ** <skg<id:unpredictable>> no id
            *** <skg<id:unpredictable>> also no id
        "};
  let mut after_adding_missing_info: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(
      with_missing_info).unwrap();
  add_missing_info_to_trees(
    &mut after_adding_missing_info);
  let expected_forest: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(
      without_missing_info ). unwrap();
  assert!(
    compare_trees_modulo_id(
      &after_adding_missing_info,
      &expected_forest),
    "add_missing_info_to_trees: Forests not equivalent modulo ID."
  ); }
