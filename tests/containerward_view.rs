// cargo test test_containerward_view
//
// NOTE: This test currently FAILS because containerward_view.rs does not
// use the fetch_relationship_data() infrastructure from content_view.rs.
// Instead, it manually constructs metadata in metadata_for_element_of_path(),
// which hardcodes parentIsContent=false for all nodes.
//
// In a containerward path, nodes that contain their parent should have
// parentIsContent=true, which renders as 'containsParent' in the metadata.
// This test expects that behavior, so it will fail until containerward_view
// is refactored to use the proper relationship-fetching infrastructure.

use indoc::indoc;
use skg::test_utils::run_with_test_db;
use skg::mk_org_text::containerward_org_view;
use skg::types::{ID, SkgConfig};

use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
#[ignore] // This test is expected to fail until containerward_view uses fetch_relationship_data()
fn test_containerward_view_should_fail (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-containerward-view",
    "tests/containerward_view/fixtures",
    "/tmp/tantivy-test-containerward-view",
    |config, driver| Box::pin ( async move {
      test_containerward_org_view (
        config, driver ) . await ?;
      Ok (( )) } )) }

async fn test_containerward_org_view (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {

  // A root.
  let result1 = containerward_org_view(
    driver, config, &ID("1".to_string()), 1).await?;
  let expected1 = "* (skg (id 1) (rels notInParent)) 1\n";
  assert_eq!(result1, expected1);

  // A linear path: 111 -> 11 -> 1
  // - Node 111 is the terminus (notInParent, no containsParent)
  // - Node 11 contains 111, so should have containsParent
  // - Node 1 contains 11, so should have containsParent
  let result2 = containerward_org_view(
    driver, config, &ID("111".to_string()), 2).await?;
  let expected2 = indoc! {"
    ** (skg (id 111) (rels notInParent)) 111
    *** (skg (id 11) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 11
    **** (skg (id 1) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 1
    "};
  assert_eq!(result2, expected2);

  // A cycle: 2 -> 211 -> 21 -> 2
  // All non-terminus nodes should have containsParent
  let result3 = containerward_org_view(
    driver, config, &ID("2".to_string()), 1).await?;
  let expected3 = indoc! {"
    * (skg (id 2) cycle (rels notInParent)) 2
    ** (skg (id 211) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 211
    *** (skg (id 21) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 21
    **** (skg (id 2) (relToParent parentIgnores) cycle indefinitive (rels notInParent containsParent)) 2
    "};
  assert_eq!(result3, expected3);

  // An immediately forked containment path.
  // (Fork order is undefined, so both options are considered.)
  // Branches 1 and 2 both contain the terminus 'shared', so should have containsParent
  let result4 = containerward_org_view(
    driver, config, &ID("shared".to_string()), 3).await?;
  let expected4a = indoc! {"
    *** (skg (id shared) (rels notInParent)) shared
    **** (skg (id 1) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 1
    **** (skg (id 2) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 2
    "};
  let expected4b = indoc! {"
    *** (skg (id shared) (rels notInParent)) shared
    **** (skg (id 2) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 2
    **** (skg (id 1) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 1
    "};
  assert!(result4 == expected4a ||
          result4 == expected4b,
          "Expected one of {:?} or {:?}, got {:?}",
          expected4a, expected4b, result4);

  // An eventually-forking path, with body text at its terminus.
  // All non-terminus nodes should have containsParent
  let result5 = containerward_org_view(
    driver, config, &ID("shared_1".to_string()), 1).await?;
  let expected5a = indoc! {"
    * (skg (id shared_1) (rels notInParent)) shared_1
    Some body text.
    The second line of body text.
    ** (skg (id shared) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) shared
    *** (skg (id 1) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 1
    *** (skg (id 2) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 2
    "};
  let expected5b = indoc! {"
    * (skg (id shared_1) (rels notInParent)) shared_1
    Some body text.
    The second line of body text.
    ** (skg (id shared) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) shared
    *** (skg (id 2) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 2
    *** (skg (id 1) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) 1
    "};
  assert!(result5 == expected5a ||
          result5 == expected5b,
          "Expected one of {:?} or {:?}, got {:?}",
          expected5a, expected5b, result5);

  // A fork and a cycle, but the cycle lies after the fork,
  // so it is not discovered.
  // Both branches contain the terminus, so should have containsParent
  let result6 = containerward_org_view(
    driver, config, &ID("shared_cyclic".to_string()), 1).await?;
  let expected6a = indoc! {"
    * (skg (id shared_cyclic) (rels notInParent)) shared_cyclic
    ** (skg (id shared_cyclic_1) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) shared_cyclic_1
    ** (skg (id uncyclic_container) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) uncyclic_container
    "};
  let expected6b = indoc! {"
    * (skg (id shared_cyclic) (rels notInParent)) shared_cyclic
    ** (skg (id uncyclic_container) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) uncyclic_container
    ** (skg (id shared_cyclic_1) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) shared_cyclic_1
    "};
  assert!(result6 == expected6a ||
          result6 == expected6b,
          "Expected one of {:?} or {:?}, got {:?}",
          expected6a, expected6b, result6);

  // Starting at a different point,
  // both the fork and the cycle are discovered.
  // All non-terminus nodes should have containsParent
  let result7 = containerward_org_view(
    driver, config, &ID("shared_cyclic_1".to_string()), 1).await?;
  let expected7a = indoc! {"
    * (skg (id shared_cyclic_1) cycle (rels notInParent)) shared_cyclic_1
    ** (skg (id shared_cyclic) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) shared_cyclic
    *** (skg (id shared_cyclic_1) (relToParent parentIgnores) cycle indefinitive (rels notInParent containsParent)) shared_cyclic_1
    *** (skg (id uncyclic_container) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) uncyclic_container
    "};
  let expected7b = indoc! {"
    * (skg (id shared_cyclic_1) cycle (rels notInParent)) shared_cyclic_1
    ** (skg (id shared_cyclic) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) shared_cyclic
    *** (skg (id uncyclic_container) (relToParent parentIgnores) indefinitive (rels notInParent containsParent)) uncyclic_container
    *** (skg (id shared_cyclic_1) (relToParent parentIgnores) cycle indefinitive (rels notInParent containsParent)) shared_cyclic_1
    "};
  assert!(result7 == expected7a ||
          result7 == expected7b,
          "Expected one of {:?} or {:?}, got {:?}",
          expected7a, expected7b, result7);

  Ok (( )) }
