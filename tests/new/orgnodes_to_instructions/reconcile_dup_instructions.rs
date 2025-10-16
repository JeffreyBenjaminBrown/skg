// I spec'd these tests by hand.
// They are much briefer than those by Claude
// (at tests/new/orgnodes_to_instructions/reconcile_dup_instructions/by_claude.rs),
// but they catch most of the tricky logic.

use indoc::indoc;
use skg::save::{org_to_uninterpreted_nodes, orgnodes_to_dirty_save_instructions, find_inconsistent_instructions};
use skg::save::orgnodes_to_instructions::reconcile_dup_instructions::reconcile_dup_instructions;
use skg::test_utils::run_with_test_db;
use skg::types::ID;
use std::error::Error;

#[test]
fn test_inconsistent_delete() {
  let input = indoc! {"
        * (skg (id 1)) 1
        * (skg (id 1) toDelete) 2
    "};

  let trees = org_to_uninterpreted_nodes(input).unwrap();
  let (inconsistent_deletes, _) = find_inconsistent_instructions(&trees);
  assert!(!inconsistent_deletes.is_empty(),
          "Should detect inconsistent toDelete");

  let instructions =
    orgnodes_to_dirty_save_instructions(trees)
    . unwrap();
  assert_eq!(instructions.len(), 2);
  assert_eq!(instructions[0].0.ids[0], ID::from("1"));
  assert_eq!(instructions[1].0.ids[0], ID::from("1"));
  assert_ne!(instructions[0].1.toDelete, instructions[1].1.toDelete);
}

#[test]
fn test_deletions_excluded (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-deletions-excluded",
    "tests/new/orgnodes_to_instructions/reconcile_dup_instructions/fixtures",
    "/tmp/tantivy-test-deletions-excluded",
    |config, driver| Box::pin ( async move {
      let input = indoc! {"
            * (skg (id 1)) 1
            ** (skg (id 2) toDelete) 2
            ** (skg (id 3)) 3
        "};

      let trees = org_to_uninterpreted_nodes(input)?;
      let instructions = orgnodes_to_dirty_save_instructions(trees)?;
      let reduced = reconcile_dup_instructions(config, driver, instructions).await?;

      assert_eq!(reduced.len(), 3); // There are 3 instructions.
      let id1_instruction = reduced.iter()
        .find(|(node, _)| node.ids.contains(&ID::from("1")))
        .expect("Should have instruction for id:1");
      let id2_instruction = reduced.iter()
        .find(|(node, _)| node.ids.contains(&ID::from("2")))
        .expect("Should have instruction for id:2");
      assert_eq!(id1_instruction.1.toDelete, false);
      assert_eq!(id2_instruction.1.toDelete, true);
      assert_eq!( // id 1 should contain 3 and not 2 (which is being deleted)
        id1_instruction.0.contains, vec![ID::from("3")]);
      Ok (( )) } ) ) }

#[test]
fn test_defining_node_defines (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-defining-node",
    "tests/new/orgnodes_to_instructions/reconcile_dup_instructions/fixtures",
    "/tmp/tantivy-test-defining-node",
    |config, driver| Box::pin ( async move {
      let input = indoc! {"
            * (skg (id 1) mightContainMore) 1 adder
            Ignored body.
            ** (skg (id 2)) 2
            * (skg (id 1)) 1 definer
            ** (skg (id 3)) 3
        "};

      let trees = org_to_uninterpreted_nodes(input)?;
      let instructions = orgnodes_to_dirty_save_instructions(trees)?;
      let reduced = reconcile_dup_instructions(config, driver, instructions).await?;

      assert_eq!(reduced.len(), 3); // 3 unique ids (id 1 is dup'd)
      let id1_instruction = reduced.iter()
        .find(|(node, _)| node.ids.contains(&ID::from("1")))
        .unwrap();
      assert_eq!(id1_instruction.0.title, "1 definer");
      // Defining instruction should define body completely, even if None
      assert_eq!(id1_instruction.0.body, None);
      assert_eq!(id1_instruction.0.contains, vec![ID::from("3"), ID::from("2")]);
      Ok (( )) } ) ) }

#[test]
fn test_adding_without_definer (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-adding-without-definer",
    "tests/new/orgnodes_to_instructions/reconcile_dup_instructions/fixtures",
    "/tmp/tantivy-test-adding-without-definer",
    |config, driver| Box::pin ( async move {
      let input = indoc! {"
            * (skg (id 1) mightContainMore) 1 adder
            ** (skg (id 2)) 2
            ** (skg (id 4)) 4
            ** (skg (id 4) mightContainMore) 4 again
        "};

      let trees = org_to_uninterpreted_nodes(input)?;
      let instructions = orgnodes_to_dirty_save_instructions(trees)?;
      let reduced = reconcile_dup_instructions(config, driver, instructions).await?;

      let id1_instruction = reduced.iter()
        .find(|(node, _)| node.ids.contains(&ID::from("1")))
        .expect("Should have instruction for id:1");

      assert_eq!(
        // even a mightContainMore will clobber the title on disk
        id1_instruction.0.title,
        "1 adder");
      assert_eq!(
        // Body comes from disk since no instruction provides one.
        id1_instruction.0.body,
        Some("body from disk".to_string()));
      assert_eq!(
        // Since there is no defining node, contents are read from disk,
        // and then the mightContainMore node with id 1
        // appends its contents, with deduplication.
        id1_instruction.0.contains,
        vec![ ID::from("2"),
              ID::from("3"),
              ID::from("4"), ]);
      Ok (( )) } ) ) }
