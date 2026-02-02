// I spec'd these tests by hand.
// They are much briefer than those by Claude
// (at tests/new/orgnodes_to_instructions/reconcile_same_id_instructions/by_claude.rs),
// but they catch most of the tricky logic.

use indoc::indoc;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::orgnodes_to_instructions::to_naive_instructions::naive_saveinstructions_from_forest;
use skg::from_text::buffer_to_orgnodes::validate_tree::contradictory_instructions::find_inconsistent_instructions;
use skg::from_text::orgnodes_to_instructions::reconcile_same_id_instructions::reconcile_same_id_instructions;
use skg::test_utils::{run_with_test_db, extract_skgnode_if_save_else_error};
use skg::types::misc::ID;
use skg::types::save::{DefineNode, SaveNode, DeleteNode};
use skg::types::unchecked_orgnode::unchecked_to_checked_tree;
use std::error::Error;

/// Get the primary ID from any DefineNode variant.
fn get_id(instr: &DefineNode) -> &ID {
  match instr {
    DefineNode::Save(SaveNode(node)) => &node.ids[0],
    DefineNode::Delete(DeleteNode { id, .. }) => id }}

#[test]
fn test_inconsistent_delete() {
  let input = indoc! {"
        * (skg (node (id 1) (source main))) 1
        * (skg (node (id 1) (source main) (editRequest delete))) 2
    "};

  let unchecked_forest = org_to_uninterpreted_nodes(input).unwrap().0;
  let (inconsistent_deletes, _, _) = find_inconsistent_instructions(&unchecked_forest);
  assert!(!inconsistent_deletes.is_empty(),
          "Should detect inconsistent toDelete");

  let forest = unchecked_to_checked_tree(unchecked_forest).unwrap();
  let instructions =
    naive_saveinstructions_from_forest(forest)
    . unwrap();
  assert_eq!(instructions.len(), 2);
  assert_eq!(get_id(&instructions[0]), &ID::from("1"));
  assert_eq!(get_id(&instructions[1]), &ID::from("1"));
  // One should be Delete, the other not
  let is_delete_0: bool = instructions[0].is_delete();
  let is_delete_1: bool = instructions[1].is_delete();
  assert_ne!(is_delete_0, is_delete_1); }

#[test]
fn test_deletions_excluded (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-deletions-excluded",
    "tests/new/orgnodes_to_instructions/reconcile_same_id_instructions/fixtures",
    "/tmp/tantivy-test-deletions-excluded",
    |config, driver, _tantivy| Box::pin ( async move {
      let input = indoc! {"
            * (skg (node (id 1) (source main))) 1
            ** (skg (node (id 2) (source main) (editRequest delete))) 2
            ** (skg (node (id 3) (source main))) 3
        "};

      let unchecked_forest = org_to_uninterpreted_nodes(input)?.0;
      let forest = unchecked_to_checked_tree(unchecked_forest)?;
      let instructions = naive_saveinstructions_from_forest(forest)?;
      let reduced = reconcile_same_id_instructions(config, driver, instructions).await?;

      assert_eq!(reduced.len(), 3); // There are 3 instructions.
      let id1_instruction = reduced.iter()
        .find(|instr| get_id(instr) == &ID::from("1"))
        .expect("Should have instruction for id:1");
      let id2_instruction = reduced.iter()
        .find(|instr| get_id(instr) == &ID::from("2"))
        .expect("Should have instruction for id:2");
      assert!(!id1_instruction.is_delete());
      assert!(id2_instruction.is_delete());
      assert_eq!(
        // id 1 should contain 3 and not 2 (which is being deleted)
        extract_skgnode_if_save_else_error(id1_instruction).contains,
        Some(vec![ID::from("3")]));
      Ok (( )) } ) ) }

#[test]
fn test_defining_node_defines (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-defining-node",
    "tests/new/orgnodes_to_instructions/reconcile_same_id_instructions/fixtures",
    "/tmp/tantivy-test-defining-node",
    |config, driver, _tantivy| Box::pin ( async move {
      let input = indoc! {"
            * (skg (node (id 1) (source main) indefinitive)) 1 adder
            Ignored body.
            ** (skg (node (id 2) (source main))) 2
            * (skg (node (id 1) (source main))) 1 definer
            ** (skg (node (id 3) (source main))) 3
        "};

      let unchecked_forest = org_to_uninterpreted_nodes(input)?.0;
      let forest = unchecked_to_checked_tree(unchecked_forest)?;
      let instructions = naive_saveinstructions_from_forest(forest)?;
      let reduced = reconcile_same_id_instructions(config, driver, instructions).await?;

      assert_eq!(reduced.len(), 3); // 3 unique ids (id 1 is dup'd)
      let id1_instruction = reduced.iter()
        .find(|instr| get_id(instr) == &ID::from("1"))
        .unwrap();
      assert_eq!( extract_skgnode_if_save_else_error(id1_instruction).title,
                  "1 definer");
      { // Defining instruction should define body completely, even if None
        assert_eq!(extract_skgnode_if_save_else_error(id1_instruction).body,
                   None); }
      { // Only definer's contents are used
        assert_eq!(extract_skgnode_if_save_else_error(id1_instruction).contains,
                   Some(vec![ID::from("3")])); }
      Ok (( )) } )) }

#[test]
fn test_adding_without_definer (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-adding-without-definer",
    "tests/new/orgnodes_to_instructions/reconcile_same_id_instructions/fixtures",
    "/tmp/tantivy-test-adding-without-definer",
    |config, driver, _tantivy| Box::pin ( async move {
      let input = indoc! {"
            * (skg (node (id 1) (source main) indefinitive)) 1 adder
            ** (skg (node (id 2) (source main))) 2
            ** (skg (node (id 4) (source main))) 4
            ** (skg (node (id 4) (source main) indefinitive)) 4 again
        "};
      let unchecked_forest = org_to_uninterpreted_nodes(input)?.0;
      let forest = unchecked_to_checked_tree(unchecked_forest)?;
      let instructions = naive_saveinstructions_from_forest(forest)?;
      let reduced = reconcile_same_id_instructions(
        config, driver, instructions).await?;

      { // id:1 is indefinitive-only, so it should be filtered out
        let id1_instruction = reduced.iter()
          .find(|instr| get_id(instr) == &ID::from("1"));
        assert!(id1_instruction.is_none(),
                "Indefinitive-only nodes should be filtered out"); }

      { // id:2 has a definitive instruction, so it should be present
        let id2_instruction = reduced.iter()
          .find(|instr| get_id(instr) == &ID::from("2"))
          .expect("Should have instruction for id:2");
        assert_eq!(extract_skgnode_if_save_else_error(id2_instruction).title,
                   "2"); }

      { // id:4 has one definitive and one indefinitive
        // The indefinitive should be filtered, only definitive kept
        let id4_instruction = reduced.iter()
          .find(|instr| get_id(instr) == &ID::from("4"))
          .expect("Should have instruction for id:4");
        assert_eq!( // Contains should only have what the definitive specified, and no data from the indefinitive
          extract_skgnode_if_save_else_error(id4_instruction).title, "4");
        assert_eq!(extract_skgnode_if_save_else_error(id4_instruction).contains,
                   Some( vec![] )); }

      Ok (( )) } )) }
