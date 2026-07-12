// cargo test --test save birth_and_indefinitive

use skg::dbs::filesystem::one_node::nodecomplete_from_id;
use skg::from_text::buffer_to_validated_saveplan;
use skg::save::update_fs_from_saveinstructions;
use skg::save::update_typedb_from_saveinstructions;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, members_of};
use skg::types::nodes::complete::NodeComplete;


use indoc::indoc;
use std::error::Error;
use std::fs;
use std::path::PathBuf;

#[test]
fn test_birth_and_indefinitive(
) -> Result<(), Box<dyn Error>> {
  let fixtures_path: PathBuf =
    PathBuf::from (
      "tests/save/birth_and_indefinitive/fixtures" );
  let backup_path: PathBuf =
    PathBuf::from (
      "tests/save/birth_and_indefinitive/fixtures_backup" );

  // Backup fixtures
  if backup_path . exists () {
    fs::remove_dir_all (&backup_path) ?; }
  copy_dir_all ( &fixtures_path, &backup_path ) ?;

  // Run test with database
  let result : Result<(), Box<dyn Error>> =
    run_with_test_db (
      "skg-test-parentIs",
      "tests/save/birth_and_indefinitive/fixtures",
      "/tmp/tantivy-test-parentIs",
      |config, driver, _tantivy| Box::pin ( async move {
        // Simulate user saving this org buffer:
        // Node 1 contains node 2 (which has parentIs=Independent and indef)
        // Node 2 contains node 3 (already) and should contain node 4 (new)
        // Node 2 should NOT affect node 1 because parentIs=Independent
        let org_text = indoc! {"
          * (skg (node (id 1) (source main))) 1
          ** (skg (node (id 2) (source main) (parentIs independent) indef)) 2
          *** (skg (node (id 4) (source main))) 4
        "};
        let ( _viewforest, save_plan, _warnings ) =
          buffer_to_validated_saveplan(
            org_text,
            config,
            driver, None ) . await?;
        update_typedb_from_saveinstructions(
          &config . db_name,
          driver,
          &save_plan . define_nodes,
          &[],
          None, ). await?;
        update_fs_from_saveinstructions(
          &save_plan . define_nodes,
          &[],
          config . clone(), )?;

        { // verify indef is treated correctly
          let node2 : NodeComplete =
            nodecomplete_from_id(
              config, driver, &ID("2" . to_string() ))
            . await?;
        assert_eq!(
          members_of (&node2 . contains),
          vec![ ID("3" . to_string()) ],
          "Node 2 should only contain [3]. It might look like 4 was appended, but because node 2 is 'indefinitive', that node 4 child should be ignored by node 2." ); }

        { // verify parentIs=Independent is treated correctly
          let node1 : NodeComplete =
            nodecomplete_from_id(
              config, driver, &ID("1" . to_string() ))
            . await?;
        assert!(
          node1 . contains . is_empty(),
          "Node 1 should have empty contents (empty when read from disk), because its child 2 has parentIs=Independent in its metadata." ); }

        Ok (( )) } )
    );

  // Restore fixtures from backup
  fs::remove_dir_all (&fixtures_path) ?;
  copy_dir_all ( &backup_path, &fixtures_path ) ?;
  fs::remove_dir_all (&backup_path) ?;

  result }

/// Recursively copy a directory
fn copy_dir_all(
  src: &PathBuf,
  dst: &PathBuf,
) -> std::io::Result<()> {
  fs::create_dir_all (dst)?;
  for entry in fs::read_dir (src)? {
    let entry = entry?;
    let ty = entry . file_type()?;
    if ty . is_dir() {
      copy_dir_all(
        &entry . path(),
        &dst . join(entry . file_name()),
      )?;
    } else {
      fs::copy( entry . path(),
                dst . join(entry . file_name()))?; }}
  Ok (( ))
}
