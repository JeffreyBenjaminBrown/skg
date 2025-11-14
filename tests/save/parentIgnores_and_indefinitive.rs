// cargo test --test save parentIgnores_and_indefinitive

use skg::media::file_io::one_node::read_node_from_id;
use skg::read_buffer::buffer_to_save_instructions;
use skg::save::fs::update_fs_from_saveinstructions;
use skg::save::typedb::update_typedb_from_saveinstructions;
use skg::test_utils::run_with_test_db;
use skg::types::misc::ID;
use skg::types::skgnode::SkgNode;

use indoc::indoc;
use std::error::Error;
use std::fs;
use std::path::PathBuf;

#[test]
fn test_parentignores_and_indefinitive(
) -> Result<(), Box<dyn Error>> {
  let fixtures_path: PathBuf =
    PathBuf::from (
      "tests/save/parentIgnores_and_indefinitive/fixtures" );
  let backup_path: PathBuf =
    PathBuf::from (
      "tests/save/parentIgnores_and_indefinitive/fixtures_backup" );

  // Backup fixtures
  if backup_path . exists () {
    fs::remove_dir_all ( &backup_path ) ?; }
  copy_dir_all ( &fixtures_path, &backup_path ) ?;

  // Run test with database
  let result : Result<(), Box<dyn Error>> =
    run_with_test_db (
      "skg-test-parentignores",
      "tests/save/parentIgnores_and_indefinitive/fixtures",
      "/tmp/tantivy-test-parentignores",
      | config, driver | Box::pin ( async move {
        // Simulate user saving this org buffer:
        // Node 1 contains node 2 (which has treatment=parentIgnores and indefinitive)
        // Node 2 contains node 3 (already) and should contain node 4 (new)
        // Node 2 should NOT affect node 1 because treatment=parentIgnores
        let org_text = indoc! {"
          * (skg (id 1) (source main)) 1
          ** (skg (id 2) (source main) (code (relToParent parentIgnores) indefinitive)) 2
          *** (skg (id 4) (source main)) 4
        "};

        // Get save instructions
        let (_orgnode_forest, save_instructions, _merge_instructions) =
          buffer_to_save_instructions(
            org_text,
            config,
            driver,
          )
          .await?;

        // Apply to database
        update_typedb_from_saveinstructions(
          &config.db_name,
          driver,
          &save_instructions,
        )
        .await?;

        // Apply to filesystem
        update_fs_from_saveinstructions(
          save_instructions,
          config.clone(),
        )?;

        { // verify indefinitive is treated correctly
          let (node2, _source): (SkgNode, _) =
          read_node_from_id(
            config, driver, &ID("2".to_string() ))
          .await?;
        assert_eq!(
          node2.contains,
          Some(vec![ ID("3".to_string()),
                     ID("4".to_string()) ]),
          "Node 2 should contain [3, 4], because 4 was appended, due to the 'indefinitive' in the metadata for node 2."
        ); }

        { // verify parentIgnores is treated correctly
          let (node1, _source): (SkgNode, _) =
          read_node_from_id(config, driver, &ID("1".to_string()))
            .await?;
        assert_eq!(
          node1.contains,
          None,
          "Node 1 should have empty contents (None when read from disk), because its child 2 has 'treatment=parentIgnores' in its metadata." ); }

        Ok (( )) } )
    );

  // Restore fixtures from backup
  fs::remove_dir_all ( &fixtures_path ) ?;
  copy_dir_all ( &backup_path, &fixtures_path ) ?;
  fs::remove_dir_all ( &backup_path ) ?;

  result }

/// Recursively copy a directory
fn copy_dir_all(
  src: &PathBuf,
  dst: &PathBuf,
) -> std::io::Result<()> {
  fs::create_dir_all(dst)?;
  for entry in fs::read_dir(src)? {
    let entry = entry?;
    let ty = entry.file_type()?;
    if ty.is_dir() {
      copy_dir_all(
        &entry.path(),
        &dst.join(entry.file_name()),
      )?;
    } else {
      fs::copy( entry.path(),
                dst.join(entry.file_name()))?; }}
  Ok (( ))
}
