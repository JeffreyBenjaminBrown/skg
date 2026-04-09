// cargo test --test save birth_and_indefinitive

use skg::dbs::filesystem::one_node::skgnode_from_id;
use skg::from_text::buffer_to_viewnode_forest_and_save_instructions;
use skg::save::update_fs_from_saveinstructions;
use skg::save::update_typedb_from_saveinstructions;
use skg::test_utils::run_with_test_db;
use skg::types::misc::ID;
use skg::types::skgnode::SkgNode;
use skg::types::memory::SkgNodeMap;

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
      "skg-test-birth",
      "tests/save/birth_and_indefinitive/fixtures",
      "/tmp/tantivy-test-birth",
      |config, driver, _tantivy| Box::pin ( async move {
        // Simulate user saving this org buffer:
        // Node 1 contains node 2 (which has birth=Independent and indefinitive)
        // Node 2 contains node 3 (already) and should contain node 4 (new)
        // Node 2 should NOT affect node 1 because birth=Independent
        let org_text = indoc! {"
          * (skg (node (id 1) (source main))) 1
          ** (skg (node (id 2) (source main) (birth independent) indefinitive)) 2
          *** (skg (node (id 4) (source main))) 4
        "};
        let (_viewnode_forest, save_instructions, _merge_instructions, _source_moves) =
          buffer_to_viewnode_forest_and_save_instructions(
            org_text,
            config,
            driver,
            &SkgNodeMap::new(), ) . await?;
        update_typedb_from_saveinstructions(
          &config . db_name,
          driver,
          &save_instructions,
          &[], ). await?;
        update_fs_from_saveinstructions(
          save_instructions,
          &[],
          config . clone(), )?;

        { // verify indefinitive is treated correctly
          let node2 : SkgNode =
            skgnode_from_id(
              config, driver, &ID("2" . to_string() ))
            . await?;
        assert_eq!(
          node2 . contains,
          vec![ ID("3" . to_string()) ],
          "Node 2 should only contain [3]. It might look like 4 was appended, but because node 2 is 'indefinitive', that node 4 child should be ignored by node 2." ); }

        { // verify birth=Independent is treated correctly
          let node1 : SkgNode =
            skgnode_from_id(
              config, driver, &ID("1" . to_string() ))
            . await?;
        assert!(
          node1 . contains . is_empty(),
          "Node 1 should have empty contents (empty when read from disk), because its child 2 has birth=Independent in its metadata." ); }

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
