// cargo test --test save parentIgnores_and_mightContainMore

use indoc::indoc;
use skg::file_io::{read_node_from_id, update_fs_from_saveinstructions};
use skg::save::buffer_to_save_instructions;
use skg::test_utils::populate_test_db_from_fixtures;
use skg::typedb::update::update_typedb_from_saveinstructions;
use skg::types::{ID, SkgConfig, SkgNode};

use futures::executor::block_on;
use std::error::Error;
use std::fs;
use std::path::PathBuf;
use typedb_driver::{Credentials, DriverOptions, TypeDBDriver};

#[test]
fn test_parentignores_and_mightcontainmore(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    // Setup
    let driver: TypeDBDriver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?,
    )
    .await?;

    let fixtures_path = PathBuf::from(
      "tests/save/parentIgnores_and_mightContainMore/fixtures"
    );
    let backup_path = PathBuf::from(
      "tests/save/parentIgnores_and_mightContainMore/fixtures_backup"
    );

    let config: SkgConfig = SkgConfig {
      db_name: "skg-test-parentignores".into(),
      skg_folder: fixtures_path.clone(),
      tantivy_folder: "irrelevant".into(),
      port: 1730,
    };

    // Backup fixtures
    if backup_path.exists() {
      fs::remove_dir_all(&backup_path)?;
    }
    copy_dir_all(&fixtures_path, &backup_path)?;

    // Populate test database from fixtures
    let index_folder: &str = config
      .skg_folder
      .to_str()
      .expect("Invalid UTF-8 in path");
    populate_test_db_from_fixtures(
      index_folder,
      &config.db_name,
      &driver,
    )
    .await?;

    // Simulate user saving this org buffer:
    // Node 1 contains node 2 (which has treatment=parentIgnores and mightContainMore)
    // Node 2 contains node 3 (already) and should contain node 4 (new)
    // Node 2 should NOT affect node 1 because treatment=parentIgnores
    let org_text = indoc! {"
      * (skg (id 1)) 1
      ** (skg (id 2) (treatment parentIgnores) mightContainMore) 2
      *** (skg (id 4)) 4
    "};

    // Get save instructions
    let (_orgnode_forest, save_instructions) =
      buffer_to_save_instructions(
        org_text,
        &config,
        &driver,
      )
      .await?;

    // Apply to database
    update_typedb_from_saveinstructions(
      &config.db_name,
      &driver,
      &save_instructions,
    )
    .await?;

    // Apply to filesystem
    update_fs_from_saveinstructions(
      save_instructions,
      config.clone(),
    )?;

    { // verify mightContainMore is treated correctly
      let node2: SkgNode =
      read_node_from_id(
        &config, &driver, &ID("2".to_string() ))
      .await?;
    assert_eq!(
      node2.contains,
      vec![ ID("3".to_string()),
            ID("4".to_string()) ],
      "Node 2 should contain [3, 4], because 4 was appended, due to the 'mightContainMore' in the metadata for node 2."
    ); }

    { // verify parentIgnores is treated correctly
      let node1: SkgNode =
      read_node_from_id(&config, &driver, &ID("1".to_string()))
        .await?;
    assert_eq!(
      node1.contains,
      vec![],
      "Node 1 should, like before it was saved, have empty contents, because its child 2 has 'treatment=parentIgnores' in its metadata." ); }

    { // Restore fixtures from backup
      fs::remove_dir_all(&fixtures_path)?;
      copy_dir_all(&backup_path, &fixtures_path)?;
      fs::remove_dir_all(&backup_path)?; }
    Ok(( )) } ) }

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
