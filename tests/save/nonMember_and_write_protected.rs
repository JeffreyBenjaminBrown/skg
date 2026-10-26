// cargo test --test save nonMember_and_write_protected

use skg::dbs::filesystem::one_node::graphnode_from_gnid;
use skg::from_text::buffer_to_validated_saveplan;
use skg::save::update_fs_from_nodeInstructions;
use skg::test_utils::{copy_dir_all, run_with_test_stores};
use skg::types::misc::{ID, members_of};
use skg::types::nodes::complete::Graphnode;


use indoc::indoc;
use std::error::Error;
use std::fs;
use std::path::PathBuf;

#[test]
fn test_nonMember_and_write_protected(
) -> Result<(), Box<dyn Error>> {
  let fixtures_path: PathBuf =
    PathBuf::from (
      "tests/save/nonMember_and_write_protected/fixtures" );
  let backup_path: PathBuf =
    PathBuf::from (
      "tests/save/nonMember_and_write_protected/fixtures_backup" );

  // Backup fixtures
  if backup_path . exists () {
    fs::remove_dir_all (&backup_path) ?; }
  copy_dir_all ( &fixtures_path, &backup_path ) ?;

  // Run test with database
  let result : Result<(), Box<dyn Error>> =
    run_with_test_stores (
      "skg-test-noClaim",
      "tests/save/nonMember_and_write_protected/fixtures",
      "/tmp/tantivy-test-noClaim",
      |config, _tantivy| Box::pin ( async move {
        // Simulate user saving this org buffer:
        // Node 1 contains node 2 (which is noClaim and write-protected)
        // Node 2 contains node 3 (already); node 4 (new) sits under it
        // without affecting it, as a write-protected node holds no member.
        // Node 2 should not affect node 1 because it is noClaim
        let org_text = indoc! {"
          * (skg node (gnid 1) (skgRepos (nodeRepo main))) 1
          ** (skg node (gnid 2) (skgRepos (nodeRepo main)) noClaim writeProtected) 2
          *** (skg node (gnid 4) (skgRepos (nodeRepo main)) noClaim) 4
        "};
        let ( _viewforest, save_plan, _warnings ) =
          buffer_to_validated_saveplan(
            org_text,
            config, None ) ?;
        update_fs_from_nodeInstructions(
          &save_plan . node_instructions,
          &[],
          config . clone(), )?;

        { // verify writeProtected is treated correctly
          let node2 : Graphnode =
            graphnode_from_gnid(
              config, &ID("2" . to_string() ))
?;
        assert_eq!(
          members_of (&node2 . contains),
          vec![ ID("3" . to_string()) ],
          "Node 2 should only contain [3]. Node 2 is 'writeProtected', so it writes no content." ); }

        { // verify noClaim is treated correctly
          let node1 : Graphnode =
            graphnode_from_gnid(
              config, &ID("1" . to_string() ))
?;
        assert!(
          node1 . contains . is_empty(),
          "Node 1 should have empty contents (empty when read from disk), because its child 2 has noClaim in its metadata." ); }

        Ok (( )) } )
    );

  // Restore fixtures from backup
  fs::remove_dir_all (&fixtures_path) ?;
  copy_dir_all ( &backup_path, &fixtures_path ) ?;
  fs::remove_dir_all (&backup_path) ?;

  result }

