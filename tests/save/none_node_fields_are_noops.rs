use std::io;
use std::path::PathBuf;

use skg::save::none_node_fields_are_noops::clobber_none_fields_with_data_from_disk;
use skg::types::{ ID, SkgConfig, SkgNode, empty_skgnode };

#[test]
fn test_none_aliases_get_replaced_with_disk_aliases (
) -> io::Result<()> {

  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : PathBuf::from (
      "tests/save/none_node_fields_are_noops/fixtures"),
    tantivy_folder : PathBuf::from (
      "/tmp/tantivy"), };

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.aliases = None; }
    let result : SkgNode =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.aliases,
      Some ( vec![ "alias 1 on disk".to_string (),
                   "alias 2 on disk".to_string () ]),
      "None aliases from client should be replaced aliases from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.aliases = Some ( vec![] ); }
    let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.aliases,
      Some ( vec![] ),
      "Some ( [] ) aliases from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.aliases = Some ( vec![ "new alias".to_string () ] ); }
    let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.aliases,
      Some ( vec![ "new alias".to_string () ] ),
      "Aliases from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }
