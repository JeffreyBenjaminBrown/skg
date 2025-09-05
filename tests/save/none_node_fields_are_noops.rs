use std::io;
use std::path::PathBuf;
use tempfile::tempdir;

use skg::save::none_node_fields_are_noops::clobber_none_fields_with_data_from_disk;
use skg::types::{ SkgConfig, SkgNode, ID, empty_skgnode };
use skg::file_io::write_node;

#[test]
fn test_clobber_preserves_user_aliases_when_present (
) -> io::Result<()> {

  let dir = tempdir ()?;
  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : dir.path ().to_path_buf (),
    tantivy_folder : PathBuf::from ("/tmp/tantivy"),
  };

  let mut disk_node : SkgNode =
    empty_skgnode ();
  { disk_node.title    = "Title from disk".to_string ();
    disk_node.aliases  = Some ( vec![ "disk_alias_1".to_string (),
                                       "disk_alias_2".to_string () ]);
    disk_node.ids      = vec![ ID::new ("test_id") ];
    disk_node.body     = Some ( "Body from disk".to_string () ); }

  let file_path =
    dir.path ().join ("test_id.skg");
  write_node ( &disk_node, &file_path )?;

  let mut user_node : SkgNode =
    empty_skgnode ();
  { user_node.title    = "Title from user".to_string ();
    user_node.aliases  = Some ( vec![ "user_alias_1".to_string (),
                                       "user_alias_2".to_string () ]);
    user_node.ids      = vec![ ID::new ("test_id") ];
    user_node.body     = Some ( "Body from user".to_string () ); }

  let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node.clone () )?;

  assert_eq! ( result.aliases,
               Some ( vec![ "user_alias_1".to_string (),
                            "user_alias_2".to_string () ]),
               "User aliases should be preserved when present" );

  assert_eq! ( result.title, "Title from user",
               "Title should remain from user" );
  assert_eq! ( result.body, Some ( "Body from user".to_string () ),
               "Body should remain from user" );

  Ok (())
}

#[test]
fn test_clobber_replaces_none_aliases_with_disk_aliases (
) -> io::Result<()> {

  let dir =
    tempdir ()?;
  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : dir.path ().to_path_buf (),
    tantivy_folder : PathBuf::from ("/tmp/tantivy"),
  };

  let mut disk_node : SkgNode = empty_skgnode ();
  { disk_node.title    = "Title from disk".to_string ();
    disk_node.aliases  = Some ( vec![ "disk_alias_1".to_string (),
                                       "disk_alias_2".to_string () ]);
    disk_node.ids      = vec![ ID::new ("test_id") ];
    disk_node.body     = Some ( "Body from disk".to_string () ); }

  let file_path =
    dir.path ().join ("test_id.skg");
  write_node ( &disk_node, &file_path )?;

  let mut user_node : SkgNode =
    empty_skgnode ();
  { user_node.title    = "Title from user".to_string ();
    user_node.aliases  = None;
    user_node.ids      = vec![ ID::new ("test_id") ];
    user_node.body     = Some ( "Body from user".to_string () ); }

  let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node.clone () )?;

  assert_eq! ( result.aliases,
               Some ( vec![ "disk_alias_1".to_string (),
                            "disk_alias_2".to_string () ]),
               "None aliases should be replaced with disk aliases" );

  assert_eq! ( result.title, "Title from user",
               "Title should remain from user" );
  assert_eq! ( result.body, Some ( "Body from user".to_string () ),
               "Body should remain from user" );

  Ok (())
}

#[test]
fn test_clobber_returns_user_node_when_file_not_found () -> io::Result<()> {
  let dir =
    tempdir ()?;
  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : dir.path ().to_path_buf (),
    tantivy_folder : PathBuf::from ("/tmp/tantivy"),
  };

  let mut user_node : SkgNode =
    empty_skgnode ();
  { user_node.title    = "Title from user".to_string ();
    user_node.aliases  = Some ( vec![ "user_alias".to_string () ]);
    user_node.ids      = vec![ ID::new ("nonexistent_id") ];
    user_node.body     = Some ( "Body from user".to_string () ); }

  let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node.clone () )?;

  assert_eq! ( result, user_node,
               "User node should be returned unchanged when file not found" );

  Ok (())
}

#[test]
fn test_clobber_none_aliases_from_disk_none () -> io::Result<()> {
  let dir =
    tempdir ()?;
  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : dir.path ().to_path_buf (),
    tantivy_folder : PathBuf::from ("/tmp/tantivy"),
  };

  let mut disk_node : SkgNode =
    empty_skgnode ();
  { disk_node.title    = "Title from disk".to_string ();
    disk_node.aliases  = None;
    disk_node.ids      = vec![ ID::new ("test_id") ];
    disk_node.body     = Some ( "Body from disk".to_string () ); }

  let file_path =
    dir.path ().join ("test_id.skg");
  write_node ( &disk_node, &file_path )?;

  let mut user_node : SkgNode =
    empty_skgnode ();
  { user_node.title    = "Title from user".to_string ();
    user_node.aliases  = None;
    user_node.ids      = vec![ ID::new ("test_id") ];
    user_node.body     = Some ( "Body from user".to_string () ); }

  let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node.clone () )?;

  assert_eq! ( result.aliases, None,
               "None aliases should remain None when disk also has None" );

  assert_eq! ( result.title, "Title from user",
               "Title should remain from user" );

  Ok (())
}

#[test]
fn test_clobber_empty_aliases_from_user () -> io::Result<()> {
  let dir =
    tempdir ()?;
  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : dir.path ().to_path_buf (),
    tantivy_folder : PathBuf::from ("/tmp/tantivy"),
  };

  let mut disk_node : SkgNode =
    empty_skgnode ();
  { disk_node.title    = "Title from disk".to_string ();
    disk_node.aliases  = Some ( vec![ "disk_alias".to_string () ]);
    disk_node.ids      = vec![ ID::new ("test_id") ];
    disk_node.body     = None; }

  let file_path =
    dir.path ().join ("test_id.skg");
  write_node ( &disk_node, &file_path )?;

  let mut user_node : SkgNode =
    empty_skgnode ();
  { user_node.title    = "Title from user".to_string ();
    user_node.aliases  = Some ( vec![] );
    user_node.ids      = vec![ ID::new ("test_id") ];
    user_node.body     = None; }

  let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node.clone () )?;

  assert_eq! ( result.aliases, Some ( vec![] ),
               "Empty aliases from user should be preserved, not replaced" );

  Ok (())
}

#[test]
fn test_clobber_errors_on_no_ids () {
  let dir =
    tempdir ().unwrap ();
  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : dir.path ().to_path_buf (),
    tantivy_folder : PathBuf::from ("/tmp/tantivy"),
  };

  let mut user_node : SkgNode =
    empty_skgnode ();
  { user_node.title    = "Title".to_string ();
    user_node.aliases  = None;
    user_node.ids      = vec![];
    user_node.body     = None; }

  let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node );

  assert! ( result.is_err (),
            "Should error when node has no IDs" );

  match result {
    Err (e) => {
      assert_eq! ( e.kind (), io::ErrorKind::InvalidInput,
                   "Should be InvalidInput error" );
      assert! ( e.to_string ().contains ("no IDs"),
                "Error message should mention 'no IDs'" );
    },
    Ok (_) => panic! ("Expected error but got success"),
  }
}
