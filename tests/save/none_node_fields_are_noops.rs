// cargo test none_node_fields_are_noops

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
      "/tmp/tantivy"),
    port           : 1730,
    delete_on_quit : false, };

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

#[test]
fn test_none_subscribes_to_get_replaced_with_disk_subscribes_to (
) -> io::Result<()> {

  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : PathBuf::from (
      "tests/save/none_node_fields_are_noops/fixtures"),
    tantivy_folder : PathBuf::from (
      "/tmp/tantivy"),
    port           : 1730,
    delete_on_quit : false, };

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.subscribes_to = None; }
    let result : SkgNode =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.subscribes_to,
      Some ( vec![ ID::new("sub_1_on_disk"),
                   ID::new("sub_2_on_disk") ]),
      "None subscribes_to from client should be replaced with subscribes_to from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.subscribes_to = Some ( vec![] ); }
    let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.subscribes_to,
      Some ( vec![] ),
      "Some ( [] ) subscribes_to from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.subscribes_to = Some ( vec![ ID::new("new_sub") ] ); }
    let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.subscribes_to,
      Some ( vec![ ID::new("new_sub") ] ),
      "subscribes_to from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

#[test]
fn test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides (
) -> io::Result<()> {

  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : PathBuf::from (
      "tests/save/none_node_fields_are_noops/fixtures"),
    tantivy_folder : PathBuf::from (
      "/tmp/tantivy"),
    port           : 1730,
    delete_on_quit : false, };

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.hides_from_its_subscriptions = None; }
    let result : SkgNode =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.hides_from_its_subscriptions,
      Some ( vec![ ID::new("hide_1_on_disk") ]),
      "None hides_from_its_subscriptions from client should be replaced with hides from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.hides_from_its_subscriptions = Some ( vec![] ); }
    let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.hides_from_its_subscriptions,
      Some ( vec![] ),
      "Some ( [] ) hides_from_its_subscriptions from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.hides_from_its_subscriptions = Some ( vec![ ID::new("new_hide") ] ); }
    let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.hides_from_its_subscriptions,
      Some ( vec![ ID::new("new_hide") ] ),
      "hides_from_its_subscriptions from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

#[test]
fn test_none_overrides_view_of_get_replaced_with_disk_overrides (
) -> io::Result<()> {

  let config = SkgConfig {
    db_name        : "test_db".to_string (),
    skg_folder     : PathBuf::from (
      "tests/save/none_node_fields_are_noops/fixtures"),
    tantivy_folder : PathBuf::from (
      "/tmp/tantivy"),
    port           : 1730,
    delete_on_quit : false, };

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.overrides_view_of = None; }
    let result : SkgNode =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.overrides_view_of,
      Some ( vec![ ID::new("override_1_on_disk"),
                   ID::new("override_2_on_disk"),
                   ID::new("override_3_on_disk") ]),
      "None overrides_view_of from client should be replaced with overrides from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.overrides_view_of = Some ( vec![] ); }
    let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.overrides_view_of,
      Some ( vec![] ),
      "Some ( [] ) overrides_view_of from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.overrides_view_of = Some ( vec![ ID::new("new_override") ] ); }
    let result =
    clobber_none_fields_with_data_from_disk (
      &config, user_node )?;
    assert_eq! (
      result.overrides_view_of,
      Some ( vec![ ID::new("new_override") ] ),
      "overrides_view_of from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }
