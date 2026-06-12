// cargo test none_node_fields_are_noops

use std::error::Error;

use skg::dbs::filesystem::one_node::optnodecomplete_from_id;
use skg::from_text::supplement_from_disk::{ canonicalize_ids_from_disk, detect_source_move, supplement_unspecified_fields_from_disk, };
use skg::test_utils::run_with_shared_test_db;
use skg::types::misc::{ID, MSV, SkgConfig, TantivyIndex};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::save::SourceMove;

use std::sync::Arc;
use typedb_driver::TypeDBDriver;


#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str =
    "tests/save/none_node_fields_are_noops/fixtures";
  run_with_shared_test_db (
    "skg-test-save-none-node-fields-are-noops",
    |s| Box::pin ( async move {
      s . reset ("test_none_aliases_get_replaced_with_disk_aliases", fixtures) . await ?;
      test_none_aliases_get_replaced_with_disk_aliases (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_none_subscribes_to_get_replaced_with_disk_subscribes_to", fixtures) . await ?;
      test_none_subscribes_to_get_replaced_with_disk_subscribes_to (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides", fixtures) . await ?;
      test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_none_overrides_view_of_get_replaced_with_disk_overrides", fixtures) . await ?;
      test_none_overrides_view_of_get_replaced_with_disk_overrides (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn supplement_from_disk_then_extract_nodecomplete (
  config    : &SkgConfig,
  driver    : &typedb_driver::TypeDBDriver,
  user_node : NodeComplete
) -> Result<NodeComplete, Box<dyn Error>> {
  let pid : ID = user_node . pid . clone();
  let disk_node : NodeComplete =
    optnodecomplete_from_id (config, driver, &pid) . await ?
      . ok_or ("Expected node on disk") ?;
  let canonicalized : NodeComplete =
    canonicalize_ids_from_disk (user_node, &disk_node) ?;
  let _source_move : Option<SourceMove> =
    detect_source_move (
      config,
      &pid,
      &canonicalized . source,
      &disk_node . source) ?;
  Ok (supplement_unspecified_fields_from_disk (
    canonicalized, &disk_node)) }

async fn test_none_aliases_get_replaced_with_disk_aliases (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_aliases_get_replaced_with_disk_aliases_logic (
        config, driver ) . await
    }

async fn test_none_aliases_get_replaced_with_disk_aliases_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . aliases = MSV::Unspecified; }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . aliases,
      MSV::Specified ( vec![ "alias 1 on disk" . to_string (),
                   "alias 2 on disk" . to_string () ]),
      "Unspecified aliases from client should be replaced aliases from disk." ); }

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . aliases = MSV::Specified ( vec![] ); }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . aliases,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) aliases from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . aliases = MSV::Specified ( vec![ "new alias" . to_string () ] ); }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . aliases,
      MSV::Specified ( vec![ "new alias" . to_string () ] ),
      "Aliases from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

async fn test_none_subscribes_to_get_replaced_with_disk_subscribes_to (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_subscribes_to_get_replaced_with_disk_subscribes_to_logic (
        config, driver ) . await
    }

async fn test_none_subscribes_to_get_replaced_with_disk_subscribes_to_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . subscribes_to = MSV::Unspecified; }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . subscribes_to,
      MSV::Specified ( vec![ ID::new ("sub_1_on_disk"),
                   ID::new ("sub_2_on_disk") ]),
      "Unspecified subscribes_to from client should be replaced with subscribes_to from disk." ); }

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . subscribes_to = MSV::Specified ( vec![] ); }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . subscribes_to,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) subscribes_to from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . subscribes_to = MSV::Specified ( vec![ ID::new ("new_sub") ] ); }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . subscribes_to,
      MSV::Specified ( vec![ ID::new ("new_sub") ] ),
      "subscribes_to from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

async fn test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides_logic (
        config, driver ) . await
    }

async fn test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hides_from_its_subscriptions = MSV::Unspecified; }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . hides_from_its_subscriptions,
      MSV::Specified ( vec![ ID::new ("hide_1_on_disk") ]),
      "Unspecified hides_from_its_subscriptions from client should be replaced with hides from disk." ); }

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hides_from_its_subscriptions = MSV::Specified ( vec![] ); }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . hides_from_its_subscriptions,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) hides_from_its_subscriptions from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hides_from_its_subscriptions = MSV::Specified ( vec![ ID::new ("new_hide") ] ); }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . hides_from_its_subscriptions,
      MSV::Specified ( vec![ ID::new ("new_hide") ] ),
      "hides_from_its_subscriptions from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

async fn test_none_overrides_view_of_get_replaced_with_disk_overrides (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_overrides_view_of_get_replaced_with_disk_overrides_logic (
        config, driver ) . await
    }

async fn test_none_overrides_view_of_get_replaced_with_disk_overrides_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . overrides_view_of = MSV::Unspecified; }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . overrides_view_of,
      MSV::Specified ( vec![ ID::new ("override_1_on_disk"),
                   ID::new ("override_2_on_disk"),
                   ID::new ("override_3_on_disk") ]),
      "Unspecified overrides_view_of from client should be replaced with overrides from disk." ); }

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . overrides_view_of = MSV::Specified ( vec![] ); }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . overrides_view_of,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) overrides_view_of from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : NodeComplete = empty_node_complete ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . overrides_view_of = MSV::Specified ( vec![ ID::new ("new_override") ] ); }
    let result : NodeComplete =
      supplement_from_disk_then_extract_nodecomplete (
        &config, &driver, user_node ) . await ?;
    assert_eq! (
      result . overrides_view_of,
      MSV::Specified ( vec![ ID::new ("new_override") ] ),
      "overrides_view_of from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }
