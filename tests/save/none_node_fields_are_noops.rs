// cargo test none_node_fields_are_noops

use std::error::Error;

use skg::from_text::supplement_from_disk::
  supplement_none_fields_from_disk_if_save;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, SkgConfig};
use skg::types::save::{DefineNode, SaveNode};
use skg::types::skgnode::{SkgNode, empty_skgnode};

async fn supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
  config    : &SkgConfig,
  graph     : &neo4rs::Graph,
  user_node : SkgNode
) -> Result<SkgNode, Box<dyn Error>> {
  let result : DefineNode =
    supplement_none_fields_from_disk_if_save (
      config, graph,
      DefineNode::Save(SaveNode(user_node)) ). await ?;
  match result {
    DefineNode::Save(SaveNode(node)) => Ok(node),
    DefineNode::Delete(_) =>
      Err("Expected Save, got Delete".into()) }}

#[test]
fn test_none_aliases_get_replaced_with_disk_aliases (
) -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-none-aliases",
    "tests/save/none_node_fields_are_noops/fixtures",
    "/tmp/tantivy-test-none-aliases",
    |config, graph, _tantivy| Box::pin ( async move {
      test_none_aliases_get_replaced_with_disk_aliases_logic (
        config, graph ) . await
    } )) }

async fn test_none_aliases_get_replaced_with_disk_aliases_logic (
  config : &SkgConfig,
  graph : &neo4rs::Graph,
) -> Result < (), Box<dyn Error> > {

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.aliases = None; }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.aliases,
      Some ( vec![ "alias 1 on disk".to_string (),
                   "alias 2 on disk".to_string () ]),
      "None aliases from client should be replaced aliases from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.aliases = Some ( vec![] ); }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.aliases,
      Some ( vec![] ),
      "Some ( [] ) aliases from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.aliases = Some ( vec![ "new alias".to_string () ] ); }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.aliases,
      Some ( vec![ "new alias".to_string () ] ),
      "Aliases from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

#[test]
fn test_none_subscribes_to_get_replaced_with_disk_subscribes_to (
) -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-none-subscribes",
    "tests/save/none_node_fields_are_noops/fixtures",
    "/tmp/tantivy-test-none-subscribes",
    |config, graph, _tantivy| Box::pin ( async move {
      test_none_subscribes_to_get_replaced_with_disk_subscribes_to_logic (
        config, graph ) . await
    } )) }

async fn test_none_subscribes_to_get_replaced_with_disk_subscribes_to_logic (
  config : &SkgConfig,
  graph : &neo4rs::Graph,
) -> Result < (), Box<dyn Error> > {

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.subscribes_to = None; }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.subscribes_to,
      Some ( vec![ ID::new("sub_1_on_disk"),
                   ID::new("sub_2_on_disk") ]),
      "None subscribes_to from client should be replaced with subscribes_to from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.subscribes_to = Some ( vec![] ); }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.subscribes_to,
      Some ( vec![] ),
      "Some ( [] ) subscribes_to from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.subscribes_to = Some ( vec![ ID::new("new_sub") ] ); }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.subscribes_to,
      Some ( vec![ ID::new("new_sub") ] ),
      "subscribes_to from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

#[test]
fn test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides (
) -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-none-hides",
    "tests/save/none_node_fields_are_noops/fixtures",
    "/tmp/tantivy-test-none-hides",
    |config, graph, _tantivy| Box::pin ( async move {
      test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides_logic (
        config, graph ) . await
    } )) }

async fn test_none_hides_from_its_subscriptions_get_replaced_with_disk_hides_logic (
  config : &SkgConfig,
  graph : &neo4rs::Graph,
) -> Result < (), Box<dyn Error> > {

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.hides_from_its_subscriptions = None; }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.hides_from_its_subscriptions,
      Some ( vec![ ID::new("hide_1_on_disk") ]),
      "None hides_from_its_subscriptions from client should be replaced with hides from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.hides_from_its_subscriptions = Some ( vec![] ); }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.hides_from_its_subscriptions,
      Some ( vec![] ),
      "Some ( [] ) hides_from_its_subscriptions from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.hides_from_its_subscriptions = Some ( vec![ ID::new("new_hide") ] ); }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.hides_from_its_subscriptions,
      Some ( vec![ ID::new("new_hide") ] ),
      "hides_from_its_subscriptions from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

#[test]
fn test_none_overrides_view_of_get_replaced_with_disk_overrides (
) -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-none-overrides",
    "tests/save/none_node_fields_are_noops/fixtures",
    "/tmp/tantivy-test-none-overrides",
    |config, graph, _tantivy| Box::pin ( async move {
      test_none_overrides_view_of_get_replaced_with_disk_overrides_logic (
        config, graph ) . await
    } )) }

async fn test_none_overrides_view_of_get_replaced_with_disk_overrides_logic (
  config : &SkgConfig,
  graph : &neo4rs::Graph,
) -> Result < (), Box<dyn Error> > {

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.overrides_view_of = None; }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
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
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.overrides_view_of,
      Some ( vec![] ),
      "Some ( [] ) overrides_view_of from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : SkgNode = empty_skgnode ();
    { user_node.title   = "Title from user".to_string ();
      user_node.ids     = vec![ ID::new ("test_node") ];
      user_node.overrides_view_of = Some ( vec![ ID::new("new_override") ] ); }
    let result : SkgNode =
      supplement_none_fields_from_disk_if_save_THEN_extract_skgnode (
        &config, &graph, user_node ). await ?;
    assert_eq! (
      result.overrides_view_of,
      Some ( vec![ ID::new("new_override") ] ),
      "overrides_view_of from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }
