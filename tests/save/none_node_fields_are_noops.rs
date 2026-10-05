// cargo test none_node_fields_are_noops

use std::error::Error;

use skg::dbs::filesystem::one_node::optgraphnode_from_skgid;
use skg::from_text::supplement_from_disk::{ canonicalize_skgids_from_disk, detect_skgrepo_move, supplement_unspecified_fields_from_disk, };
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{ID, MSV, SkgConfig, SkgrepoName, TantivyIndex, members_msv, rel_partners_at_relRepo_msv};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::types::save::SkgrepoMove;



#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str =
    "tests/save/none_node_fields_are_noops/fixtures";
  run_with_shared_test_stores (
    "skg-test-save-none-node-fields-are-noops",
    |s| Box::pin ( async move {
      s . reset ("test_none_aliases_get_replaced_with_disk_aliases", fixtures) ?;
      test_none_aliases_get_replaced_with_disk_aliases (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_none_subscribesTo_get_replaced_with_disk_subscribesTo", fixtures) ?;
      test_none_subscribesTo_get_replaced_with_disk_subscribesTo (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_none_hidesFromSubs_get_replaced_with_disk_hides", fixtures) ?;
      test_none_hidesFromSubs_get_replaced_with_disk_hides (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_none_overrides_get_replaced_with_disk_overrides", fixtures) ?;
      test_none_overrides_get_replaced_with_disk_overrides (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn supplement_from_disk_then_extract_graphnode (
  config    : &SkgConfig,

  user_node : Graphnode
) -> Result<Graphnode, Box<dyn Error>> {
  let pid : ID = user_node . pid . clone();
  let disk_node : Graphnode =
    optgraphnode_from_skgid (config, &pid) ?
      . ok_or ("Expected node on disk") ?;
  let canonicalized : Graphnode =
    canonicalize_skgids_from_disk (user_node, &disk_node) ?;
  let _skgrepo_move : Option<SkgrepoMove> =
    detect_skgrepo_move (
      config,
      &pid,
      &canonicalized . home_skgrepo,
      &disk_node . home_skgrepo) ?;
  Ok (supplement_unspecified_fields_from_disk (
    canonicalized, &disk_node)) }

async fn test_none_aliases_get_replaced_with_disk_aliases (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_aliases_get_replaced_with_disk_aliases_logic (
        config ) . await
    }

async fn test_none_aliases_get_replaced_with_disk_aliases_logic (
  config : &SkgConfig,

) -> Result < (), Box<dyn Error> > {

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . aliases = MSV::Unspecified; }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . aliases),
      MSV::Specified ( vec![ "alias 1 on disk" . to_string (),
                   "alias 2 on disk" . to_string () ]),
      "Unspecified aliases from client should be replaced aliases from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . aliases = MSV::Specified ( vec![] ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      result . aliases,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) aliases from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . aliases = rel_partners_at_relRepo_msv (
        & SkgrepoName::from ("main"),
        MSV::Specified ( vec![ "new alias" . to_string () ] ) ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . aliases),
      MSV::Specified ( vec![ "new alias" . to_string () ] ),
      "Aliases from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

async fn test_none_subscribesTo_get_replaced_with_disk_subscribesTo (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_subscribesTo_get_replaced_with_disk_subscribesTo_logic (
        config ) . await
    }

async fn test_none_subscribesTo_get_replaced_with_disk_subscribesTo_logic (
  config : &SkgConfig,

) -> Result < (), Box<dyn Error> > {

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . subscribesTo = MSV::Unspecified; }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . subscribesTo),
      MSV::Specified ( vec![ ID::new ("sub_1_on_disk"),
                   ID::new ("sub_2_on_disk") ]),
      "Unspecified subscribesTo from client should be replaced with subscribesTo from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . subscribesTo = MSV::Specified ( vec![] ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      result . subscribesTo,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) subscribesTo from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . subscribesTo = rel_partners_at_relRepo_msv (
        & SkgrepoName::from ("main"),
        MSV::Specified ( vec![ ID::new ("new_sub") ] ) ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . subscribesTo),
      MSV::Specified ( vec![ ID::new ("new_sub") ] ),
      "subscribesTo from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

async fn test_none_hidesFromSubs_get_replaced_with_disk_hides (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_hidesFromSubs_get_replaced_with_disk_hides_logic (
        config ) . await
    }

async fn test_none_hidesFromSubs_get_replaced_with_disk_hides_logic (
  config : &SkgConfig,

) -> Result < (), Box<dyn Error> > {

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hidesFromSubs = MSV::Unspecified; }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . hidesFromSubs),
      MSV::Specified ( vec![ ID::new ("hide_1_on_disk") ]),
      "Unspecified hidesFromSubs from client should be replaced with hides from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hidesFromSubs = MSV::Specified ( vec![] ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      result . hidesFromSubs,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) hidesFromSubs from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hidesFromSubs = rel_partners_at_relRepo_msv (
        & SkgrepoName::from ("main"),
        MSV::Specified ( vec![ ID::new ("new_hide") ] ) ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . hidesFromSubs),
      MSV::Specified ( vec![ ID::new ("new_hide") ] ),
      "hidesFromSubs from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

async fn test_none_overrides_get_replaced_with_disk_overrides (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_overrides_get_replaced_with_disk_overrides_logic (
        config ) . await
    }

async fn test_none_overrides_get_replaced_with_disk_overrides_logic (
  config : &SkgConfig,

) -> Result < (), Box<dyn Error> > {

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . overrides = MSV::Unspecified; }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . overrides),
      MSV::Specified ( vec![ ID::new ("override_1_on_disk"),
                   ID::new ("override_2_on_disk"),
                   ID::new ("override_3_on_disk") ]),
      "Unspecified overrides from client should be replaced with overrides from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . overrides = MSV::Specified ( vec![] ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      result . overrides,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) overrides from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . overrides = rel_partners_at_relRepo_msv (
        & SkgrepoName::from ("main"),
        MSV::Specified ( vec![ ID::new ("new_override") ] ) ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . overrides),
      MSV::Specified ( vec![ ID::new ("new_override") ] ),
      "overrides from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }
