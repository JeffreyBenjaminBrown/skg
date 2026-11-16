// cargo test none_node_fields_are_noops

use std::error::Error;

use skg::dbs::filesystem::one_node::optgraphnode_from_gnid;
use skg::from_text::supplement_from_disk::{ canonicalize_gnids_from_disk, detect_skgrepo_move, supplement_unspecified_fields_from_disk, };
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
      s . reset ("test_none_forks_get_replaced_with_disk_forks", fixtures) ?;
      test_none_forks_get_replaced_with_disk_forks (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_none_hides_get_replaced_with_disk_hides", fixtures) ?;
      test_none_hides_get_replaced_with_disk_hides (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn supplement_from_disk_then_extract_graphnode (
  config    : &SkgConfig,

  user_node : Graphnode
) -> Result<Graphnode, Box<dyn Error>> {
  let pid : ID = user_node . pid . clone();
  let disk_node : Graphnode =
    optgraphnode_from_gnid (config, &pid) ?
      . ok_or ("Expected node on disk") ?;
  let canonicalized : Graphnode =
    canonicalize_gnids_from_disk (user_node, &disk_node) ?;
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

async fn test_none_forks_get_replaced_with_disk_forks (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_forks_get_replaced_with_disk_forks_logic (
        config ) . await
    }

async fn test_none_forks_get_replaced_with_disk_forks_logic (
  config : &SkgConfig,

) -> Result < (), Box<dyn Error> > {

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . forks = MSV::Unspecified; }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . forks),
      MSV::Specified ( vec![ ID::new ("sub_1_on_disk"),
                   ID::new ("sub_2_on_disk"),
                   ID::new ("override_1_on_disk"),
                   ID::new ("override_2_on_disk"),
                   ID::new ("override_3_on_disk") ]),
      "Unspecified forks from client should be replaced with forks from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . forks = MSV::Specified ( vec![] ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      result . forks,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) forks from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . forks = rel_partners_at_relRepo_msv (
        & SkgrepoName::from ("main"),
        MSV::Specified ( vec![ ID::new ("new_sub") ] ) ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . forks),
      MSV::Specified ( vec![ ID::new ("new_sub") ] ),
      "forks from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }

async fn test_none_hides_get_replaced_with_disk_hides (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_none_hides_get_replaced_with_disk_hides_logic (
        config ) . await
    }

async fn test_none_hides_get_replaced_with_disk_hides_logic (
  config : &SkgConfig,

) -> Result < (), Box<dyn Error> > {

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hides = MSV::Unspecified; }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . hides),
      MSV::Specified ( vec![ ID::new ("hide_1_on_disk") ]),
      "Unspecified hides from client should be replaced with hides from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hides = MSV::Specified ( vec![] ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      result . hides,
      MSV::Specified ( vec![] ),
      "Specified ( [] ) hides from client should be preserved, not replaced by data from disk." ); }

  { let mut user_node : Graphnode = empty_graphnode ();
    { user_node . title   = "Title from user" . to_string ();
      user_node . pid     = ID::new ("test_node");
      user_node . hides = rel_partners_at_relRepo_msv (
        & SkgrepoName::from ("main"),
        MSV::Specified ( vec![ ID::new ("new_hide") ] ) ); }
    let result : Graphnode =
      supplement_from_disk_then_extract_graphnode (
        &config, user_node ) . await ?;
    assert_eq! (
      members_msv (&result . hides),
      MSV::Specified ( vec![ ID::new ("new_hide") ] ),
      "hides from client should be preserved, not replaced by data from disk." ); }

  Ok (( )) }
