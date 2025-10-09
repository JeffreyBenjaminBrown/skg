// cargo test test_containerward_view

use indoc::indoc;
use skg::typedb::init::populate_test_db_from_fixtures;
use skg::mk_org_text::containerward_org_view;
use skg::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  Credentials,
  DriverOptions,
  TypeDBDriver, };

#[test]
fn test_containerward_view (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_database () . await ?;
    test_containerward_org_view (
      &config, &driver ) . await ?;
    Ok (( )) } ) }

async fn setup_test_database (
) -> Result < ( SkgConfig, TypeDBDriver ), Box<dyn Error> > {
  let config : SkgConfig =
    SkgConfig {
      db_name        : "skg-test-containerward-view"       . into(),
      skg_folder     : "tests/containerward_view/fixtures" . into(),
      tantivy_folder : "irrelevant"                        . into(),
      port           : 1730 };
  let index_folder : &str =
    config . skg_folder . to_str ()
    . expect ("Invalid UTF-8 in tantivy index path");
  let driver : TypeDBDriver =
    TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
  populate_test_db_from_fixtures (
    index_folder,
    & config . db_name,
    & driver ). await ?;
  Ok (( config, driver )) }

async fn test_containerward_org_view (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {

  // A root.
  let result1 = containerward_org_view(
    driver, config, &ID("1".to_string()), 1).await?;
  let expected1 = "* <skg<id:1>> 1\n";
  assert_eq!(result1, expected1);

  // A linear path.
  let result2 = containerward_org_view(
    driver, config, &ID("111".to_string()), 2).await?;
  let expected2 = indoc! {"
    ** <skg<id:111>> 111
    *** <skg<id:11,relToOrgParent:container,mightContainMore>> 11
    **** <skg<id:1,relToOrgParent:container,mightContainMore>> 1
    "};
  assert_eq!(result2, expected2);

  // A cycle.
  let result3 = containerward_org_view(
    driver, config, &ID("2".to_string()), 1).await?;
  let expected3 = indoc! {"
    * <skg<id:2,cycle>> 2
    ** <skg<id:211,relToOrgParent:container,mightContainMore>> 211
    *** <skg<id:21,relToOrgParent:container,mightContainMore>> 21
    **** <skg<id:2,relToOrgParent:container,cycle,mightContainMore>> 2
    "};
  assert_eq!(result3, expected3);

  // An immediately forked containment path.
  // (Fork order is undefined, so both options are considered.)
  let result4 = containerward_org_view(
    driver, config, &ID("shared".to_string()), 3).await?;
  let expected4a = indoc! {"
    *** <skg<id:shared>> shared
    **** <skg<id:1,relToOrgParent:container,mightContainMore>> 1
    **** <skg<id:2,relToOrgParent:container,mightContainMore>> 2
    "};
  let expected4b = indoc! {"
    *** <skg<id:shared>> shared
    **** <skg<id:2,relToOrgParent:container,mightContainMore>> 2
    **** <skg<id:1,relToOrgParent:container,mightContainMore>> 1
    "};
  assert!(result4 == expected4a ||
          result4 == expected4b,
          "Expected one of {:?} or {:?}, got {:?}",
          expected4a, expected4b, result4);

  // An eventually-forking path, with body text at its terminus.
  let result5 = containerward_org_view(
    driver, config, &ID("shared_1".to_string()), 1).await?;
  let expected5a = indoc! {"
    * <skg<id:shared_1>> shared_1
    Some body text.
    The second line of body text.
    ** <skg<id:shared,relToOrgParent:container,mightContainMore>> shared
    *** <skg<id:1,relToOrgParent:container,mightContainMore>> 1
    *** <skg<id:2,relToOrgParent:container,mightContainMore>> 2
    "};
  let expected5b = indoc! {"
    * <skg<id:shared_1>> shared_1
    Some body text.
    The second line of body text.
    ** <skg<id:shared,relToOrgParent:container,mightContainMore>> shared
    *** <skg<id:2,relToOrgParent:container,mightContainMore>> 2
    *** <skg<id:1,relToOrgParent:container,mightContainMore>> 1
    "};
  assert!(result5 == expected5a ||
          result5 == expected5b,
          "Expected one of {:?} or {:?}, got {:?}",
          expected5a, expected5b, result5);

  // A fork and a cycle, but the cycle lies after the fork,
  // so it is not discovered.
  let result6 = containerward_org_view(
    driver, config, &ID("shared_cyclic".to_string()), 1).await?;
  let expected6a = indoc! {"
    * <skg<id:shared_cyclic>> shared_cyclic
    ** <skg<id:shared_cyclic_1,relToOrgParent:container,mightContainMore>> shared_cyclic_1
    ** <skg<id:uncyclic_container,relToOrgParent:container,mightContainMore>> uncyclic_container
    "};
  let expected6b = indoc! {"
    * <skg<id:shared_cyclic>> shared_cyclic
    ** <skg<id:uncyclic_container,relToOrgParent:container,mightContainMore>> uncyclic_container
    ** <skg<id:shared_cyclic_1,relToOrgParent:container,mightContainMore>> shared_cyclic_1
    "};
  assert!(result6 == expected6a ||
          result6 == expected6b,
          "Expected one of {:?} or {:?}, got {:?}",
          expected6a, expected6b, result6);

  // Starting at a different point,
  // both the fork and the cycle are discovered.
  let result7 = containerward_org_view(
    driver, config, &ID("shared_cyclic_1".to_string()), 1).await?;
  let expected7a = indoc! {"
    * <skg<id:shared_cyclic_1,cycle>> shared_cyclic_1
    ** <skg<id:shared_cyclic,relToOrgParent:container,mightContainMore>> shared_cyclic
    *** <skg<id:shared_cyclic_1,relToOrgParent:container,cycle,mightContainMore>> shared_cyclic_1
    *** <skg<id:uncyclic_container,relToOrgParent:container,mightContainMore>> uncyclic_container
    "};
  let expected7b = indoc! {"
    * <skg<id:shared_cyclic_1,cycle>> shared_cyclic_1
    ** <skg<id:shared_cyclic,relToOrgParent:container,mightContainMore>> shared_cyclic
    *** <skg<id:uncyclic_container,relToOrgParent:container,mightContainMore>> uncyclic_container
    *** <skg<id:shared_cyclic_1,relToOrgParent:container,cycle,mightContainMore>> shared_cyclic_1
    "};
  assert!(result7 == expected7a ||
          result7 == expected7b,
          "Expected one of {:?} or {:?}, got {:?}",
          expected7a, expected7b, result7);

  Ok (( )) }
