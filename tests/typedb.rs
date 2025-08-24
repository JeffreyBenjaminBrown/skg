// cargo test --test typedb -- --nocapture

// TODO | PITFALL:
// Deletes any existing TypeDB database named `skg-test`,

use futures::executor::block_on;
use futures::StreamExt;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{
  Credentials,
  DriverOptions,
  TransactionType,
  TypeDBDriver, };

use skg::render::org::single_root_content_view;
use skg::save::orgfile_to_orgnodes::parse_skg_org_to_nodes;
use skg::typedb::create::overwrite_and_populate_new_db;
use skg::typedb::search::{
  extract_payload_from_typedb_string_rep,
  find_container_of,
  filepath_from_id_via_typedb, };
use skg::types::{ID, OrgNode};

#[test]
fn test_typedb_integration(
) -> Result<(), Box<dyn Error>> {
  // Use block_on to run async code in a synchronous test
  block_on(async {
    let driver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
    let db_name = "skg-test";

    overwrite_and_populate_new_db (
      "tests/typedb/fixtures", db_name, &driver )
      . await?;

    let path_to_4 = filepath_from_id_via_typedb (
      db_name, &driver, &ID("4".to_string() ) ) . await?;
    let path_to_44 = filepath_from_id_via_typedb (
      db_name, &driver, &ID("44".to_string() ) ) . await?;
    assert_eq!(path_to_4,  "tests/typedb/fixtures/4.skg");
    assert_eq!(path_to_44, "tests/typedb/fixtures/4.skg");

    let has_extra_id_pairs = collect_all_of_some_binary_rel(
      db_name,
      &driver,
      r#" match
            $n isa node, has id $ni;
            $e isa extra_id, has id $ei;
            $rel isa has_extra_id (node: $n,
                                   extra_id: $e);
          select $ni, $ei;"#,
      "ni",
      "ei"
    ).await?;
    let expected_has_extra_id: HashSet<_> = [
      ("2", "22"),
      ("3", "33"),
      ("4", "44"),
      ("5", "55"), ].iter()
      .map(|(a, b)| (a.to_string(), b.to_string()))
      .collect();
    assert_eq!(has_extra_id_pairs, expected_has_extra_id);

    let contains_pairs = collect_all_of_some_binary_rel(
      db_name,
      &driver,
      r#" match
            $container isa node, has id $container_id;
            $contained isa node, has id $contained_id;
            $rel isa contains (container: $container,
                               contained: $contained);
          select $container_id, $contained_id;"#,
      "container_id",
      "contained_id"
    ).await?;
    let mut expected_contains = HashSet::new();
    expected_contains.insert(("1".to_string(), "2".to_string()));
    expected_contains.insert(("1".to_string(), "3".to_string()));
    expected_contains.insert(("a".to_string(), "b".to_string()));
    expected_contains.insert(("b".to_string(), "c".to_string()));
    expected_contains.insert(("c".to_string(), "b".to_string()));
    assert_eq!(contains_pairs, expected_contains);

    let hyperlink_pairs = collect_all_of_some_binary_rel(
      db_name,
      &driver,
      r#" match
            $source isa node, has id $source_id;
            $dest   isa node, has id $dest_id;
            $rel    isa hyperlinks_to (source: $source,
                                       dest:   $dest);
          select $source_id, $dest_id;"#,
      "source_id",
      "dest_id"
    ).await?;
    let mut expected_contains = HashSet::new();
    expected_contains.insert(("5".to_string(), "2".to_string()));
    expected_contains.insert(("5".to_string(), "3".to_string()));
    expected_contains.insert(("5".to_string(), "5".to_string()));
    assert_eq!(hyperlink_pairs, expected_contains);

    let subscribes_pairs = collect_all_of_some_binary_rel(
      db_name,
      &driver,
      r#" match
            $subscriber isa node, has id $from;
            $subscribee isa node, has id $to;
            $rel isa subscribes (subscriber: $subscriber,
                                 subscribee: $subscribee);
          select $from, $to;"#,
      "from",
      "to"
    ).await?;
    let expected_subscribes : HashSet<_> = [
      ("2", "4"),
      ("2", "5"),
      ("3", "4"),
      ("3", "5"), ].iter()
      .map(|(a, b)| (a.to_string(), b.to_string()))
      .collect();
    assert_eq!(subscribes_pairs, expected_subscribes);

    let hides_pairs = collect_all_of_some_binary_rel(
      db_name,
      &driver,
      r#" match
            $hider isa node, has id $from;
            $hidden isa node, has id $to;
            $rel isa hides_from_its_subscriptions
              ( hider:  $hider,
                hidden: $hidden);
          select $from, $to;"#,
      "from",
      "to"
    ).await?;
    let mut expected_hides_from_its_subscriptions = HashSet::new();
    expected_hides_from_its_subscriptions.insert((
      "1".to_string(), "4".to_string()));
    expected_hides_from_its_subscriptions.insert((
      "1".to_string(), "5".to_string()));
    assert_eq!(hides_pairs, expected_hides_from_its_subscriptions);

    let replacement_pairs = collect_all_of_some_binary_rel(
      db_name,
      &driver,
      r#" match
            $replacement isa node, has id $from;
            $replaced isa node, has id $to;
            $rel isa overrides_view_of (replacement: $replacement,
                                        replaced:    $replaced);
          select $from, $to;"#,
      "from",
      "to"
    ).await?;
    let mut expected_replacements = HashSet::new();
    expected_replacements.insert((
      "5".to_string(), "3".to_string()));
    expected_replacements.insert((
      "5".to_string(), "4".to_string()));
    assert_eq!(replacement_pairs, expected_replacements);

    let container_node = find_container_of(
      db_name, &driver, &ID("2".to_string() )
    ).await?;
    assert_eq!(container_node, ID("1".to_string() ) );

    test_recursive_document ( db_name, &driver )
      . await ?;

    Ok (( )) } ) }

async fn test_recursive_document (
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let result_org_text : String =
    single_root_content_view (
      db_name,
      driver,
      &ID ( "a".to_string () )
    ) . await ?;
  let result_forest : Vec<OrgNode> =
    parse_skg_org_to_nodes (
      &result_org_text );

  // Expected OrgNode tree (mirrors the old s-exp structure).
  //
  // a [focused]
  // └─ b (body "b has a body")
  //    └─ c
  //       └─ b [repeated]
  //            (body "Repeated above. Edit there, not here.")
  let expected_forest : Vec<OrgNode> =
    vec! [ OrgNode {
      id       : Some ( ID::from ("a") ),
      heading  : "a" . to_string (),
      body     : None,
      folded   : false,
      focused  : false,
      repeated : false,
      branches : vec! [ OrgNode {
        id       : Some ( ID::from ("b") ),
        heading  : "b" . to_string (),
        body     : Some ( "b has a body" . to_string () ),
        folded   : false,
        focused  : false,
        repeated : false,
        branches : vec! [ OrgNode {
          id       : Some ( ID::from ("c") ),
          heading  : "c" . to_string (),
          body     : None,
          folded   : false,
          focused  : false,
          repeated : false,
          branches : vec! [ OrgNode {
            id       : Some ( ID::from ("b") ),
            heading  : "b" . to_string (),
            body     : None, // PITFALL: There *is* a body in the received org text. But it is discarded, because the node is a repeat.
            folded   : false,
            focused  : false,
            repeated : true,
            branches : vec! [], } ], } ], } ], } ];

  // With PartialEq derived on OrgNode, we can compare the full structures directly.
  assert_eq! (
    result_forest,
    expected_forest,
    "Rendered OrgNode forest does not match expected." );
  Ok (( )) }

async fn collect_all_of_some_binary_rel(
  db_name: &str,
  driver: &TypeDBDriver,
  query: &str,
  member1_variable: &str, // PITFALL: Must correspond to `query`. It's not the role name, but rather a variable, i.e. preceded with `$`.
  member2_variable: &str, // PITFALL: Must correspond to `query`. It's not the role name, but rather a variable, i.e. preceded with `$`.
) -> Result<HashSet<(String, String)>, Box<dyn Error>> {
  let tx = driver.transaction(
    db_name, TransactionType::Read).await?;
  let answer = tx.query(query).await?;
  let mut stream = answer.into_rows();
  let mut results: HashSet<(String, String)> = HashSet::new();

  while let Some(row_result) = stream.next().await {
    let row = row_result?;
    let id1_raw = match row.get(member1_variable)? {
      Some(c) => c.to_string(),
      None => "unknown".to_string() };
    let id2_raw = match row.get(member2_variable)? {
      Some(c) => c.to_string(),
      None => "unknown".to_string() };
    let id1 = ID ( extract_payload_from_typedb_string_rep (
      &id1_raw) );
    let id2 = ID ( extract_payload_from_typedb_string_rep (
      &id2_raw) );
    results.insert ( (id1.to_string(),
                      id2.to_string() ) ); }
  Ok (results) }
