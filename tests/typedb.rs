// cargo test --test typedb -- --nocapture

use skg::render::single_root_view;
use skg::save::orgfile_to_orgnodes::parse_skg_org_to_nodes;
use skg::typedb::init::populate_test_db_from_fixtures;
use skg::typedb::nodes::create_only_nodes_with_no_ids_present;
use skg::typedb::relationships::delete_out_links;
use skg::typedb::search::extract_payload_from_typedb_string_rep;
use skg::typedb::search::find_container_of;
use skg::typedb::search::pid_from_id;
use skg::types::{ID, SkgNode, OrgNodeInterp, NodeWithEphem, SkgConfig};

use futures::StreamExt;
use futures::executor::block_on;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{
  answer::{ ConceptRow, QueryAnswer },
  Credentials,
  DriverOptions,
  Transaction,
  TransactionType,
  TypeDBDriver, };

#[test]
fn test_typedb_integration (
) -> Result<(), Box<dyn Error>> {
  // Use block_on to run async code in a synchronous test
  block_on(async {
    let driver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
    let config = SkgConfig {
      db_name        : "skg-test"              . into(),
      skg_folder     : "tests/typedb/fixtures" . into(),
      tantivy_folder : "irrelevant"            . into() };
    let index_folder : &str =
      config . skg_folder . to_str ()
      . expect ("Invalid UTF-8 in tantivy index path");

    populate_test_db_from_fixtures (
      index_folder,
      & config . db_name,
      & driver
    ) . await ?;

    let path_to_4 = pid_from_id ( & config . db_name,
                                    & driver,
                                    & ID("4".to_string() ),
    ) . await ?;
    let path_to_44 = pid_from_id ( & config . db_name,
                                     & driver,
                                     & ID("44".to_string() )
    ) . await ?;
    assert_eq!(path_to_4,  ID("4" . to_string () ));
    assert_eq!(path_to_44, ID("4" . to_string () ));

    let has_extra_id_pairs = collect_all_of_some_binary_rel(
      & config . db_name,
      & driver,
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
      & config . db_name,
      & driver,
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
      & config . db_name,
      & driver,
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
      & config . db_name,
      & driver,
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
      & config . db_name,
      & driver,
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
      & config . db_name,
      & driver,
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
      & config . db_name,
      & driver,
      & ID("2".to_string() )
    ).await?;
    assert_eq!(container_node, ID("1".to_string() ) );

    test_recursive_document (
      & driver, & config
    ) . await ?;
    test_create_only_nodes_with_no_ids_present (
      & config . db_name,
      & driver,
    ) . await ?;
    test_delete_out_links_contains_container (
      & config . db_name,
      & driver,
    ) . await ?;

    Ok (( )) } ) }

pub async fn test_delete_out_links_contains_container (
  db_name : &str,
  driver  : &TypeDBDriver
) -> Result < (), Box<dyn Error> > {
  // The README at fixtures-2/ draws the initial contains tree.
  populate_test_db_from_fixtures (
    "tests/typedb/fixtures-2/",
    db_name,
    driver
  ) . await ?;

  // Sanity: initial contains edges include the full tree.
  let before_pairs = collect_all_of_some_binary_rel(
    db_name,
    driver,
    r#" match
          $inner isa node, has id $inner_id;
          $outer isa node, has id $outer_id;
          $rel isa contains (container: $inner,
                             contained: $outer);
        select $inner_id, $outer_id;"#,
    "inner_id",
    "outer_id"
  ).await?;
  let mut expected_before = std::collections::HashSet::new();
  expected_before.insert(("1".to_string(), "2".to_string()));
  expected_before.insert(("1".to_string(), "3".to_string()));
  expected_before.insert(("3".to_string(), "4".to_string()));
  expected_before.insert(("3".to_string(), "5".to_string()));
  assert_eq!(before_pairs, expected_before);

  // Delete relationship to contents for nodes 3, 4, 5.
  let deleted_for_ids : usize =
    delete_out_links (
      db_name,
      driver,
      &vec![ ID::from("3"),
             ID::from("4"),
             ID::from("5") ],
      "contains",
      "container"
    ) . await ?;
  assert_eq!(deleted_for_ids, 3);

  // Only the relationships (1→2) and (1→3) should remain.
  let after_pairs = collect_all_of_some_binary_rel (
    db_name,
    driver,
    r#" match
          $container isa node, has id $container_id;
          $contained isa node, has id $contained_id;
          $rel isa contains (container: $container,
                             contained: $contained);
        select $container_id, $contained_id;"#,
    "container_id",
    "contained_id"
  ).await?;
  let mut expected_after = std::collections::HashSet::new();
  expected_after.insert(("1".to_string(), "2".to_string()));
  expected_after.insert(("1".to_string(), "3".to_string()));
  assert_eq!(after_pairs, expected_after);
  Ok (( )) }

pub async fn test_create_only_nodes_with_no_ids_present (
  db_name : &str,
  driver  : &TypeDBDriver
) -> Result < (), Box<dyn Error> > {

  // Rebuild DB from fixtures and schema.
  populate_test_db_from_fixtures (
    "tests/typedb/fixtures/",
    db_name,
    driver
  ) . await ?;

  // Baseline node count.
  let initial_number_of_nodes : usize =
    count_nodes ( db_name, driver ) . await ?;

  // Prepare two SkgNodes: one existing ("a"), one new ("new").
  // Keep other fields minimal/empty as requested.
  let fn_a  : SkgNode = SkgNode {
    title                    : "ignore this string" . to_string (),
    aliases                  : None,
    ids                      : vec![ ID::from ( "a" ) ],
    body                     : None,
    contains                 : vec![],
    subscribes_to            : vec![],
    hides_from_its_subscriptions : vec![],
    overrides_view_of        : vec![],
  };
  let fn_new : SkgNode = SkgNode {
    title                    : "ignore this string" . to_string (),
    aliases                  : None,
    ids                      : vec![ ID::from ( "new" ) ],
    body                     : None,
    contains                 : vec![],
    subscribes_to            : vec![],
    hides_from_its_subscriptions : vec![],
    overrides_view_of        : vec![],
  };

  // Attempt to create only unknown nodes among { "a", "new" }.
  // Expect exactly 1 creation (the id "new").
  let created_count : usize =
    create_only_nodes_with_no_ids_present (
      db_name,
      driver,
      &vec! [ fn_a.clone (), fn_new.clone () ]
    ) . await ?;
  assert_eq! (
    created_count, 1,
    "Exactly one node should be created (with id 'new')." );

  // Lookups:
  // - 'a' should exist (from fixtures)
  // - 'new' should now exist (just created)
  // - 'absent' should not exist
  assert! (
    pid_from_id ( db_name, driver, &ID::from ( "a" ) )
      . await . is_ok (),
    "Expected lookup of existing id 'a' to succeed." );

  assert! (
    pid_from_id ( db_name, driver, &ID::from ( "new" ) )
      . await . is_ok (),
    "Expected lookup of newly created id 'new' to succeed." );

  assert! (
    pid_from_id ( db_name, driver, &ID::from ( "absent" ) )
      . await . is_err (),
    "Expected lookup of missing id 'absent' to fail." );

  // Final count should be +1 compared to initial.
  let final_number_of_nodes : usize =
    count_nodes ( db_name, driver ) . await ?;
  assert_eq! (
    final_number_of_nodes,
    initial_number_of_nodes + 1,
    "SkgNode count should increase by exactly 1 (only 'new' was created)." );

  Ok ( () ) }

/// Helper: count all `node` entities in the DB by streaming rows.
/// (Avoids reliance on `count;` semantics.)
async fn count_nodes (
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result < usize, Box<dyn Error> > {

  let tx : Transaction =
    driver . transaction (
      db_name,
      TransactionType::Read
    ) . await ?;

  let answer : QueryAnswer =
    tx . query (
      r#"match
           $n isa node, has id $id;
         select $id;"#
    ) . await ?;

  let mut rows = answer.into_rows ();
  let mut n : usize = 0;
  while let Some ( row_res ) = rows . next () . await {
    let _row : ConceptRow = row_res ?;
    n += 1; }
  Ok ( n ) }

async fn test_recursive_document (
  driver  : &TypeDBDriver,
  config  : &SkgConfig
) -> Result<(), Box<dyn Error>> {
  let result_org_text : String =
    single_root_view (
      driver,
      config,
      &ID ( "a".to_string () )
    ) . await ?;
  let result_forest : Vec<OrgNodeInterp> =
    parse_skg_org_to_nodes (
      & result_org_text );

  // Expected OrgNodeInterp tree
  //
  // a [focused]
  // └─ b (body "b has a body")
  //    └─ c
  //       └─ b [repeated]
  //            (body "Repeated above. Edit there, not here.")
  let expected_forest : Vec<OrgNodeInterp> =
    vec! [ OrgNodeInterp::Content(NodeWithEphem {
      id       : Some ( ID::from ("a") ),
      title    : "a" . to_string (),
      aliases  : None,
      body     : None,
      folded   : false,
      focused  : false,
      repeated : false,
      branches : vec! [ OrgNodeInterp::Content(NodeWithEphem {
        id       : Some ( ID::from ("b") ),
        title    : "b" . to_string (),
        aliases  : None,
        body     : Some ( "b has a body" . to_string () ),
        folded   : false,
        focused  : false,
        repeated : false,
        branches : vec! [ OrgNodeInterp::Content(NodeWithEphem {
          id       : Some ( ID::from ("c") ),
          aliases  : None,
          title    : "c" . to_string (),
          body     : None,
          folded   : false,
          focused  : false,
          repeated : false,
          branches : vec! [ OrgNodeInterp::Content(NodeWithEphem {
            id       : Some ( ID::from ("b") ),
            title    : "b" . to_string (),
            aliases  : None,
            body     : None, // PITFALL: There *is* a body in the received org text. But it is discarded, because the node is a repeat.
            folded   : false,
            focused  : false,
            repeated : true,
            branches : vec! [], }) ], }) ], }) ], }) ];

  // With PartialEq derived on OrgNodeInterp, we can compare the full structures directly.
  assert_eq! (
    result_forest,
    expected_forest,
    "Rendered OrgNodeInterp forest does not match expected." );
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
