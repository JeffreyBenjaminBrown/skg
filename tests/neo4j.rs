// cargo test --test neo4j -- --nocapture

mod neo4j {
  pub mod search;
  pub mod delete_nodes;
  pub mod update_neo4j_from_saveinstructions;
}

mod util {
  pub mod search;
}

use ego_tree::Tree;
use skg::dbs::neo4j::nodes::create_only_nodes_with_no_ids_present;
use skg::dbs::neo4j::relationships::delete_out_links;
use skg::dbs::neo4j::search::pid_and_source_from_id;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::{ID, SkgConfig};
use skg::types::skgnode::{SkgNode, empty_skgnode};
use skg::types::unchecked_viewnode::{UncheckedViewNode, unchecked_to_checked_tree};
use skg::types::viewnode::{ViewNode, ViewNodeKind};

use neo4rs::Graph;
use std::collections::HashSet;
use std::error::Error;

#[test]
fn test_neo4j_all_relationships (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-neo4j-relationships",
    "tests/typedb/fixtures",
    "/tmp/tantivy-test-neo4j-relationships",
    |config, graph, _tantivy| Box::pin ( async move {
      test_all_relationships ( config, graph ) . await ?;
      Ok (( )) } ) ) }

#[test]
fn test_neo4j_recursive_document (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-neo4j-recursive",
    "tests/typedb/fixtures",
    "/tmp/tantivy-test-neo4j-recursive",
    |config, graph, _tantivy| Box::pin ( async move {
      test_recursive_document ( graph, config ) . await ?;
      Ok (( )) } ) ) }

#[test]
fn test_neo4j_create_only_nodes (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-neo4j-create-nodes",
    "tests/typedb/fixtures",
    "/tmp/tantivy-test-neo4j-create-nodes",
    |config, graph, _tantivy| Box::pin ( async move {
      test_create_only_nodes_with_no_ids_present (
        graph ) . await ?;
      Ok (( )) } ) ) }

#[test]
fn test_neo4j_delete_out_links (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-neo4j-delete-links",
    "tests/typedb/fixtures-2",
    "/tmp/tantivy-test-neo4j-delete-links",
    |config, graph, _tantivy| Box::pin ( async move {
      test_delete_out_links_contains_container (
        graph ) . await ?;
      Ok (( )) } ) ) }

async fn test_all_relationships (
  config : &SkgConfig,
  graph  : &Graph
) -> Result<(), Box<dyn Error>> {

  let has_extra_id_pairs: HashSet<(String, String)> =
    collect_all_of_some_binary_rel(
      graph,
      "MATCH (a:IdAlias) RETURN a.primary_id AS pid, a.id AS alias_id",
      "pid",
      "alias_id"
    ).await?;
  let expected_has_extra_id: HashSet<_> = [
    ("2", "22"),
    ("3", "33"),
    ("4", "44"),
    ("5", "55"), ].iter()
    .map(|(a, b)| (a.to_string(), b.to_string()))
    .collect();
  assert_eq!(has_extra_id_pairs, expected_has_extra_id);

  let contains_pairs: HashSet<(String, String)> =
    collect_all_of_some_binary_rel(
      graph,
      "MATCH (a:Node)-[:contains]->(b:Node) RETURN a.id AS container_id, b.id AS contained_id",
      "container_id",
      "contained_id"
    ).await?;
  let mut expected_contains: HashSet<(String, String)> = HashSet::new();
  expected_contains.insert(("1".to_string(), "2".to_string()));
  expected_contains.insert(("1".to_string(), "3".to_string()));
  expected_contains.insert(("a".to_string(), "b".to_string()));
  expected_contains.insert(("b".to_string(), "c".to_string()));
  expected_contains.insert(("c".to_string(), "b".to_string()));
  assert_eq!(contains_pairs, expected_contains);

  let textlink_pairs: HashSet<(String, String)> =
    collect_all_of_some_binary_rel(
      graph,
      "MATCH (a:Node)-[:textlinks_to]->(b:Node) RETURN a.id AS source_id, b.id AS dest_id",
      "source_id",
      "dest_id"
    ).await?;
  let mut expected_textlinks: HashSet<(String, String)> = HashSet::new();
  expected_textlinks.insert(("5".to_string(), "2".to_string()));
  expected_textlinks.insert(("5".to_string(), "3".to_string()));
  expected_textlinks.insert(("5".to_string(), "5".to_string()));
  assert_eq!(textlink_pairs, expected_textlinks);

  let subscribes_pairs: HashSet<(String, String)> =
    collect_all_of_some_binary_rel(
      graph,
      "MATCH (a:Node)-[:subscribes]->(b:Node) RETURN a.id AS from_id, b.id AS to_id",
      "from_id",
      "to_id"
    ).await?;
  let expected_subscribes : HashSet<_> = [
    ("2", "4"),
    ("2", "5"),
    ("3", "4"),
    ("3", "5"), ].iter()
    .map(|(a, b)| (a.to_string(), b.to_string()))
    .collect();
  assert_eq!(subscribes_pairs, expected_subscribes);

  let hides_pairs: HashSet<(String, String)> =
    collect_all_of_some_binary_rel(
      graph,
      "MATCH (a:Node)-[:hides]->(b:Node) RETURN a.id AS from_id, b.id AS to_id",
      "from_id",
      "to_id"
    ).await?;
  let mut expected_hides_from_its_subscriptions: HashSet<(String, String)> = HashSet::new();
  expected_hides_from_its_subscriptions.insert((
    "1".to_string(), "4".to_string()));
  expected_hides_from_its_subscriptions.insert((
    "1".to_string(), "5".to_string()));
  assert_eq!(hides_pairs, expected_hides_from_its_subscriptions);

  let replacement_pairs: HashSet<(String, String)> =
    collect_all_of_some_binary_rel(
      graph,
      "MATCH (a:Node)-[:overrides]->(b:Node) RETURN a.id AS from_id, b.id AS to_id",
      "from_id",
      "to_id"
    ).await?;
  let mut expected_replacements: HashSet<(String, String)> = HashSet::new();
  expected_replacements.insert((
    "5".to_string(), "3".to_string()));
  expected_replacements.insert((
    "5".to_string(), "4".to_string()));
  assert_eq!(replacement_pairs, expected_replacements);

  Ok (( )) }

async fn test_delete_out_links_contains_container (
  graph : &Graph
) -> Result < (), Box<dyn Error> > {
  // The README at fixtures-2/ draws the initial contains tree.
  // Sanity: initial contains edges include the full tree.
  let before_pairs: HashSet<(String, String)> =
    collect_all_of_some_binary_rel(
      graph,
      "MATCH (a:Node)-[:contains]->(b:Node) RETURN a.id AS inner_id, b.id AS outer_id",
      "inner_id",
      "outer_id"
    ).await?;
  let mut expected_before: HashSet<(String, String)> = std::collections::HashSet::new();
  expected_before.insert(("1".to_string(), "2".to_string()));
  expected_before.insert(("1".to_string(), "3".to_string()));
  expected_before.insert(("3".to_string(), "4".to_string()));
  expected_before.insert(("3".to_string(), "5".to_string()));
  assert_eq!(before_pairs, expected_before);

  // Delete relationship to contents for nodes 3, 4, 5.
  delete_out_links (
    graph,
    &vec![ ID::from("3"),
           ID::from("4"),
           ID::from("5") ],
    "contains"
  ) . await ?;

  // Only the relationships (1->2) and (1->3) should remain.
  let after_pairs: HashSet<(String, String)> =
    collect_all_of_some_binary_rel (
      graph,
      "MATCH (a:Node)-[:contains]->(b:Node) RETURN a.id AS container_id, b.id AS contained_id",
      "container_id",
      "contained_id"
    ).await?;
  let mut expected_after: HashSet<(String, String)> = std::collections::HashSet::new();
  expected_after.insert(("1".to_string(), "2".to_string()));
  expected_after.insert(("1".to_string(), "3".to_string()));
  assert_eq!(after_pairs, expected_after);
  Ok (( )) }

async fn test_create_only_nodes_with_no_ids_present (
  graph : &Graph
) -> Result < (), Box<dyn Error> > {

  // Baseline node count.
  let initial_number_of_nodes : usize =
    count_nodes ( graph ) . await ?;

  // Prepare two SkgNodes: one existing ("a"), one new ("new").
  // Keep other fields minimal/empty as requested.
  let mut fn_a : SkgNode = empty_skgnode ();
  { fn_a.title = "ignore this string" . to_string ();
    fn_a.ids   = vec![ ID::from ( "a" ) ]; }

  let mut fn_new : SkgNode = empty_skgnode ();
  { fn_new.title = "ignore this string" . to_string ();
    fn_new.ids   = vec![ ID::from ( "new" ) ]; }

  // Attempt to create only unknown nodes among { "a", "new" }.
  // Expect exactly 1 creation (the id "new").
  let created_count : usize =
    create_only_nodes_with_no_ids_present (
      graph,
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
    pid_and_source_from_id ( graph, &ID::from ( "a" ) )
      . await . unwrap () . is_some (),
    "Expected lookup of existing id 'a' to return Some." );

  assert! (
    pid_and_source_from_id ( graph, &ID::from ( "new" ) )
      . await . unwrap () . is_some (),
    "Expected lookup of newly created id 'new' to return Some." );

  assert! (
    pid_and_source_from_id ( graph, &ID::from ( "absent" ) )
      . await . unwrap () . is_none (),
    "Expected lookup of missing id 'absent' to return None." );

  // Final count should be +1 compared to initial.
  let final_number_of_nodes : usize =
    count_nodes ( graph ) . await ?;
  assert_eq! (
    final_number_of_nodes,
    initial_number_of_nodes + 1,
    "SkgNode count should increase by exactly 1 (only 'new' was created)." );

  Ok ( () ) }

/// Helper: count all Node entities in Neo4j.
async fn count_nodes (
  graph : &Graph,
) -> Result < usize, Box<dyn Error> > {
  let mut result_stream = graph.execute(
    neo4rs::query("MATCH (n:Node) RETURN count(n) AS cnt")
  ).await?;
  let row = result_stream.next().await?.unwrap();
  let cnt: i64 = row.get("cnt")?;
  Ok(cnt as usize) }

async fn test_recursive_document (
  graph  : &Graph,
  config : &SkgConfig
) -> Result<(), Box<dyn Error>> {
  let result_org_text : String =
    single_root_view (
      graph,
      config,
      &ID ( "a".to_string () ),
      false
    ) . await ?;
  let unchecked_forest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes ( & result_org_text )
    . map_err ( |e| format! ( "Parse error: {}", e ) ) ? . 0;
  let result_forest : Tree<ViewNode> =
    unchecked_to_checked_tree ( unchecked_forest )
    . map_err ( |e| format! ( "Check error: {}", e ) ) ?;

  let tree_roots : Vec<_> =
    result_forest.root().children().collect();
  assert_eq! ( tree_roots.len (), 1,
    "Expected exactly 1 root node" );

  let root_node_ref = &tree_roots[0];
  let root_node : &ViewNode = root_node_ref . value ();

  // Root node should be "a"
  assert! ( matches! ( &root_node.kind, ViewNodeKind::True (_) ),
    "should be TrueNode" );
  let ViewNodeKind::True ( root_t ) = &root_node.kind
    else { unreachable!() };
  assert_eq! ( &root_t.id, &ID::from ("a"),
    "Root node should have id 'a'" );
  assert_eq! ( root_node.title(), "a",
    "Root node should have title 'a'" );

  // Root should have 1 child: "b"
  let mut root_children = root_node_ref . children ();
  let b_node_ref = root_children . next ()
    . expect ( "Root should have child 'b'" );
  let b_node : &ViewNode = b_node_ref . value ();

  assert! ( matches! ( &b_node.kind, ViewNodeKind::True (_) ),
    "should be TrueNode" );
  let ViewNodeKind::True ( b_t ) = &b_node.kind
    else { unreachable!() };
  assert_eq! ( &b_t.id, &ID::from ("b"),
    "First child should have id 'b'" );
  assert_eq! ( b_node.title(), "b",
    "First child should have title 'b'" );
  assert_eq! ( b_node.body(), Some ( &"b has a body" . to_string () ),
    "Node 'b' should have body 'b has a body'" );
  assert! ( ! b_t.indefinitive,
    "First occurrence of 'b' should not be marked as indefinitive" );

  // "b" should have 1 child: "c"
  let mut b_children = b_node_ref . children ();
  let c_node_ref = b_children . next ()
    . expect ( "Node 'b' should have child 'c'" );
  let c_node : &ViewNode = c_node_ref . value ();

  assert! ( matches! ( &c_node.kind, ViewNodeKind::True (_) ),
    "should be TrueNode" );
  let ViewNodeKind::True ( c_t ) = &c_node.kind
    else { unreachable!() };
  assert_eq! ( &c_t.id, &ID::from ("c"),
    "Child of 'b' should have id 'c'" );
  assert_eq! ( c_node.title(), "c",
    "Child of 'b' should have title 'c'" );

  // "c" should have 1 child: "b" (repeated)
  let mut c_children = c_node_ref . children ();
  let b_repeat_ref = c_children . next ()
    . expect ( "Node 'c' should have child 'b' (repeated)" );
  let b_repeat : &ViewNode = b_repeat_ref . value ();

  assert! ( matches! ( &b_repeat.kind, ViewNodeKind::True (_) ),
    "should be TrueNode" );
  let ViewNodeKind::True ( b_repeat_t ) = &b_repeat.kind
    else { unreachable!() };
  assert_eq! ( &b_repeat_t.id, &ID::from ("b"),
    "Child of 'c' should have id 'b'" );
  assert_eq! ( b_repeat.title(), "b",
    "Repeated node should have title 'b'" );
  assert! ( b_repeat_t.indefinitive,
    "Second occurrence of 'b' should be marked as indefinitive" );

  // Repeated "b" should have no children (body and children ignored for repeated nodes)
  assert! ( b_repeat_ref . children () . next () . is_none (),
    "Repeated node 'b' should have no children" );

  Ok (( )) }

async fn collect_all_of_some_binary_rel(
  graph : &Graph,
  query : &str,
  col1  : &str,
  col2  : &str,
) -> Result<HashSet<(String, String)>, Box<dyn Error>> {
  let mut result_stream = graph.execute(neo4rs::query(query)).await?;
  let mut results: HashSet<(String, String)> = HashSet::new();
  while let Some(row) = result_stream.next().await? {
    let v1: String = row.get(col1)?;
    let v2: String = row.get(col2)?;
    results.insert((v1, v2)); }
  Ok(results) }
