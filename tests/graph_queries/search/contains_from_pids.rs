// cargo test grouped_sources graph_queries::search::contains_from_pids

use skg::test_utils::run_with_test_graph;
use skg::dbs::graph_queries::relations::contains_from_pids;
use skg::types::misc::ID;

use std::collections::{HashMap, HashSet};
use std::error::Error;

#[test]
fn the_tests (
) -> Result<(), Box<dyn Error>> {
  run_with_test_graph (
    "skg-test-graph-search-contains-from-pids",
    "tests/graph_queries/search/contains_from_pids/fixtures",
    "/tmp/tantivy-test-graph-search-contains-from-pids",
    |_config, graph, _tantivy| Box::pin ( async move {
      test_contains_from_pids (
        &graph . load_full () . graph ) ?;
      Ok (( )) } )
  ) }

fn test_contains_from_pids (
  graph : &skg::dbs::in_rust_graph::InRustGraph,
) -> Result<(), Box<dyn Error>> {

  let input_pids : Vec<ID> =
    vec! [ ID ( "1"  . to_string () ),
           ID ( "2"  . to_string () ),
           ID ( "3"  . to_string () ),
           ID ( "11" . to_string () ) ];

  let ( container_to_contents, content_to_containers )
    : ( HashMap < ID, HashSet < ID > >, HashMap < ID, HashSet < ID > > ) =
    contains_from_pids (graph, &input_pids, None);

  // Expected container_to_contents:
  //   1 -> {2, 3}
  //   3 -> {1}
  let mut expected_container_to_contents : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  expected_container_to_contents . insert (
    ID ( "1" . to_string () ),
    HashSet::from ( [ ID ( "2" . to_string () ),
                      ID ( "3" . to_string () ) ] ) );
  expected_container_to_contents . insert (
    ID ( "3" . to_string () ),
    HashSet::from ( [ ID ( "1" . to_string () ) ] ) );

  // Expected content_to_containers:
  //   2 -> {1}
  //   3 -> {1}
  //   1 -> {3}
  let mut expected_content_to_containers : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  expected_content_to_containers . insert (
    ID ( "2" . to_string () ),
    HashSet::from ( [ ID ( "1" . to_string () ) ] ) );
  expected_content_to_containers . insert (
    ID ( "3" . to_string () ),
    HashSet::from ( [ ID ( "1" . to_string () ) ] ) );
  expected_content_to_containers . insert (
    ID ( "1" . to_string () ),
    HashSet::from ( [ ID ( "3" . to_string () ) ] ) );

  assert_eq! ( container_to_contents, expected_container_to_contents );
  assert_eq! ( content_to_containers, expected_content_to_containers );

  Ok (( )) }
