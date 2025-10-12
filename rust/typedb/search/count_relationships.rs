use std::error::Error;
use std::collections::HashMap;
use typedb_driver::{
  answer::{QueryAnswer, ConceptDocument},
  answer::concept_document::Node,
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use futures::StreamExt;

use crate::types::ID;
use crate::typedb::util::{
  extract_id_from_node,
  build_id_disjunction};


/// PURPOSE: Count how many times each node appears as 'contained'
/// in a 'contains' relationship.
/// Takes a vector of IDs and returns a map from each ID to its count.
/// IDs with count 0 are included in the result.
pub async fn count_containers (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids : &[ID]
) -> Result < HashMap < ID, usize >, Box < dyn Error > > {

  if ids . is_empty () {
    return Ok ( HashMap::new () ); }

  let mut result : HashMap < ID, usize > =
    HashMap::new ();

  // Initialize all IDs with count 0
  for id in ids {
    result . insert ( id . clone (), 0 ); }

  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;

  let disjunction_clauses : String =
    build_id_disjunction ( ids, "node_id" );

  // Query to find all containers for each node in the input set
  let query : String =
    format! (
      r#"match
        {{ $node isa node, has id $node_id; }} or
        {{ $e isa extra_id, has id $node_id;
           $rel isa has_extra_id ( extra_id: $e ); }};
        {};
        fetch {{
          "node_id": $node_id,
          "containers": [
            match
              {{ $container isa node, has id $container_id;
                 $contained isa node, has id $node_id;
                 $rel2 isa contains ( container: $container,
                                      contained: $contained ); }} or
              {{ $container isa node, has id $container_id;
                 $e2 isa extra_id, has id $node_id;
                 $contained isa node;
                 $extra_rel isa has_extra_id ( node: $contained,
                                               extra_id: $e2 );
                 $rel2 isa contains ( container: $container,
                                      contained: $contained ); }};
            fetch {{ "container_id": $container_id }};
          ]
        }};"#,
      disjunction_clauses );

  let answer : QueryAnswer =
    tx . query ( query ) . await ?;

  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = answer
  { // Process the nested subquery results
    while let Some ( doc_result )
      = stream . next () . await
    { let doc : ConceptDocument =
        doc_result ?;

      if let Some ( Node::Map ( ref map ) ) = doc . root {
        // Extract the node_id
        let node_id_opt : Option < ID > =
          map . get ( "node_id" )
          . and_then ( extract_id_from_node );

        if let Some ( node_id ) = node_id_opt {
          // Count the containers
          if let Some ( Node::List ( containers_list ) ) =
            map . get ( "containers" )
          {
            let count : usize = containers_list . len ();
            result . insert ( node_id, count );
          }
        }
      }
    }
  }

  Ok ( result )
}
