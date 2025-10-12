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


/// Generic function to count relationships for a collection of nodes.
/// Counts how many relationships exist where each input node plays a specified role.
/// For example, to count containers: relation="contains", input_role="contained"
/// To count contents: relation="contains", input_role="container"
async fn count_relations (
  db_name    : &str,
  driver     : &TypeDBDriver,
  ids        : &[ID],
  relation   : &str,
  input_role : &str
) -> Result < HashMap < ID, usize >,
              Box < dyn Error > > {
  if ids . is_empty () {
    return Ok ( HashMap::new () ); }
  let mut result : HashMap < ID, usize > =
    HashMap::new ();
  for id in ids { // Initialize all in result IDs with count 0
    result . insert ( id . clone (), 0 ); }
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let disjunction_clauses : String =
    build_id_disjunction ( ids, "node_id" );
  let other_role : &str =
    if input_role == "contained" { "container"
    } else if input_role == "container" { "contained"
    } else { return Err ( format! (
      "Unknown role: {}", input_role ). into () ); };
  let query : String =
    format! ( r#"match
        {{ $node isa node, has id $node_id; }} or
        {{ $e isa extra_id, has id $node_id;
           $rel isa has_extra_id ( extra_id: $e ); }};
        {};
        fetch {{
          "node_id": $node_id,
          "related": [
            match
              {{ $other isa node, has id $other_id;
                 $input isa node, has id $node_id;
                 $rel2 isa {} ( {}: $input,
                                {}: $other ); }} or
              {{ $other isa node, has id $other_id;
                 $e2 isa extra_id, has id $node_id;
                 $input isa node;
                 $extra_rel isa has_extra_id ( node: $input,
                                               extra_id: $e2 );
                 $rel2 isa {} ( {}: $input,
                                {}: $other ); }};
            fetch {{ "other_id": $other_id }};
          ]
        }};"#,
      disjunction_clauses,
      relation, input_role, other_role,
      relation, input_role, other_role );
  let answer : QueryAnswer =
    tx . query ( query ). await ?;

  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = answer
  { // Process the nested subquery results
    while let Some ( doc_result )
      = stream . next () . await
    { let doc : ConceptDocument =
        doc_result ?;
      if let Some ( Node::Map ( ref map ) )
      = doc . root
      { // Extract the node_id
        let node_id_opt : Option < ID > =
          map . get ( "node_id" )
          . and_then ( extract_id_from_node );
        if let Some ( node_id ) = node_id_opt {
          if let Some ( Node::List ( related_list ) ) =
            map . get ( "related" )
          { // count themem
            let count : usize = related_list . len ();
            result . insert ( node_id, count );
          }} }} }
  Ok (result) }

/// PURPOSE: Count how many times each node appears as 'contained'
/// in a 'contains' relationship.
/// Takes a vector of IDs and returns a map from each ID to its count.
/// IDs with count 0 are included in the result.
pub async fn count_containers (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids : &[ID]
) -> Result < HashMap < ID, usize >,
              Box < dyn Error > > {
  count_relations ( db_name,
                    driver,
                    ids,
                    "contains",
                    "contained"
  ). await }

/// PURPOSE: Count how many nodes each node contains
/// in a 'contains' relationship.
/// Takes a vector of IDs and returns a map from each ID to its count.
/// IDs with count 0 are included in the result.
pub async fn count_contents (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids : &[ID]
) -> Result < HashMap < ID, usize >,
              Box < dyn Error > > {
  count_relations ( db_name,
                    driver,
                    ids,
                    "contains",
                    "container"
  ). await }
