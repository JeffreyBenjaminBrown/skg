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

use crate::types::misc::ID;
use crate::dbs::typedb::util::concept_document::{
  extract_id_from_node,
  build_id_disjunction};
use crate::dbs::typedb::util::conjugate_binary_role;


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

/// PURPOSE: Count how many nodes link to each node
/// in a 'textlinks_to' relationship.
/// Takes a vector of IDs and returns a map from each ID to its count.
/// IDs with count 0 are included in the result.
pub async fn count_link_sources (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids : &[ID]
) -> Result < HashMap < ID, usize >,
              Box < dyn Error > > {
  count_relations ( db_name,
                    driver,
                    ids,
                    "textlinks_to",
                    "dest"
  ). await }

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
  let other_role : &str =
    conjugate_binary_role ( relation, input_role ) ?;
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = {
    let answer : QueryAnswer =
      tx . query ( {
        let disjunction_clauses : String =
          build_id_disjunction ( ids, "node_id" );
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
        query } ). await ?;
    answer }
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
          { // count them
            result . insert ( node_id, {
              let count : usize = related_list . len ();
              count } );
          }} }} }
  Ok (result) }

/// Check whether each input ID participates in a given relation
/// in either role. One TypeDB round-trip per call.
/// Returns HashMap<ID, bool>.
///
/// This largely duplicates `count_relations`, with these differences:
/// - takes both role names and uses an OR so a node matches
///   regardless of which role it plays
/// - returns bool (non-empty related list) rather than a count
async fn participates_in_relation (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids     : &[ID],
  relation : &str,
  role_a   : &str,
  role_b   : &str,
) -> Result < HashMap < ID, bool >,
              Box < dyn Error > > {
  if ids . is_empty () {
    return Ok ( HashMap::new () ); }
  let mut result : HashMap < ID, bool > =
    HashMap::new ();
  for id in ids {
    result . insert ( id . clone (), false ); }
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = {
    let answer : QueryAnswer =
      tx . query ( {
        let disjunction_clauses : String =
          build_id_disjunction ( ids, "node_id" );
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
                       $input_a isa node, has id $node_id;
                       $rel_a isa {} ( {}: $input_a,
                                       {}: $other ); }} or
                    {{ $other isa node, has id $other_id;
                       $e_b isa extra_id, has id $node_id;
                       $input_b isa node;
                       $extra_rel_b isa has_extra_id ( node: $input_b,
                                                       extra_id: $e_b );
                       $rel_b isa {} ( {}: $input_b,
                                       {}: $other ); }} or
                    {{ $other isa node, has id $other_id;
                       $input_c isa node, has id $node_id;
                       $rel_c isa {} ( {}: $input_c,
                                       {}: $other ); }} or
                    {{ $other isa node, has id $other_id;
                       $e_d isa extra_id, has id $node_id;
                       $input_d isa node;
                       $extra_rel_d isa has_extra_id ( node: $input_d,
                                                       extra_id: $e_d );
                       $rel_d isa {} ( {}: $input_d,
                                       {}: $other ); }};
                  fetch {{ "other_id": $other_id }};
                ]
              }};"#,
            disjunction_clauses,
            relation, role_a, role_b,
            relation, role_a, role_b,
            relation, role_b, role_a,
            relation, role_b, role_a );
        query } ). await ?;
    answer }
  { while let Some ( doc_result )
      = stream . next () . await
    { let doc : ConceptDocument =
        doc_result ?;
      if let Some ( Node::Map ( ref map ) )
      = doc . root
      { let node_id_opt : Option < ID > =
          map . get ( "node_id" )
          . and_then ( extract_id_from_node );
        if let Some ( node_id ) = node_id_opt {
          if let Some ( Node::List ( related_list ) ) =
            map . get ( "related" )
          { result . insert (
              node_id,
              ! related_list . is_empty () );
          }} }} }
  Ok (result) }

pub async fn has_subscribes (
  db_name : &str, driver : &TypeDBDriver, ids : &[ID]
) -> Result<HashMap<ID, bool>, Box<dyn Error>> {
  participates_in_relation ( db_name, driver, ids,
    "subscribes", "subscriber", "subscribee" ) . await }

pub async fn has_overrides (
  db_name : &str, driver : &TypeDBDriver, ids : &[ID]
) -> Result<HashMap<ID, bool>, Box<dyn Error>> {
  participates_in_relation ( db_name, driver, ids,
    "overrides_view_of", "replacement", "replaced" ) . await }
