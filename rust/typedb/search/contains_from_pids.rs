use std::error::Error;
use std::collections::{HashMap, HashSet};
use typedb_driver::{
  answer::{QueryAnswer, ConceptDocument},
  answer::concept_document::{Node, Leaf},
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use futures::StreamExt;

use crate::types::ID;
use crate::typedb::util::extract_payload_from_typedb_string_rep;


/// PURPOSE: Run one TypeDB query, using nested subqueries,
/// to find all 'contains' relations among a collection of nodes.
/// Takes a vector of IDs (PIDs) and returns two maps:
/// 1. container_to_contents: Map from each container to the set of nodes it contains
/// 2. content_to_containers: Map from each contained node to the set of containers
/// If a value (set) is empty, the corresponding key is omitted from the map.
pub async fn contains_from_pids (
  db_name : &str,
  driver  : &TypeDBDriver,
  pids : &[ID]
) -> Result <
    ( HashMap < ID, HashSet < ID > >, // maps container to contents
      HashMap < ID, HashSet < ID > >), // maps content to containers
  Box < dyn Error > > {

  if pids . is_empty () {
    return Ok (( HashMap::new (), HashMap::new () )); }
  let mut container_to_contents : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  let mut content_to_containers : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  let pid_set : HashSet<ID> = // the input pids
    pids . iter () . cloned () . collect ();
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;

  let disjunction_clauses : String = (
    // looks like "{$node_id == "1";} or {$node_id == "2";} or ..."
    pids
    . iter ()
    . map ( | pid |
              format! ( "{{$node_id == \"{}\";}}",
                           pid . 0 ) )
    . collect::< Vec < _ > > ()
    . join ( " or " ));
  let query : String = (
    // - Outer match finds each node in our input set
    // - First nested fetch finds what this node contains (if any)
    // - Second nested fetch finds what contains this node (if any)
    format! (
      r#"match
        {{ $node isa node, has id $node_id; }} or
        {{ $e isa extra_id, has id $node_id;
           $rel isa has_extra_id ( extra_id: $e ); }};
        {};
        fetch {{
          "node_id": $node_id,
          "contains": [
            match
              {{ $node2 isa node, has id $contained_id;
                 $node3 isa node, has id $node_id;
                 $rel2 isa contains ( container: $node3,
                                      contained: $node2 ); }} or
              {{ $node2 isa node, has id $contained_id;
                 $e2 isa extra_id, has id $node_id;
                 $node3 isa node;
                 $extra_rel isa has_extra_id ( node: $node3,
                                               extra_id: $e2 );
                 $rel2 isa contains ( container: $node3,
                                      contained: $node2 ); }};
            fetch {{ "contained_id": $contained_id }};
          ],
          "contained_by": [
            match
              {{ $node4 isa node, has id $container_id;
                 $node5 isa node, has id $node_id;
                 $rel3 isa contains ( container: $node4,
                                      contained: $node5 ); }} or
              {{ $node4 isa node, has id $container_id;
                 $e3 isa extra_id, has id $node_id;
                 $node5 isa node;
                 $extra_rel2 isa has_extra_id ( node: $node5,
                                                extra_id: $e3 );
                 $rel3 isa contains ( container: $node4,
                                      contained: $node5 ); }};
            fetch {{ "container_id": $container_id }};
          ]
        }};"#,
      disjunction_clauses ) );

  let answer : QueryAnswer =
    tx . query ( query ) . await ?;

  if let QueryAnswer::ConceptDocumentStream ( _, mut stream )
    = answer
  { // Process the nested subquery results.
    // Each document has the structure:
    //   {"node_id": <id>, "contains": [...], "contained_by": [...]}
    while let Some ( doc_result )
      = stream . next () . await
    { let doc : ConceptDocument =
        doc_result ?;
      if let Some ( Node::Map ( ref map ) )
      = doc . root
      { let node_id_opt : Option < ID > =
          map . get ( "node_id" )
          . and_then ( extract_id_from_node );
        if let Some ( node_id ) = node_id_opt {
          if let Some ( Node::List ( contains_list ) ) = (
            // Extract contained nodes (what this node contains)
            map . get ( "contains" ))
          { for item in contains_list {
            if let Some ( contained_id ) =
              extract_id_from_map ( item, "contained_id" )
            { // Only track if contained_id is in our input set
              if pid_set . contains ( & contained_id ) {
                container_to_contents
                  . entry ( node_id . clone () )
                  . or_insert_with ( HashSet::new )
                  . insert ( contained_id );
              }} }}
          if let Some ( Node::List ( contained_by_list ) ) = (
            // Extract containers (what contains this node)
            map . get ( "contained_by" ))
          { for item in contained_by_list {
            if let Some ( container_id ) =
              extract_id_from_map ( item, "container_id" )
            { // Only track if container_id is in our input set
              if pid_set . contains ( & container_id ) {
                content_to_containers
                  . entry ( node_id . clone () )
                  . or_insert_with ( HashSet::new )
                  . insert ( container_id ); }} }} }} }}
  Ok (( container_to_contents,
        content_to_containers )) }

/// Extract ID from a Node
fn extract_id_from_node (
  node : & Node
) -> Option < ID > {
  if let Node::Leaf ( Some ( leaf ) ) = node {
    if let Leaf::Concept ( concept ) = leaf {
      return Some ( ID (
        extract_payload_from_typedb_string_rep (
          & concept . to_string () )) ); }}
  None }

/// Extract ID from a map node at a specific key
fn extract_id_from_map (
  node : & Node,
  key  : &str
) -> Option < ID > {
  if let Node::Map ( inner_map ) = node {
    inner_map . get ( key )
      . and_then ( extract_id_from_node )
  } else { None }}
