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
use ego_tree::{NodeRef, NodeMut, NodeId};

use crate::types::{OrgNode, ID};
use crate::media::typedb::util::{
  extract_id_from_node,
  extract_id_from_map,
  build_id_disjunction};


/// Collect IDs for bulk PID lookup
pub fn collect_ids_in_tree (
  node_ref : NodeRef < OrgNode >,
  ids_to_lookup : & mut Vec < ID >
) {
  if let Some ( ref id )
    = node_ref . value () . metadata . id
  { // Collect ID if present
    ids_to_lookup . push ( id . clone () ); }
  for child in node_ref . children () { // Recurse
    collect_ids_in_tree (
      child,
      ids_to_lookup ); } }

/// Assign PIDs from the bulk lookup results
pub fn assign_pids_throughout_tree_from_map (
  mut node_ref : NodeMut < OrgNode >,
  pid_map : & HashMap < ID, Option < ID > >
) {
  if let Some ( ref current_id )
    = node_ref . value () . metadata . id . clone ()
  { // Assign PID if we have one
    if let Some ( Some ( pid )) =
      pid_map . get ( current_id )
    { node_ref . value () . metadata . id =
      Some ( pid . clone () ); }}
  { // Process children recursively
    let node_id : NodeId = node_ref . id ();
    let child_ids : Vec < NodeId > = {
      let tree = node_ref . tree ();
      tree . get ( node_id ) . unwrap ()
        . children () . map ( | child | child . id () )
        . collect () };
    for child_id in child_ids {
      if let Some ( child_mut )
        = node_ref . tree () . get_mut ( child_id )
      { assign_pids_throughout_tree_from_map (
        child_mut, pid_map ); }} }}

/// PURPOSE: Run one TypeDB query, using nested subqueries,
/// to look up PIDs for multiple IDs.
/// (That it is one query complicates things but makes them faster.)
/// Return a HashMap mapping each ID to its PID (or None if not found).
pub async fn pids_from_ids (
  db_name : &str,
  driver  : &TypeDBDriver,
  node_ids : &[ID]
) -> Result < HashMap < ID, Option < ID > >, Box < dyn Error > > {
  if node_ids . is_empty () {
    return Ok ( HashMap::new () ); }
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let disjunction_clauses : String =
    build_id_disjunction ( node_ids, "id" );
  let query : String =
    format! (
      // Query for (id, primary_id) pairs.
      // Outer match finds IDs, either as node IDs or extra IDs.
      // Inner match finds PIDs.
      r#"match
        {{ $node isa node, has id $id; }} or
        {{ $e isa extra_id, has id $id;
           $rel isa has_extra_id ( extra_id: $e ); }};
        {};
        fetch {{
          "id": $id,
          "primary_ids": [
            match
              {{ $node2 isa node, has id $id2, has id $primary_id;
                 $node2 has id $id; }} or
              {{ $e2 isa extra_id, has id $id2;
                 $node2 isa node, has id $primary_id;
                 $rel2 isa has_extra_id ( node: $node2,
                                          extra_id: $e2 );
                 $e2 has id $id; }};
            fetch {{ "primary_id": $primary_id }};
          ]
        }};"#,
      disjunction_clauses );
  let answer : QueryAnswer =
    tx . query ( query ) . await ?;
  let mut result : HashMap < ID, Option < ID > > =
    HashMap::new ();
  for node_id in node_ids {
    // Initialize all IDs with None
    result . insert ( node_id . clone (), None ); }
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = answer
  { // Process the nested subquery results.
    // Each document has the structure:
    //   {"id": <some-id>, "primary_ids": [{"primary_id": <pid>}]}
    // The nested "primary_ids" list comes from the inner match/fetch.
    while let Some ( doc_result )
      = stream . next () . await
    { let doc : ConceptDocument =
        doc_result ?;

      // TypeDB returns documents as a tree of Nodes.
      // Root is Option<Node>, which can be Map, List, or Leaf.
      // We expect root to be a Map with keys "id" and "primary_ids".
      if let Some ( Node::Map ( ref map ) ) = doc . root {

        // Extract the ID from the outer fetch
        let id_opt : Option < ID > =
          map . get ( "id" )
          . and_then ( extract_id_from_node );

        // Extract the primary_id from the nested subquery result.
        // The value at "primary_ids" is a Node::List with one Map.
        let primary_id_opt : Option < ID > =
          map . get ( "primary_ids" )
          . and_then (
            | node : & Node |
            // Unwrap Node::List, get first element
            if let Node::List ( list ) = node
            { list . first () }
            else { None } )
          . and_then (
            | first_node : & Node |
            // Extract "primary_id" from the map
            extract_id_from_map ( first_node, "primary_id" ) );

        if let Some ( id ) = id_opt {
          result . insert ( id, primary_id_opt ); }} }}
  Ok ( result ) }
