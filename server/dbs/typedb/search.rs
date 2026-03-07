pub mod all_graphnodestats;
pub mod contains_from_pids;
pub mod hidden_in_subscribee_content;

use futures::StreamExt;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer, ConceptDocument,
           concept_document::{Node, Leaf}},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::dbs::typedb::util::ConceptRowStream;
use crate::dbs::typedb::util::concept_document::{build_id_disjunction, extract_id_from_node};
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;
use crate::types::misc::{ID, SourceName};

/// Fast container lookup by primary ID.
/// Assumes the input is a primary ID (not an extra ID).
/// Skips the extra_id `or` pattern used by find_related_nodes,
/// which is the dominant cost in that query.
pub(crate) async fn find_container_ids_of_pid (
  db_name : &str,
  driver  : &TypeDBDriver,
  pid     : &ID,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let mut stream : ConceptRowStream = {
    let answer : QueryAnswer = tx . query (
      format! ( r#"match
        $contained isa node, has id "{}";
        $container isa node, has id $container_id;
        $rel isa contains ( container: $container,
                            contained: $contained );
        select $container_id;"#,
        pid ) ) . await ?;
    answer } . into_rows ();
  let mut result : HashSet<ID> = HashSet::new ();
  while let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row . get ("container_id") ? {
      result . insert ( ID (
        extract_payload_from_typedb_string_rep (
          & concept . to_string () )) ); } }
  Ok (result) }

/// Generalized function to find related nodes via a specified relationship.
/// Returns the IDs of nodes in the `output_role` position
/// related to any of the input nodes.
pub async fn find_related_nodes (
  db_name     : &str,
  driver      : &TypeDBDriver,
  nodes       : &[ID],
  relation    : &str,
  input_role  : &str,
  output_role : &str
) -> Result < HashSet<ID>, Box<dyn Error> > {
  if nodes . is_empty () {
    return Ok ( HashSet::new () ); }
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let input_id_var : String =
    format!("{}_id", input_role);
  let output_id_var : String =
    format!("{}_id", output_role);
  let mut stream : ConceptRowStream = {
    let answer : QueryAnswer = tx . query ( {
      let input_disjunction : String =
        build_id_disjunction ( nodes, &input_id_var );
      let match_clause : String =
        format!( r#" match
                       ${} isa node, has id ${};
                    {{ ${} isa node, has id ${}; }} or
                    {{ ${} isa node;
                       $e isa extra_id, has id ${};
                       $extra_rel isa has_extra_id ( node:     ${},
                                                     extra_id: $e ); }};
                    {};"#,
                     output_role, output_id_var,
                     input_role, input_id_var,
                     input_role, input_id_var, input_role,
                     input_disjunction );
      let relationship_and_select : String = format!( r#"
                       $rel isa {} ( {}: ${},
                                     {}: ${} );
                       select ${};"#,
                       relation, input_role, input_role,
                       output_role, output_role,
                       output_id_var );
      let query : String = format!(
        "{}{}", match_clause, relationship_and_select);
      query } ) . await?;
    answer } . into_rows ();
  let mut related_nodes : HashSet<ID> = HashSet::new ();
  while let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row . get (&output_id_var) ? {
      related_nodes . insert ( ID (
        extract_payload_from_typedb_string_rep (
          &concept . to_string () )) ); }}
  Ok (related_nodes) }

/// Runs a single TypeDB query to get both PID and source.
/// Returns None if not found.
pub async fn pid_and_source_from_id (
  db_name : &str,
  driver  : &TypeDBDriver,
  skgid  : &ID
) -> Result < Option<(ID, SourceName)>, Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = {
    let answer : QueryAnswer = tx . query ( {
      let query : String = format! (
        r#"match
          $node isa node,
                has id $primary_id,
                has source $source;
          {{ $node has id "{}"; }} or
          {{ $e   isa     extra_id, has id "{}";
             $rel isa has_extra_id ( node: $node,
                                     extra_id: $e ); }} ;
          fetch {{
            "primary_id": $primary_id,
            "source": $source
          }};"#,
        skgid,
        skgid );
      query } ) . await ?;
    answer } {
      if let Some (doc_result) = stream . next () . await {
        let doc : ConceptDocument = doc_result ?;
        if let Some ( Node::Map ( ref map ) ) = doc . root {
          let primary_id_opt : Option < ID > =
            map . get ("primary_id")
            . and_then (extract_id_from_node);
          let source_opt : Option < SourceName > =
            map . get ("source")
            . and_then ( | node : & Node | {
              if let Node::Leaf ( Some (leaf) ) = node {
                if let Leaf::Concept (concept) = leaf {
                  return Some ( SourceName::from (
                    extract_payload_from_typedb_string_rep (
                      & concept . to_string () ) ) ); }}
              None } );
          if let ( Some (pid), Some (source) )
            = ( primary_id_opt, source_opt )
          { return Ok ( Some ( ( pid, source ) ) ); }} }}
  Ok (None) }
