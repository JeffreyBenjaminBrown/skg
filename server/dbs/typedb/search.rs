pub mod all_graphnodestats;
pub mod contains_from_pids;
pub mod hidden_in_subscribee_content;

use futures::stream::{self, StreamExt};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer, ConceptDocument,
           concept_document::{Node, Leaf}},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::dbs::typedb::util::ConceptRowStream;
use crate::dbs::typedb::util::concept_document::extract_id_from_node;
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;
use crate::types::misc::{ID, SourceName};

/// Fast container lookup by primary ID.
/// Assumes the input is a primary ID (not an extra ID).
/// Skips the extra_id `or` pattern used by find_related_nodes,
/// which is the dominant cost in that query.
pub async fn find_container_ids_of_pid (
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
/// Sends one query per input ID, bounded by TYPEDB_CONCURRENT_TRANSACTIONS.
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
  let relation : String    = relation    . to_string ();
  let input_role : String  = input_role  . to_string ();
  let output_role : String = output_role . to_string ();
  let sets : Vec < Result < HashSet<ID>, Box<dyn Error> > > =
    stream::iter ( nodes . iter ()
      . map ( |id| find_related_nodes_for_one_id (
                db_name, driver, id,
                &relation, &input_role, &output_role )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  let mut result : HashSet<ID> = HashSet::new ();
  for set in sets {
    result . extend ( set ? ); }
  Ok (result) }

/// Find related nodes for a single input ID.
async fn find_related_nodes_for_one_id (
  db_name     : &str,
  driver      : &TypeDBDriver,
  id          : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  let output_id_var : String =
    format! ("{}_id", output_role);
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let mut stream : ConceptRowStream = {
    let answer : QueryAnswer = tx . query ( format! (
      r#"match
           ${output} isa node, has id ${output_id};
           {{ ${input} isa node, has id "{}"; }} or
           {{ ${input} isa node;
              $e isa extra_id, has id "{}";
              $extra_rel isa has_extra_id ( node:     ${input},
                                            extra_id: $e ); }};
           $rel isa {relation} ( {input}: ${input},
                                  {output}: ${output} );
           select ${output_id};"#,
      id, id,
      input = input_role,
      output = output_role,
      output_id = output_id_var,
      relation = relation ) ) . await ?;
    answer } . into_rows ();
  let mut found : HashSet<ID> = HashSet::new ();
  while let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row . get (&output_id_var) ? {
      found . insert ( ID (
        extract_payload_from_typedb_string_rep (
          &concept . to_string () )) ); } }
  Ok (found) }

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
