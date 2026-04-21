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
/// Consults the in-Rust memory first (via the process-global handle
/// set at startup); falls back to TypeDB if memory isn't initialized.
pub async fn find_container_ids_of_pid (
  db_name : &str,
  driver  : &TypeDBDriver,
  pid     : &ID,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  if let Some (graph_snap) = crate::dbs::memory::snapshot_global () {
    let result : HashSet<ID> = graph_snap . contained_by . get (pid)
      . map ( |s| s . iter () . cloned () . collect () )
      . unwrap_or_default ();
    return Ok (result); }
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
/// Consults the in-Rust memory first (via the process-global handle
/// set at startup); falls back to TypeDB if memory isn't initialized.
/// Sends one query per input ID (TypeDB fallback path), bounded by
/// TYPEDB_CONCURRENT_TRANSACTIONS.
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
  if let Some (graph_snap) = crate::dbs::memory::snapshot_global () {
    return Ok ( find_related_nodes_from_memory (
      &graph_snap, nodes, relation, input_role, output_role ) ); }
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

/// In-memory counterpart of 'find_related_nodes'. Dispatches on
/// (relation, input_role, output_role) to the appropriate forward
/// field or inverse index on 'Graph'. Inputs are resolved through
/// 'extra_id_to_pid' first.
fn find_related_nodes_from_memory (
  graph       : &crate::dbs::memory::Graph,
  nodes       : &[ID],
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> HashSet<ID> {
  let mut out : HashSet<ID> = HashSet::new ();
  for raw_id in nodes {
    let pid : ID = match graph . pid_of (raw_id) {
      Some (p) => p,
      None     => continue };
    match (relation, input_role, output_role) {
      // Forward lookups: read the field on the node.
      ("contains",                     "container",   "contained")  =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . contains . iter () . cloned () ); },
      ("subscribes",                   "subscriber",  "subscribee") =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . subscribes_to . or_default () . iter () . cloned () ); },
      ("hides_from_its_subscriptions", "hider",       "hidden")     =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . hides_from_its_subscriptions . or_default () . iter () . cloned () ); },
      ("overrides_view_of",            "replacement", "replaced")   =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . overrides_view_of . or_default () . iter () . cloned () ); },
      ("textlinks_to",                 "source",      "dest")       =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . textlinks_to . iter () . cloned () ); },
      // Inverse lookups: consult the inverse index.
      ("contains",                     "contained",   "container")  =>
        if let Some (s) = graph . contained_by . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("subscribes",                   "subscribee",  "subscriber") =>
        if let Some (s) = graph . subscribers_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("hides_from_its_subscriptions", "hidden",      "hider")      =>
        if let Some (s) = graph . hiders_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("overrides_view_of",            "replaced",    "replacement") =>
        if let Some (s) = graph . replacements_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("textlinks_to",                 "dest",        "source")     =>
        if let Some (s) = graph . textlinks_in . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      // Unknown (relation, input, output) combination: shouldn't
      // happen — the five outbound types have exactly two roles
      // each. Return empty (caller will get no matches).
      _ => {},
    } }
  out }

/// Find related nodes for a single input ID. TypeDB-backed;
/// bypasses the in-Rust memory shortcut. Exposed at crate scope
/// for the audit helper which needs the raw TypeDB answer.
pub(crate) async fn find_related_nodes_for_one_id (
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

/// Returns the primary ID and source of a node, given any of its
/// IDs (primary or extra). Returns None if not found.
///
/// Consults the in-Rust memory first (via the process-global handle
/// set at startup); falls back to TypeDB if memory isn't initialized
/// or doesn't have the id. Memory hits skip the TypeDB round-trip
/// entirely, which is the hot-path win for Plan C.
pub async fn pid_and_source_from_id (
  db_name : &str,
  driver  : &TypeDBDriver,
  skgid  : &ID
) -> Result < Option<(ID, SourceName)>, Box<dyn Error> > {
  if let Some (graph_snap) = crate::dbs::memory::snapshot_global () {
    if let Some (ps) = graph_snap . pid_and_source (skgid) {
      return Ok ( Some (ps) ); } }
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
