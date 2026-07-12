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

use crate::consts::typedb_concurrent_transactions;
use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::dbs::typedb::util::ConceptRowStream;
use crate::dbs::typedb::util::concept_document::extract_id_from_node;
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;
use crate::types::misc::{ID, SourceName, members_of};

/// Fast container lookup by primary ID.
/// Assumes the input is a primary ID (not an extra ID).
/// Reads the in-Rust graph (populated at server startup). The
/// TypeDB branch below is exercised only by tests that bypass
/// 'init_global_handle_for_first_time_or_panic'; in the running server the in-Rust graph path is
/// always taken.
pub async fn find_container_ids_of_pid (
  db_name : &str,
  driver  : &TypeDBDriver,
  pid     : &ID,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  if let Some (graph_snap) = snapshot_global () {
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
/// Reads the in-Rust graph (populated at server startup). The
/// TypeDB branch below is exercised only by tests that bypass
/// 'init_global_handle_for_first_time_or_panic'; in the running server the in-Rust graph path is
/// always taken. The TypeDB path sends one query per input ID,
/// bounded by 'typedb_concurrent_transactions'.
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
  if let Some (graph_snap) = snapshot_global () {
    return Ok ( find_related_nodes_from_in_rust_graph (
      &graph_snap, nodes, relation, input_role, output_role ) ); }
  let relation : String    = relation    . to_string ();
  let input_role : String  = input_role  . to_string ();
  let output_role : String = output_role . to_string ();
  let sets : Vec < Result < HashSet<ID>, Box<dyn Error> > > =
    stream::iter ( nodes . iter ()
      . map ( |id| find_related_nodes_for_one_id (
                db_name, driver, id,
                &relation, &input_role, &output_role )) )
    . buffer_unordered ( typedb_concurrent_transactions () )
    . collect () . await;
  let mut result : HashSet<ID> = HashSet::new ();
  for set in sets {
    result . extend ( set ? ); }
  Ok (result) }

/// In-Rust-graph counterpart of 'find_related_nodes'. Dispatches on
/// (relation, input_role, output_role) to the appropriate forward
/// field or inverse index on 'Graph'. Inputs are mapped to their
/// corresponding pids (which might be themselves) via
/// 'extra_id_to_pid' first.
pub fn find_related_nodes_from_in_rust_graph (
  graph       : &InRustGraph,
  nodes       : &[ID],
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> HashSet<ID> {
  let mut out : HashSet<ID> = HashSet::new ();
  for input_id in nodes {
    let pid : ID = match graph . pid_of (input_id) {
      Some (p) => p,
      None     => continue };
    // Forward fields on NodeRust mirror disk and thus carry raw IDs;
    // callers expect canonical pids (to match what TypeDB's query
    // would return after its has_extra_id lookups). Map the ID of
    // each second member (see schema.tql) to its corresponding PID
    // (which might be itself) before inserting.
    let pid_or_self = |id: &ID| -> ID {
      graph . pid_of (id) . unwrap_or_else ( || id . clone () ) };
    match (relation, input_role, output_role) {
      // Forward lookups: read the field on the node.
      ("contains",                     "container",   "contained")  =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of (& n . contains) . iter () . map (&pid_or_self) ); },
      ("subscribes",                   "subscriber",  "subscribee") =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . subscribes_to . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("hides_from_its_subscriptions", "hider",       "hidden")     =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . hides_from_its_subscriptions . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("overrides_view_of",            "overrider",   "overridden") =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . overrides_view_of . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("textlinks_to",                 "source",      "dest")       =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . textlinks_to
                         . iter () . map (&pid_or_self) ); },
      // Inverse lookups: consult the inverse index.
      ("contains",                     "contained",   "container")   =>
        if let Some (s) = graph . contained_by . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("subscribes",                   "subscribee",  "subscriber")  =>
        if let Some (s) = graph . subscribers_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("hides_from_its_subscriptions", "hidden",      "hider")       =>
        if let Some (s) = graph . hiders_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("overrides_view_of",            "overridden",  "overrider")   =>
        if let Some (s) = graph . overriders_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("textlinks_to",                 "dest",        "source")      =>
        if let Some (s) = graph . textlinks_in . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      // Unknown (relation, input, output) combination: shouldn't
      // happen — the five outbound types have exactly two roles
      // each. Return empty (caller will get no matches).
      _ => {},
    } }
  out }

/// Find related nodes for a single input ID. TypeDB-backed;
/// bypasses the in-Rust graph shortcut. Exposed at crate scope
/// for the audit helper which needs the raw TypeDB answer.
///
/// Opens its own read transaction. For hot loops (e.g. the paced
/// audit) that issue many queries back-to-back, use the in-tx
/// variant below to amortize transaction setup.
pub(crate) async fn find_related_nodes_for_one_id (
  db_name     : &str,
  driver      : &TypeDBDriver,
  id          : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  find_related_nodes_for_one_id_in_tx (
    &tx, id, relation, input_role, output_role ) . await }

/// 'find_related_nodes_for_one_id' variant that reuses a caller-
/// provided read transaction. Multiple calls on the same 'tx' share
/// its setup cost, which (under load) is tens of milliseconds per
/// open — dominant over query execution for the audit's small
/// results.
pub(crate) async fn find_related_nodes_for_one_id_in_tx (
  tx          : &Transaction,
  id          : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  let output_id_var : String =
    format! ("{}_id", output_role);
  // Query shape: start from a specific input id (indexed lookup),
  // follow the relation, fetch the output's id. The earlier shape
  // led with '$output isa node, has id $out_id' which the planner
  // read as "enumerate all nodes + their ids" — ~29k work per
  // query. This form constrains $input first, then reaches $output
  // through the relation.
  //
  // The 'or' handles the case where 'id' is an extra_id of some
  // node rather than a primary id. For the audit (which always
  // passes primary pids), the extra_id branch never matches, but
  // the disjunction still costs planning + evaluation. Callers
  // that know they have a primary pid should prefer
  // 'find_related_nodes_for_one_primary_pid_in_tx' below.
  let mut stream : ConceptRowStream = {
    let answer : QueryAnswer = tx . query ( format! (
      r#"match
           {{ ${input} isa node, has id "{}"; }} or
           {{ $e isa extra_id, has id "{}";
              $extra_rel isa has_extra_id ( node:     ${input},
                                            extra_id: $e ); }};
           $rel isa {relation} ( {input}: ${input},
                                  {output}: ${output} );
           ${output} has id ${output_id};
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

/// Like 'find_related_nodes_for_one_id_in_tx' but assumes the
/// given id is a primary pid (not an extra_id). Omits the
/// extra_id disjunction from the query — substantial planner +
/// evaluator savings. The audit always has primary pids in hand;
/// this is the right variant for it.
pub(crate) async fn find_related_nodes_for_one_primary_pid_in_tx (
  tx          : &Transaction,
  pid         : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  let output_id_var : String =
    format! ("{}_id", output_role);
  let mut stream : ConceptRowStream = {
    let answer : QueryAnswer = tx . query ( format! (
      r#"match
           ${input} isa node, has id "{}";
           $rel isa {relation} ( {input}: ${input},
                                  {output}: ${output} );
           ${output} has id ${output_id};
           select ${output_id};"#,
      pid,
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
/// Reads the in-Rust graph (populated at server startup). The
/// TypeDB branch below is exercised only by tests that bypass
/// 'init_global_handle_for_first_time_or_panic', or on the rare case that in-Rust graph is
/// initialized but doesn't hold the id (normally in-Rust graph contains
/// every node across all sources). In the running server the
/// in-Rust graph path covers the hot case.
pub async fn pid_and_source_from_id (
  db_name : &str,
  driver  : &TypeDBDriver,
  skgid  : &ID
) -> Result < Option<(ID, SourceName)>, Box<dyn Error> > {
  if let Some (graph_snap) = snapshot_global () {
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
          $node isa node, has id $primary_id;
          $src isa source, has source_name $source;
          $source_rel isa has_source (node: $node, source: $src);
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
