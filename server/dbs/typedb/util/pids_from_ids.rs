use std::error::Error;
use std::collections::HashMap;
use typedb_driver::{
  answer::{QueryAnswer, ConceptDocument},
  answer::concept_document::Node,
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use futures::stream::{self, StreamExt};
use ego_tree::{NodeRef, NodeMut, NodeId, Tree};

use crate::types::unchecked_viewnode::{UncheckedViewNode, UncheckedViewNodeKind};
use crate::types::misc::ID;
use crate::types::memory::SkgNodeMap;
use crate::dbs::typedb::util::concept_document::extract_id_from_node;

/// Collect all IDs, batch-lookup their PIDs in TypeDB, then replace.
/// Only query TypeDB for nodes not already in the skgnodemap.
pub async fn replace_ids_with_pids(
  forest  : &mut Tree<UncheckedViewNode>,
  root_id : NodeId,
  db_name : &str,
  driver  : &TypeDBDriver,
  pool    : &SkgNodeMap,
) -> Result<(), Box<dyn Error>> {
  let mut all_ids: Vec<ID> = Vec::new();
  collect_ids_in_tree( forest . root(),
                       &mut all_ids );
  let mut pids_from_pool: HashMap<ID, Option<ID>> =
    HashMap::new();
  let mut unknown_ids: Vec<ID> = // not in the pool
    Vec::new();
  for id in all_ids {
    if pool . contains_key (&id) {
      pids_from_pool . insert ( id . clone(), Some (id) );
    } else { unknown_ids . push (id); }}
  tracing::debug!("replace_ids_with_pids: {} in pool, {} need TypeDB query",
            pids_from_pool . len(), unknown_ids . len());
  let pids_from_typedb: HashMap<ID, Option<ID>> =
    pids_from_ids( db_name, driver, &unknown_ids
    ) . await?;
  pids_from_pool . extend (pids_from_typedb);
  if let Some (root_mut) = forest . get_mut (root_id) {
    assign_pids_throughout_tree_from_map(
      root_mut, &pids_from_pool); }
  Ok(( )) }

/// Collect IDs for bulk PID lookup
pub fn collect_ids_in_tree (
  node_ref : NodeRef < UncheckedViewNode >,
  ids_to_lookup : & mut Vec < ID >
) {
  if let UncheckedViewNodeKind::True (t) =
    &node_ref . value () . kind
  { if let Some (id) = &t . id_opt
    { ids_to_lookup . push ( id . clone () ); }}
  for child in node_ref . children () { // Recurse
    collect_ids_in_tree (
      child,
      ids_to_lookup ); } }

pub fn assign_pids_throughout_tree_from_map (
  mut node_ref : NodeMut < UncheckedViewNode >,
  pid_map : & HashMap < ID, Option < ID > >
) {
  if let UncheckedViewNodeKind::True (t)
    = &mut node_ref . value() . kind
    { let pid_opt : Option < ID > = t . id_opt . as_ref ()
        . and_then ( |id| pid_map . get (id) )
        . and_then ( |opt| opt . clone () );
      if let Some (pid) = pid_opt {
        t . id_opt = Some (pid); }}
  { // Recurse into children
    for child_treeid in {
      let treeid : NodeId = node_ref . id ();
      let child_treeids : Vec < NodeId > = {
        let tree : &Tree<UncheckedViewNode> = node_ref . tree ();
        tree . get (treeid) . unwrap ()
          . children () . map ( | child | child . id () )
          . collect () };
      child_treeids } {
      if let Some (child_mut)
        = node_ref . tree () . get_mut (child_treeid)
      { assign_pids_throughout_tree_from_map (
        child_mut, pid_map ); }} }}

/// Look up PIDs for multiple IDs.
/// Sends one query per ID, bounded by TYPEDB_CONCURRENT_TRANSACTIONS.
/// Returns a HashMap mapping each ID to its PID (or None if not found).
pub async fn pids_from_ids (
  db_name  : &str,
  driver   : &TypeDBDriver,
  node_ids : &[ID]
) -> Result < HashMap < ID, Option < ID > >, Box < dyn Error > > {
  if node_ids . is_empty () {
    return Ok ( HashMap::new () ); }
  let results : Vec < (ID, Option<ID>) > =
    stream::iter ( node_ids . iter ()
      . map ( |id| pid_from_one_id (
                db_name, driver, id . clone () )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  let result : HashMap < ID, Option < ID > > =
    results . into_iter () . collect ();
  Ok (result) }

/// Look up the PID for a single ID.
async fn pid_from_one_id (
  db_name : &str,
  driver  : &TypeDBDriver,
  id      : ID,
) -> (ID, Option<ID>) {
  let pid : Option<ID> =
    pid_from_one_id_inner (db_name, driver, &id)
    . await . ok () . flatten ();
  (id, pid) }

async fn pid_from_one_id_inner (
  db_name : &str,
  driver  : &TypeDBDriver,
  id      : &ID,
) -> Result < Option<ID>, Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = {
    let answer : QueryAnswer = tx . query ( format! (
      r#"match
           {{ $node isa node, has id $primary_id;
              $node has id "{}"; }} or
           {{ $e isa extra_id, has id "{}";
              $rel isa has_extra_id ( extra_id: $e, node: $node );
              $node isa node, has id $primary_id; }};
           fetch {{
             "primary_id": $primary_id
           }};"#,
      id, id ) ) . await ?;
    answer }
  { if let Some (doc_result) = stream . next () . await {
      let doc : ConceptDocument = doc_result ?;
      if let Some ( Node::Map ( ref map ) ) = doc . root {
        return Ok (
          map . get ("primary_id")
          . and_then (extract_id_from_node) ); }}}
  Ok (None) }
