use std::error::Error;
use std::collections::HashMap;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use futures::StreamExt;

use crate::types::ID;

/// Runs one query per ID, but in parallel.
/// TODO ? SOLVED: Could nested subqueries do this?
/// See the TypeDB feature gallery:
///   https://typedb.com/features
/// If/when TypeDB becomes able to express it,
/// run these all in a single query.
/// That will require the ability to pass a list of (x)
/// to Rust and get a list of (x,y).
/// (Currently I can get a list of (y) from a list of (x),
/// but I don't know how to track which y corresponds to which x.)
pub async fn pids_from_ids (
  db_name : &str,
  driver  : &TypeDBDriver,
  node_ids : &[ID]
) -> Result < HashMap<ID, Option<ID>>, Box<dyn Error> > {
  if node_ids.is_empty() {
    return Ok ( HashMap::new() ); }
  let mut output_acc : HashMap<ID, Option<ID>> =
    HashMap::new();
  let query_futures : Vec<_> =
    node_ids.iter()
    . map ( |node_id| {
      let future = pid_from_id (
        db_name, driver, node_id );
      async move {
        let pid_result = future.await;
        (node_id.clone(), pid_result) }} )
    . collect ();
  let query_results = // run them all
    futures::future::join_all ( query_futures ).await;
  for (node_id, pid_result) in query_results {
    output_acc.insert (
      node_id, pid_result ? ); }
  Ok ( output_acc ) }
