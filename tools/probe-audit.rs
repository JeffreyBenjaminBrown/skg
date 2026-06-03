/* probe-audit
 * Ad-hoc: after rebuild-dbs, check whether the suspicious orphan
 * relations flagged in earlier audits.org still exist in TypeDB or
 * in the freshly-rebuilt InRustGraph.
 *
 * Checks three pids:
 *   7be4b170 — was flagged 'contains contained mismatch':
 *              InRustGraph {1ff18102}, typedb {d923d1bd, 1ff18102}
 *   aba033b6 — was flagged 'textlinks_to source mismatch':
 *              InRustGraph {ee08b16a}, typedb {}
 *   bb78d57e — was flagged 'textlinks_to source mismatch' yesterday
 *              (should no longer be in InRustGraph post-rebuild; the .skg
 *              file is only in public-1-mangled/)
 *
 * Prints both InRustGraph's and TypeDB's answer for each, so we can see
 * whether rebuild cleared the orphans.
 */

use skg::dbs::filesystem::multiple_nodes::check_for_duplicate_ids_across_sources;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::in_rust_graph::{
  audit::{Mismatch, audit_one_node},
  InRustGraph, InRustGraphHandle,
  init_global_handle_for_first_time_or_panic, new_handle,
};
use skg::types::misc::{ID, SkgConfig};
use skg::types::nodes::complete::NodeComplete;

use std::env;
use std::sync::Arc;
use typedb_driver::{
  Credentials, DriverOptions, Transaction, TransactionType, TypeDBDriver,
};

#[tokio::main]
async fn main () -> Result<(), Box<dyn std::error::Error>> {
  let args : Vec<String> = env::args () . collect ();
  let config_path : &str =
    if args . len () > 1 { &args[1] } else { "data/skgconfig.toml" };
  let config : SkgConfig = load_config (config_path) ?;

  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config) ?;
  check_for_duplicate_ids_across_sources (
    &nodes, &config . data_root) ?;
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&nodes);
  let handle : InRustGraphHandle = new_handle (graph);
  init_global_handle_for_first_time_or_panic (Arc::clone (&handle));
  let snap : Arc<InRustGraph> = handle . load_full ();
  println! ("Loaded graph: {} nodes", snap . nodes . len ());

  let driver : TypeDBDriver = TypeDBDriver::new (
    "127.0.0.1:1729",
    Credentials::new ("admin", "password"),
    DriverOptions::new (false, None) ?
  ) . await ?;
  let tx : Transaction = driver . transaction (
    &config . db_name, TransactionType::Read ) . await ?;

  for pid_str in [
    "7be4b170-3339-441e-853a-7d4e2176d821",
    "aba033b6-d94d-4570-a728-a38d05ee24de",
    "bb78d57e-ff94-4690-be92-9ea4c9580adc",
  ] {
    let pid : ID = ID (pid_str . to_string ());
    println! ("\n=== {} ===", pid_str);
    let in_in_rust_graph : bool = snap . nodes . contains_key (&pid);
    println! ("  in InRustGraph.nodes?     {}", in_in_rust_graph);
    if ! in_in_rust_graph {
      println! ("  (skipping — not a node in this graph)");
      continue; }
    let mms = audit_one_node (&snap, &tx, &pid) . await ?;
    if mms . is_empty () {
      println! ("  NO mismatches — InRustGraph and TypeDB agree.");
    } else {
      for m in &mms {
        match m {
          Mismatch::Relationship (m) => {
            println! (
              "  MISMATCH: {} {} \n    InRustGraph: {:?}\n    typedb: {:?}",
              m . relation, m . role, m . in_rust_graph, m . typedb); }
          Mismatch::Source (m) => {
            println! (
              "  SOURCE MISMATCH\n    InRustGraph: {}\n    typedb: {}",
              m . in_rust_graph,
              m . typedb); }
          Mismatch::MissingTypedbNode { pid } => {
            println! (
              "  MISSING TYPEDB NODE: {}",
              pid); }} } } }
  Ok (( )) }
