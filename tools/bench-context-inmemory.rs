/* bench-context-inmemory
 * Benchmark the fully in-memory context computation pipeline.
 * Usage: cargo run --bin bench-context-inmemory <config-path>
 *   default config: data/real/skgconfig.toml
 *
 * No TypeDB needed — reads .skg files from disk and computes
 * context types entirely in memory.
 */

use skg::context::{
  compute_and_store_context_types,
  contains_maps_from_nodes,
  had_id_set_from_nodes,
  link_targets_from_nodes,
  ContainsMap,
  ReverseContainsMap,
};
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::init::create_empty_tantivy_index;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::skgnode::SkgNode;

use std::collections::HashSet;
use std::env;
use std::time::Instant;

fn main () -> Result<(), Box<dyn std::error::Error>> {
  let args : Vec<String> = env::args () . collect ();
  let config_path : &str =
    if args . len () > 1 { &args[1] }
    else { "data/real/skgconfig.toml" };

  println! ("=== In-Memory Context Pipeline Benchmark ===");
  println! ("Config: {}", config_path);

  let config : SkgConfig = load_config (config_path) ?;

  // Step 1: Read all .skg files
  let t0 : Instant = Instant::now ();
  let nodes : Vec<SkgNode> =
    read_all_skg_files_from_sources (&config) ?;
  let read_time : f64 = t0 . elapsed () . as_secs_f64 ();
  println! ("\n1. Read {} .skg files: {:.3}s", nodes . len (), read_time);

  // Step 2: Extract context data from nodes
  let t1 : Instant = Instant::now ();
  let had_id_set : HashSet<ID> =
    had_id_set_from_nodes (&nodes);
  let had_time : f64 = t1 . elapsed () . as_secs_f64 ();
  println! ("2a. had_id_set ({} nodes): {:.6}s",
            had_id_set . len (), had_time);

  let t2 : Instant = Instant::now ();
  let all_node_ids : HashSet<ID> =
    nodes . iter ()
    . filter_map ( |n| n . primary_id () . ok () . cloned () )
    . collect ();
  let ids_time : f64 = t2 . elapsed () . as_secs_f64 ();
  println! ("2b. all_node_ids ({} nodes): {:.6}s",
            all_node_ids . len (), ids_time);

  let t3 : Instant = Instant::now ();
  let link_targets : HashSet<ID> =
    link_targets_from_nodes (&nodes);
  let links_time : f64 = t3 . elapsed () . as_secs_f64 ();
  println! ("2c. link_targets ({} targets): {:.6}s",
            link_targets . len (), links_time);

  let t4 : Instant = Instant::now ();
  let ( contains_map, reverse_map )
    : ( ContainsMap, ReverseContainsMap )
    = contains_maps_from_nodes (&nodes);
  let maps_time : f64 = t4 . elapsed () . as_secs_f64 ();
  let edge_count : usize =
    contains_map . values () . map ( |v| v . len () ) . sum ();
  println! ("2d. contains_maps ({} containers, {} edges): {:.6}s",
            contains_map . len (), edge_count, maps_time);

  let extract_total : f64 =
    had_time + ids_time + links_time + maps_time;
  println! ("    Extract total: {:.6}s", extract_total);

  // Step 3: Create a temp Tantivy index for the benchmark
  let tantivy_dir : tempfile::TempDir =
    tempfile::TempDir::new () ?;
  let tantivy_index : TantivyIndex =
    create_empty_tantivy_index (tantivy_dir . path ()) ?;
  // Populate Tantivy with node titles so context types can be stored
  let t5 : Instant = Instant::now ();
  let indexed : usize =
    skg::dbs::tantivy::update_index_with_nodes (
      &nodes, &tantivy_index ) ?;
  let tantivy_pop_time : f64 = t5 . elapsed () . as_secs_f64 ();
  println! ("3. Tantivy populated ({} docs): {:.3}s",
            indexed, tantivy_pop_time);

  // Step 4: Run context computation
  let t6 : Instant = Instant::now ();
  let context_types =
    compute_and_store_context_types (
      &tantivy_index,
      &had_id_set,
      &all_node_ids,
      &link_targets,
      &contains_map,
      &reverse_map ) ?;
  let context_time : f64 = t6 . elapsed () . as_secs_f64 ();
  println! ("4. Context computation: {:.3}s", context_time);
  println! ("   {} nodes received context types",
            context_types . len ());

  // Summary
  let total : f64 = read_time + extract_total
    + tantivy_pop_time + context_time;
  println! ("\n=== Summary ===");
  println! ("  Read .skg files:     {:.3}s ({:.0}%)",
            read_time, read_time / total * 100.0);
  println! ("  Extract context data: {:.6}s ({:.1}%)",
            extract_total, extract_total / total * 100.0);
  println! ("  Populate Tantivy:    {:.3}s ({:.0}%)",
            tantivy_pop_time, tantivy_pop_time / total * 100.0);
  println! ("  Context computation: {:.3}s ({:.0}%)",
            context_time, context_time / total * 100.0);
  println! ("  Total:               {:.3}s", total);

  // Run context computation 3 more times to get variance
  println! ("\n=== Repeated context computation (3 runs) ===");
  for i in 1 ..= 3 {
    let tantivy_index2 : TantivyIndex =
      create_empty_tantivy_index (
        &tantivy_dir . path () . join (format! ("run{}", i)) ) ?;
    skg::dbs::tantivy::update_index_with_nodes (
      &nodes, &tantivy_index2 ) ?;
    let t : Instant = Instant::now ();
    let _ = compute_and_store_context_types (
      &tantivy_index2,
      &had_id_set,
      &all_node_ids,
      &link_targets,
      &contains_map,
      &reverse_map ) ?;
    println! ("  Run {}: {:.3}s", i, t . elapsed () . as_secs_f64 ()); }

  println! ("\n=== Done ===");
  Ok (( )) }
