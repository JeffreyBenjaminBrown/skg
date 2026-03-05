/* bench-context
 * Benchmark context computation strategies.
 * Usage: cargo run --bin bench-context [db-name]
 *   default db-name: skg-real
 *
 * Expects TypeDB to be running on 127.0.0.1:1729
 * with the named database already populated.
 */

use skg::context::{
  compute_and_store_context_types,
  ContextOriginType,
};
use skg::dbs::typedb::search::find_related_nodes;
use skg::dbs::typedb::util::extract_payload_from_typedb_string_rep;
use skg::types::misc::ID;

use futures::StreamExt;
use std::collections::{HashMap, HashSet};
use std::env;
use std::time::Instant;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Credentials,
  DriverOptions,
  Transaction,
  TransactionType,
  TypeDBDriver,
};

type ConceptRowStream =
  futures::stream::BoxStream<'static,
                             typedb_driver::Result<ConceptRow>>;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  let args: Vec<String> = env::args().collect();
  let db_name: &str =
    if args.len() > 1 { &args[1] } else { "skg-real" };

  println!("=== Context Computation Benchmark ===");
  println!("Database: {}", db_name);

  let driver: TypeDBDriver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?
  ).await?;

  // First: count nodes
  let t0: Instant = Instant::now();
  let node_count: usize = count_nodes(db_name, &driver).await?;
  println!("\nNode count: {} ({:.3}s)", node_count, t0.elapsed().as_secs_f64());

  // E0: Benchmark origin identification queries individually
  println!("\n--- E0: Origin identification ---");

  let t1: Instant = Instant::now();
  let roots: HashSet<ID> = find_all_roots(db_name, &driver).await?;
  let roots_time: f64 = t1.elapsed().as_secs_f64();
  println!("  Roots:             {:>6} found in {:.3}s", roots.len(), roots_time);

  let t2: Instant = Instant::now();
  let targets: HashSet<ID> = find_all_link_targets(db_name, &driver).await?;
  let targets_time: f64 = t2.elapsed().as_secs_f64();
  println!("  Link targets:      {:>6} found in {:.3}s", targets.len(), targets_time);

  let t3: Instant = Instant::now();
  let multi: HashSet<ID> = find_all_multi_contained(db_name, &driver).await?;
  let multi_time: f64 = t3.elapsed().as_secs_f64();
  println!("  Multi-contained:   {:>6} found in {:.3}s", multi.len(), multi_time);

  // All three in parallel
  let t4: Instant = Instant::now();
  let (r1, r2, r3) = futures::join!(
    find_all_roots(db_name, &driver),
    find_all_link_targets(db_name, &driver),
    find_all_multi_contained(db_name, &driver)
  );
  r1?; r2?; r3?;
  let parallel_time: f64 = t4.elapsed().as_secs_f64();
  println!("  All 3 in parallel:          {:.3}s (vs {:.3}s sequential)",
           parallel_time, roots_time + targets_time + multi_time);

  // Build origin map
  let mut origin_types: HashMap<ID, ContextOriginType> = HashMap::new();
  for id in &multi {
    origin_types.insert(id.clone(), ContextOriginType::MultiContained); }
  for id in &targets {
    origin_types.insert(id.clone(), ContextOriginType::Target); }
  for id in &roots {
    origin_types.insert(id.clone(), ContextOriginType::Root); }
  println!("\n  Total origins:     {:>6}", origin_types.len());

  // E1: In-memory contains map
  println!("\n--- E1: Load full contains map into memory ---");
  let t5: Instant = Instant::now();
  let contains_map: HashMap<ID, Vec<ID>> =
    load_full_contains_map(db_name, &driver).await?;
  let load_time: f64 = t5.elapsed().as_secs_f64();
  let edge_count: usize = contains_map.values().map(|v| v.len()).sum();
  println!("  {} containers, {} edges loaded in {:.3}s",
           contains_map.len(), edge_count, load_time);

  // Also build reverse map (contained -> containers)
  let t5b: Instant = Instant::now();
  let mut reverse_map: HashMap<ID, Vec<ID>> = HashMap::new();
  for (container, contained_list) in &contains_map {
    for contained in contained_list {
      reverse_map.entry(contained.clone())
        .or_insert_with(Vec::new)
        .push(container.clone()); } }
  println!("  Reverse map built in {:.3}s", t5b.elapsed().as_secs_f64());

  // E2: In-memory growth vs per-query growth
  println!("\n--- E2: Context growth strategies ---");

  // Pick 20 random origins for comparison
  let sample_origins: Vec<ID> = origin_types.keys()
    .take(20).cloned().collect();
  let origins_set: HashSet<ID> = origin_types.keys().cloned().collect();

  // Strategy A: per-node TypeDB queries (current implementation)
  let t6: Instant = Instant::now();
  let mut total_members_a: usize = 0;
  for origin in &sample_origins {
    let members: HashSet<ID> =
      grow_context_queries(db_name, &driver, origin, &origins_set).await?;
    total_members_a += members.len(); }
  let query_time: f64 = t6.elapsed().as_secs_f64();
  println!("  Per-node queries (20 origins): {:.3}s, {} total members",
           query_time, total_members_a);

  // Strategy B: in-memory lookup
  let t7: Instant = Instant::now();
  let mut total_members_b: usize = 0;
  for origin in &sample_origins {
    let members: HashSet<ID> =
      grow_context_inmemory(origin, &origins_set, &contains_map);
    total_members_b += members.len(); }
  let memory_time: f64 = t7.elapsed().as_secs_f64();
  println!("  In-memory lookup  (20 origins): {:.3}s, {} total members",
           memory_time, total_members_b);
  println!("  Speedup: {:.0}x", query_time / memory_time.max(0.000001));

  // Verify they agree
  if total_members_a != total_members_b {
    println!("  WARNING: member counts differ! queries={} memory={}",
             total_members_a, total_members_b);
  } else {
    println!("  Results agree."); }

  // E3: Full in-memory growth of ALL origins
  println!("\n--- E3: Full in-memory growth of all {} origins ---", origin_types.len());
  let t8: Instant = Instant::now();
  let mut all_members: usize = 0;
  let mut context_sizes: Vec<usize> = Vec::new();
  for (origin, _) in &origin_types {
    let members: HashSet<ID> =
      grow_context_inmemory(origin, &origins_set, &contains_map);
    context_sizes.push(members.len());
    all_members += members.len(); }
  let full_growth_time: f64 = t8.elapsed().as_secs_f64();
  context_sizes.sort();
  let median_size: usize =
    if context_sizes.is_empty() { 0 }
    else { context_sizes[context_sizes.len() / 2] };
  let max_size: usize = context_sizes.last().copied().unwrap_or(0);
  println!("  {:.3}s for {} origins", full_growth_time, origin_types.len());
  println!("  Total members: {}, median context size: {}, max: {}",
           all_members, median_size, max_size);

  // E4: Cycle detection (in-memory)
  println!("\n--- E4: Cycle detection ---");
  let t9: Instant = Instant::now();
  let all_node_ids: HashSet<ID> = find_all_node_ids(db_name, &driver).await?;
  println!("  {} total nodes fetched in {:.3}s",
           all_node_ids.len(), t9.elapsed().as_secs_f64());

  let covered: HashSet<ID> = {
    let mut s: HashSet<ID> = HashSet::new();
    for (origin, _) in &origin_types {
      let members: HashSet<ID> =
        grow_context_inmemory(origin, &origins_set, &contains_map);
      s.extend(members); }
    s };
  let remainder: HashSet<ID> =
    all_node_ids.difference(&covered).cloned().collect();
  println!("  Covered: {}, remainder: {} (potential cycle members)",
           covered.len(), remainder.len());

  if !remainder.is_empty() {
    let t10: Instant = Instant::now();
    let cycle_result = detect_cycles_inmemory(&remainder, &reverse_map);
    println!("  Cycle detection (in-memory): {:.3}s",
             t10.elapsed().as_secs_f64());
    println!("  Cycles found: {}, total cycle members: {}",
             cycle_result.len(),
             cycle_result.iter().map(|c| c.len()).sum::<usize>()); }

  // E5: Compare concurrency levels for per-query growth
  println!("\n--- E5: Concurrency sweep (per-query growth, 20 origins) ---");
  for concurrency in [1usize, 4, 16, 64] {
    let t: Instant = Instant::now();
    // Sequential loop with join_all per frontier, can't easily vary
    // outer concurrency without tokio::spawn (driver isn't Send).
    // Instead, measure the per-query approach at different frontier sizes.
    let mut total: usize = 0;
    for origin in &sample_origins {
      let members: HashSet<ID> =
        grow_context_queries_with_concurrency(
          db_name, &driver, origin, &origins_set, concurrency).await?;
      total += members.len(); }
    println!("  concurrency={:>3}: {:.3}s ({} members)",
             concurrency, t.elapsed().as_secs_f64(), total); }

  // E6: Batch query experiment
  println!("\n--- E6: Batch content queries ---");
  // Pick a sample of nodes that have children
  let parents_with_children: Vec<ID> = contains_map.keys()
    .take(100).cloned().collect();
  if parents_with_children.len() >= 20 {
    for batch_size in [1, 5, 10, 20] {
      let batch: &[ID] = &parents_with_children[..batch_size.min(parents_with_children.len())];
      let t: Instant = Instant::now();
      // Per-node queries
      for pid in batch {
        let _: HashSet<ID> = find_related_nodes(
          db_name, &driver, &[pid.clone()],
          "contains", "container", "contained"
        ).await?; }
      let per_node_time: f64 = t.elapsed().as_secs_f64();

      let t2: Instant = Instant::now();
      // Single batch query
      let _: HashSet<ID> = find_related_nodes(
        db_name, &driver, batch,
        "contains", "container", "contained"
      ).await?;
      let batch_time: f64 = t2.elapsed().as_secs_f64();

      println!("  batch_size={:>2}: per-node {:.3}s, batch {:.3}s, ratio {:.1}x",
               batch_size, per_node_time, batch_time,
               per_node_time / batch_time.max(0.000001)); } }

  // E7: End-to-end timing comparison
  println!("\n--- E7: End-to-end estimate ---");
  let load_map_time: f64 = load_time;
  let growth_time: f64 = full_growth_time;
  let origin_id_time: f64 = parallel_time;
  let total_inmemory: f64 = origin_id_time + load_map_time + growth_time;
  println!("  In-memory strategy:");
  println!("    Origin identification:  {:.3}s", origin_id_time);
  println!("    Load contains map:      {:.3}s", load_map_time);
  println!("    Grow all contexts:      {:.3}s", growth_time);
  println!("    Total:                  {:.3}s", total_inmemory);
  println!("  Per-query extrapolation (from 20-origin sample):");
  let extrapolated: f64 =
    origin_id_time + (query_time / 20.0) * origin_types.len() as f64;
  println!("    Estimated total:        {:.1}s", extrapolated);
  println!("    Speedup from in-memory: {:.0}x",
           extrapolated / total_inmemory.max(0.001));

  println!("\n=== Done ===");
  Ok(())
}


// ---------- Helper functions ----------

async fn count_nodes(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<usize, Box<dyn std::error::Error>> {
  let tx: Transaction = driver.transaction(db_name, TransactionType::Read).await?;
  let mut stream: ConceptRowStream = {
    let answer: QueryAnswer = tx.query(
      "match $n isa node, has id $nid; select $nid;".to_string()
    ).await?;
    answer }.into_rows();
  let mut count: usize = 0;
  while let Some(row_result) = stream.next().await {
    let _: ConceptRow = row_result?;
    count += 1; }
  Ok(count) }

async fn find_all_roots(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<HashSet<ID>, Box<dyn std::error::Error>> {
  let tx: Transaction = driver.transaction(db_name, TransactionType::Read).await?;
  let mut stream: ConceptRowStream = {
    let answer: QueryAnswer = tx.query(
      r#"match
        $node isa node, has id $nid;
        not { $rel isa contains ( contained: $node ); };
        select $nid;"#.to_string()
    ).await?;
    answer }.into_rows();
  let mut result: HashSet<ID> = HashSet::new();
  while let Some(row_result) = stream.next().await {
    let row: ConceptRow = row_result?;
    if let Some(concept) = row.get("nid")? {
      result.insert(ID(extract_payload_from_typedb_string_rep(&concept.to_string()))); } }
  Ok(result) }

async fn find_all_link_targets(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<HashSet<ID>, Box<dyn std::error::Error>> {
  let tx: Transaction = driver.transaction(db_name, TransactionType::Read).await?;
  let mut stream: ConceptRowStream = {
    let answer: QueryAnswer = tx.query(
      r#"match
        $node isa node, has id $nid;
        $rel isa textlinks_to ( dest: $node );
        select $nid;"#.to_string()
    ).await?;
    answer }.into_rows();
  let mut result: HashSet<ID> = HashSet::new();
  while let Some(row_result) = stream.next().await {
    let row: ConceptRow = row_result?;
    if let Some(concept) = row.get("nid")? {
      result.insert(ID(extract_payload_from_typedb_string_rep(&concept.to_string()))); } }
  Ok(result) }

async fn find_all_multi_contained(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<HashSet<ID>, Box<dyn std::error::Error>> {
  let tx: Transaction = driver.transaction(db_name, TransactionType::Read).await?;
  let mut stream: ConceptRowStream = {
    let answer: QueryAnswer = tx.query(
      r#"match
        $node isa node, has id $nid;
        $c1 isa node;
        $c2 isa node;
        $r1 isa contains ( container: $c1, contained: $node );
        $r2 isa contains ( container: $c2, contained: $node );
        not { $c1 is $c2; };
        select $nid;"#.to_string()
    ).await?;
    answer }.into_rows();
  let mut result: HashSet<ID> = HashSet::new();
  while let Some(row_result) = stream.next().await {
    let row: ConceptRow = row_result?;
    if let Some(concept) = row.get("nid")? {
      result.insert(ID(extract_payload_from_typedb_string_rep(&concept.to_string()))); } }
  Ok(result) }

async fn find_all_node_ids(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<HashSet<ID>, Box<dyn std::error::Error>> {
  let tx: Transaction = driver.transaction(db_name, TransactionType::Read).await?;
  let mut stream: ConceptRowStream = {
    let answer: QueryAnswer = tx.query(
      "match $node isa node, has id $nid; select $nid;".to_string()
    ).await?;
    answer }.into_rows();
  let mut result: HashSet<ID> = HashSet::new();
  while let Some(row_result) = stream.next().await {
    let row: ConceptRow = row_result?;
    if let Some(concept) = row.get("nid")? {
      result.insert(ID(extract_payload_from_typedb_string_rep(&concept.to_string()))); } }
  Ok(result) }

/// Load the entire container->contained mapping from TypeDB.
async fn load_full_contains_map(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<HashMap<ID, Vec<ID>>, Box<dyn std::error::Error>> {
  let tx: Transaction = driver.transaction(db_name, TransactionType::Read).await?;
  let mut stream: ConceptRowStream = {
    let answer: QueryAnswer = tx.query(
      r#"match
        $container isa node, has id $cid;
        $contained isa node, has id $did;
        $rel isa contains ( container: $container,
                            contained: $contained );
        select $cid, $did;"#.to_string()
    ).await?;
    answer }.into_rows();
  let mut result: HashMap<ID, Vec<ID>> = HashMap::new();
  while let Some(row_result) = stream.next().await {
    let row: ConceptRow = row_result?;
    let container_id: Option<ID> = row.get("cid")?
      .map(|c| ID(extract_payload_from_typedb_string_rep(&c.to_string())));
    let contained_id: Option<ID> = row.get("did")?
      .map(|c| ID(extract_payload_from_typedb_string_rep(&c.to_string())));
    if let (Some(cid), Some(did)) = (container_id, contained_id) {
      result.entry(cid).or_insert_with(Vec::new).push(did); } }
  Ok(result) }

/// Grow context using per-node TypeDB queries with configurable concurrency.
async fn grow_context_queries_with_concurrency(
  db_name: &str,
  driver: &TypeDBDriver,
  origin: &ID,
  origins: &HashSet<ID>,
  max_concurrent: usize,
) -> Result<HashSet<ID>, Box<dyn std::error::Error>> {
  let mut members: HashSet<ID> = HashSet::new();
  members.insert(origin.clone());
  let mut frontier: Vec<ID> = vec![origin.clone()];
  while !frontier.is_empty() {
    let frontier_vecs: Vec<Vec<ID>> = frontier.iter()
      .map(|pid| vec![pid.clone()])
      .collect();
    let results: Vec<Result<HashSet<ID>, Box<dyn std::error::Error>>> =
      futures::stream::iter(
        frontier_vecs.iter().map(|pids| {
          async move {
            find_related_nodes(
              db_name, driver, pids,
              "contains", "container", "contained"
            ).await } }))
      .buffer_unordered(max_concurrent)
      .collect()
      .await;
    let mut next_frontier: Vec<ID> = Vec::new();
    for result in results {
      let contents: HashSet<ID> = result?;
      for content_id in contents {
        if origins.contains(&content_id) && &content_id != origin { continue; }
        if members.contains(&content_id) { continue; }
        members.insert(content_id.clone());
        next_frontier.push(content_id); } }
    frontier = next_frontier; }
  Ok(members) }

/// Grow context using per-node TypeDB queries (current approach).
async fn grow_context_queries(
  db_name: &str,
  driver: &TypeDBDriver,
  origin: &ID,
  origins: &HashSet<ID>,
) -> Result<HashSet<ID>, Box<dyn std::error::Error>> {
  let mut members: HashSet<ID> = HashSet::new();
  members.insert(origin.clone());
  let mut frontier: Vec<ID> = vec![origin.clone()];
  while !frontier.is_empty() {
    let frontier_vecs: Vec<Vec<ID>> = frontier.iter()
      .map(|pid| vec![pid.clone()])
      .collect();
    let futures: Vec<_> = frontier_vecs.iter()
      .map(|pids| find_related_nodes(
        db_name, driver, pids,
        "contains", "container", "contained"))
      .collect();
    let results: Vec<Result<HashSet<ID>, Box<dyn std::error::Error>>> =
      futures::future::join_all(futures).await;
    let mut next_frontier: Vec<ID> = Vec::new();
    for result in results {
      let contents: HashSet<ID> = result?;
      for content_id in contents {
        if origins.contains(&content_id) && &content_id != origin { continue; }
        if members.contains(&content_id) { continue; }
        members.insert(content_id.clone());
        next_frontier.push(content_id); } }
    frontier = next_frontier; }
  Ok(members) }

/// Grow context using in-memory contains map.
fn grow_context_inmemory(
  origin: &ID,
  origins: &HashSet<ID>,
  contains_map: &HashMap<ID, Vec<ID>>,
) -> HashSet<ID> {
  let mut members: HashSet<ID> = HashSet::new();
  members.insert(origin.clone());
  let mut frontier: Vec<ID> = vec![origin.clone()];
  while !frontier.is_empty() {
    let mut next_frontier: Vec<ID> = Vec::new();
    for pid in &frontier {
      if let Some(children) = contains_map.get(pid) {
        for child in children {
          if origins.contains(child) && child != origin { continue; }
          if members.contains(child) { continue; }
          members.insert(child.clone());
          next_frontier.push(child.clone()); } } }
    frontier = next_frontier; }
  members }

/// Detect cycles in a set of nodes using in-memory reverse map.
fn detect_cycles_inmemory(
  remainder: &HashSet<ID>,
  reverse_map: &HashMap<ID, Vec<ID>>,
) -> Vec<HashSet<ID>> {
  let mut cycles: Vec<HashSet<ID>> = Vec::new();
  let mut visited: HashSet<ID> = HashSet::new();
  for start in remainder {
    if visited.contains(start) { continue; }
    let mut path: Vec<ID> = vec![start.clone()];
    let mut path_set: HashSet<ID> = HashSet::from([start.clone()]);
    let mut current: ID = start.clone();
    loop {
      let containers: &[ID] = reverse_map.get(&current)
        .map(|v| v.as_slice()).unwrap_or(&[]);
      if containers.is_empty() || containers.len() > 1 {
        // Not a simple chain — mark path as visited and move on
        for n in &path { visited.insert(n.clone()); }
        break; }
      let container: &ID = &containers[0];
      if path_set.contains(container) {
        // Found cycle
        let cycle_start: usize = path.iter()
          .position(|id| id == container).unwrap();
        let cycle: HashSet<ID> = path[cycle_start..].iter().cloned().collect();
        for n in &path { visited.insert(n.clone()); }
        for n in &cycle { visited.insert(n.clone()); }
        cycles.push(cycle);
        break;
      } else {
        path.push(container.clone());
        path_set.insert(container.clone());
        current = container.clone(); } } }
  cycles }
