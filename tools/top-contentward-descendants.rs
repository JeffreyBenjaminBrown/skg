/* top-contentward-descendants: print nodes with the largest
 * transitive contentward descendants count (all nodes reachable
 * by following 'contains' from the node). Cycles and diamonds
 * are counted once.
 * Usage: cargo run --bin top-contentward-descendants [<config>] [<top-n>]
 *   default config: data/skgconfig.toml
 *   default top-n: 20
 *
 * Simpler than a recursive TypeQL query: we read .skg files once,
 * build the 'contains' adjacency map, then do one BFS per node.
 * O(nodes * avg_descendants) — a few seconds on 29k nodes.
 */

use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::filesystem::multiple_nodes::check_for_duplicate_ids_across_sources;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::types::misc::{ID, SkgConfig};
use skg::types::nodes::complete::NodeComplete;

use std::collections::{HashMap, HashSet, VecDeque};
use std::env;

fn main () -> Result<(), Box<dyn std::error::Error>> {
  let args : Vec<String> = env::args () . collect ();
  let config_path : &str =
    if args . len () > 1 { &args[1] }
    else { "data/skgconfig.toml" };
  let top_n : usize =
    if args . len () > 2 {
      args[2] . parse () . unwrap_or (20) }
    else { 20 };
  let config : SkgConfig = load_config (config_path) ?;
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config) ?;
  check_for_duplicate_ids_across_sources (
    &nodes, &config . data_root) ?;
  // Adjacency: pid → list of pids it directly contains.
  let adj : HashMap<ID, &[ID]> =
    nodes . iter ()
    . map ( |n| (n . pid . clone (), n . contains . as_slice ()) )
    . collect ();
  // Map titles for display.
  let title_of : HashMap<ID, &str> =
    nodes . iter ()
    . map ( |n| (n . pid . clone (), n . title . as_str ()) )
    . collect ();
  let mut by_descendants : Vec<(usize, &NodeComplete)> =
    nodes . iter ()
    . map ( |n| {
      let count : usize = bfs_descendants_count (&adj, &n . pid);
      (count, n) } )
    . collect ();
  by_descendants . sort_by ( |a, b| b . 0 . cmp (&a . 0) );
  println! ("Top {} nodes by transitive contentward descendants (out of {}):",
           top_n, nodes . len ());
  println! ("(self not counted; cycles and diamonds each counted once)");
  for (count, node) in by_descendants . iter () . take (top_n) {
    println! ("{:>6}  {}  {}",
             count,
             node . pid . as_str (),
             title_of . get (&node . pid) . unwrap_or (&"?")); }
  Ok (()) }

/// BFS from `origin` over `contains`, counting every reachable
/// descendant exactly once (not including the origin itself).
fn bfs_descendants_count (
  adj    : &HashMap<ID, &[ID]>,
  origin : &ID,
) -> usize {
  let mut visited : HashSet<ID> = HashSet::new ();
  let mut queue   : VecDeque<ID> = VecDeque::new ();
  queue . push_back ( origin . clone () );
  visited . insert ( origin . clone () );
  while let Some (curr) = queue . pop_front () {
    if let Some (children) = adj . get (&curr) {
      for child in *children {
        if visited . insert ( child . clone () ) {
          queue . push_back ( child . clone () ); } } } }
  // Subtract 1 for the origin itself.
  visited . len () . saturating_sub (1) }
