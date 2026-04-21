/* top-contents: print nodes with the largest outbound 'contains'
 * list. Reads .skg files directly; no TypeDB needed.
 * Usage: cargo run --bin top-contents [<config-path>] [<top-n>]
 *   default config: data/skgconfig.toml
 *   default top-n: 20
 */

use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::types::misc::SkgConfig;
use skg::types::nodes::complete::NodeComplete;

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
  let mut by_contents_len : Vec<(usize, &NodeComplete)> =
    nodes . iter ()
    . map ( |n| (n . contains . len (), n) )
    . collect ();
  by_contents_len . sort_by ( |a, b| b . 0 . cmp (&a . 0) );
  println! ("Top {} nodes by direct contents count (out of {}):",
           top_n, nodes . len ());
  for (count, node) in by_contents_len . iter () . take (top_n) {
    println! ("{:>5}  {}  {}",
             count,
             node . pid . as_str (),
             node . title); }
  Ok (()) }
