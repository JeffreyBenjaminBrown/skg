/// USAGE:
/// There is an optional command-line argument: the config file path.
/// See the SkgConfig type at server/types/misc.rs,
/// or the example at data/skgconfig.toml.

use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::init::initialize_dbs;
use skg::serve::serve;
use skg::serve::timing_log::{clear_timing_log, timed};
use skg::types::misc::{SkgConfig, TantivyIndex};

use std::error::Error;
use std::env;
use std::sync::Arc;
use neo4rs::Graph;
use tokio::runtime::Runtime;

fn main() -> Result<(), Box<dyn Error>> {
  let args: Vec<String> = env::args().collect();

  let config: SkgConfig = load_config (
    & { let config_path: String =
          if args.len() > 1 { // config from command line, if given
            args[1].clone()
          } else { // default config
            "data/skgconfig.toml".to_string() };
        config_path } ) ?;

  clear_timing_log ( &config );
  let rt : Runtime = Runtime::new() ?;
  let (graph, tantivy_index)
    : (Arc<Graph>, TantivyIndex)
    = timed ( &config, "initialize_dbs",
              || initialize_dbs ( &config, rt.handle() ));

  serve (config, graph, tantivy_index, rt.handle().clone())
    . map_err ( |e| Box::new(e)
                 as Box<dyn Error>) ?;
  Ok (( )) }
