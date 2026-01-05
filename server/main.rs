/* USAGE
There is an optional command-line argument: the config file path.
See the SkgConfig type at server/types/misc.rs,
or the example at data/skgconfig.toml.
*/

use skg::dbs::filesystem::misc::load_config;
use skg::init::initialize_dbs;
use skg::serve::serve;
use skg::types::misc::{SkgConfig, TantivyIndex};

use std::error::Error;
use std::env;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

fn main() -> Result<(), Box<dyn Error>> {
  let args: Vec<String> = env::args().collect();

  let config_path: String =
    if args.len() > 1 { // config from command line, if given
      args[1].clone()
    } else { // default config
      "data/skgconfig.toml".to_string() };
  let config: SkgConfig = load_config (&config_path) ?;

  let (typedb_driver, tantivy_index)
    : (Arc<TypeDBDriver>, TantivyIndex)
    = initialize_dbs ( &config );

  serve (config, typedb_driver, tantivy_index)
    . map_err ( |e| Box::new(e)
                 as Box<dyn Error>) ?;
  Ok (( )) }
