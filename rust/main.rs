/* USAGE
There is an optional command-line argument: the config file path.
See the SkgConfig type at rust/types/misc.rs,
or the example at data/skgconfig.toml.
*/

use skg::file_io::load_config;
use skg::types::{ SkgConfig };
use std::env;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let args: Vec<String> = env::args().collect();

  let config_path: String =
    if args.len() > 1 { // config from command line, if given
      args[1].clone()
    } else { // default config
      "data/skgconfig.toml".to_string() };
  let config: SkgConfig = load_config (&config_path) ?;

  skg::serve::serve (config)
    . map_err ( |e| Box::new(e)
                 as Box<dyn std::error::Error>) ?;
  Ok (( )) }
