/// USAGE:
/// There is an optional command-line argument: the config file path.
/// See the SkgConfig type at server/types/misc.rs,
/// or the example at data/skgconfig.toml.
///
/// Subcommand: import-org-roam <org-dir> <skg-output-dir> <source-nickname>
/// Converts org-roam .org files to .skg files.

use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::init::initialize_dbs;
use skg::import_org_roam::import_org_roam_directory;
use skg::serve::serve;
use skg::serve::timing_log::{clear_timing_log, timed};
use skg::types::misc::{SkgConfig, SourceName, TantivyIndex};

use std::error::Error;
use std::env;
use std::path::Path;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

fn main() -> Result<(), Box<dyn Error>> {
  let args: Vec<String> = env::args() . collect();

  if args . len() > 1 && args[1] == "import-org-roam" {
    return run_import (&args); }

  let config: SkgConfig = load_config (
    & { let config_path: String =
          if args . len() > 1 { // config from command line, if given
            args[1] . clone()
          } else { // default config
            "data/skgconfig.toml" . to_string() };
        config_path } ) ?;

  clear_timing_log (&config);
  let (typedb_driver, tantivy_index)
    : (Arc<TypeDBDriver>, TantivyIndex)
    = timed ( &config, "initialize_dbs",
              || initialize_dbs (&config));

  serve (config, typedb_driver, tantivy_index)
    . map_err ( |e| Box::new (e)
                 as Box<dyn Error>) ?;
  Ok (( )) }

fn run_import (
  args : &[String],
) -> Result<(), Box<dyn Error>> {
  if args . len() < 5 {
    eprintln! ("Usage: cargo run -- import-org-roam <org-dir> <skg-output-dir> <source-nickname>");
    std::process::exit (1); }
  let org_dir    : &Path       = Path::new (&args[2]);
  let output_dir : &Path       = Path::new (&args[3]);
  let source     : SourceName  = SourceName::from (&args[4]);
  let stats : skg::import_org_roam::ImportStats =
    import_org_roam_directory (org_dir, output_dir, &source)?;
  println! ("{}", stats);
  for err in &stats . errors {
    eprintln! ("  {}", err); }
  Ok (( )) }
