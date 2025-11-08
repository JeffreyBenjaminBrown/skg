use crate::types::misc::SkgConfig;

use std::fs;
use std::path::Path;

pub fn load_config (
  path: &str )
  -> Result <SkgConfig,
             Box<dyn std::error::Error>> {

    if !Path::new(path).exists() {
      return Err(format!("Config file not found: {}",
                         path)
                 . into() ); }
    let contents: String = fs::read_to_string (path) ?;
    let config: SkgConfig = toml::from_str (&contents) ?;
    Ok (config) }
