use crate::types::misc::{SkgConfig, SkgfileSource};

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;

/// If a source path does not exist:
/// - If it is marked owned (in the config), create it.
/// - If it is foreign, fail.
pub fn validate_source_paths_creating_owned_ones_if_needed (
  sources: &HashMap<String, SkgfileSource>
) -> io::Result<()> {
  for (nickname, source) in sources.iter() {
    if !source.path.exists() { // If it doesn't exist
      if source.user_owns_it { // and it's owned, create it
        fs::create_dir_all(&source.path)?;
        eprintln!("Created directory for source '{}': {:?}",
                  nickname, source.path);
      } else { // and it's foreign, fail
        return Err(io::Error::new(
          io::ErrorKind::NotFound,
          format!("Foreign source '{}' path does not exist: {:?}",
                  nickname, source.path )) ); }} }
  Ok(( )) }

pub fn load_config (
  path: &str )
  -> Result <SkgConfig,
             Box<dyn std::error::Error>> {

    if !Path::new(path).exists() {
      return Err(format!("Config file not found: {}",
                         path)
                 . into( )); }
    let contents: String = fs::read_to_string (path) ?;
    let config: SkgConfig = toml::from_str (&contents) ?;
    validate_source_paths_creating_owned_ones_if_needed(
      &config.sources)?;
    Ok (config) }
