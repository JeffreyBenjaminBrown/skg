use crate::types::misc::{SkgConfig, SkgfileSource};

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;

/// If a source path does not exist:
/// - If it is marked owned (in the config), create it.
/// - If it is foreign, fail.
fn validate_source_paths_creating_owned_ones_if_needed (
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

/// Load config from TOML file with optional overrides for testing.
///
/// - If `db_name` is Some, overrides db_name and sets tantivy_folder to /tmp/tantivy-{db_name}
/// - `source_overrides` replaces paths for the specified source nicknames
///
/// # Examples
/// ```ignore
/// // Override just source paths:
/// let config = load_config_with_overrides(
///   "tests/my_test/fixtures/skgconfig.toml",
///   None,
///   &[("output", PathBuf::from("/tmp/output"))],
/// ).unwrap();
///
/// // Override just db_name (for tests needing unique databases):
/// let config = load_config_with_overrides(
///   "tests/my_test/fixtures/skgconfig.toml",
///   Some("skg-test-my-test"),
///   &[],
/// ).unwrap();
///
/// // Override both (for tests that copy fixtures to temp):
/// let config = load_config_with_overrides(
///   "tests/my_test/fixtures/skgconfig.toml",
///   Some("skg-test-my-test"),
///   &[("main", PathBuf::from("/tmp/fixtures-copy"))],
/// ).unwrap();
/// ```
fn load_config_with_overrides (
  path             : &str,
  db_name          : Option<&str>, // None for no override
  source_overrides : &[(&str, std::path::PathBuf)],
) -> Result <SkgConfig, Box<dyn std::error::Error>> {
  if !Path::new(path).exists() {
    return Err(format!("Config file not found: {}", path).into()); }
  let contents: String = fs::read_to_string(path)?;
  let mut config: SkgConfig = toml::from_str(&contents)?;
  if let Some(name) = db_name {
    config.db_name = name.to_string();
    config.tantivy_folder =
      std::path::PathBuf::from(format!("/tmp/tantivy-{}", name)); }
  for (nickname, new_path) in source_overrides {
    if let Some(source) = config.sources.get_mut(*nickname) {
      source.path = new_path.clone();
    } else {
      return Err(format!(
        "Source '{}' not found in config", nickname).into()); }}
  validate_source_paths_creating_owned_ones_if_needed(&config.sources)?;
  Ok(config) }
