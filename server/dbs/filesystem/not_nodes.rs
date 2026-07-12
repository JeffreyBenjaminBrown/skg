use crate::types::misc::{SkgConfig, SkgfileSource, SourceName};

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// If a source path does not exist:
/// - If it is marked owned (in the config), create it.
/// - If it is foreign, fail.
pub fn validate_source_paths_creating_owned_ones_if_needed (
  sources: &HashMap<SourceName, SkgfileSource>
) -> io::Result<()> {
  for (source_name, source) in sources . iter() {
    if !source . path . exists() { // If it doesn't exist
      if source . user_owns_it { // and it's owned, create it
        fs::create_dir_all(&source . path)?;
        tracing::info!("Created directory for source '{}': {:?}",
                  source_name, source . path);
      } else { // and it's foreign, fail
        return Err(io::Error::new(
          io::ErrorKind::NotFound,
          format!("Foreign source '{}' path does not exist: {:?}",
                  source_name, source . path )) ); }} }
  Ok(( )) }

/// The source names in TOML declaration order, read from the raw
/// '[[sources]]' array. The parsed 'SkgConfig.sources' is a HashMap and
/// loses order, so the loaders re-extract it here to fill
/// 'SkgConfig.source_order'. LOAD-BEARING: declaration order is the
/// privacy order, most public first (see the chokepoint methods on
/// 'SkgConfig'). Empty when the TOML has no parseable sources array.
fn source_order_from_toml (
  contents : &str,
) -> Vec<SourceName> {
  toml::from_str::<toml::Value> (contents) . ok ()
    . as_ref ()
    . and_then ( |v| v . get ("sources") )
    . and_then ( |s| s . as_array () )
    . map ( |arr| arr . iter ()
            . filter_map ( |t| t . get ("name")
                           . and_then ( |n| n . as_str () )
                           . map (SourceName::from) )
            . collect () )
    . unwrap_or_default () }

/// Named source-sets are retired: source-sets are now the prefixes of
/// the config's privacy order (see TODO/user-owned_autofork_chain/
/// 5_plan.org, work item privacy-order). A config still defining
/// '[[source_sets]]' would silently mean something else than its
/// author intended, so its presence is a hard error.
fn reject_retired_source_sets_config (
  contents : &str,
) -> Result<(), Box<dyn std::error::Error>> {
  let has_source_sets : bool =
    toml::from_str::<toml::Value> (contents) . ok ()
    . as_ref ()
    . map ( |v| v . get ("source_sets") . is_some () )
    . unwrap_or (false);
  if has_source_sets {
    return Err ( concat! (
      "This config defines [[source_sets]], a retired mechanism. ",
      "Source-sets are now the PREFIXES of the [[sources]] order: ",
      "list your sources most-public-first, and select a set by ",
      "naming the most private source to make available (or 'all'). ",
      "See TODO/user-owned_autofork_chain/5_plan.org, work item ",
      "privacy-order. Delete the [[source_sets]] entries and, if a ",
      "deleted set was your default_source_set, replace that with a ",
      "source name or 'all'." ) . into () ); }
  Ok (( )) }

pub fn load_config (
  path: &str )
  -> Result <SkgConfig,
             Box<dyn std::error::Error>>
{ if !Path::new (path) . exists() {
    return Err(format!("Config file not found: {}",
                       path)
               . into( )); }
  let contents: String = fs::read_to_string (path) ?;
  reject_retired_source_sets_config (&contents) ?;
  let mut config: SkgConfig =
    toml::from_str (&contents) ?;
  config . source_order = source_order_from_toml (&contents);
  config . config_path =
    fs::canonicalize (path)
    . unwrap_or_else ( |_| PathBuf::from (path) );
  config . data_root = {
    // Canonicalized so that downstream joins (make_paths_absolute,
    // path_from_pid_and_source, strip_prefix in get_file_path) yield
    // absolute paths uniformly -- matters for paths whose files do
    // not exist on disk (e.g. Deleted phantoms), where the
    // canonicalize-in-handler fallback would otherwise leave the
    // raw path relative while data_root is absolute, and
    // strip_prefix would silently fail.
    let raw : PathBuf = Path::new (path)
      . parent ()
      . unwrap_or ( Path::new (".") )
      . to_path_buf ();
    fs::canonicalize (&raw) . unwrap_or (raw) };
  make_paths_absolute (&mut config);
  validate_source_sets (&config)?;
  validate_source_paths_creating_owned_ones_if_needed(
    &config . sources)?;
  Ok (config) }

/// Load config from TOML file with optional overrides for testing.
///
/// - If `db_name` is Some, overrides db_name and sets tantivy_folder to /tmp/tantivy-{db_name}
/// - `source_overrides` replaces paths for the specified source names
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
pub fn load_config_with_overrides (
  path             : &str,
  db_name          : Option<&str>, // None for no override
  source_overrides : &[(&str, std::path::PathBuf)],
) -> Result <SkgConfig, Box<dyn std::error::Error>> {
  if !Path::new (path) . exists() {
    return Err(format!("Config file not found: {}", path) . into()); }
  let contents: String = fs::read_to_string (path)?;
  reject_retired_source_sets_config (&contents)?;
  let mut config: SkgConfig =
    toml::from_str (&contents)?;
  config . source_order = source_order_from_toml (&contents);
  config . config_path =
    fs::canonicalize (path)
    . unwrap_or_else ( |_| PathBuf::from (path) );
  config . data_root = {
    // See load_config above for why canonicalize is necessary here.
    let raw : PathBuf = Path::new (path)
      . parent ()
      . unwrap_or ( Path::new (".") )
      . to_path_buf ();
    fs::canonicalize (&raw) . unwrap_or (raw) };
  make_paths_absolute (&mut config);
  validate_source_sets (&config)?;
  if let Some (name) = db_name {
    config . db_name = name . to_string();
    config . tantivy_folder =
      std::path::PathBuf::from(format!("/tmp/tantivy-{}", name)); }
  for (source_name, new_path) in source_overrides {
    let key : SourceName = SourceName::from (*source_name);
    if let Some (source) = config . sources . get_mut (&key) {
      source . path = new_path . clone();
    } else {
      return Err(format!(
        "Source '{}' not found in config", source_name) . into()); }}
  validate_source_paths_creating_owned_ones_if_needed(
    &config . sources)?;
  Ok (config) }

fn validate_source_sets (
  config : &SkgConfig,
) -> Result<(), Box<dyn std::error::Error>> {
  if config . sources . contains_key (&SourceName::from ("all")) {
    return Err ("Configured source may not be named 'all'" . into ()); }
  if config . default_source_set . 0 != "all"
  && ! config . sources . contains_key (
       &SourceName::from ( config . default_source_set . 0 . as_str () )) {
    return Err (format! (
      "default_source_set '{}' names no configured source. It must be 'all' or the name of the most private source to make available.",
      config . default_source_set
    ) . into ()); }
  Ok (()) }

/// Resolve relative paths in the config against data_root.
/// Absolute paths are left unchanged.
fn make_paths_absolute (
  config : &mut SkgConfig,
) {
  let root : PathBuf = config . data_root . clone ();
  if config . tantivy_folder . is_relative () {
    config . tantivy_folder = root . join (
      &config . tantivy_folder ); }
  for source in config . sources . values_mut () {
    if source . path . is_relative () {
      source . path = root . join (
        &source . path ); } } }
