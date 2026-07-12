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
/// author intended, so its presence is a hard error. Likewise
/// 'user_owns_it': ownership is now derived from the source's path
/// (under 'owned_folder' = owned), and serde would silently IGNORE
/// the unknown key -- a silent ownership flip -- so it too is a hard
/// error.
fn reject_retired_config_keys (
  contents : &str,
) -> Result<(), Box<dyn std::error::Error>> {
  let parsed : Option<toml::Value> =
    toml::from_str::<toml::Value> (contents) . ok ();
  let has_source_sets : bool =
    parsed . as_ref ()
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
  let has_user_owns_it : bool =
    parsed . as_ref ()
    . and_then ( |v| v . get ("sources") )
    . and_then ( |s| s . as_array () )
    . map ( |arr| arr . iter ()
            . any ( |t| t . get ("user_owns_it") . is_some () ))
    . unwrap_or (false);
  if has_user_owns_it {
    return Err ( concat! (
      "This config sets 'user_owns_it' on a source, a retired key. ",
      "Ownership is now derived from the source's path: sources ",
      "under the config's owned_folder (default \"owned\", intended ",
      "layout DATA_ROOT/AUTHOR/REPO) are owned; all others are ",
      "foreign. Move each owned source's directory under that ",
      "folder, update its 'path', and delete the 'user_owns_it' ",
      "lines. bash/migrate-to-author-folders.sh does this for a ",
      "whole config at once. See ",
      "TODO/user-owned_autofork_chain/5_plan.org, work item ",
      "privacy-order." ) . into () ); }
  Ok (( )) }

/// Fills the DERIVED parts of each source. Must run AFTER
/// 'make_paths_absolute' (so 'data_root' and absolute paths exist),
/// with 'raw_paths' captured from the sources BEFORE it:
/// - 'user_owns_it': true iff the source's absolute path sits under
///   DATA_ROOT/OWNED_FOLDER (the author-folder layout: the user's
///   own author folder holds exactly the owned sources).
/// - herald-label defaulting: a source whose 'name' was defaulted
///   (== its raw path string) and which has no configured
///   abbreviation gets one -- for an owned source, the path
///   relative to the owned folder (e.g. "owned/notes" reads as
///   "notes"); a foreign source keeps the full "author/repo" form,
///   mirroring the folder layout.
fn derive_ownership_and_labels (
  config    : &mut SkgConfig,
  raw_paths : &HashMap<SourceName, PathBuf>,
) {
  let owned_root : PathBuf =
    config . data_root . join ( &config . owned_folder );
  for source in config . sources . values_mut () {
    source . user_owns_it =
      source . path . starts_with (&owned_root);
    let raw_path_string : Option<String> =
      raw_paths . get ( &source . name )
      . map ( |p| p . to_string_lossy () . into_owned () );
    let name_was_defaulted : bool =
      Some ( source . name . 0 . as_str () )
      == raw_path_string . as_deref ();
    if name_was_defaulted
    && source . abbreviation . is_none ()
    && source . user_owns_it {
      let trimmed : String =
        source . path . strip_prefix (&owned_root)
        . map ( |p| p . to_string_lossy () . into_owned () )
        . unwrap_or_default ();
      if ! trimmed . is_empty () { // path == owned folder: keep full
        source . abbreviation = Some (trimmed); }}}}

pub fn load_config (
  path: &str )
  -> Result <SkgConfig,
             Box<dyn std::error::Error>>
{ if !Path::new (path) . exists() {
    return Err(format!("Config file not found: {}",
                       path)
               . into( )); }
  let contents: String = fs::read_to_string (path) ?;
  reject_retired_config_keys (&contents) ?;
  
  let mut config: SkgConfig =
    toml::from_str (&contents) ?;
  config . source_order = source_order_from_toml (&contents);
  let raw_paths : HashMap<SourceName, PathBuf> =
    config . sources . iter ()
    . map ( |(name, s)| (name . clone (), s . path . clone ()) )
    . collect ();
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
  derive_ownership_and_labels (&mut config, &raw_paths);
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
  reject_retired_config_keys (&contents)?;
  let mut config: SkgConfig =
    toml::from_str (&contents)?;
  config . source_order = source_order_from_toml (&contents);
  let raw_paths : HashMap<SourceName, PathBuf> =
    config . sources . iter ()
    . map ( |(name, s)| (name . clone (), s . path . clone ()) )
    . collect ();
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
  derive_ownership_and_labels (&mut config, &raw_paths);
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
