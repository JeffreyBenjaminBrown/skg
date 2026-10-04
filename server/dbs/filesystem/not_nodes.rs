use crate::types::misc::{SkgConfig, SkgfileRepo, RepoName};

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// If a repo path does not exist:
/// - If it is marked owned (in the config), create it.
/// - If it is foreign, fail.
pub fn validate_repo_paths_creating_owned_ones_if_needed (
  repos: &HashMap<RepoName, SkgfileRepo>
) -> io::Result<()> {
  for (repo_name, repo) in repos . iter() {
    if !repo . path . exists() { // If it doesn't exist
      if repo . user_owns_it { // and it's owned, create it
        fs::create_dir_all(&repo . path)?;
        tracing::info!("Created directory for source '{}': {:?}",
                  repo_name, repo . path);
      } else { // and it's foreign, fail
        return Err(io::Error::new(
          io::ErrorKind::NotFound,
          format!("Foreign source '{}' path does not exist: {:?}",
                  repo_name, repo . path )) ); }} }
  Ok(( )) }

/// The repo names in TOML declaration order, read from the raw
/// '[[repos]]' array. The parsed 'SkgConfig.repos' is a HashMap and
/// loses order, so the loaders re-extract it here to fill
/// 'SkgConfig.repo_order'. LOAD-BEARING: declaration order is the
/// privacy order, most public first (see the chokepoint methods on
/// 'SkgConfig'). Empty when the TOML has no parseable repos array.
fn repo_order_from_toml (
  contents : &str,
) -> Vec<RepoName> {
  toml::from_str::<toml::Value> (contents) . ok ()
    . as_ref ()
    . and_then ( |v| v . get ("repos") )
    . and_then ( |s| s . as_array () )
    . map ( |arr| arr . iter ()
            . filter_map ( |t| t . get ("name")
                           . and_then ( |n| n . as_str () )
                           . map (RepoName::from) )
            . collect () )
    . unwrap_or_default () }

/// Named repo-sets are retired: repo-sets are now the prefixes of
/// the config's privacy order (see TODO/user-owned_autofork_chain/
/// 5_plan.org, work item privacy-order). A config still defining
/// '[[repo_sets]]' would silently mean something else than its
/// author intended, so its presence is a hard error. Likewise
/// 'user_owns_it': ownership is now derived from the repo's path
/// (under 'owned_folder' = owned), and serde would silently IGNORE
/// the unknown key -- a silent ownership flip -- so it too is a hard
/// error.
fn reject_retired_config_keys (
  contents : &str,
) -> Result<(), Box<dyn std::error::Error>> {
  let parsed : Option<toml::Value> =
    toml::from_str::<toml::Value> (contents) . ok ();
  let retired : Vec<&str> =
    ["db_name", "delete_on_quit", "auto_audit_daily"]
    . into_iter ()
    . filter (|key| parsed . as_ref ()
      .map (|value| value . get (*key) . is_some ())
      . unwrap_or (false))
    . collect ();
  if ! retired . is_empty () {
    return Err (format! (
      "This config sets retired TypeDB keys: {}. TypeDB has been removed; delete these keys from skgconfig.toml.",
      retired . join (", ")) . into ()); }
  let has_repo_sets : bool =
    parsed . as_ref ()
    . map ( |v| v . get ("repo_sets") . is_some () )
    . unwrap_or (false);
  if has_repo_sets {
    return Err ( concat! (
      "This config defines [[repo_sets]], a retired mechanism. ",
      "Source-sets are now the PREFIXES of the [[repos]] order: ",
      "list your sources most-public-first, and select a set by ",
      "naming the most private source to make available (or 'all'). ",
      "See TODO/user-owned_autofork_chain/5_plan.org, work item ",
      "privacy-order. Delete the [[repo_sets]] entries and, if a ",
      "deleted set was your default_repo_set, replace that with a ",
      "source name or 'all'." ) . into () ); }
  let has_user_owns_it : bool =
    parsed . as_ref ()
    . and_then ( |v| v . get ("repos") )
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

/// Fills the DERIVED parts of each repo. Must run AFTER
/// 'make_paths_absolute' (so 'data_root' and absolute paths exist),
/// with 'raw_paths' captured from the repos BEFORE it:
/// - 'user_owns_it': true iff the repo's absolute path sits under
///   DATA_ROOT/OWNED_FOLDER (the author-folder layout: the user's
///   own author folder holds exactly the owned repos).
/// - herald-label defaulting: a repo whose 'name' was defaulted
///   (== its raw path string) and which has no configured
///   abbreviation gets one -- for an owned repo, the path
///   relative to the owned folder (e.g. "owned/notes" reads as
///   "notes"); a foreign repo keeps the full "author/repo" form,
///   mirroring the folder layout.
fn derive_ownership_and_labels (
  config    : &mut SkgConfig,
  raw_paths : &HashMap<RepoName, PathBuf>,
) {
  let owned_root : PathBuf =
    config . data_root . join ( &config . owned_folder );
  for repo in config . repos . values_mut () {
    repo . user_owns_it =
      repo . path . starts_with (&owned_root);
    let raw_path_string : Option<String> =
      raw_paths . get ( &repo . name )
      . map ( |p| p . to_string_lossy () . into_owned () );
    let name_was_defaulted : bool =
      Some ( repo . name . 0 . as_str () )
      == raw_path_string . as_deref ();
    if name_was_defaulted
    && repo . abbreviation . is_none ()
    && repo . user_owns_it {
      let trimmed : String =
        repo . path . strip_prefix (&owned_root)
        . map ( |p| p . to_string_lossy () . into_owned () )
        . unwrap_or_default ();
      if ! trimmed . is_empty () { // path == owned folder: keep full
        repo . abbreviation = Some (trimmed); }}}}

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
  config . repo_order = repo_order_from_toml (&contents);
  let raw_paths : HashMap<RepoName, PathBuf> =
    config . repos . iter ()
    . map ( |(name, s)| (name . clone (), s . path . clone ()) )
    . collect ();
  config . config_path =
    fs::canonicalize (path)
    . unwrap_or_else ( |_| PathBuf::from (path) );
  config . data_root = {
    // Canonicalized so that downstream joins (make_paths_absolute,
    // path_from_pid_and_repo, strip_prefix in get_file_path) yield
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
  validate_repo_sets (&config)?;
  validate_repo_paths_creating_owned_ones_if_needed(
    &config . repos)?;
  Ok (config) }

/// Load config from TOML file with optional overrides for testing.
///
/// - If `test_name` is Some, sets tantivy_folder to /tmp/tantivy-{test_name}
/// - `repo_overrides` replaces paths for the specified repo names
///
/// # Examples
/// ```ignore
/// // Override just repo paths:
/// let config = load_config_with_overrides(
///   "tests/my_test/fixtures/skgconfig.toml",
///   None,
///   &[("output", PathBuf::from("/tmp/output"))],
/// ).unwrap();
///
/// // Select a unique test Tantivy folder:
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
  test_name        : Option<&str>,
  repo_overrides : &[(&str, std::path::PathBuf)],
) -> Result <SkgConfig, Box<dyn std::error::Error>> {
  if !Path::new (path) . exists() {
    return Err(format!("Config file not found: {}", path) . into()); }
  let contents: String = fs::read_to_string (path)?;
  reject_retired_config_keys (&contents)?;
  let mut config: SkgConfig =
    toml::from_str (&contents)?;
  config . repo_order = repo_order_from_toml (&contents);
  let raw_paths : HashMap<RepoName, PathBuf> =
    config . repos . iter ()
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
  validate_repo_sets (&config)?;
  if let Some (name) = test_name {
    config . tantivy_folder =
      std::path::PathBuf::from(format!("/tmp/tantivy-{}", name)); }
  for (repo_name, new_path) in repo_overrides {
    let key : RepoName = RepoName::from (*repo_name);
    if let Some (repo) = config . repos . get_mut (&key) {
      repo . path = new_path . clone();
    } else {
      return Err(format!(
        "Source '{}' not found in config", repo_name) . into()); }}
  validate_repo_paths_creating_owned_ones_if_needed(
    &config . repos)?;
  Ok (config) }

fn validate_repo_sets (
  config : &SkgConfig,
) -> Result<(), Box<dyn std::error::Error>> {
  if config . repos . contains_key (&RepoName::from ("all")) {
    return Err ("Configured source may not be named 'all'" . into ()); }
  if config . default_repo_set . 0 != "all"
  && ! config . repos . contains_key (
       &RepoName::from ( config . default_repo_set . 0 . as_str () )) {
    return Err (format! (
      "default_repo_set '{}' names no configured source. It must be 'all' or the name of the most private source to make available.",
      config . default_repo_set
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
  for repo in config . repos . values_mut () {
    if repo . path . is_relative () {
      repo . path = root . join (
        &repo . path ); } } }

#[cfg(test)]
mod retired_typedb_config_tests {
  use super::reject_retired_config_keys;

  #[test]
  fn each_retired_key_is_rejected_and_all_present_keys_are_named () {
    for key in ["db_name", "delete_on_quit", "auto_audit_daily"] {
      let input = format! ("{} = true\n", key);
      let message = reject_retired_config_keys (&input)
        . expect_err ("retired TypeDB config key must be rejected")
        . to_string ();
      assert! (message . contains (key), "{}", message);
      assert! (message . contains ("TypeDB has been removed"), "{}", message); }

    let message = reject_retired_config_keys (
      "db_name = 'old'\ndelete_on_quit = true\nauto_audit_daily = false\n")
      . expect_err ("all retired keys must be rejected")
      . to_string ();
    for key in ["db_name", "delete_on_quit", "auto_audit_daily"] {
      assert! (message . contains (key), "{}", message); }}
}
