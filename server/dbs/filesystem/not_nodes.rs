use crate::telescope::dependencies_manifest::read_foreign_manifest_orders;
use crate::types::misc::{SkgConfig, Skgrepo, SkgrepoName};

use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Components, Path, PathBuf};

/// If a skgrepo path does not exist:
/// - If it is marked owned (in the config), create it.
/// - If it is foreign, fail.
pub fn validate_skgrepo_paths_creating_owned_ones_if_needed (
  skgrepos: &HashMap<SkgrepoName, Skgrepo>
) -> io::Result<()> {
  for (skgrepo_name, skgrepo) in skgrepos . iter() {
    if !skgrepo . path . exists() { // If it doesn't exist
      if skgrepo . owned { // and it's owned, create it
        fs::create_dir_all(&skgrepo . path)?;
        tracing::info!("Created directory for repo '{}': {:?}",
                  skgrepo_name, skgrepo . path);
      } else { // and it's foreign, fail
        return Err(io::Error::new(
          io::ErrorKind::NotFound,
          format!("Foreign repo '{}' path does not exist: {:?}",
                  skgrepo_name, skgrepo . path )) ); }} }
  Ok(( )) }

/// The skgrepo names in TOML declaration order, read from the raw
/// '[[repos]]' array. The parsed 'SkgConfig.repos' is a HashMap and
/// loses order, so the loaders re-extract it here to fill
/// 'SkgConfig.skgrepo_order'. LOAD-BEARING: the owned entries'
/// declaration order is the privacy order, most public first (see the
/// chokepoint methods on 'SkgConfig'). An entry without a 'name' is named by its 'path' as
/// written, as 'SkgrepoToml' names it. Empty when the TOML has no
/// parseable skgrepos array.
fn skgrepo_order_from_toml (
  contents : &str,
) -> Vec<SkgrepoName> {
  toml::from_str::<toml::Value> (contents) . ok ()
    . as_ref ()
    . and_then ( |v| v . get ("repos") )
    . and_then ( |s| s . as_array () )
    . map ( |arr| arr . iter ()
            . filter_map ( |t| t . get ("name")
                           . or ( t . get ("path") )
                           . and_then ( |n| n . as_str () )
                           . map (SkgrepoName::from) )
            . collect () )
    . unwrap_or_default () }

/// Named skgrepo-sets are retired: skgrepo-sets are now the prefixes of
/// the config's privacy ladder (see 'docs/telescopes.org', "The privacy
/// order"). A config still defining
/// '[[repo_sets]]' would silently mean something else than its
/// author intended, so its presence is a hard error. Likewise
/// 'user_owns_it' and 'owned_folder': ownership is the per-entry
/// 'foreign' flag, and serde would silently ignore either key -- a
/// silent ownership flip -- so each is a hard error.
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
  let has_skgrepo_sets : bool =
    parsed . as_ref ()
    . map ( |v| v . get ("repo_sets") . is_some () )
    . unwrap_or (false);
  if has_skgrepo_sets {
    return Err ( concat! (
      "This config defines [[repo_sets]], a retired mechanism. ",
      "Repo-sets are now the prefixes of the owned [[repos]] order: ",
      "list your owned repos most-public-first, and select a set by ",
      "naming the most private owned repo to show. ",
      "See docs/telescopes.org, \"The privacy order\". ",
      "Delete the [[repo_sets]] entries and, if a ",
      "deleted set was your default_repo_set, replace that with a ",
      "repo name, or leave it out for no restriction." ) . into () ); }
  let has_user_owns_it : bool =
    parsed . as_ref ()
    . and_then ( |v| v . get ("repos") )
    . and_then ( |s| s . as_array () )
    . map ( |arr| arr . iter ()
            . any ( |t| t . get ("user_owns_it") . is_some () ))
    . unwrap_or (false);
  if has_user_owns_it {
    return Err ( concat! (
      "This config sets 'user_owns_it' on a repo, a retired key. ",
      "Each [[repos]] entry is owned unless it says ",
      "'foreign = true'. Delete the 'user_owns_it' lines, and add ",
      "'foreign = true' to each entry that had ",
      "'user_owns_it = false'. See docs/api-and-formats.org, ",
      "\"Skgrepos\"." ) . into () ); }
  let has_owned_folder : bool =
    parsed . as_ref ()
    . map ( |v| v . get ("owned_folder") . is_some () )
    . unwrap_or (false);
  if has_owned_folder {
    return Err ( concat! (
      "This config sets 'owned_folder', a retired key. No folder ",
      "name means anything to skg: each [[repos]] entry is owned ",
      "unless it says 'foreign = true'. ",
      "bash/migrate-to-foreign-flag.sh rewrites a config this way ",
      "(and renames its owned folder). See ",
      "docs/api-and-formats.org, \"Skgrepos\"." ) . into () ); }
  Ok (( )) }

/// Herald-label defaulting. Must run after 'make_paths_absolute' (so
/// 'data_root' and absolute paths exist), with 'raw_paths' captured
/// from the skgrepos before it. A skgrepo whose 'name' was defaulted
/// (== its raw path string) and which has no configured abbreviation
/// gets one if it is owned: its path below the data root, minus the
/// first component, which in the intended layout DATA_ROOT/AUTHOR/REPO
/// is the author folder (e.g. "jeff/notes" reads as "notes"). A
/// foreign skgrepo keeps the full "author/repo" form, mirroring the
/// folder layout.
fn derive_default_abbreviations (
  config    : &mut SkgConfig,
  raw_paths : &HashMap<SkgrepoName, PathBuf>,
) {
  let data_root : PathBuf = config . data_root . clone ();
  for skgrepo in config . skgrepos . values_mut () {
    let raw_path_string : Option<String> =
      raw_paths . get ( &skgrepo . name )
      . map ( |p| p . to_string_lossy () . into_owned () );
    let name_was_defaulted : bool =
      Some ( skgrepo . name . 0 . as_str () )
      == raw_path_string . as_deref ();
    if name_was_defaulted
    && skgrepo . abbreviation . is_none ()
    && skgrepo . owned {
      skgrepo . abbreviation =
        path_below_data_root_minus_author_folder (
          &skgrepo . path, &data_root ); }}}

/// The part of 'path' below 'data_root' after its first component
/// (the author folder), or None if 'path' is not below 'data_root'
/// or has no component after the first.
fn path_below_data_root_minus_author_folder (
  path      : &Path,
  data_root : &Path,
) -> Option<String> {
  let below_data_root : &Path =
    path . strip_prefix (data_root) . ok () ?;
  let mut components : Components =
    below_data_root . components ();
  components . next () ?;
  let rest : String =
    components . as_path () . to_string_lossy () . into_owned ();
  if rest . is_empty () { None } else { Some (rest) }}

/// Top-level keys that skg once read under another spelling, each
/// beside its current spelling, so that the warning about the old one
/// can name the new one.
const RENAMED_SKGCONFIG_KEYS : [(&str, &str); 1] = [
  ("max_role_tree_depth", "max_roleTree_depth"), ];

/// Deserialized only to find the top-level keys of a skgconfig.toml
/// that 'SkgConfig' does not know: serde hands the flattened config
/// every key it knows, and the catch-all table keeps the rest.
#[derive(Deserialize)]
struct SkgConfigBesideUnknownKeys {
  #[serde (flatten)]
  _config      : SkgConfig,
  #[serde (flatten)]
  unknown_keys : toml::Table, }

/// One warning per top-level key of the config text that 'SkgConfig'
/// does not know, which serde would otherwise ignore silently.
/// A key that was renamed gets its current spelling named.
/// Runs after 'SkgConfig' parsed the same text, so this parse cannot
/// fail for a reason the first did not report; if it does anyway,
/// there are no warnings.
pub fn unknown_top_level_config_key_warnings (
  contents : &str,
) -> Vec<String> {
  let unknown_keys : Vec<String> =
    toml::from_str::<SkgConfigBesideUnknownKeys> (contents)
    . map ( |parsed| parsed . unknown_keys . keys () . cloned ()
            . collect () )
    . unwrap_or_default ();
  unknown_keys . iter ()
    . map ( |key| {
      let unknown : String = format! (
        "skgconfig.toml sets '{}', a key skg does not know, so it has no effect.",
        key );
      match RENAMED_SKGCONFIG_KEYS . iter ()
        . find ( |(old, _new)| old == key )
      { Some ((_old, new)) => format! (
          "{} Its current spelling is '{}'.", unknown, new ),
        None => unknown }} )
    . collect () }

/// The config at 'path', beside one warning per top-level key it sets
/// that skg does not know (see 'unknown_top_level_config_key_warnings').
/// An unknown key is warned about, never refused.
pub fn load_config (
  path: &str )
  -> Result <(SkgConfig, Vec<String>),
             Box<dyn std::error::Error>>
{ let (mut config, contents) : (SkgConfig, String) =
    config_from_file_before_checking_paths (path) ?;
  validate_skgrepo_paths_creating_owned_ones_if_needed(
    &config . skgrepos)?;
  read_foreign_manifests_into_config (&mut config);
  Ok (( config,
        unknown_top_level_config_key_warnings (&contents) )) }

/// What the foreign skgrepos' manifests say of their fold order
/// ('read_foreign_manifest_orders'), recorded in 'config'. Runs once
/// the skgrepo paths are final.
pub(crate) fn read_foreign_manifests_into_config (
  config : &mut SkgConfig,
) {
  let orders = read_foreign_manifest_orders (config);
  config . set_foreign_manifest_orders (orders); }

/// The config the TOML file at 'path' states, its paths made absolute,
/// with the file's text; its skgrepo paths are not yet checked
/// ('validate_skgrepo_paths_creating_owned_ones_if_needed'), so that a
/// test can replace them first ('load_config_with_overrides' in
/// 'server/test_utils.rs').
pub(crate) fn config_from_file_before_checking_paths (
  path : &str,
) -> Result <(SkgConfig, String), Box<dyn std::error::Error>> {
  if !Path::new (path) . exists() {
    return Err(format!("Config file not found: {}",
                       path)
               . into( )); }
  let contents: String = fs::read_to_string (path) ?;
  reject_retired_config_keys (&contents) ?;
  let mut config: SkgConfig =
    toml::from_str (&contents) ?;
  config . skgrepo_order = skgrepo_order_from_toml (&contents);
  let raw_paths : HashMap<SkgrepoName, PathBuf> =
    config . skgrepos . iter ()
    . map ( |(name, s)| (name . clone (), s . path . clone ()) )
    . collect ();
  config . config_path =
    fs::canonicalize (path)
    . unwrap_or_else ( |_| PathBuf::from (path) );
  config . data_root = {
    // Canonicalized so that downstream joins (make_paths_absolute,
    // path_from_pid_and_repo, strip_prefix in get_file_path) yield
    // absolute paths uniformly -- matters for paths whose files do
    // not exist on disk (e.g. ghosts), where the
    // canonicalize-in-handler fallback would otherwise leave the
    // raw path relative while data_root is absolute, and
    // strip_prefix would silently fail.
    let raw : PathBuf = Path::new (path)
      . parent ()
      . unwrap_or ( Path::new (".") )
      . to_path_buf ();
    fs::canonicalize (&raw) . unwrap_or (raw) };
  make_paths_absolute (&mut config);
  derive_default_abbreviations (&mut config, &raw_paths);
  validate_skgrepo_sets (&config)?;
  Ok (( config, contents )) }

fn validate_skgrepo_sets (
  config : &SkgConfig,
) -> Result<(), Box<dyn std::error::Error>> {
  if let Some (default) = &config . default_skgrepo_set {
    if ! config . skgrepos . contains_key (default) {
      return Err (format! (
        "default_repo_set '{}' names no configured repo. It must name the most private owned repo to show; left out, it is the config's most private owned repo.",
        default ) . into ()); }
    if ! config . skgrepo_is_owned (default) {
      return Err (format! (
        "default_repo_set '{}' names a foreign repo. It must name the most private owned repo to show: foreign repos are not on the privacy ladder, and are shown unless a connection hides them.",
        default ) . into ()); }}
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
  for skgrepo in config . skgrepos . values_mut () {
    if skgrepo . path . is_relative () {
      skgrepo . path = root . join (
        &skgrepo . path ); } } }

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
