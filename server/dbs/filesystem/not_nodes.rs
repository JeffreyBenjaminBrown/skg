use crate::types::misc::{SkgConfig, SourceCatalog, SourceName};

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[cfg(unix)]
use std::os::unix::fs::{DirBuilderExt, PermissionsExt};

/// If a source path does not exist:
/// - If it is marked owned (in the config), create it.
/// - If it is foreign, fail.
pub fn validate_source_paths_creating_owned_ones_if_needed (
  sources: &SourceCatalog,
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
                  source_name, source . path )) ); }
    } else if ! source . path . is_dir () {
      return Err (io::Error::new (
        io::ErrorKind::InvalidInput,
        format! ("Source '{}' path is not a directory: {:?}",
                 source_name, source . path) )); }}
  Ok(( )) }

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
/// using the raw configured paths retained by 'SourceCatalog':
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
  config : &mut SkgConfig,
) {
  let owned_root : PathBuf =
    config . data_root . join ( &config . owned_folder );
  for source_name in config . sources . ordered_names () {
    let configured_path : Option<PathBuf> = config . sources
      . configured_path (&source_name) . map (Path::to_path_buf);
    let source = config . sources . get_mut (&source_name)
      . expect ("ordered source exists");
    source . user_owns_it =
      source . path . starts_with (&owned_root);
    let raw_path_string : Option<String> =
      configured_path . as_ref ()
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

/// Resolve each validated directory to its physical identity, reject aliases,
/// and make the canonical absolute path the one used for filesystem access.
/// The catalog retains the exact configured path separately.
fn resolve_source_directory_identities (
  config : &mut SkgConfig,
) -> io::Result<()> {
  let mut first_source_by_identity : HashMap<PathBuf, SourceName> =
    HashMap::new ();
  for source_name in config . sources . ordered_names () {
    let path : PathBuf = config . sources . get (&source_name)
      . expect ("ordered source exists") . path . clone ();
    let identity : PathBuf = fs::canonicalize (&path) ?;
    if let Some (first) = first_source_by_identity . get (&identity) {
      return Err (io::Error::new (
        io::ErrorKind::InvalidInput,
        format! (
          "Configured sources '{}' ({}) and '{}' ({}) name the same physical directory: {}",
          first,
          config . sources . configured_path (first)
            . unwrap_or (Path::new ("")) . display (),
          source_name,
          config . sources . configured_path (&source_name)
            . unwrap_or (Path::new ("")) . display (),
          identity . display () ))); }
    first_source_by_identity . insert (
      identity . clone (), source_name . clone ());
    config . sources . set_resolved_directory (&source_name, identity);
  }
  Ok (( ))
}

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
  derive_ownership_and_labels (&mut config);
  validate_source_sets (&config)?;
  validate_source_paths_creating_owned_ones_if_needed(
    &config . sources)?;
  resolve_source_directory_identities (&mut config)?;
  validate_and_create_maintenance_archive_root (&mut config)?;
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
  derive_ownership_and_labels (&mut config);
  validate_source_sets (&config)?;
  if let Some (name) = db_name {
    config . db_name = name . to_string();
    config . tantivy_folder =
      std::path::PathBuf::from(format!("/tmp/tantivy-{}", name)); }
  for (source_name, new_path) in source_overrides {
    let key : SourceName = SourceName::from (*source_name);
    if ! config . sources . set_path_override (&key, new_path . clone ()) {
      return Err(format!(
        "Source '{}' not found in config", source_name) . into()); }}
  validate_source_paths_creating_owned_ones_if_needed(
    &config . sources)?;
  resolve_source_directory_identities (&mut config)?;
  validate_and_create_maintenance_archive_root (&mut config)?;
  Ok (config) }

/// Establish a private archive root whose physical identity cannot overlap a
/// source in either direction. This makes source observation and archive
/// traversal disjoint by construction instead of relying on exclusions.
fn validate_and_create_maintenance_archive_root (
  config : &mut SkgConfig,
) -> io::Result<()> {
  let configured : &Path = &config . maintenance_archive_folder;
  if configured . as_os_str () . is_empty () {
    return Err (io::Error::new (
      io::ErrorKind::InvalidInput,
      "maintenance_archive_folder may not be empty")); }
  if configured . components () . any (|component| matches! (
       component, std::path::Component::ParentDir))
  {
    return Err (io::Error::new (
      io::ErrorKind::InvalidInput,
      "maintenance_archive_folder may not contain '..'")); }
  let archive_path : PathBuf = if configured . is_absolute () {
    configured . to_path_buf ()
  } else {
    config . data_root . join (configured) };
  reject_archive_source_overlap (&archive_path, config)?;
  create_private_directory_all (&archive_path)?;
  let identity : PathBuf = fs::canonicalize (&archive_path)?;
  reject_archive_source_overlap (&identity, config)?;
  config . maintenance_archive_identity = identity;
  Ok (( ))
}

pub(crate) fn reject_archive_source_overlap (
  archive : &Path,
  config  : &SkgConfig,
) -> io::Result<()> {
  for source in config . sources . values () {
    let source_identity : &Path = config . sources
      . directory_identity (&source . name)
      . unwrap_or (&source . path);
    if archive == source_identity
    || archive . starts_with (source_identity)
    || source_identity . starts_with (archive)
    {
      return Err (io::Error::new (
        io::ErrorKind::InvalidInput,
        format! (
          "maintenance archive '{}' and source '{}' ({}) must be structurally disjoint",
          archive . display (), source . name, source_identity . display ()))); }}
  Ok (( ))
}

fn create_private_directory_all (path : &Path) -> io::Result<()> {
  #[cfg(unix)]
  {
    let mut builder = fs::DirBuilder::new ();
    builder . recursive (true) . mode (0o700) . create (path)?;
    fs::set_permissions (path, fs::Permissions::from_mode (0o700))?;
  }
  #[cfg(not(unix))]
  {
    fs::create_dir_all (path)?;
    tracing::warn! (
      path = %path . display (),
      "cannot enforce POSIX 0700 permissions on this platform");
  }
  Ok (( ))
}

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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::ID;
  use tempfile::tempdir;

  fn write_config (
    root    : &Path,
    sources : &str,
  ) -> PathBuf {
    let path : PathBuf = root . join ("skgconfig.toml");
    fs::write (
      &path,
      format! (
        "db_name = \"test\"\ntantivy_folder = \"tantivy\"\n{}",
        sources )) . unwrap ();
    path
  }

  #[test]
  fn mixed_named_and_unnamed_sources_keep_declaration_order () {
    let temp = tempdir () . unwrap ();
    let path : PathBuf = write_config (
      temp . path (),
      concat! (
        "\n[[sources]]\npath = \"owned/first\"\n",
        "\n[[sources]]\nname = \"middle\"\npath = \"owned/second\"\n",
        "\n[[sources]]\npath = \"owned/third\"\n" ));
    let config : SkgConfig = load_config (path . to_str () . unwrap ())
      . unwrap ();
    assert_eq! (
      config . ordered_sources (),
      ["owned/first", "middle", "owned/third"]
        . into_iter () . map (SourceName::from) . collect::<Vec<_>> () );
    assert_eq! (
      config . sources . configured_path (
        &SourceName::from ("owned/first")),
      Some (Path::new ("owned/first")) );
    for source in config . sources . values () {
      assert! (source . path . is_absolute ());
      assert_eq! (
        config . sources . directory_identity (&source . name),
        Some (source . path . as_path ()) ); }
  }

  #[test]
  fn two_sources_may_not_alias_one_physical_directory () {
    let temp = tempdir () . unwrap ();
    fs::create_dir_all (temp . path () . join ("owned/shared"))
      . unwrap ();
    let path : PathBuf = write_config (
      temp . path (),
      concat! (
        "\n[[sources]]\nname = \"first\"\npath = \"owned/shared\"\n",
        "\n[[sources]]\nname = \"alias\"\npath = \"owned/shared/.\"\n" ));
    let error : String = match load_config (path . to_str () . unwrap ()) {
      Ok (_)  => panic! ("duplicate physical source directory was accepted"),
      Err (e) => e . to_string (), };
    assert! (error . contains ("first"), "{}", error);
    assert! (error . contains ("alias"), "{}", error);
    assert! (error . contains ("same physical directory"), "{}", error);
  }

  #[test]
  fn archive_root_defaults_beside_config_and_is_disjoint () {
    let temp = tempdir () . unwrap ();
    let path : PathBuf = write_config (
      temp . path (),
      "\n[[sources]]\nname = \"notes\"\npath = \"owned/notes\"\n" );
    let config : SkgConfig = load_config (path . to_str () . unwrap ())
      . unwrap ();
    assert_eq! (
      config . maintenance_archive_folder,
      PathBuf::from ("unsaved-work-interrupted-by-rebuild"));
    assert_eq! (
      config . maintenance_archive_identity,
      fs::canonicalize (temp . path () . join (
        "unsaved-work-interrupted-by-rebuild")) . unwrap ());
    #[cfg(unix)]
    assert_eq! (
      fs::metadata (&config . maintenance_archive_identity)
        . unwrap () . permissions () . mode () & 0o777,
      0o700);
  }

  #[test]
  fn archive_root_may_not_contain_or_be_contained_by_a_source () {
    for archive in ["owned", "owned/notes/incidents"] {
      let temp = tempdir () . unwrap ();
      let path : PathBuf = temp . path () . join ("skgconfig.toml");
      fs::write (&path, format! (
        "db_name = \"test\"\ntantivy_folder = \"tantivy\"\nmaintenance_archive_folder = {:?}\n\n[[sources]]\nname = \"notes\"\npath = \"owned/notes\"\n",
        archive)) . unwrap ();
      let error = load_config (path . to_str () . unwrap ())
        . err () . expect ("overlapping archive was accepted")
        . to_string ();
      assert! (error . contains ("structurally disjoint"), "{}", error);
    }
  }

  #[test]
  fn archive_root_rejects_parent_traversal () {
    let temp = tempdir () . unwrap ();
    let path : PathBuf = temp . path () . join ("skgconfig.toml");
    fs::write (&path, concat! (
      "db_name = \"test\"\n",
      "tantivy_folder = \"tantivy\"\n",
      "maintenance_archive_folder = \"../archive\"\n",
      "\n[[sources]]\nname = \"notes\"\npath = \"owned/notes\"\n"))
      . unwrap ();
    let error = load_config (path . to_str () . unwrap ())
      . err () . expect ("parent traversal was accepted") . to_string ();
    assert! (error . contains ("may not contain '..'"), "{}", error);
  }

  #[cfg(unix)]
  #[test]
  fn symlinked_source_keeps_configured_path_but_uses_physical_identity () {
    use std::os::unix::fs::symlink;
    let temp = tempdir () . unwrap ();
    let real : PathBuf = temp . path () . join ("owned/real");
    let alias : PathBuf = temp . path () . join ("owned/alias");
    fs::create_dir_all (&real) . unwrap ();
    symlink (&real, &alias) . unwrap ();
    let path : PathBuf = write_config (
      temp . path (),
      "\n[[sources]]\nname = \"linked\"\npath = \"owned/alias\"\n" );
    let config : SkgConfig = load_config (path . to_str () . unwrap ())
      . unwrap ();
    let name : SourceName = SourceName::from ("linked");
    assert_eq! (
      config . sources . configured_path (&name),
      Some (Path::new ("owned/alias")) );
    assert_eq! (
      config . sources . directory_identity (&name),
      Some (fs::canonicalize (&real) . unwrap () . as_path ()) );
    assert_eq! (
      config . sources . source_and_pid_for_direct_path (
        &alias . join ("X.skg")),
      Some ((name, ID::from ("X"))) );
  }
}
