//! Private, content-addressed artifacts for durable query results.
//!
//! The coordinator journals only this artifact's metadata.  Result bytes are
//! staged under the maintenance journal's private directory and are read back
//! only after the operation, path, length, and SHA-256 digest all agree.

use super::journal::MaintenanceJournalStore;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::{self, DirEntry, File, Metadata, OpenOptions};
use std::io::Write;
use std::path::{Component, Path, PathBuf};
use uuid::Uuid;

#[cfg(unix)]
use std::os::unix::fs::{DirBuilderExt, MetadataExt, OpenOptionsExt, PermissionsExt};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct QueryArtifact {
  pub operation_id : String,
  pub path         : PathBuf,
  pub bytes        : u64,
  pub sha256       : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueryArtifactStore {
  root : PathBuf,
}

impl QueryArtifactStore {
  pub fn alongside (journal : &MaintenanceJournalStore) -> Self {
    Self { root: journal . directory () . join ("query-results") }
  }

  pub fn stage (
    &self,
    operation_id : &str,
    content      : &str,
  ) -> Result<QueryArtifact, String> {
    validate_operation_id (operation_id)?;
    ensure_private_directory_all (&self . root)?;
    let bytes : &[u8] = content . as_bytes ();
    let sha256 : String = sha256_hex (bytes);
    let final_path : PathBuf = self . root . join (format! (
      "{}-{}.result", operation_id, sha256));
    validate_final_path (&self . root, &final_path)?;

    let prefix : String = format! ("{}-", operation_id);
    for entry in fs::read_dir (&self . root) . map_err (|error| error . to_string ())? {
      let entry : DirEntry = entry . map_err (|error| error . to_string ())?;
      let path : PathBuf = entry . path ();
      let name : String = entry . file_name () . to_string_lossy () . into_owned ();
      if !name . starts_with (&prefix) || !name . ends_with (".result") { continue; }
      require_private_regular_file (&path)?;
      if path != final_path {
        return Err ("query artifact operation ID was reused with different bytes" . into ()); }
    }
    if fs::symlink_metadata (&final_path) . is_ok () {
      require_private_regular_file (&final_path)?;
      let existing : Vec<u8> = fs::read (&final_path) . map_err (|error| error . to_string ())?;
      if existing != bytes {
        return Err ("query artifact path contains different bytes" . into ()); }
      sync_file (&final_path)?;
      sync_directory (&self . root)?;
      return artifact_from (operation_id, final_path, bytes); }

    let temporary : PathBuf = self . root . join (format! (
      ".stage-{}-{}", operation_id, Uuid::new_v4 ()));
    write_private_file (&temporary, bytes)?;
    match fs::rename (&temporary, &final_path) {
      Ok (( )) => {},
      Err (error) if error . kind () == std::io::ErrorKind::AlreadyExists => {
        require_private_regular_file (&final_path)?;
        let existing : Vec<u8> = fs::read (&final_path)
          . map_err (|read_error| read_error . to_string ())?;
        let _ = fs::remove_file (&temporary);
        if existing != bytes {
          return Err ("query artifact path contains different bytes" . into ()); }
        sync_file (&final_path)?;
      },
      Err (error) => {
        let _ = fs::remove_file (&temporary);
        return Err (error . to_string ()); }
    }
    sync_directory (&self . root)?;
    artifact_from (operation_id, final_path, bytes)
  }

  pub fn read (&self, artifact : &QueryArtifact) -> Result<String, String> {
    validate_operation_id (&artifact . operation_id)?;
    if artifact . sha256 . len () != 64
    || !artifact . sha256 . bytes () . all (|byte| byte . is_ascii_hexdigit ()) {
      return Err ("query artifact has an invalid SHA-256 digest" . into ()); }
    let expected : PathBuf = self . root . join (format! (
      "{}-{}.result", artifact . operation_id, artifact . sha256));
    validate_final_path (&self . root, &artifact . path)?;
    if artifact . path != expected {
      return Err ("query artifact path does not match operation and digest" . into ()); }
    require_private_regular_file (&artifact . path)?;
    let bytes : Vec<u8> = fs::read (&artifact . path)
      . map_err (|error| error . to_string ())?;
    if bytes . len () as u64 != artifact . bytes {
      return Err ("query artifact byte length changed" . into ()); }
    if sha256_hex (&bytes) != artifact . sha256 {
      return Err ("query artifact SHA-256 changed" . into ()); }
    String::from_utf8 (bytes)
      . map_err (|_| "query artifact is not valid UTF-8" . into ())
  }
}

fn artifact_from (
  operation_id : &str,
  path         : PathBuf,
  bytes        : &[u8],
) -> Result<QueryArtifact, String> {
  require_private_regular_file (&path)?;
  Ok (QueryArtifact { operation_id: operation_id . into (),
    path, bytes: bytes . len () as u64, sha256: sha256_hex (bytes) })
}

fn validate_operation_id (operation_id : &str) -> Result<(), String> {
  Uuid::parse_str (operation_id)
    . map_err (|_| "query artifact operation ID is not a UUID" . to_string ())?;
  Ok (( ))
}

fn validate_final_path (root : &Path, path : &Path) -> Result<(), String> {
  if path . parent () != Some (root)
  || path . file_name () . is_none_or (|name| !name . to_string_lossy () . ends_with (".result")) {
    return Err ("query artifact path is outside its private store" . into ()); }
  let relative : &Path = path . strip_prefix (root)
    . map_err (|_| "query artifact path is outside its private store" . to_string ())?;
  if relative . components () . any (|component| !matches! (component, Component::Normal (_))) {
    return Err ("query artifact path contains unsafe components" . into ()); }
  require_private_directory (root)
}

fn write_private_file (path : &Path, bytes : &[u8]) -> Result<(), String> {
  let mut options = OpenOptions::new ();
  options . write (true) . create_new (true);
  #[cfg(unix)]
  options . mode (0o600);
  let mut file : File = options . open (path) . map_err (|error| error . to_string ())?;
  file . write_all (bytes) . map_err (|error| error . to_string ())?;
  file . sync_all () . map_err (|error| error . to_string ())?;
  drop (file);
  Ok (( ))
}

fn ensure_private_directory_all (path : &Path) -> Result<(), String> {
  if fs::symlink_metadata (path) . is_ok () {
    return require_private_directory (path); }
  let mut missing : Vec<PathBuf> = Vec::new ();
  let mut cursor : &Path = path;
  while fs::symlink_metadata (cursor) . is_err () {
    missing . push (cursor . to_path_buf ());
    cursor = cursor . parent ()
      . ok_or_else (|| format! ("directory has no existing parent: {}", path . display ()))?;
  }
  for directory in missing . into_iter () . rev () {
    let mut builder = fs::DirBuilder::new ();
    #[cfg(unix)]
    builder . mode (0o700);
    match builder . create (&directory) {
      Ok (( )) => {},
      Err (_) if fs::symlink_metadata (&directory) . is_ok () =>
        require_private_directory (&directory)?,
      Err (error) => return Err (error . to_string ()),
    }
  }
  require_private_directory (path)?;
  if let Some (parent) = path . parent () {
    sync_directory (parent)?; }
  Ok (( ))
}

fn require_private_directory (path : &Path) -> Result<(), String> {
  let metadata : Metadata = fs::symlink_metadata (path)
    . map_err (|error| error . to_string ())?;
  if metadata . file_type () . is_symlink () || !metadata . is_dir () {
    return Err ("query artifact store is not a real directory" . into ()); }
  #[cfg(unix)]
  if metadata . permissions () . mode () & 0o777 != 0o700 {
    return Err ("query artifact store directory is not mode 0700" . into ()); }
  Ok (( ))
}

fn require_private_regular_file (path : &Path) -> Result<(), String> {
  let metadata : Metadata = fs::symlink_metadata (path)
    . map_err (|error| error . to_string ())?;
  if metadata . file_type () . is_symlink () || !metadata . is_file () {
    return Err ("query artifact is not a regular file" . into ()); }
  #[cfg(unix)]
  if metadata . permissions () . mode () & 0o777 != 0o600 || metadata . nlink () != 1 {
    return Err ("query artifact does not have private file ownership" . into ()); }
  Ok (( ))
}

fn sync_directory (path : &Path) -> Result<(), String> {
  #[cfg(unix)]
  File::open (path) . map_err (|error| error . to_string ())?
    . sync_all () . map_err (|error| error . to_string ())?;
  Ok (( ))
}

fn sync_file (path : &Path) -> Result<(), String> {
  File::open (path) . map_err (|error| error . to_string ())?
    . sync_all () . map_err (|error| error . to_string ())
}

fn sha256_hex (bytes : &[u8]) -> String {
  format! ("{:x}", Sha256::digest (bytes))
}

#[cfg(test)]
mod tests {
  use super::*;
  use tempfile::tempdir;

  fn store () -> QueryArtifactStore {
    let directory : tempfile::TempDir = tempdir () . unwrap ();
    let journal : MaintenanceJournalStore = MaintenanceJournalStore::at_root (
      directory . path () . join ("state"), directory . path () . join ("config"));
    let store : QueryArtifactStore = QueryArtifactStore::alongside (&journal);
    std::mem::forget (directory);
    store
  }

  #[test]
  fn stage_and_verified_read_round_trip () {
    let store : QueryArtifactStore = store ();
    let operation_id : String = Uuid::new_v4 () . to_string ();
    let artifact : QueryArtifact = store . stage (&operation_id, "private result") . unwrap ();
    assert_eq! (store . read (&artifact) . unwrap (), "private result");
  }

  #[test]
  fn identical_replay_is_idempotent_and_changed_bytes_refused () {
    let store : QueryArtifactStore = store ();
    let operation_id : String = Uuid::new_v4 () . to_string ();
    let first : QueryArtifact = store . stage (&operation_id, "one") . unwrap ();
    let second : QueryArtifact = store . stage (&operation_id, "one") . unwrap ();
    assert_eq! (first, second);
    assert! (store . stage (&operation_id, "two") . is_err ());
  }

  #[test]
  fn tamper_and_outside_paths_are_refused () {
    let store : QueryArtifactStore = store ();
    let operation_id : String = Uuid::new_v4 () . to_string ();
    let mut artifact : QueryArtifact = store . stage (&operation_id, "one") . unwrap ();
    fs::write (&artifact . path, b"tampered") . unwrap ();
    assert! (store . read (&artifact) . is_err ());
    artifact . path = PathBuf::from ("/tmp/outside.result");
    assert! (store . read (&artifact) . is_err ());
  }

  #[cfg(unix)]
  #[test]
  fn symlink_and_permissions_are_rejected () {
    let store : QueryArtifactStore = store ();
    let operation_id : String = Uuid::new_v4 () . to_string ();
    let artifact : QueryArtifact = store . stage (&operation_id, "one") . unwrap ();
    let metadata : Metadata = fs::metadata (&artifact . path) . unwrap ();
    assert_eq! (metadata . permissions () . mode () & 0o777, 0o600);
    let root : PathBuf = artifact . path . parent () . unwrap () . to_path_buf ();
    fs::remove_file (&artifact . path) . unwrap ();
    std::os::unix::fs::symlink ("/tmp", &artifact . path) . unwrap ();
    assert! (store . read (&artifact) . is_err ());
    assert_eq! (fs::metadata (root) . unwrap () . permissions () . mode () & 0o777, 0o700);
  }
}
