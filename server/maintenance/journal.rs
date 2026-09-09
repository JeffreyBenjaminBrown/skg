use super::coordinator::MaintenanceCoordinator;

use serde::{Deserialize, Serialize};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

#[cfg(unix)]
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

const JOURNAL_FORMAT_VERSION : u32 = 1;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
struct JournalPayload {
  format_version  : u32,
  config_identity : PathBuf,
  coordinator     : MaintenanceCoordinator,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
struct JournalEnvelope {
  payload         : JournalPayload,
  payload_blake3  : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LoadedJournal {
  pub coordinator : MaintenanceCoordinator,
  pub path         : PathBuf,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MalformedJournal {
  pub path   : PathBuf,
  pub reason : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JournalLoadReport {
  pub active    : Option<LoadedJournal>,
  pub malformed : Vec<MalformedJournal>,
}

impl JournalLoadReport {
  /// A committed record wins over unused temporary publications. Without a
  /// valid committed record, ambiguous files cannot mean a clean startup.
  pub fn require_authority (self) -> Result<Option<LoadedJournal>, String> {
    if self . active . is_some () || self . malformed . is_empty () {
      return Ok (self . active); }
    Err (format! (
      "maintenance recovery is required before granting authority: {}",
      self . malformed . iter () . map (|record|
        format! ("{}: {}", record . path . display (), record . reason))
        . collect::<Vec<_>> () . join ("; "))) }
}

#[derive(Clone, Debug)]
pub struct MaintenanceJournalStore {
  directory       : PathBuf,
  config_identity : PathBuf,
}

impl MaintenanceJournalStore {
  pub fn for_config (config_path : &Path) -> Self {
    let config_identity = config_path . canonicalize ()
      . unwrap_or_else (|_| config_path . to_path_buf ());
    let base = std::env::var_os ("XDG_STATE_HOME") . map (PathBuf::from)
      . or_else (|| std::env::var_os ("HOME")
        . map (|home| PathBuf::from (home) . join (".local/state")))
      . unwrap_or_else (|| std::env::temp_dir () . join ("skg-state"));
    Self::at_root (base . join ("skg/maintenance"), config_identity)
  }

  pub fn at_root (
    root            : PathBuf,
    config_identity : PathBuf,
  ) -> Self {
    let key = blake3::hash (
      config_identity . to_string_lossy () . as_bytes ())
      . to_hex () . to_string ();
    Self {
      directory: root . join (&key[..16]),
      config_identity,
    }
  }

  pub(crate) fn directory (&self) -> &Path { &self . directory }

  pub(crate) fn config_identity (&self) -> &Path { &self . config_identity }

  pub fn persist (
    &self,
    coordinator : &MaintenanceCoordinator,
  ) -> Result<PathBuf, String> {
    create_private_directory_all (&self . directory)?;
    let payload = JournalPayload {
      format_version: JOURNAL_FORMAT_VERSION,
      config_identity: self . config_identity . clone (),
      coordinator: coordinator . clone (),
    };
    let payload_bytes = serde_yaml::to_string (&payload)
      . map_err (|error| error . to_string ())? . into_bytes ();
    let envelope = JournalEnvelope {
      payload,
      payload_blake3: blake3::hash (&payload_bytes)
        . to_hex () . to_string (),
    };
    let bytes = serde_yaml::to_string (&envelope)
      . map_err (|error| error . to_string ())? . into_bytes ();
    let temporary = self . directory . join (format! (
      ".active.{}.tmp", uuid::Uuid::new_v4 ()));
    let final_path = self . directory . join ("active.yaml");
    write_private_file (&temporary, &bytes)?;
    let reread = fs::read (&temporary) . map_err (|error| error . to_string ())?;
    if reread != bytes {
      return Err (format! (
        "maintenance journal short write at {}", temporary . display ())); }
    validate_envelope (&reread, &self . config_identity)?;
    fs::rename (&temporary, &final_path)
      . map_err (|error| error . to_string ())?;
    sync_directory (&self . directory)?;
    Ok (final_path)
  }

  /// Load authority without deleting or renaming malformed evidence.
  pub fn load (&self) -> JournalLoadReport {
    let active_path = self . directory . join ("active.yaml");
    let mut malformed = Vec::new ();
    let active = match fs::read (&active_path) {
      Ok (bytes) => match validate_envelope (&bytes, &self . config_identity) {
        Ok (coordinator) => Some (LoadedJournal {
          coordinator, path: active_path,
        }),
        Err (reason) => {
          malformed . push (MalformedJournal {
            path: active_path, reason,
          });
          None }
      },
      Err (error) if error . kind () == std::io::ErrorKind::NotFound => None,
      Err (error) => {
        malformed . push (MalformedJournal {
          path: active_path, reason: error . to_string (),
        });
        None }
    };
    if let Ok (entries) = fs::read_dir (&self . directory) {
      for entry in entries . flatten () {
        let path = entry . path ();
        if path == self . directory . join ("active.yaml") { continue; }
        let name = path . file_name () . and_then (|name| name . to_str ())
          . unwrap_or_default ();
        if name . starts_with (".active.") && name . ends_with (".tmp") {
          malformed . push (MalformedJournal {
            path,
            reason: "incomplete atomic journal publication" . into (),
          });
        }
      }
    }
    malformed . sort_by (|left, right| left . path . cmp (&right . path));
    JournalLoadReport { active, malformed }
  }

  pub fn remove_completed (
    &self,
    coordinator : &MaintenanceCoordinator,
  ) -> Result<(), String> {
    if !matches! (coordinator . state, super::types::CoordinatorState::Idle) {
      return Err ("cannot compact a non-idle maintenance journal" . into ()); }
    let path = self . directory . join ("active.yaml");
    match fs::remove_file (&path) {
      Ok (( )) => sync_directory (&self . directory),
      Err (error) if error . kind () == std::io::ErrorKind::NotFound => Ok (( )),
      Err (error) => Err (error . to_string ()),
    }
  }
}

fn validate_envelope (
  bytes           : &[u8],
  config_identity : &Path,
) -> Result<MaintenanceCoordinator, String> {
  let envelope : JournalEnvelope = serde_yaml::from_slice (bytes)
    . map_err (|error| error . to_string ())?;
  if envelope . payload . format_version != JOURNAL_FORMAT_VERSION {
    return Err (format! (
      "unsupported maintenance journal version {}",
      envelope . payload . format_version)); }
  if envelope . payload . config_identity != config_identity {
    return Err ("maintenance journal belongs to a different config" . into ()); }
  let payload_bytes = serde_yaml::to_string (&envelope . payload)
    . map_err (|error| error . to_string ())? . into_bytes ();
  let checksum = blake3::hash (&payload_bytes) . to_hex () . to_string ();
  if checksum != envelope . payload_blake3 {
    return Err ("maintenance journal checksum mismatch" . into ()); }
  Ok (envelope . payload . coordinator)
}

fn write_private_file (path : &Path, bytes : &[u8]) -> Result<(), String> {
  let mut options = OpenOptions::new ();
  options . write (true) . create_new (true);
  #[cfg(unix)]
  options . mode (0o600);
  let mut file = options . open (path) . map_err (|error| error . to_string ())?;
  file . write_all (bytes) . map_err (|error| error . to_string ())?;
  file . sync_all () . map_err (|error| error . to_string ())?;
  drop (file);
  Ok (( ))
}

fn create_private_directory_all (path : &Path) -> Result<(), String> {
  fs::create_dir_all (path) . map_err (|error| error . to_string ())?;
  #[cfg(unix)]
  fs::set_permissions (path, fs::Permissions::from_mode (0o700))
    . map_err (|error| error . to_string ())?;
  Ok (( ))
}

fn sync_directory (path : &Path) -> Result<(), String> {
  #[cfg(unix)]
  {
    let mut directory = File::open (path) . map_err (|error| error . to_string ())?;
    let mut byte = [0u8; 0];
    directory . read (&mut byte) . ok ();
    directory . sync_all () . map_err (|error| error . to_string ())?;
  }
  Ok (( ))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::maintenance::types::MaintenanceOrigin;
  use tempfile::tempdir;

  #[test]
  fn journal_round_trips_and_only_idle_can_be_removed () {
    let temp = tempdir () . unwrap ();
    let store = MaintenanceJournalStore::at_root (
      temp . path () . join ("state"), PathBuf::from ("/config"));
    let mut coordinator = MaintenanceCoordinator::new ();
    coordinator . begin (MaintenanceOrigin::Pull, None) . unwrap ();
    let path = store . persist (&coordinator) . unwrap ();
    assert_eq! (
      store . load () . active . unwrap () . coordinator,
      coordinator);
    assert! (store . remove_completed (&coordinator) . is_err ());
    coordinator . state = crate::maintenance::types::CoordinatorState::Idle;
    store . persist (&coordinator) . unwrap ();
    store . remove_completed (&coordinator) . unwrap ();
    assert! (!path . exists ());
  }

  #[test]
  fn malformed_and_incomplete_journals_are_retained_and_reported () {
    let temp = tempdir () . unwrap ();
    let store = MaintenanceJournalStore::at_root (
      temp . path () . join ("state"), PathBuf::from ("/config"));
    create_private_directory_all (&store . directory) . unwrap ();
    let active = store . directory . join ("active.yaml");
    let partial = store . directory . join (".active.crash.tmp");
    fs::write (&active, b"invalid: [") . unwrap ();
    fs::write (&partial, b"partial") . unwrap ();
    let report = store . load ();
    assert! (report . active . is_none ());
    assert_eq! (report . malformed . len (), 2);
    assert! (report . require_authority () . is_err ());
    assert! (active . exists ());
    assert! (partial . exists ());
  }

  #[test]
  fn committed_journal_proves_unused_temporary_record_is_not_authority () {
    let temp : tempfile::TempDir = tempdir () . unwrap ();
    let store : MaintenanceJournalStore = MaintenanceJournalStore::at_root (
      temp . path () . join ("state"), PathBuf::from ("/config"));
    store . persist (&MaintenanceCoordinator::new ()) . unwrap ();
    let temporary : PathBuf = store . directory . join (".active.unused.tmp");
    fs::write (&temporary, b"incomplete") . unwrap ();
    assert! (store . load () . require_authority () . unwrap () . is_some ());
    assert! (temporary . exists ()); }
}
