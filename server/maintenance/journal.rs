use super::coordinator::MaintenanceCoordinator;

use serde::{Deserialize, Serialize};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

#[cfg(unix)]
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

const JOURNAL_FORMAT_VERSION : u32 = 2;

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

/// Check the original payload before deserializing coordinator defaults. In
/// particular, adding version 2 ledger/census fields must not change the bytes
/// used to authenticate a known version 1 publication.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct OriginalEnvelope {
  payload : serde_yaml::Value,
  payload_blake3 : String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct OriginalPayload {
  format_version : u32,
  config_identity : PathBuf,
  coordinator : serde_yaml::Value,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CoordinatorV1 {
  epoch : super::types::MaintenanceEpoch,
  observation_sequence : super::types::ObservationSequence,
  state : super::types::CoordinatorState,
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
    coordinator . validate_journal_authority ()?;
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
    self . preserve_original_v1 (&final_path)?;
    fs::rename (&temporary, &final_path)
      . map_err (|error| error . to_string ())?;
    sync_directory (&self . directory)?;
    Ok (final_path)
  }

  /// A migrated publication never destroys the sole original evidence of the
  /// old protocol's authority. The backup is exact and private, and its name
  /// is content-addressed so a repeated migration cannot replace other bytes.
  fn preserve_original_v1 (&self, path : &Path) -> Result<(), String> {
    let bytes : Vec<u8> = match fs::read (path) {
      Ok (bytes) => bytes,
      Err (error) if error . kind () == std::io::ErrorKind::NotFound => return Ok (( )),
      Err (error) => return Err (error . to_string ()), };
    let original : OriginalPayload = checked_original (&bytes, &self . config_identity)?;
    validate_envelope (&bytes, &self . config_identity)?;
    if original . format_version != 1 { return Ok (( )); }
    let archive : PathBuf = self . directory . join (format! (
      "active.v1.{}.yaml", blake3::hash (&bytes) . to_hex ()));
    match fs::read (&archive) {
      Ok (existing) if existing == bytes => return Ok (( )),
      Ok (_) => return Err ("version 1 journal preservation path contains different bytes" . into ()),
      Err (error) if error . kind () == std::io::ErrorKind::NotFound => {},
      Err (error) => return Err (error . to_string ()), }
    write_private_file (&archive, &bytes)?;
    sync_directory (&self . directory) }

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
    if !matches! (coordinator . state, super::types::CoordinatorState::Idle)
    || !coordinator . committed_incidents . is_empty ()
    || !coordinator . query_waits . waits . is_empty () {
      return Err ("cannot compact a maintenance journal with retained incident identity or obligations" . into ()); }
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
  let payload : OriginalPayload = checked_original (bytes, config_identity)?;
  let coordinator : MaintenanceCoordinator = match payload . format_version {
    1 => {
      let original : CoordinatorV1 = serde_yaml::from_value (payload . coordinator)
        . map_err (|error| error . to_string ())?;
      MaintenanceCoordinator {
        epoch: original . epoch,
        observation_sequence: original . observation_sequence,
        state: original . state,
        committed_incidents: Default::default (),
        query_waits: Default::default (),
      } . migrate_known_v1 ()? }
    JOURNAL_FORMAT_VERSION => {
      if payload . coordinator . get ("committed_incidents") . is_none () {
        return Err ("version 2 journal lacks its retained incident ledger" . into ()); }
      serde_yaml::from_value (payload . coordinator)
        . map_err (|error| error . to_string ())? }
    _ => unreachable! ("checked original version"), };
  coordinator . validate_journal_authority ()?;
  Ok (coordinator) }

fn checked_original (
  bytes : &[u8],
  config_identity : &Path,
) -> Result<OriginalPayload, String> {
  let envelope : OriginalEnvelope = serde_yaml::from_slice (bytes)
    . map_err (|error| error . to_string ())?;
  let payload_bytes : Vec<u8> = serde_yaml::to_string (&envelope . payload)
    . map_err (|error| error . to_string ())? . into_bytes ();
  let checksum : String = blake3::hash (&payload_bytes) . to_hex () . to_string ();
  if checksum != envelope . payload_blake3 {
    return Err ("maintenance journal checksum mismatch" . into ()); }
  let payload : OriginalPayload = serde_yaml::from_value (envelope . payload)
    . map_err (|error| error . to_string ())?;
  if !matches! (payload . format_version, 1 | JOURNAL_FORMAT_VERSION) {
    return Err (format! (
      "unsupported maintenance journal version {}",
      payload . format_version)); }
  if payload . config_identity != config_identity {
    return Err ("maintenance journal belongs to a different config" . into ()); }
  Ok (payload)
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
  use crate::maintenance::types::*;
  use crate::types::store_state::{GraphGeneration, ManifestRevision};
  use tempfile::tempdir;

  /// Reproduce the known version 1 field inventory and its original checksum.
  /// The fixture deliberately omits the new defaulted coordinator fields.
  fn version_one_bytes (coordinator : &MaintenanceCoordinator) -> Vec<u8> {
    let mut value : serde_yaml::Value = serde_yaml::to_value (coordinator) . unwrap ();
    let map : &mut serde_yaml::Mapping = value . as_mapping_mut () . unwrap ();
    map . remove ("committed_incidents");
    map . remove ("query_waits");
    if let Some (details) = map . get_mut ("state")
        . and_then (|state| state . get_mut ("details"))
        . and_then (serde_yaml::Value::as_mapping_mut) {
      details . remove ("census_frozen");
      details . remove ("legacy_census_obligations"); }
    encoded_payload (1, value) }

  fn encoded_payload (version : u32, coordinator : serde_yaml::Value) -> Vec<u8> {
    let mut payload : serde_yaml::Mapping = serde_yaml::Mapping::new ();
    payload . insert ("format_version" . into (), version . into ());
    payload . insert ("config_identity" . into (), "/config" . into ());
    payload . insert ("coordinator" . into (), coordinator);
    let payload : serde_yaml::Value = serde_yaml::Value::Mapping (payload);
    let bytes : String = serde_yaml::to_string (&payload) . unwrap ();
    let mut envelope : serde_yaml::Mapping = serde_yaml::Mapping::new ();
    envelope . insert ("payload" . into (), payload);
    envelope . insert ("payload_blake3" . into (),
      blake3::hash (bytes . as_bytes ()) . to_hex () . to_string () . into ());
    serde_yaml::to_string (&envelope) . unwrap () . into_bytes () }

  fn ready_legacy_coordinator () -> MaintenanceCoordinator {
    let mut coordinator : MaintenanceCoordinator = MaintenanceCoordinator::new ();
    let candidate : CandidateSummary = CandidateSummary {
      id: CandidateId::new (), base_graph_generation: GraphGeneration::INITIAL,
      base_manifest_revision: ManifestRevision::INITIAL,
      covered_sequence: ObservationSequence::INITIAL, changed_primary_ids: Vec::new (), };
    let active : ActiveMaintenance = coordinator . begin (
      MaintenanceOrigin::FullRebuild, Some (candidate)) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial-archive" . into ()) . unwrap ();
    coordinator . record_server_evidence (
      &active . incident_id, active . epoch, ServerEvidenceRecord {
        path: "evidence" . into (), bundle_sha256: "server-bundle" . into (),
        artifact_count: 1, total_file_bytes: 2, }) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch, MaintenancePhase::FullRebuildExclusive) . unwrap ();
    coordinator . store_rebuilt (&active . incident_id, active . epoch, SelectedStoreRecord {
      graph_generation: GraphGeneration::INITIAL . successor (),
      manifest_revision: ManifestRevision::INITIAL . successor (),
      tantivy_generation: 1, tantivy_outcome: "committed" . into (), }) . unwrap ();
    let Some (CommittedIncident::Settling (active)) =
      coordinator . committed_incidents . remove (&active . incident_id) else { unreachable! (); };
    coordinator . state = CoordinatorState::Active (active);
    coordinator }

  fn write_original (store : &MaintenanceJournalStore, bytes : &[u8]) -> PathBuf {
    create_private_directory_all (&store . directory) . unwrap ();
    let path : PathBuf = store . directory . join ("active.yaml");
    write_private_file (&path, bytes) . unwrap ();
    path }

  #[test]
  fn known_v1_authenticates_before_defaults_and_preserves_original_on_publication () {
    let directory : tempfile::TempDir = tempdir () . unwrap ();
    let store : MaintenanceJournalStore = MaintenanceJournalStore::at_root (
      directory . path () . join ("state"), "/config" . into ());
    let mut original : MaintenanceCoordinator = MaintenanceCoordinator::new ();
    let incident : ActiveMaintenance = original . begin (MaintenanceOrigin::Pull, None) . unwrap ();
    let bytes : Vec<u8> = version_one_bytes (&original);
    let path : PathBuf = write_original (&store, &bytes);
    let migrated : MaintenanceCoordinator = store . load () . require_authority ()
      . unwrap () . unwrap () . coordinator;
    assert_eq! (fs::read (&path) . unwrap (), bytes);
    assert! (migrated . incident (&incident . incident_id, incident . epoch) . unwrap () . census_frozen);
    assert! (!migrated . state . policy () . skg_saves_allowed);
    store . persist (&migrated) . unwrap ();
    let archive : PathBuf = store . directory . join (format! (
      "active.v1.{}.yaml", blake3::hash (&bytes) . to_hex ()));
    assert_eq! (fs::read (&archive) . unwrap (), bytes);
    assert_eq! (checked_original (&fs::read (&path) . unwrap (), Path::new ("/config"))
      . unwrap () . format_version, 2);
    assert_eq! (store . load () . require_authority () . unwrap () . unwrap () . coordinator, migrated); }

  #[test]
  fn known_v1_selected_incident_becomes_pending_report_without_reselecting_its_graph () {
    let original : MaintenanceCoordinator = ready_legacy_coordinator ();
    let CoordinatorState::Active (active) : &CoordinatorState = &original . state else { unreachable! (); };
    let id : IncidentId = active . incident_id . clone ();
    let epoch : MaintenanceEpoch = active . epoch;
    let mut migrated : MaintenanceCoordinator = validate_envelope (
      &version_one_bytes (&original), Path::new ("/config")) . unwrap ();
    assert_eq! (migrated . state, CoordinatorState::Idle);
    assert_eq! (migrated . incident (&id, epoch) . unwrap () . selected_store, active . selected_store);
    let successor : ActiveMaintenance = migrated . begin (MaintenanceOrigin::Pull, None) . unwrap ();
    let graph_work : CoordinatorState = migrated . state . clone ();
    migrated . record_view_settlements (&id, epoch, Vec::new ()) . unwrap ();
    assert_eq! (migrated . state, graph_work);
    assert_eq! (migrated . incident (&successor . incident_id, successor . epoch)
      . unwrap () . phase, MaintenancePhase::PreparingArchive);
    assert_eq! (migrated . incidents () . len (), 2); }

  #[test]
  fn known_v1_unbound_view_authority_remains_visible_and_blocks_writes () {
    let mut original : MaintenanceCoordinator = ready_legacy_coordinator ();
    let CoordinatorState::Active (active) : &mut CoordinatorState = &mut original . state else { unreachable! (); };
    let pending : PendingViewEnrollment = PendingViewEnrollment {
      view_uri: "late-old-view" . into (), graph_generation: 1,
      presentation_generation: 1, server_revision: 2, application_token: 3, };
    active . pending_view_enrollments . insert (pending . view_uri . clone (), pending . clone ());
    let mut migrated : MaintenanceCoordinator = validate_envelope (
      &version_one_bytes (&original), Path::new ("/config")) . unwrap ();
    assert! (!migrated . state . policy () . skg_saves_allowed);
    let CoordinatorState::Active (retained) : &CoordinatorState = &migrated . state else { unreachable! (); };
    assert_eq! (retained . phase, MaintenancePhase::BlockedStoreHealth);
    assert_eq! (retained . legacy_census_obligations . as_ref () . unwrap ()
      . pending_view_enrollments . get ("late-old-view"), Some (&pending));
    let before : MaintenanceCoordinator = migrated . clone ();
    assert! (!migrated . enroll_pending_view (PendingViewEnrollment {
      view_uri: "new-read-only-view" . into (), ..pending }) . unwrap ());
    assert_eq! (migrated, before); }

  #[test]
  fn known_v1_bound_late_buffers_remain_finite_report_obligations () {
    let mut original : MaintenanceCoordinator = ready_legacy_coordinator ();
    let CoordinatorState::Active (active) : &mut CoordinatorState = &mut original . state else { unreachable! (); };
    let late : FrozenBufferRecord = serde_yaml::from_str (
      "buffer_id: legacy-late\nkind: content-view\nview_uri: legacy-uri\ngraph_generation: 1\npresentation_generation: 0\nserver_revision: 4\napplication_token: 9\ndirty: false\nundo_required: false\nlast_fetched_sha256: before\ncurrent_sha256: before\n") . unwrap ();
    active . presentation_buffer_census . insert (late . buffer_id . clone (), late . clone ());
    let id : IncidentId = active . incident_id . clone ();
    let epoch : MaintenanceEpoch = active . epoch;
    let mut migrated : MaintenanceCoordinator = validate_envelope (
      &version_one_bytes (&original), Path::new ("/config")) . unwrap ();
    let retained : &ActiveMaintenance = migrated . incident (&id, epoch) . unwrap ();
    assert! (retained . buffer_census . is_empty ());
    assert_eq! (retained . presentation_census () . get ("legacy-late"), Some (&late));
    assert! (retained . legacy_census_obligations . is_some ());
    let mut new_result : FrozenBufferRecord = late;
    new_result . buffer_id = "new-result" . into ();
    let before : MaintenanceCoordinator = migrated . clone ();
    assert! (migrated . enroll_presentation_census (
      vec![new_result], Some (epoch . get ())) . unwrap () . is_empty ());
    assert_eq! (migrated, before); }

  #[test]
  fn ambiguous_v1_selected_record_and_unknown_version_never_become_idle () {
    let mut ambiguous : MaintenanceCoordinator = ready_legacy_coordinator ();
    let CoordinatorState::Active (active) : &mut CoordinatorState = &mut ambiguous . state else { unreachable! (); };
    active . selected_store = None;
    let directory : tempfile::TempDir = tempdir () . unwrap ();
    let store : MaintenanceJournalStore = MaintenanceJournalStore::at_root (
      directory . path () . join ("state"), "/config" . into ());
    let bytes : Vec<u8> = version_one_bytes (&ambiguous);
    let path : PathBuf = write_original (&store, &bytes);
    assert! (store . load () . require_authority () . is_err ());
    assert_eq! (fs::read (&path) . unwrap (), bytes);
    let unknown : Vec<u8> = encoded_payload (99,
      serde_yaml::to_value (MaintenanceCoordinator::new ()) . unwrap ());
    assert! (validate_envelope (&unknown, Path::new ("/config"))
      . unwrap_err () . contains ("unsupported")); }

  #[test]
  fn terminal_ack_keeps_known_v1_identity_and_prevents_implicit_compaction () {
    let directory : tempfile::TempDir = tempdir () . unwrap ();
    let store : MaintenanceJournalStore = MaintenanceJournalStore::at_root (
      directory . path () . join ("state"), "/config" . into ());
    let mut original : MaintenanceCoordinator = MaintenanceCoordinator::new ();
    let active : ActiveMaintenance = original . begin (MaintenanceOrigin::Pull, None) . unwrap ();
    original . finish (&active . incident_id, active . epoch,
      TerminalDisposition::FailedBeforeArchive) . unwrap ();
    let mut migrated : MaintenanceCoordinator = validate_envelope (
      &version_one_bytes (&original), Path::new ("/config")) . unwrap ();
    assert_eq! (migrated . state, CoordinatorState::Idle);
    assert! (migrated . acknowledge_terminal (&active . incident_id, active . epoch) . unwrap ());
    assert! (!migrated . acknowledge_terminal (&active . incident_id, active . epoch) . unwrap ());
    store . persist (&migrated) . unwrap ();
    assert! (store . remove_completed (&migrated) . is_err ());
    assert! (migrated . terminal_incident (&active . incident_id, active . epoch) . is_some ());
    let loaded : MaintenanceCoordinator = store . load () . require_authority () . unwrap () . unwrap () . coordinator;
    assert_eq! (loaded, migrated); }

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
