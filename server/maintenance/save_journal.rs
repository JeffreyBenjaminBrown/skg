//! Durable filesystem transactions for ordinary multi-path saves.
//!
//! The runtime owner is responsible for serializing calls to this store with
//! graph publication and other source writers.  This module owns only the
//! durable filesystem part of that protocol: retained before/after bytes,
//! staging, effect authorization, recoverable application, and the terminal
//! client outcome.

use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashSet};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Component, Path, PathBuf};

#[cfg(unix)]
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

const SAVE_JOURNAL_FORMAT_VERSION : u32 = 1;
const COMPLETION_MARKER_FORMAT_VERSION : u32 = 1;
const MAX_OPERATION_ID_BYTES : usize = 1_024;
const MAX_FINGERPRINT_BYTES : usize = 8_192;
const MAX_INTERPRETATION_IDENTITY_BYTES : usize = 8_192;
const MAX_MUTATIONS_PER_OPERATION : usize = 100_000;
const MAX_PATH_BYTES : usize = 32_768;
const MAX_BLOB_BYTES : u64 = 1 << 30;
const MAX_TOTAL_BLOB_BYTES : u64 = 4 << 30;
const MAX_RECORD_BYTES : u64 = 64 << 20;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SaveInterpretationEvidence {
  pub identity : String,
  pub bytes    : Vec<u8>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct DurablePathMutation {
  pub path   : PathBuf,
  pub before : Option<Vec<u8>>,
  pub after  : Option<Vec<u8>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct DurableSaveRequest {
  pub operation_id             : String,
  pub request_base_fingerprint : String,
  pub interpretation_evidence : SaveInterpretationEvidence,
  pub mutations                : Vec<DurablePathMutation>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct DurableSaveOutcome {
  pub resulting_base_fingerprint : String,
  pub client_result               : Vec<u8>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum SaveOperationStatus {
  StagingUnAuthorized,
  PreparedUnAuthorized,
  Authorized {
    applied_path_count : usize,
    total_path_count   : usize,
  },
  AppliedAwaitingCommit,
  Refused {
    outcome               : DurableSaveOutcome,
    delivery_acknowledged : bool,
  },
  Committed {
    outcome               : DurableSaveOutcome,
    delivery_acknowledged : bool,
  },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SaveOperationSnapshot {
  pub operation_id             : String,
  pub request_base_fingerprint : String,
  pub binding_blake3           : String,
  pub interpretation_identity  : String,
  pub status                   : SaveOperationStatus,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct MalformedSaveJournal {
  pub path   : PathBuf,
  pub reason : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SavePathConflict {
  pub path            : PathBuf,
  pub expected_before : String,
  pub expected_after  : String,
  pub actual          : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SaveJournalLoadReport {
  pub operations              : Vec<SaveOperationSnapshot>,
  pub malformed               : Vec<MalformedSaveJournal>,
  pub incomplete_publications : Vec<PathBuf>,
}

impl SaveJournalLoadReport {
  /// No caller may derive write authority from a store with an ambiguous
  /// active record.  A temporary initial publication has never been eligible
  /// for authorization and is therefore reported separately.
  pub(crate) fn require_clean (
    self,
  ) -> Result<Vec<SaveOperationSnapshot>, SaveJournalError> {
    if self . malformed . is_empty () { Ok (self . operations) }
    else { Err (SaveJournalError::MalformedJournals (self . malformed)) }
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct StartupSaveRecoveryReport {
  pub operations                : Vec<SaveOperationSnapshot>,
  pub has_unresolved_operations : bool,
  pub incomplete_publications   : Vec<PathBuf>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum SaveJournalError {
  InvalidRequest (String),
  OperationNotFound {
    operation_id : String,
  },
  OperationIdConflict {
    operation_id : String,
    reason       : String,
  },
  WrongOperationState {
    operation_id : String,
    reason       : String,
  },
  FilesystemConflicts {
    operation_id : String,
    conflicts    : Vec<SavePathConflict>,
  },
  MalformedJournals (Vec<MalformedSaveJournal>),
  Io {
    path   : PathBuf,
    action : String,
    reason : String,
  },
}

impl Display for SaveJournalError {
  fn fmt (&self, formatter : &mut Formatter<'_>) -> fmt::Result {
    match self {
      SaveJournalError::InvalidRequest (reason) =>
        write! (formatter, "invalid durable save request: {}", reason),
      SaveJournalError::OperationNotFound { operation_id } =>
        write! (formatter, "durable save operation {:?} was not found",
          operation_id),
      SaveJournalError::OperationIdConflict { operation_id, reason } =>
        write! (formatter, "durable save operation {:?} conflicts: {}",
          operation_id, reason),
      SaveJournalError::WrongOperationState { operation_id, reason } =>
        write! (formatter, "durable save operation {:?} is not ready: {}",
          operation_id, reason),
      SaveJournalError::FilesystemConflicts { operation_id, conflicts } =>
        write! (formatter,
          "durable save operation {:?} has {} filesystem conflict(s): {}",
          operation_id, conflicts . len (),
          conflicts . iter () . map (|conflict| format! (
            "{} (before {}, after {}, actual {})",
            conflict . path . display (), conflict . expected_before,
            conflict . expected_after, conflict . actual))
            . collect::<Vec<String>> () . join ("; ")),
      SaveJournalError::MalformedJournals (journals) =>
        write! (formatter,
          "durable save recovery is required for {} malformed record(s): {}",
          journals . len (), journals . iter () . map (|journal| format! (
            "{}: {}", journal . path . display (), journal . reason))
            . collect::<Vec<String>> () . join ("; ")),
      SaveJournalError::Io { path, action, reason } =>
        write! (formatter, "could not {} {}: {}",
          action, path . display (), reason),
    }
  }
}

impl Error for SaveJournalError {}

#[derive(Clone, Debug)]
pub(crate) struct SaveJournalStore {
  root : PathBuf,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct BlobReference {
  file_name : String,
  byte_len  : u64,
  blake3    : String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct JournalMutation {
  path                   : PathBuf,
  before                 : Option<BlobReference>,
  after                  : Option<BlobReference>,
  final_unix_mode        : Option<u32>,
  destination_stage_path : Option<PathBuf>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct JournalOutcome {
  resulting_base_fingerprint : String,
  client_result               : BlobReference,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case", tag = "status", deny_unknown_fields)]
enum JournalState {
  StagingUnAuthorized,
  PreparedUnAuthorized,
  Authorized {
    applied_path_indices : BTreeSet<usize>,
  },
  Refused {
    outcome               : JournalOutcome,
    delivery_acknowledged : bool,
  },
  Committed {
    outcome               : JournalOutcome,
    delivery_acknowledged : bool,
  },
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct JournalRecordPayload {
  format_version           : u32,
  operation_id             : String,
  request_base_fingerprint : String,
  binding_blake3           : String,
  interpretation_identity  : String,
  interpretation_evidence  : BlobReference,
  mutations                : Vec<JournalMutation>,
  state                    : JournalState,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct JournalRecordEnvelope {
  payload        : JournalRecordPayload,
  payload_blake3 : String,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
enum CompletionTerminal {
  Refused,
  Committed,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
enum CompletionCleanupState {
  Pending,
  Complete,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct CompletionMarkerPayload {
  format_version             : u32,
  operation_id               : String,
  request_base_fingerprint   : String,
  binding_blake3             : String,
  interpretation_identity    : String,
  predecessor_record_blake3  : String,
  terminal                   : CompletionTerminal,
  outcome                    : JournalOutcome,
  cleanup_state              : CompletionCleanupState,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct CompletionMarkerEnvelope {
  payload        : CompletionMarkerPayload,
  payload_blake3 : String,
}

#[derive(Clone, Debug)]
struct LoadedJournalRecord {
  directory : PathBuf,
  payload   : JournalRecordPayload,
}

#[derive(Clone, Debug)]
struct LoadedCompletionMarker {
  directory : PathBuf,
  payload   : CompletionMarkerPayload,
}

#[derive(Clone, Debug)]
enum LoadedSaveOperation {
  Active    (LoadedJournalRecord),
  Completed (LoadedCompletionMarker),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PathDisposition {
  Before,
  After,
  NoChange,
}

impl SaveJournalStore {
  pub(crate) fn at_root (root : PathBuf) -> Self { Self { root } }

  pub(crate) fn root (&self) -> &Path { &self . root }

  /// Publish retained input and same-filesystem stage files.  This does not
  /// authorize any destination mutation.
  pub(crate) fn prepare (
    &self,
    request : &DurableSaveRequest,
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    validate_request (request, &self . root)?;
    let binding_blake3 : String = request_binding_blake3 (request);
    if let Some (existing) = self . load_operation_direct (
      &request . operation_id)? {
      match existing {
        LoadedSaveOperation::Active (mut record) => {
          require_matching_request (
            &record . payload, request, &binding_blake3)?;
          if matches! (record . payload . state,
              JournalState::StagingUnAuthorized)
          {
            self . finish_staging (&mut record)?; }
          return snapshot (&record); },
        LoadedSaveOperation::Completed (marker) => {
          require_matching_completion (
            &marker . payload, request, &binding_blake3)?;
          return completion_snapshot (&marker); },
      }}
    validate_destination_parents (&request . mutations)?;
    let _ : Vec<PathDisposition> = inspect_batch_before_authorization (
      &request . operation_id, &request . mutations)?;
    create_private_directory_all (&self . root)?;
    let operation_key : String = operation_key (&request . operation_id);
    let final_directory : PathBuf =
      self . root . join (format! ("operation-{}", operation_key));
    let temporary_directory : PathBuf = self . root . join (format! (
      ".operation-{}-{}.tmp", operation_key, uuid::Uuid::new_v4 ()));
    create_private_directory (&temporary_directory)?;
    let payload : JournalRecordPayload =
      build_initial_payload (request, &operation_key, &temporary_directory)?;
    persist_record (&temporary_directory, &payload)?;
    fs::rename (&temporary_directory, &final_directory) . map_err (|error|
      io_error (&final_directory, "publish operation directory", error))?;
    sync_directory (&self . root)?;
    let mut loaded : LoadedJournalRecord = LoadedJournalRecord {
      directory: final_directory,
      payload,
    };
    self . finish_staging (&mut loaded)?;
    snapshot (&loaded)
  }

  /// Durably grant effect authority after proving the entire destination
  /// batch is still at its recorded before-values.
  pub(crate) fn authorize (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    let mut loaded : LoadedJournalRecord = match self . load_operation_require_clean (
      operation_id, request_base_fingerprint)? {
      LoadedSaveOperation::Active (loaded) => loaded,
      LoadedSaveOperation::Completed (marker) =>
        return completion_snapshot (&marker),
    };
    match loaded . payload . state {
      JournalState::StagingUnAuthorized => return Err (
        SaveJournalError::WrongOperationState {
          operation_id: operation_id . into (),
          reason: "destination staging has not completed" . into (),
        }),
      JournalState::PreparedUnAuthorized => {},
      JournalState::Authorized { .. } | JournalState::Committed { .. }
      | JournalState::Refused { .. } =>
        return snapshot (&loaded),
    }
    let mutations : Vec<DurablePathMutation> =
      materialize_mutations (&loaded)?;
    let _ : Vec<PathDisposition> = inspect_batch_before_authorization (
      operation_id, &mutations)?;
    ensure_all_stage_files (&loaded, &mutations)?;
    let _ : Vec<PathDisposition> = inspect_batch_before_authorization (
      operation_id, &mutations)?;
    loaded . payload . state = JournalState::Authorized {
      applied_path_indices: BTreeSet::new (),
    };
    persist_record (&loaded . directory, &loaded . payload)?;
    snapshot (&loaded)
  }

  /// Apply or resume an authorized batch.  Returning from this function only
  /// means all path effects are durable; the save still needs its graph/search
  /// publication and `record_committed_outcome`.
  pub(crate) fn apply_authorized (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    self . apply_authorized_with_progress_hook (
      operation_id, request_base_fingerprint, &mut |_completed| {
        #[cfg(test)]
        crate::runtime::save_operations::socket_tests::crash_point (
          &format! ("after-path-{}", _completed));
      })
  }

  /// Persist the result that a caller may acknowledge.  The caller must have
  /// completed matching graph/search publication before invoking this method.
  pub(crate) fn record_committed_outcome (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
    outcome                  : &DurableSaveOutcome,
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    validate_fingerprint (
      "resulting base fingerprint", &outcome . resulting_base_fingerprint)?;
    let mut loaded : LoadedJournalRecord = match self . load_operation_require_clean (
      operation_id, request_base_fingerprint)? {
      LoadedSaveOperation::Active (loaded) => loaded,
      LoadedSaveOperation::Completed (marker) => {
        let existing : SaveOperationSnapshot = completion_snapshot (&marker)?;
        if matches! (&existing . status, SaveOperationStatus::Committed {
          outcome: existing_outcome, .. } if existing_outcome == outcome)
        { return Ok (existing); }
        return Err (SaveJournalError::OperationIdConflict {
          operation_id: operation_id . into (),
          reason: "a different terminal outcome is already recorded" . into (),
        }); },
    };
    if let JournalState::Committed { .. } = &loaded . payload . state {
      let existing : SaveOperationSnapshot = snapshot (&loaded)?;
      if matches! (&existing . status, SaveOperationStatus::Committed {
        outcome: existing_outcome, .. } if existing_outcome == outcome)
      { return Ok (existing); }
      return Err (SaveJournalError::OperationIdConflict {
        operation_id: operation_id . into (),
        reason: "a different terminal outcome is already recorded" . into (),
      }); }
    let applied_path_indices : BTreeSet<usize> = match &loaded . payload . state {
      JournalState::Authorized { applied_path_indices } =>
        applied_path_indices . clone (),
      JournalState::StagingUnAuthorized
      | JournalState::PreparedUnAuthorized
      | JournalState::Refused { .. } => return Err (
        SaveJournalError::WrongOperationState {
          operation_id: operation_id . into (),
          reason: "filesystem effects have not been authorized" . into (),
        }),
      JournalState::Committed { .. } => unreachable! (),
    };
    if applied_path_indices . len () != loaded . payload . mutations . len () {
      return Err (SaveJournalError::WrongOperationState {
        operation_id: operation_id . into (),
        reason: "the full filesystem batch has not been durably applied" . into (),
      }); }
    let mutations : Vec<DurablePathMutation> =
      materialize_mutations (&loaded)?;
    inspect_batch_after_application (operation_id, &mutations)?;
    let outcome_reference : BlobReference = write_or_repair_unpublished_blob (
      &loaded . directory, "outcome.bin", &outcome . client_result)?;
    loaded . payload . state = JournalState::Committed {
      outcome: JournalOutcome {
        resulting_base_fingerprint:
          outcome . resulting_base_fingerprint . clone (),
        client_result: outcome_reference,
      },
      delivery_acknowledged: false,
    };
    persist_record (&loaded . directory, &loaded . payload)?;
    snapshot (&loaded)
  }

  /// Retire a prepared operation without granting file-effect authority.
  /// An authorized batch must recover or commit, never become a refusal.
  pub(crate) fn record_refused_outcome (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
    outcome                  : &DurableSaveOutcome,
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    validate_fingerprint (
      "resulting base fingerprint", &outcome . resulting_base_fingerprint)?;
    let mut loaded : LoadedJournalRecord = match self . load_operation_require_clean (
      operation_id, request_base_fingerprint)? {
      LoadedSaveOperation::Active (loaded) => loaded,
      LoadedSaveOperation::Completed (marker) => {
        let existing : SaveOperationSnapshot = completion_snapshot (&marker)?;
        if matches! (&existing . status, SaveOperationStatus::Refused {
          outcome: old, .. } if old == outcome) { return Ok (existing); }
        return Err (SaveJournalError::OperationIdConflict {
          operation_id: operation_id . into (),
          reason: "a different refusal is already recorded" . into (),
        }); },
    };
    match &loaded . payload . state {
      JournalState::StagingUnAuthorized | JournalState::PreparedUnAuthorized => {},
      JournalState::Refused { .. } => {
        let existing : SaveOperationSnapshot = snapshot (&loaded)?;
        if matches! (&existing . status, SaveOperationStatus::Refused {
          outcome: old, .. } if old == outcome) { return Ok (existing); }
        return Err (SaveJournalError::OperationIdConflict {
          operation_id: operation_id . into (),
          reason: "a different refusal is already recorded" . into (),
        }); },
      JournalState::Authorized { .. } | JournalState::Committed { .. } =>
        return Err (SaveJournalError::WrongOperationState {
          operation_id: operation_id . into (),
          reason: "authorized source effects cannot become a refusal" . into (),
        }),
    }
    let client_result : BlobReference = write_or_repair_unpublished_blob (
      &loaded . directory, "outcome.bin", &outcome . client_result)?;
    loaded . payload . state = JournalState::Refused {
      outcome: JournalOutcome {
        resulting_base_fingerprint: outcome . resulting_base_fingerprint . clone (),
        client_result,
      },
      delivery_acknowledged: false,
    };
    persist_record (&loaded . directory, &loaded . payload)?;
    snapshot (&loaded)
  }

  pub(crate) fn acknowledge_delivery (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    self . acknowledge_delivery_with_progress_hook (
      operation_id, request_base_fingerprint, &mut |_| {})
  }

  fn acknowledge_delivery_with_progress_hook (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
    progress_hook            : &mut dyn FnMut (&str),
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    let loaded : LoadedSaveOperation = self . load_operation_require_clean (
      operation_id, request_base_fingerprint)?;
    let marker : LoadedCompletionMarker = match loaded {
      LoadedSaveOperation::Completed (marker) => marker,
      LoadedSaveOperation::Active (record) => {
        if !matches! (record . payload . state,
          JournalState::Committed { .. } | JournalState::Refused { .. })
        {
          return Err (SaveJournalError::WrongOperationState {
            operation_id: operation_id . into (),
            reason: "there is no committed client result to acknowledge" . into (),
          }); }
        let payload : CompletionMarkerPayload =
          completion_marker_from_record (&record)?;
        persist_completion_marker (&record . directory, &payload)?;
        progress_hook ("completion-marker-published");
        LoadedCompletionMarker {
          directory: record . directory,
          payload,
        }
      },
    };
    let marker : LoadedCompletionMarker =
      self . finish_completion_cleanup (&marker, progress_hook)?;
    completion_snapshot (&marker)
  }

  fn finish_completion_cleanup (
    &self,
    marker        : &LoadedCompletionMarker,
    progress_hook : &mut dyn FnMut (&str),
  ) -> Result<LoadedCompletionMarker, SaveJournalError> {
    if marker . payload . cleanup_state == CompletionCleanupState::Complete {
      return Ok (marker . clone ()); }
    let record_path : PathBuf = marker . directory . join ("record.yaml");
    let predecessor : Option<JournalRecordPayload> =
      load_marker_predecessor (marker)?;
    if let Some (payload) = predecessor {
      cleanup_destination_staging (&payload)?;
      cleanup_retained_request_blobs (&marker . directory, &payload)?;
      cleanup_completion_temporaries (&marker . directory)?;
      sync_directory (&marker . directory)?;
      validate_cleanup_ready_directory (&marker . directory)?;
      progress_hook ("completion-artifacts-cleaned");
      remove_file_if_exists (&record_path, "remove compacted journal record")?;
      sync_directory (&marker . directory)?;
      progress_hook ("completion-record-removed");
    } else {
      validate_compacted_directory_contents (
        &marker . directory, CompletionCleanupState::Pending)?; }
    let mut completed : LoadedCompletionMarker = marker . clone ();
    completed . payload . cleanup_state = CompletionCleanupState::Complete;
    persist_completion_marker (&completed . directory, &completed . payload)?;
    cleanup_completion_temporaries (&completed . directory)?;
    sync_directory (&completed . directory)?;
    validate_compacted_directory_contents (
      &completed . directory, CompletionCleanupState::Complete)?;
    progress_hook ("completion-cleanup-recorded");
    Ok (completed)
  }

  pub(crate) fn status (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    let loaded : LoadedSaveOperation = self . load_operation_require_clean (
      operation_id, request_base_fingerprint)?;
    operation_snapshot (&loaded)
  }

  pub(crate) fn load_all (&self) -> SaveJournalLoadReport {
    let loaded : RawLoadReport = self . load_records ();
    let mut operations : Vec<SaveOperationSnapshot> = Vec::new ();
    let mut malformed : Vec<MalformedSaveJournal> = loaded . malformed;
    for record in loaded . records {
      match operation_snapshot (&record) {
        Ok (operation) => operations . push (operation),
        Err (error) => malformed . push (MalformedSaveJournal {
          path: operation_record_path (&record),
          reason: error . to_string (),
        }),
      }}
    operations . sort_by (|left, right|
      left . operation_id . cmp (&right . operation_id));
    malformed . sort_by (|left, right| left . path . cmp (&right . path));
    SaveJournalLoadReport {
      operations,
      malformed,
      incomplete_publications: loaded . incomplete_publications,
    }
  }

  /// Load every record before altering any destination.  If any journal is
  /// malformed, or any authorized operation has a path outside its complete
  /// before/after set, no destination is touched.
  pub(crate) fn recover_all_unfinished (
    &self,
  ) -> Result<StartupSaveRecoveryReport, SaveJournalError> {
    let records : Vec<LoadedSaveOperation> =
      self . load_records_require_clean ()?;
    for record in &records {
      if let LoadedSaveOperation::Active (record) = record {
        if !matches! (record . payload . state, JournalState::Authorized { .. }) {
          continue; }
        let mutations : Vec<DurablePathMutation> =
          materialize_mutations (record)?;
        let _ : Vec<PathDisposition> = inspect_authorized_batch (
          &record . payload . operation_id, &mutations)?; }}
    for record in &records {
      match record {
        LoadedSaveOperation::Active (record)
          if matches! (record . payload . state,
            JournalState::Authorized { .. }) => {
          self . apply_authorized (
            &record . payload . operation_id,
            &record . payload . request_base_fingerprint)?; },
        LoadedSaveOperation::Completed (marker)
          if marker . payload . cleanup_state == CompletionCleanupState::Pending => {
          self . finish_completion_cleanup (marker, &mut |_| {})?; },
        _ => {},
      }}
    let final_load : SaveJournalLoadReport = self . load_all ();
    let incomplete_publications : Vec<PathBuf> =
      final_load . incomplete_publications . clone ();
    let operations : Vec<SaveOperationSnapshot> = final_load . require_clean ()?;
    let has_unresolved_operations : bool = operations . iter () . any (|operation|
      !matches! (operation . status, SaveOperationStatus::Committed { .. }
        | SaveOperationStatus::Refused { .. }));
    Ok (StartupSaveRecoveryReport {
      operations,
      has_unresolved_operations,
      incomplete_publications,
    })
  }

  fn finish_staging (
    &self,
    loaded : &mut LoadedJournalRecord,
  ) -> Result<(), SaveJournalError> {
    let mutations : Vec<DurablePathMutation> =
      materialize_mutations (loaded)?;
    ensure_all_stage_files (loaded, &mutations)?;
    loaded . payload . state = JournalState::PreparedUnAuthorized;
    persist_record (&loaded . directory, &loaded . payload)
  }

  fn apply_authorized_with_progress_hook (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
    progress_hook            : &mut dyn FnMut (usize),
  ) -> Result<SaveOperationSnapshot, SaveJournalError> {
    let mut loaded : LoadedJournalRecord = match self . load_operation_require_clean (
      operation_id, request_base_fingerprint)? {
      LoadedSaveOperation::Active (loaded) => loaded,
      LoadedSaveOperation::Completed (marker) =>
        return completion_snapshot (&marker),
    };
    if matches! (loaded . payload . state, JournalState::Committed { .. }
        | JournalState::Refused { .. }) {
      return snapshot (&loaded); }
    if !matches! (loaded . payload . state, JournalState::Authorized { .. }) {
      return Err (SaveJournalError::WrongOperationState {
        operation_id: operation_id . into (),
        reason: "the prepared batch has no durable effect authorization" . into (),
      }); }
    let mutations : Vec<DurablePathMutation> =
      materialize_mutations (&loaded)?;
    let mut dispositions : Vec<PathDisposition> = inspect_authorized_batch (
      operation_id, &mutations)?;
    ensure_stage_files_for_before_values (
      &loaded, &mutations, &dispositions)?;
    dispositions = inspect_authorized_batch (operation_id, &mutations)?;
    for index in 0 .. mutations . len () {
      let disposition : PathDisposition = inspect_one_authorized_path (
        operation_id, &mutations [index])?;
      match disposition {
        PathDisposition::Before => apply_one_mutation (
          &loaded, index, &mutations [index])?,
        PathDisposition::After | PathDisposition::NoChange => {},
      }
      let applied : &mut BTreeSet<usize> = match &mut loaded . payload . state {
        JournalState::Authorized { applied_path_indices } =>
          applied_path_indices,
        _ => unreachable! (),
      };
      applied . insert (index);
      persist_record (&loaded . directory, &loaded . payload)?;
      progress_hook (index + 1);
      dispositions [index] = PathDisposition::After;
    }
    snapshot (&loaded)
  }

  fn load_operation_require_clean (
    &self,
    operation_id             : &str,
    request_base_fingerprint : &str,
  ) -> Result<LoadedSaveOperation, SaveJournalError> {
    let loaded : LoadedSaveOperation = self . load_operation_direct (operation_id)?
      . ok_or_else (|| SaveJournalError::OperationNotFound {
        operation_id: operation_id . into (),
      })?;
    if operation_request_base_fingerprint (&loaded) != request_base_fingerprint {
      return Err (SaveJournalError::OperationIdConflict {
        operation_id: operation_id . into (),
        reason: "the request/base fingerprint differs from the recorded operation"
          . into (),
      }); }
    Ok (loaded)
  }

  fn load_operation_direct (
    &self,
    operation_id : &str,
  ) -> Result<Option<LoadedSaveOperation>, SaveJournalError> {
    validate_nonempty_bounded (
      "operation ID", operation_id, MAX_OPERATION_ID_BYTES)?;
    let root_metadata : fs::Metadata = match fs::symlink_metadata (&self . root) {
      Ok (metadata) => metadata,
      Err (error) if error . kind () == std::io::ErrorKind::NotFound =>
        return Ok (None),
      Err (error) => return Err (io_error (
        &self . root, "inspect durable save journal root", error)),
    };
    if !root_metadata . file_type () . is_dir ()
    || !private_permissions_are_restrictive (&root_metadata)
    {
      return Err (SaveJournalError::MalformedJournals (vec![
        MalformedSaveJournal {
          path: self . root . clone (),
          reason: "durable save journal root is not a restrictive private directory"
            . into (),
        } ])); }
    let directory : PathBuf = self . root . join (format! (
      "operation-{}", operation_key (operation_id)));
    match fs::symlink_metadata (&directory) {
      Ok (_) => load_operation_directory (&directory)
        . map (Some)
        . map_err (|reason| SaveJournalError::MalformedJournals (vec![
          MalformedSaveJournal {
            path: operation_authority_path (&directory), reason,
          } ])),
      Err (error) if error . kind () == std::io::ErrorKind::NotFound => Ok (None),
      Err (error) => Err (io_error (
        &directory, "inspect durable save operation", error)),
    }
  }

  fn load_records_require_clean (
    &self,
  ) -> Result<Vec<LoadedSaveOperation>, SaveJournalError> {
    let report : RawLoadReport = self . load_records ();
    if report . malformed . is_empty () { Ok (report . records) }
    else { Err (SaveJournalError::MalformedJournals (report . malformed)) }
  }

  fn load_records (&self) -> RawLoadReport {
    let mut report : RawLoadReport = RawLoadReport::default ();
    let root_metadata : fs::Metadata = match fs::symlink_metadata (&self . root) {
      Ok (metadata) => metadata,
      Err (error) if error . kind () == std::io::ErrorKind::NotFound =>
        return report,
      Err (error) => {
        report . malformed . push (MalformedSaveJournal {
          path: self . root . clone (),
          reason: error . to_string (),
        });
        return report; }
    };
    if !root_metadata . file_type () . is_dir ()
    || !private_permissions_are_restrictive (&root_metadata)
    {
      report . malformed . push (MalformedSaveJournal {
        path: self . root . clone (),
        reason: "durable save journal root is not a restrictive private directory"
          . into (),
      });
      return report; }
    let entries : fs::ReadDir = match fs::read_dir (&self . root) {
      Ok (entries) => entries,
      Err (error) => {
        report . malformed . push (MalformedSaveJournal {
          path: self . root . clone (),
          reason: error . to_string (),
        });
        return report; }
    };
    for entry_result in entries {
      let entry : fs::DirEntry = match entry_result {
        Ok (entry) => entry,
        Err (error) => {
          report . malformed . push (MalformedSaveJournal {
            path: self . root . clone (),
            reason: error . to_string (),
          });
          continue; }
      };
      let path : PathBuf = entry . path ();
      let name : &str = path . file_name () . and_then (|name| name . to_str ())
        . unwrap_or_default ();
      if name . starts_with (".operation-") && name . ends_with (".tmp") {
        report . incomplete_publications . push (path);
        continue; }
      if !name . starts_with ("operation-") {
        report . malformed . push (MalformedSaveJournal {
          path,
          reason: "unexpected entry in the durable save journal root" . into (),
        });
        continue; }
      match load_operation_directory (&path) {
        Ok (record) => report . records . push (record),
        Err (reason) => report . malformed . push (MalformedSaveJournal {
          path: operation_authority_path (&path), reason,
        }),
      }
    }
    report . records . sort_by (|left, right|
      operation_directory (left) . cmp (operation_directory (right)));
    report . malformed . sort_by (|left, right|
      left . path . cmp (&right . path));
    report . incomplete_publications . sort ();
    report
  }
}

#[derive(Default)]
struct RawLoadReport {
  records                 : Vec<LoadedSaveOperation>,
  malformed               : Vec<MalformedSaveJournal>,
  incomplete_publications : Vec<PathBuf>,
}

fn validate_request (
  request      : &DurableSaveRequest,
  journal_root : &Path,
) -> Result<(), SaveJournalError> {
  if !journal_root . is_absolute () {
    return Err (SaveJournalError::InvalidRequest (format! (
      "private journal root {} is not absolute", journal_root . display ()))); }
  if journal_root . components () . any (|component|
    matches! (component, Component::CurDir | Component::ParentDir))
  {
    return Err (SaveJournalError::InvalidRequest (format! (
      "private journal root {} is not lexically normalized",
      journal_root . display ()))); }
  validate_nonempty_bounded (
    "operation ID", &request . operation_id, MAX_OPERATION_ID_BYTES)?;
  validate_fingerprint (
    "request/base fingerprint", &request . request_base_fingerprint)?;
  validate_nonempty_bounded (
    "interpretation identity",
    &request . interpretation_evidence . identity,
    MAX_INTERPRETATION_IDENTITY_BYTES)?;
  if request . mutations . len () > MAX_MUTATIONS_PER_OPERATION {
    return Err (SaveJournalError::InvalidRequest (format! (
      "the batch contains {} paths; the limit is {}",
      request . mutations . len (), MAX_MUTATIONS_PER_OPERATION))); }
  let mut seen : HashSet<&Path> = HashSet::new ();
  let mut total_blob_bytes : u64 =
    request . interpretation_evidence . bytes . len () as u64;
  if total_blob_bytes > MAX_BLOB_BYTES {
    return Err (SaveJournalError::InvalidRequest (format! (
      "interpretation evidence exceeds {} bytes", MAX_BLOB_BYTES))); }
  for mutation in &request . mutations {
    validate_destination_path (&mutation . path, journal_root)?;
    if !seen . insert (&mutation . path) {
      return Err (SaveJournalError::InvalidRequest (format! (
        "the destination path {} occurs more than once",
        mutation . path . display ()))); }
    for bytes in [&mutation . before, &mutation . after]
      . into_iter () . flatten ()
    {
      if bytes . len () as u64 > MAX_BLOB_BYTES {
        return Err (SaveJournalError::InvalidRequest (format! (
          "a retained value for {} exceeds {} bytes",
          mutation . path . display (), MAX_BLOB_BYTES))); }
      total_blob_bytes = total_blob_bytes . checked_add (bytes . len () as u64)
        . ok_or_else (|| SaveJournalError::InvalidRequest (
          "the retained byte count overflowed" . into ()))?;
    }
  }
  if total_blob_bytes > MAX_TOTAL_BLOB_BYTES {
    return Err (SaveJournalError::InvalidRequest (format! (
      "the operation retains {} bytes; the limit is {}",
      total_blob_bytes, MAX_TOTAL_BLOB_BYTES))); }
  Ok (( ))
}

fn validate_nonempty_bounded (
  field     : &str,
  value     : &str,
  max_bytes : usize,
) -> Result<(), SaveJournalError> {
  if value . is_empty () {
    return Err (SaveJournalError::InvalidRequest (format! (
      "{} is empty", field))); }
  if value . len () > max_bytes {
    return Err (SaveJournalError::InvalidRequest (format! (
      "{} exceeds {} bytes", field, max_bytes))); }
  Ok (( ))
}

fn validate_fingerprint (
  field : &str,
  value : &str,
) -> Result<(), SaveJournalError> {
  validate_nonempty_bounded (field, value, MAX_FINGERPRINT_BYTES)
}

fn validate_destination_path (
  path         : &Path,
  journal_root : &Path,
) -> Result<(), SaveJournalError> {
  if !path . is_absolute () {
    return Err (SaveJournalError::InvalidRequest (format! (
      "destination {} is not absolute", path . display ()))); }
  let path_text : &str = path . to_str () . ok_or_else (||
    SaveJournalError::InvalidRequest (format! (
      "destination {} is not valid UTF-8", path . display ())))?;
  if path_text . len () > MAX_PATH_BYTES {
    return Err (SaveJournalError::InvalidRequest (format! (
      "destination {} exceeds {} bytes", path . display (), MAX_PATH_BYTES))); }
  for component in path . components () {
    if matches! (component, Component::CurDir | Component::ParentDir) {
      return Err (SaveJournalError::InvalidRequest (format! (
        "destination {} is not lexically normalized", path . display ()))); }}
  if path . starts_with (journal_root) || journal_root . starts_with (path) {
    return Err (SaveJournalError::InvalidRequest (format! (
      "destination {} overlaps the private journal root {}",
      path . display (), journal_root . display ()))); }
  let _parent : &Path = path . parent () . ok_or_else (||
    SaveJournalError::InvalidRequest (format! (
      "destination {} has no parent directory", path . display ())))?;
  Ok (( ))
}

fn validate_destination_parents (
  mutations : &[DurablePathMutation],
) -> Result<(), SaveJournalError> {
  for mutation in mutations {
    let parent : &Path = mutation . path . parent ()
      . expect ("validated destination parent");
    let metadata : fs::Metadata = fs::metadata (parent) . map_err (|error|
      io_error (parent, "inspect destination parent", error))?;
    if !metadata . is_dir () {
      return Err (SaveJournalError::InvalidRequest (format! (
        "destination parent {} is not a directory", parent . display ()))); }}
  Ok (( ))
}

fn build_initial_payload (
  request             : &DurableSaveRequest,
  operation_key       : &str,
  temporary_directory : &Path,
) -> Result<JournalRecordPayload, SaveJournalError> {
  let evidence : BlobReference = write_or_validate_blob (
    temporary_directory, "interpretation.bin",
    &request . interpretation_evidence . bytes)?;
  let mut mutations : Vec<JournalMutation> = Vec::new ();
  for (index, mutation) in request . mutations . iter () . enumerate () {
    let before : Option<BlobReference> = match &mutation . before {
      Some (bytes) => Some (write_or_validate_blob (
        temporary_directory, &format! ("before-{:06}.bin", index), bytes)?),
      None => None,
    };
    let after : Option<BlobReference> = match &mutation . after {
      Some (bytes) => Some (write_or_validate_blob (
        temporary_directory, &format! ("after-{:06}.bin", index), bytes)?),
      None => None,
    };
    let final_unix_mode : Option<u32> = final_unix_mode (mutation)?;
    let destination_stage_path : Option<PathBuf> = mutation . after . as_ref ()
      . map (|_| destination_stage_path (
        &mutation . path, operation_key, index));
    mutations . push (JournalMutation {
      path: mutation . path . clone (),
      before,
      after,
      final_unix_mode,
      destination_stage_path,
    });
  }
  sync_directory (temporary_directory)?;
  Ok (JournalRecordPayload {
    format_version: SAVE_JOURNAL_FORMAT_VERSION,
    operation_id: request . operation_id . clone (),
    request_base_fingerprint: request . request_base_fingerprint . clone (),
    binding_blake3: request_binding_blake3 (request),
    interpretation_identity:
      request . interpretation_evidence . identity . clone (),
    interpretation_evidence: evidence,
    mutations,
    state: JournalState::StagingUnAuthorized,
  })
}

#[cfg(unix)]
fn final_unix_mode (
  mutation : &DurablePathMutation,
) -> Result<Option<u32>, SaveJournalError> {
  if mutation . after . is_none () { return Ok (None); }
  match &mutation . before {
    Some (_) => {
      let metadata : fs::Metadata = fs::symlink_metadata (&mutation . path)
        . map_err (|error| io_error (
          &mutation . path, "inspect destination permissions", error))?;
      Ok (Some (metadata . permissions () . mode () & 0o7777))
    },
    None => Ok (Some (0o644)),
  }
}

#[cfg(not(unix))]
fn final_unix_mode (
  _mutation : &DurablePathMutation,
) -> Result<Option<u32>, SaveJournalError> { Ok (None) }

fn destination_stage_path (
  destination   : &Path,
  operation_key : &str,
  index         : usize,
) -> PathBuf {
  let parent : &Path = destination . parent () . expect ("validated parent");
  parent . join (format! (".skg-save-stage-{}", operation_key))
    . join (format! ("{:06}.after", index))
}

fn request_binding_blake3 (request : &DurableSaveRequest) -> String {
  let mut hasher : blake3::Hasher = blake3::Hasher::new ();
  feed_hash (&mut hasher, b"skg-durable-save-binding-v1");
  feed_hash (&mut hasher, request . operation_id . as_bytes ());
  feed_hash (&mut hasher, request . request_base_fingerprint . as_bytes ());
  feed_hash (&mut hasher,
    request . interpretation_evidence . identity . as_bytes ());
  feed_blob_identity (&mut hasher, &request . interpretation_evidence . bytes);
  feed_hash (&mut hasher, &(request . mutations . len () as u64) . to_le_bytes ());
  for mutation in &request . mutations {
    feed_hash (&mut hasher,
      mutation . path . to_str () . expect ("validated UTF-8 path") . as_bytes ());
    feed_optional_blob_identity (&mut hasher, &mutation . before);
    feed_optional_blob_identity (&mut hasher, &mutation . after);
  }
  hasher . finalize () . to_hex () . to_string ()
}

fn feed_optional_blob_identity (
  hasher : &mut blake3::Hasher,
  bytes  : &Option<Vec<u8>>,
) {
  match bytes {
    Some (bytes) => {
      feed_hash (hasher, b"present");
      feed_blob_identity (hasher, bytes); },
    None => feed_hash (hasher, b"absent"),
  }
}

fn feed_blob_identity (hasher : &mut blake3::Hasher, bytes : &[u8]) {
  feed_hash (hasher, &(bytes . len () as u64) . to_le_bytes ());
  feed_hash (hasher, blake3::hash (bytes) . as_bytes ());
}

fn feed_hash (hasher : &mut blake3::Hasher, bytes : &[u8]) {
  hasher . update (&(bytes . len () as u64) . to_le_bytes ());
  hasher . update (bytes);
}

fn operation_key (operation_id : &str) -> String {
  blake3::hash (operation_id . as_bytes ()) . to_hex () . to_string ()
}

fn require_matching_request (
  payload        : &JournalRecordPayload,
  request        : &DurableSaveRequest,
  binding_blake3 : &str,
) -> Result<(), SaveJournalError> {
  if payload . request_base_fingerprint != request . request_base_fingerprint {
    return Err (SaveJournalError::OperationIdConflict {
      operation_id: request . operation_id . clone (),
      reason: "the request/base fingerprint differs from the recorded operation"
        . into (),
    }); }
  if payload . binding_blake3 != binding_blake3 {
    return Err (SaveJournalError::OperationIdConflict {
      operation_id: request . operation_id . clone (),
      reason: "the prepared batch or interpretation evidence differs from the recorded operation"
        . into (),
    }); }
  Ok (( ))
}

fn require_matching_completion (
  payload        : &CompletionMarkerPayload,
  request        : &DurableSaveRequest,
  binding_blake3 : &str,
) -> Result<(), SaveJournalError> {
  if payload . request_base_fingerprint != request . request_base_fingerprint {
    return Err (SaveJournalError::OperationIdConflict {
      operation_id: request . operation_id . clone (),
      reason: "the request/base fingerprint differs from the completed operation"
        . into (),
    }); }
  if payload . binding_blake3 != binding_blake3 {
    return Err (SaveJournalError::OperationIdConflict {
      operation_id: request . operation_id . clone (),
      reason: "the prepared batch or interpretation evidence differs from the completed operation"
        . into (),
    }); }
  Ok (( ))
}

fn materialize_mutations (
  loaded : &LoadedJournalRecord,
) -> Result<Vec<DurablePathMutation>, SaveJournalError> {
  let mut mutations : Vec<DurablePathMutation> = Vec::new ();
  for mutation in &loaded . payload . mutations {
    let before : Option<Vec<u8>> = read_optional_blob (
      &loaded . directory, &mutation . before)?;
    let after : Option<Vec<u8>> = read_optional_blob (
      &loaded . directory, &mutation . after)?;
    mutations . push (DurablePathMutation {
      path: mutation . path . clone (), before, after,
    });
  }
  Ok (mutations)
}

fn inspect_batch_before_authorization (
  operation_id : &str,
  mutations    : &[DurablePathMutation],
) -> Result<Vec<PathDisposition>, SaveJournalError> {
  let mut dispositions : Vec<PathDisposition> = Vec::new ();
  let mut conflicts : Vec<SavePathConflict> = Vec::new ();
  for mutation in mutations {
    match read_exact_path_value (&mutation . path) {
      Ok (actual) if actual == mutation . before =>
        dispositions . push (if mutation . before == mutation . after {
          PathDisposition::NoChange
        } else { PathDisposition::Before }),
      Ok (actual) => conflicts . push (path_conflict (mutation, &actual)),
      Err (reason) => conflicts . push (unreadable_path_conflict (
        mutation, reason)),
    }
  }
  if conflicts . is_empty () { Ok (dispositions) }
  else { Err (SaveJournalError::FilesystemConflicts {
    operation_id: operation_id . into (), conflicts,
  }) }
}

fn inspect_authorized_batch (
  operation_id : &str,
  mutations    : &[DurablePathMutation],
) -> Result<Vec<PathDisposition>, SaveJournalError> {
  let mut dispositions : Vec<PathDisposition> = Vec::new ();
  let mut conflicts : Vec<SavePathConflict> = Vec::new ();
  for mutation in mutations {
    match classify_authorized_value (mutation) {
      Ok (disposition) => dispositions . push (disposition),
      Err (conflict) => conflicts . push (conflict),
    }
  }
  if conflicts . is_empty () { Ok (dispositions) }
  else { Err (SaveJournalError::FilesystemConflicts {
    operation_id: operation_id . into (), conflicts,
  }) }
}

fn inspect_one_authorized_path (
  operation_id : &str,
  mutation     : &DurablePathMutation,
) -> Result<PathDisposition, SaveJournalError> {
  classify_authorized_value (mutation) . map_err (|conflict|
    SaveJournalError::FilesystemConflicts {
      operation_id: operation_id . into (),
      conflicts: vec![conflict],
    })
}

fn inspect_batch_after_application (
  operation_id : &str,
  mutations    : &[DurablePathMutation],
) -> Result<(), SaveJournalError> {
  let mut conflicts : Vec<SavePathConflict> = Vec::new ();
  for mutation in mutations {
    match read_exact_path_value (&mutation . path) {
      Ok (actual) if actual == mutation . after => {},
      Ok (actual) => conflicts . push (path_conflict (mutation, &actual)),
      Err (reason) => conflicts . push (unreadable_path_conflict (
        mutation, reason)),
    }
  }
  if conflicts . is_empty () { Ok (( )) }
  else { Err (SaveJournalError::FilesystemConflicts {
    operation_id: operation_id . into (), conflicts,
  }) }
}

fn classify_authorized_value (
  mutation : &DurablePathMutation,
) -> Result<PathDisposition, SavePathConflict> {
  let actual : Option<Vec<u8>> = read_exact_path_value (&mutation . path)
    . map_err (|reason| unreadable_path_conflict (mutation, reason))?;
  if mutation . before == mutation . after && actual == mutation . before {
    Ok (PathDisposition::NoChange)
  } else if actual == mutation . after {
    Ok (PathDisposition::After)
  } else if actual == mutation . before {
    Ok (PathDisposition::Before)
  } else { Err (path_conflict (mutation, &actual)) }
}

fn path_conflict (
  mutation : &DurablePathMutation,
  actual   : &Option<Vec<u8>>,
) -> SavePathConflict {
  SavePathConflict {
    path: mutation . path . clone (),
    expected_before: describe_path_value (&mutation . before),
    expected_after: describe_path_value (&mutation . after),
    actual: describe_path_value (actual),
  }
}

fn unreadable_path_conflict (
  mutation : &DurablePathMutation,
  reason   : String,
) -> SavePathConflict {
  SavePathConflict {
    path: mutation . path . clone (),
    expected_before: describe_path_value (&mutation . before),
    expected_after: describe_path_value (&mutation . after),
    actual: reason,
  }
}

fn describe_path_value (value : &Option<Vec<u8>>) -> String {
  match value {
    Some (bytes) => format! ("{} bytes, blake3 {}",
      bytes . len (), blake3::hash (bytes) . to_hex ()),
    None => "absent" . into (),
  }
}

fn read_exact_path_value (path : &Path) -> Result<Option<Vec<u8>>, String> {
  let metadata : fs::Metadata = match fs::symlink_metadata (path) {
    Ok (metadata) => metadata,
    Err (error) if error . kind () == std::io::ErrorKind::NotFound =>
      return Ok (None),
    Err (error) => return Err (format! ("could not inspect exact path: {}", error)),
  };
  if !metadata . file_type () . is_file () {
    return Err (format! (
      "expected a regular file or absence, found filesystem type {:?}",
      metadata . file_type ())); }
  #[cfg(unix)]
  let mut file : File = {
    let mut options : OpenOptions = OpenOptions::new ();
    options . read (true) . custom_flags (libc::O_NOFOLLOW);
    options . open (path) . map_err (|error| format! (
      "could not open exact regular file without following symlinks: {}", error))?
  };
  #[cfg(not(unix))]
  let mut file : File = File::open (path)
    . map_err (|error| format! ("could not open exact regular file: {}", error))?;
  let opened_metadata : fs::Metadata = file . metadata ()
    . map_err (|error| format! ("could not inspect opened file: {}", error))?;
  if !opened_metadata . is_file () {
    return Err ("opened path is not a regular file" . into ()); }
  let mut bytes : Vec<u8> = Vec::new ();
  file . read_to_end (&mut bytes)
    . map_err (|error| format! ("could not read exact bytes: {}", error))?;
  Ok (Some (bytes))
}

fn ensure_all_stage_files (
  loaded    : &LoadedJournalRecord,
  mutations : &[DurablePathMutation],
) -> Result<(), SaveJournalError> {
  for (index, mutation) in mutations . iter () . enumerate () {
    if mutation . after . is_some () && mutation . before != mutation . after {
      ensure_stage_file (loaded, index, mutation)?; }}
  Ok (( ))
}

fn ensure_stage_files_for_before_values (
  loaded       : &LoadedJournalRecord,
  mutations    : &[DurablePathMutation],
  dispositions : &[PathDisposition],
) -> Result<(), SaveJournalError> {
  for (index, (mutation, disposition)) in mutations . iter ()
    . zip (dispositions) . enumerate ()
  {
    if *disposition == PathDisposition::Before && mutation . after . is_some () {
      ensure_stage_file (loaded, index, mutation)?; }}
  Ok (( ))
}

fn ensure_stage_file (
  loaded   : &LoadedJournalRecord,
  index    : usize,
  mutation : &DurablePathMutation,
) -> Result<(), SaveJournalError> {
  let stage : &Path = loaded . payload . mutations [index]
    . destination_stage_path . as_deref () . ok_or_else (||
      SaveJournalError::MalformedJournals (vec![MalformedSaveJournal {
        path: loaded . directory . join ("record.yaml"),
        reason: format! ("mutation {} has after-bytes but no stage path", index),
      }]))?;
  let expected_stage : PathBuf = destination_stage_path (
    &mutation . path,
    &operation_key (&loaded . payload . operation_id), index);
  if stage != expected_stage {
    return Err (SaveJournalError::MalformedJournals (vec![
      MalformedSaveJournal {
        path: loaded . directory . join ("record.yaml"),
        reason: format! ("mutation {} has an invalid stage path", index),
      }])); }
  let bytes : &[u8] = mutation . after . as_deref () . expect ("checked after");
  let stage_directory : &Path = stage . parent () . expect ("derived stage parent");
  create_private_directory (stage_directory)?;
  match read_exact_path_value (stage) {
    Ok (Some (existing)) if existing == bytes => return Ok (( )),
    Ok (Some (_)) => {
      fs::remove_file (stage) . map_err (|error|
        io_error (stage, "remove incomplete destination stage", error))?;
      sync_directory (stage_directory)?; },
    Ok (None) => {},
    Err (reason) => return Err (SaveJournalError::Io {
      path: stage . into (),
      action: "validate private destination stage" . into (),
      reason,
    }),
  }
  write_new_file (
    stage, bytes,
    loaded . payload . mutations [index] . final_unix_mode)?;
  sync_directory (stage_directory)?;
  let destination_parent : &Path = mutation . path . parent ()
    . expect ("validated destination parent");
  sync_directory (destination_parent)
}

fn apply_one_mutation (
  loaded   : &LoadedJournalRecord,
  index    : usize,
  mutation : &DurablePathMutation,
) -> Result<(), SaveJournalError> {
  let parent : &Path = mutation . path . parent ()
    . expect ("validated destination parent");
  match &mutation . after {
    Some (_) => {
      let stage : &Path = loaded . payload . mutations [index]
        . destination_stage_path . as_deref () . expect ("validated stage path");
      fs::rename (stage, &mutation . path) . map_err (|error|
        io_error (&mutation . path, "atomically replace destination", error))?;
      sync_directory (parent)
    },
    None => {
      match fs::remove_file (&mutation . path) {
        Ok (( )) => {},
        Err (error) if error . kind () == std::io::ErrorKind::NotFound => {},
        Err (error) => return Err (io_error (
          &mutation . path, "delete destination", error)),
      }
      sync_directory (parent)
    },
  }
}

fn snapshot (
  loaded : &LoadedJournalRecord,
) -> Result<SaveOperationSnapshot, SaveJournalError> {
  let status : SaveOperationStatus = match &loaded . payload . state {
    JournalState::StagingUnAuthorized =>
      SaveOperationStatus::StagingUnAuthorized,
    JournalState::PreparedUnAuthorized =>
      SaveOperationStatus::PreparedUnAuthorized,
    JournalState::Authorized { applied_path_indices }
      if applied_path_indices . len () == loaded . payload . mutations . len () =>
        SaveOperationStatus::AppliedAwaitingCommit,
    JournalState::Authorized { applied_path_indices } =>
      SaveOperationStatus::Authorized {
        applied_path_count: applied_path_indices . len (),
        total_path_count: loaded . payload . mutations . len (),
      },
    JournalState::Committed { outcome, delivery_acknowledged }
    | JournalState::Refused { outcome, delivery_acknowledged } => {
      let client_result : Vec<u8> = read_blob (
        &loaded . directory, &outcome . client_result)?;
      let outcome : DurableSaveOutcome = DurableSaveOutcome {
        resulting_base_fingerprint: outcome . resulting_base_fingerprint . clone (),
        client_result,
      };
      if matches! (loaded . payload . state, JournalState::Committed { .. }) {
        SaveOperationStatus::Committed {
          outcome, delivery_acknowledged: *delivery_acknowledged }
      } else {
        SaveOperationStatus::Refused {
          outcome, delivery_acknowledged: *delivery_acknowledged }
      }
    },
  };
  Ok (SaveOperationSnapshot {
    operation_id: loaded . payload . operation_id . clone (),
    request_base_fingerprint:
      loaded . payload . request_base_fingerprint . clone (),
    binding_blake3: loaded . payload . binding_blake3 . clone (),
    interpretation_identity:
      loaded . payload . interpretation_identity . clone (),
    status,
  })
}

fn completion_snapshot (
  loaded : &LoadedCompletionMarker,
) -> Result<SaveOperationSnapshot, SaveJournalError> {
  let client_result : Vec<u8> = read_blob (
    &loaded . directory, &loaded . payload . outcome . client_result)?;
  let outcome : DurableSaveOutcome = DurableSaveOutcome {
    resulting_base_fingerprint:
      loaded . payload . outcome . resulting_base_fingerprint . clone (),
    client_result,
  };
  let status : SaveOperationStatus = match loaded . payload . terminal {
    CompletionTerminal::Committed => SaveOperationStatus::Committed {
      outcome, delivery_acknowledged: true,
    },
    CompletionTerminal::Refused => SaveOperationStatus::Refused {
      outcome, delivery_acknowledged: true,
    },
  };
  Ok (SaveOperationSnapshot {
    operation_id: loaded . payload . operation_id . clone (),
    request_base_fingerprint:
      loaded . payload . request_base_fingerprint . clone (),
    binding_blake3: loaded . payload . binding_blake3 . clone (),
    interpretation_identity:
      loaded . payload . interpretation_identity . clone (),
    status,
  })
}

fn operation_snapshot (
  loaded : &LoadedSaveOperation,
) -> Result<SaveOperationSnapshot, SaveJournalError> {
  match loaded {
    LoadedSaveOperation::Active (record) => snapshot (record),
    LoadedSaveOperation::Completed (marker) => completion_snapshot (marker),
  }
}

fn operation_directory (loaded : &LoadedSaveOperation) -> &Path {
  match loaded {
    LoadedSaveOperation::Active (record) => &record . directory,
    LoadedSaveOperation::Completed (marker) => &marker . directory,
  }
}

fn operation_record_path (loaded : &LoadedSaveOperation) -> PathBuf {
  operation_authority_path (operation_directory (loaded))
}

fn operation_authority_path (directory : &Path) -> PathBuf {
  let completion : PathBuf = directory . join ("completion.yaml");
  if completion . exists () { completion }
  else { directory . join ("record.yaml") }
}

fn operation_request_base_fingerprint (loaded : &LoadedSaveOperation) -> &str {
  match loaded {
    LoadedSaveOperation::Active (record) =>
      &record . payload . request_base_fingerprint,
    LoadedSaveOperation::Completed (marker) =>
      &marker . payload . request_base_fingerprint,
  }
}

fn completion_marker_from_record (
  loaded : &LoadedJournalRecord,
) -> Result<CompletionMarkerPayload, SaveJournalError> {
  let (terminal, outcome) : (CompletionTerminal, JournalOutcome) =
    match &loaded . payload . state {
      JournalState::Committed { outcome, .. } =>
        (CompletionTerminal::Committed, outcome . clone ()),
      JournalState::Refused { outcome, .. } =>
        (CompletionTerminal::Refused, outcome . clone ()),
      _ => return Err (SaveJournalError::WrongOperationState {
        operation_id: loaded . payload . operation_id . clone (),
        reason: "there is no terminal operation to compact" . into (),
      }),
    };
  let record_path : PathBuf = loaded . directory . join ("record.yaml");
  let record_bytes : Vec<u8> = read_bounded_file (&record_path, MAX_RECORD_BYTES)?;
  Ok (CompletionMarkerPayload {
    format_version: COMPLETION_MARKER_FORMAT_VERSION,
    operation_id: loaded . payload . operation_id . clone (),
    request_base_fingerprint:
      loaded . payload . request_base_fingerprint . clone (),
    binding_blake3: loaded . payload . binding_blake3 . clone (),
    interpretation_identity:
      loaded . payload . interpretation_identity . clone (),
    predecessor_record_blake3:
      blake3::hash (&record_bytes) . to_hex () . to_string (),
    terminal,
    outcome,
    cleanup_state: CompletionCleanupState::Pending,
  })
}

fn persist_record (
  directory : &Path,
  payload   : &JournalRecordPayload,
) -> Result<(), SaveJournalError> {
  let payload_bytes : Vec<u8> = serde_yaml::to_string (payload)
    . map_err (|error| SaveJournalError::Io {
      path: directory . join ("record.yaml"),
      action: "serialize journal record" . into (),
      reason: error . to_string (),
    })? . into_bytes ();
  let envelope : JournalRecordEnvelope = JournalRecordEnvelope {
    payload: payload . clone (),
    payload_blake3: blake3::hash (&payload_bytes) . to_hex () . to_string (),
  };
  let bytes : Vec<u8> = serde_yaml::to_string (&envelope)
    . map_err (|error| SaveJournalError::Io {
      path: directory . join ("record.yaml"),
      action: "serialize journal envelope" . into (),
      reason: error . to_string (),
    })? . into_bytes ();
  if bytes . len () as u64 > MAX_RECORD_BYTES {
    return Err (SaveJournalError::InvalidRequest (format! (
      "the journal record exceeds {} bytes", MAX_RECORD_BYTES))); }
  let temporary : PathBuf = directory . join (format! (
    ".record.{}.tmp", uuid::Uuid::new_v4 ()));
  write_new_file (&temporary, &bytes, Some (0o600))?;
  let reread : Vec<u8> = fs::read (&temporary) . map_err (|error|
    io_error (&temporary, "verify temporary journal record", error))?;
  validate_record_envelope (&reread) . map_err (|reason|
    SaveJournalError::Io {
      path: temporary . clone (),
      action: "verify temporary journal record" . into (),
      reason,
    })?;
  let final_path : PathBuf = directory . join ("record.yaml");
  fs::rename (&temporary, &final_path) . map_err (|error|
    io_error (&final_path, "publish journal record", error))?;
  sync_directory (directory)
}

fn persist_completion_marker (
  directory : &Path,
  payload   : &CompletionMarkerPayload,
) -> Result<(), SaveJournalError> {
  let payload_bytes : Vec<u8> = serde_yaml::to_string (payload)
    . map_err (|error| SaveJournalError::Io {
      path: directory . join ("completion.yaml"),
      action: "serialize save completion marker" . into (),
      reason: error . to_string (),
    })? . into_bytes ();
  let envelope : CompletionMarkerEnvelope = CompletionMarkerEnvelope {
    payload: payload . clone (),
    payload_blake3: blake3::hash (&payload_bytes) . to_hex () . to_string (),
  };
  let bytes : Vec<u8> = serde_yaml::to_string (&envelope)
    . map_err (|error| SaveJournalError::Io {
      path: directory . join ("completion.yaml"),
      action: "serialize save completion envelope" . into (),
      reason: error . to_string (),
    })? . into_bytes ();
  if bytes . len () as u64 > MAX_RECORD_BYTES {
    return Err (SaveJournalError::InvalidRequest (format! (
      "the save completion marker exceeds {} bytes", MAX_RECORD_BYTES))); }
  let temporary : PathBuf = directory . join (format! (
    ".completion.{}.tmp", uuid::Uuid::new_v4 ()));
  write_new_file (&temporary, &bytes, Some (0o600))?;
  let reread : Vec<u8> = fs::read (&temporary) . map_err (|error|
    io_error (&temporary, "verify temporary save completion marker", error))?;
  validate_completion_marker_envelope (&reread) . map_err (|reason|
    SaveJournalError::Io {
      path: temporary . clone (),
      action: "verify temporary save completion marker" . into (),
      reason,
    })?;
  let final_path : PathBuf = directory . join ("completion.yaml");
  fs::rename (&temporary, &final_path) . map_err (|error|
    io_error (&final_path, "publish save completion marker", error))?;
  sync_directory (directory)
}

fn load_operation_directory (
  directory : &Path,
) -> Result<LoadedSaveOperation, String> {
  match fs::symlink_metadata (directory . join ("completion.yaml")) {
    Ok (_) => load_completion_marker (directory)
      . map (LoadedSaveOperation::Completed),
    Err (error) if error . kind () == std::io::ErrorKind::NotFound =>
      load_record (directory) . map (LoadedSaveOperation::Active),
    Err (error) => Err (format! (
      "could not inspect save completion marker: {}", error)),
  }
}

fn load_completion_marker (
  directory : &Path,
) -> Result<LoadedCompletionMarker, String> {
  let metadata : fs::Metadata = fs::symlink_metadata (directory)
    . map_err (|error| error . to_string ())?;
  if !metadata . file_type () . is_dir ()
  || !private_permissions_are_restrictive (&metadata)
  {
    return Err (
      "completed operation entry is not a restrictive private directory"
        . into ()); }
  let marker_path : PathBuf = directory . join ("completion.yaml");
  let bytes : Vec<u8> = read_bounded_file (&marker_path, MAX_RECORD_BYTES)
    . map_err (|error| error . to_string ())?;
  let payload : CompletionMarkerPayload =
    validate_completion_marker_envelope (&bytes)?;
  let expected_directory_name : String = format! (
    "operation-{}", operation_key (&payload . operation_id));
  let directory_name : &str = directory . file_name ()
    . and_then (|name| name . to_str ()) . unwrap_or_default ();
  if directory_name != expected_directory_name {
    return Err ("operation ID does not match its completion directory" . into ()); }
  validate_completion_marker_payload (directory, &payload)?;
  let loaded : LoadedCompletionMarker = LoadedCompletionMarker {
    directory: directory . into (), payload,
  };
  match loaded . payload . cleanup_state {
    CompletionCleanupState::Pending => {
      if load_marker_predecessor_string_error (&loaded)? . is_none () {
        validate_compacted_directory_contents_string_error (
          directory, CompletionCleanupState::Pending)?; }
    },
    CompletionCleanupState::Complete => {
      if directory . join ("record.yaml") . exists () {
        return Err (
          "completed cleanup still contains its predecessor record" . into ()); }
      validate_compacted_directory_contents_string_error (
        directory, CompletionCleanupState::Complete)?;
    },
  }
  Ok (loaded)
}

fn validate_completion_marker_envelope (
  bytes : &[u8],
) -> Result<CompletionMarkerPayload, String> {
  let envelope : CompletionMarkerEnvelope = serde_yaml::from_slice (bytes)
    . map_err (|error| error . to_string ())?;
  if envelope . payload . format_version != COMPLETION_MARKER_FORMAT_VERSION {
    return Err (format! ("unsupported durable save completion marker version {}",
      envelope . payload . format_version)); }
  let payload_bytes : Vec<u8> = serde_yaml::to_string (&envelope . payload)
    . map_err (|error| error . to_string ())? . into_bytes ();
  let checksum : String =
    blake3::hash (&payload_bytes) . to_hex () . to_string ();
  if checksum != envelope . payload_blake3 {
    return Err ("durable save completion marker checksum mismatch" . into ()); }
  Ok (envelope . payload)
}

fn validate_completion_marker_payload (
  directory : &Path,
  payload   : &CompletionMarkerPayload,
) -> Result<(), String> {
  validate_nonempty_bounded (
    "operation ID", &payload . operation_id, MAX_OPERATION_ID_BYTES)
    . map_err (|error| error . to_string ())?;
  validate_fingerprint (
    "request/base fingerprint", &payload . request_base_fingerprint)
    . map_err (|error| error . to_string ())?;
  validate_nonempty_bounded (
    "interpretation identity", &payload . interpretation_identity,
    MAX_INTERPRETATION_IDENTITY_BYTES)
    . map_err (|error| error . to_string ())?;
  validate_blake3_hex (&payload . binding_blake3, "request binding")?;
  validate_blake3_hex (
    &payload . predecessor_record_blake3, "predecessor record")?;
  validate_fingerprint (
    "resulting base fingerprint", &payload . outcome . resulting_base_fingerprint)
    . map_err (|error| error . to_string ())?;
  if payload . outcome . client_result . file_name != "outcome.bin" {
    return Err ("completion marker has an invalid outcome blob name" . into ()); }
  validate_blob (directory, &payload . outcome . client_result)
}

fn validate_blake3_hex (value : &str, label : &str) -> Result<(), String> {
  if value . len () != 64
  || !value . bytes () . all (|byte|
    byte . is_ascii_digit () || (b'a' ..= b'f') . contains (&byte))
  {
    return Err (format! ("{} checksum is not lowercase BLAKE3 hex", label)); }
  Ok (( ))
}

fn load_marker_predecessor (
  marker : &LoadedCompletionMarker,
) -> Result<Option<JournalRecordPayload>, SaveJournalError> {
  load_marker_predecessor_string_error (marker) . map_err (|reason|
    SaveJournalError::MalformedJournals (vec![MalformedSaveJournal {
      path: marker . directory . join ("record.yaml"), reason,
    }]))
}

fn load_marker_predecessor_string_error (
  marker : &LoadedCompletionMarker,
) -> Result<Option<JournalRecordPayload>, String> {
  let record_path : PathBuf = marker . directory . join ("record.yaml");
  match fs::symlink_metadata (&record_path) {
    Err (error) if error . kind () == std::io::ErrorKind::NotFound =>
      return Ok (None),
    Err (error) => return Err (format! (
      "could not inspect completion predecessor: {}", error)),
    Ok (_) => {},
  }
  let bytes : Vec<u8> = read_bounded_file (&record_path, MAX_RECORD_BYTES)
    . map_err (|error| error . to_string ())?;
  let checksum : String = blake3::hash (&bytes) . to_hex () . to_string ();
  if checksum != marker . payload . predecessor_record_blake3 {
    return Err ("completion predecessor record checksum mismatch" . into ()); }
  let predecessor : JournalRecordPayload = validate_record_envelope (&bytes)?;
  if predecessor . operation_id != marker . payload . operation_id
  || predecessor . request_base_fingerprint
    != marker . payload . request_base_fingerprint
  || predecessor . binding_blake3 != marker . payload . binding_blake3
  || predecessor . interpretation_identity
    != marker . payload . interpretation_identity
  {
    return Err ("completion marker does not identify its predecessor record"
      . into ()); }
  let (terminal, outcome) : (CompletionTerminal, &JournalOutcome) =
    match &predecessor . state {
      JournalState::Committed { outcome, .. } =>
        (CompletionTerminal::Committed, outcome),
      JournalState::Refused { outcome, .. } =>
        (CompletionTerminal::Refused, outcome),
      _ => return Err (
        "completion marker predecessor is not terminal" . into ()),
    };
  if terminal != marker . payload . terminal
  || outcome != &marker . payload . outcome {
    return Err ("completion marker terminal outcome differs from its predecessor"
      . into ()); }
  Ok (Some (predecessor))
}

fn cleanup_destination_staging (
  payload : &JournalRecordPayload,
) -> Result<(), SaveJournalError> {
  let mut stage_directories : BTreeSet<PathBuf> = BTreeSet::new ();
  for mutation in &payload . mutations {
    if let Some (stage) = &mutation . destination_stage_path {
      remove_file_if_exists (stage, "remove completed destination stage")?;
      if let Some (directory) = stage . parent () {
        stage_directories . insert (directory . into ()); }
    }}
  for directory in stage_directories {
    match fs::symlink_metadata (&directory) {
      Ok (metadata) if metadata . file_type () . is_dir () => {},
      Ok (_) => return Err (SaveJournalError::Io {
        path: directory . clone (),
        action: "remove completed destination stage directory" . into (),
        reason: "path is not a directory" . into (),
      }),
      Err (error) if error . kind () == std::io::ErrorKind::NotFound => continue,
      Err (error) => return Err (io_error (
        &directory, "inspect completed destination stage directory", error)),
    }
    sync_directory (&directory)?;
    match fs::remove_dir (&directory) {
      Ok (( )) => {
        if let Some (parent) = directory . parent () { sync_directory (parent)?; }
      },
      Err (error) if error . kind () == std::io::ErrorKind::NotFound => {},
      Err (error) => return Err (io_error (
        &directory, "remove completed destination stage directory", error)),
    }}
  Ok (( ))
}

fn cleanup_retained_request_blobs (
  directory : &Path,
  payload   : &JournalRecordPayload,
) -> Result<(), SaveJournalError> {
  remove_file_if_exists (
    &directory . join (&payload . interpretation_evidence . file_name),
    "remove compacted interpretation evidence")?;
  for mutation in &payload . mutations {
    for blob in [&mutation . before, &mutation . after] {
      if let Some (blob) = blob {
        remove_file_if_exists (
          &directory . join (&blob . file_name),
          "remove compacted recovery blob")?; }}}
  Ok (( ))
}

fn cleanup_completion_temporaries (
  directory : &Path,
) -> Result<(), SaveJournalError> {
  let entries : fs::ReadDir = fs::read_dir (directory)
    . map_err (|error| io_error (
      directory, "inspect completion cleanup directory", error))?;
  for entry in entries {
    let entry : fs::DirEntry = entry . map_err (|error| io_error (
      directory, "inspect completion cleanup entry", error))?;
    let name : String = entry . file_name () . to_string_lossy () . into_owned ();
    if (name . starts_with (".completion.")
        || name . starts_with (".record."))
    && name . ends_with (".tmp") {
      remove_file_if_exists (
        &entry . path (), "remove incomplete journal publication")?; }
  }
  Ok (( ))
}

fn validate_compacted_directory_contents (
  directory : &Path,
  state     : CompletionCleanupState,
) -> Result<(), SaveJournalError> {
  validate_compacted_directory_contents_string_error (directory, state)
    . map_err (|reason| SaveJournalError::MalformedJournals (vec![
      MalformedSaveJournal {
        path: directory . join ("completion.yaml"), reason,
      } ]))
}

fn validate_compacted_directory_contents_string_error (
  directory : &Path,
  state     : CompletionCleanupState,
) -> Result<(), String> {
  for entry in fs::read_dir (directory) . map_err (|error| error . to_string ())? {
    let entry : fs::DirEntry = entry . map_err (|error| error . to_string ())?;
    let name : String = entry . file_name () . to_string_lossy () . into_owned ();
    let temporary : bool = (name . starts_with (".completion.")
      || name . starts_with (".record.")) && name . ends_with (".tmp");
    let allowed : bool = name == "completion.yaml" || name == "outcome.bin"
      || (state == CompletionCleanupState::Pending && temporary);
    if !allowed {
      return Err (format! (
        "compacted operation contains unexpected entry {:?}", name)); }
  }
  Ok (( ))
}

fn validate_cleanup_ready_directory (
  directory : &Path,
) -> Result<(), SaveJournalError> {
  for entry in fs::read_dir (directory) . map_err (|error| io_error (
    directory, "inspect compacted operation", error))? {
    let entry : fs::DirEntry = entry . map_err (|error| io_error (
      directory, "inspect compacted operation entry", error))?;
    let name : String = entry . file_name () . to_string_lossy () . into_owned ();
    if name != "completion.yaml" && name != "outcome.bin"
    && name != "record.yaml" {
      return Err (SaveJournalError::MalformedJournals (vec![
        MalformedSaveJournal {
          path: entry . path (),
          reason: "unexpected entry remains before completion cleanup" . into (),
        } ])); }
  }
  Ok (( ))
}

fn remove_file_if_exists (
  path   : &Path,
  action : &str,
) -> Result<(), SaveJournalError> {
  match fs::remove_file (path) {
    Ok (( )) => Ok (( )),
    Err (error) if error . kind () == std::io::ErrorKind::NotFound => Ok (( )),
    Err (error) => Err (io_error (path, action, error)),
  }
}

fn load_record (directory : &Path) -> Result<LoadedJournalRecord, String> {
  let metadata : fs::Metadata = fs::symlink_metadata (directory)
    . map_err (|error| error . to_string ())?;
  if !metadata . file_type () . is_dir ()
  || !private_permissions_are_restrictive (&metadata)
  {
    return Err (
      "operation entry is not a restrictive private directory" . into ()); }
  let record_path : PathBuf = directory . join ("record.yaml");
  let bytes : Vec<u8> = read_bounded_file (&record_path, MAX_RECORD_BYTES)
    . map_err (|error| error . to_string ())?;
  let payload : JournalRecordPayload = validate_record_envelope (&bytes)?;
  let directory_name : &str = directory . file_name ()
    . and_then (|name| name . to_str ()) . unwrap_or_default ();
  let expected_directory_name : String = format! (
    "operation-{}", operation_key (&payload . operation_id));
  if directory_name != expected_directory_name {
    return Err ("operation ID does not match its journal directory" . into ()); }
  validate_loaded_payload (directory, &payload)?;
  Ok (LoadedJournalRecord {
    directory: directory . into (), payload,
  })
}

fn validate_record_envelope (
  bytes : &[u8],
) -> Result<JournalRecordPayload, String> {
  let envelope : JournalRecordEnvelope = serde_yaml::from_slice (bytes)
    . map_err (|error| error . to_string ())?;
  if envelope . payload . format_version != SAVE_JOURNAL_FORMAT_VERSION {
    return Err (format! ("unsupported durable save journal version {}",
      envelope . payload . format_version)); }
  let payload_bytes : Vec<u8> = serde_yaml::to_string (&envelope . payload)
    . map_err (|error| error . to_string ())? . into_bytes ();
  let checksum : String =
    blake3::hash (&payload_bytes) . to_hex () . to_string ();
  if checksum != envelope . payload_blake3 {
    return Err ("durable save journal checksum mismatch" . into ()); }
  Ok (envelope . payload)
}

fn validate_loaded_payload (
  directory : &Path,
  payload   : &JournalRecordPayload,
) -> Result<(), String> {
  validate_nonempty_bounded (
    "operation ID", &payload . operation_id, MAX_OPERATION_ID_BYTES)
    . map_err (|error| error . to_string ())?;
  validate_fingerprint (
    "request/base fingerprint", &payload . request_base_fingerprint)
    . map_err (|error| error . to_string ())?;
  validate_nonempty_bounded (
    "interpretation identity", &payload . interpretation_identity,
    MAX_INTERPRETATION_IDENTITY_BYTES)
    . map_err (|error| error . to_string ())?;
  if payload . mutations . len () > MAX_MUTATIONS_PER_OPERATION {
    return Err (format! ("journal contains more than {} mutations",
      MAX_MUTATIONS_PER_OPERATION)); }
  if payload . interpretation_evidence . file_name != "interpretation.bin" {
    return Err ("journal has an invalid interpretation evidence name" . into ()); }
  validate_blob (directory, &payload . interpretation_evidence)?;
  let mut total_blob_bytes : u64 =
    payload . interpretation_evidence . byte_len;
  let mut seen : HashSet<&Path> = HashSet::new ();
  let key : String = operation_key (&payload . operation_id);
  let journal_root : &Path = directory . parent ()
    . ok_or_else (|| "operation directory has no journal root" . to_string ())?;
  for (index, mutation) in payload . mutations . iter () . enumerate () {
    validate_destination_path (&mutation . path, journal_root)
      . map_err (|error| format! ("mutation {}: {}", index, error))?;
    if !seen . insert (&mutation . path) {
      return Err (format! ("mutation {} repeats a destination path", index)); }
    let expected_before_name : String = format! ("before-{:06}.bin", index);
    let expected_after_name : String = format! ("after-{:06}.bin", index);
    if let Some (before) = &mutation . before {
      if before . file_name != expected_before_name {
        return Err (format! ("mutation {} has an invalid before blob name", index)); }
      total_blob_bytes = total_blob_bytes . checked_add (before . byte_len)
        . ok_or_else (|| "retained blob byte count overflowed" . to_string ())?;
      validate_blob (directory, before)?; }
    if let Some (after) = &mutation . after {
      if after . file_name != expected_after_name {
        return Err (format! ("mutation {} has an invalid after blob name", index)); }
      total_blob_bytes = total_blob_bytes . checked_add (after . byte_len)
        . ok_or_else (|| "retained blob byte count overflowed" . to_string ())?;
      validate_blob (directory, after)?;
      let expected_stage : PathBuf =
        destination_stage_path (&mutation . path, &key, index);
      if mutation . destination_stage_path . as_ref () != Some (&expected_stage) {
        return Err (format! ("mutation {} has an invalid stage path", index)); }
    } else if mutation . destination_stage_path . is_some () {
      return Err (format! ("deletion {} unexpectedly has a stage path", index)); }
    #[cfg(unix)]
    if mutation . after . is_some ()
    && !matches! (mutation . final_unix_mode, Some (mode) if mode <= 0o7777)
    {
      return Err (format! ("mutation {} has an invalid final mode", index)); }
    if mutation . after . is_none () && mutation . final_unix_mode . is_some () {
      return Err (format! ("deletion {} unexpectedly has a final mode", index)); }
  }
  if total_blob_bytes > MAX_TOTAL_BLOB_BYTES {
    return Err (format! ("journal retains more than {} bytes",
      MAX_TOTAL_BLOB_BYTES)); }
  if let JournalState::Authorized { applied_path_indices } = &payload . state {
    if applied_path_indices . iter () . any (|index|
      *index >= payload . mutations . len ())
    { return Err ("journal has an out-of-range applied path index" . into ()); }}
  if let JournalState::Committed { outcome, .. }
    | JournalState::Refused { outcome, .. } = &payload . state {
    validate_fingerprint (
      "resulting base fingerprint", &outcome . resulting_base_fingerprint)
      . map_err (|error| error . to_string ())?;
    if outcome . client_result . file_name != "outcome.bin" {
      return Err ("committed outcome has an invalid blob name" . into ()); }
    validate_blob (directory, &outcome . client_result)?; }
  validate_payload_binding (directory, payload)
}

fn validate_payload_binding (
  directory : &Path,
  payload   : &JournalRecordPayload,
) -> Result<(), String> {
  let evidence_bytes : Vec<u8> = read_blob_string_error (
    directory, &payload . interpretation_evidence)?;
  let mut mutations : Vec<DurablePathMutation> = Vec::new ();
  for mutation in &payload . mutations {
    mutations . push (DurablePathMutation {
      path: mutation . path . clone (),
      before: read_optional_blob_string_error (directory, &mutation . before)?,
      after: read_optional_blob_string_error (directory, &mutation . after)?,
    });
  }
  let request : DurableSaveRequest = DurableSaveRequest {
    operation_id: payload . operation_id . clone (),
    request_base_fingerprint: payload . request_base_fingerprint . clone (),
    interpretation_evidence: SaveInterpretationEvidence {
      identity: payload . interpretation_identity . clone (),
      bytes: evidence_bytes,
    },
    mutations,
  };
  if request_binding_blake3 (&request) != payload . binding_blake3 {
    return Err ("durable save request binding checksum mismatch" . into ()); }
  Ok (( ))
}

fn write_or_validate_blob (
  directory : &Path,
  file_name : &str,
  bytes     : &[u8],
) -> Result<BlobReference, SaveJournalError> {
  if bytes . len () as u64 > MAX_BLOB_BYTES {
    return Err (SaveJournalError::InvalidRequest (format! (
      "blob {} exceeds {} bytes", file_name, MAX_BLOB_BYTES))); }
  let reference : BlobReference = BlobReference {
    file_name: file_name . into (),
    byte_len: bytes . len () as u64,
    blake3: blake3::hash (bytes) . to_hex () . to_string (),
  };
  let path : PathBuf = directory . join (file_name);
  match read_exact_path_value (&path) {
    Ok (Some (existing)) if existing == bytes => return Ok (reference),
    Ok (Some (_)) => return Err (SaveJournalError::Io {
      path,
      action: "validate retained recovery blob" . into (),
      reason: "existing bytes differ" . into (),
    }),
    Ok (None) => {},
    Err (reason) => return Err (SaveJournalError::Io {
      path,
      action: "validate retained recovery blob" . into (), reason,
    }),
  }
  write_new_file (&path, bytes, Some (0o600))?;
  sync_directory (directory)?;
  Ok (reference)
}

fn write_or_repair_unpublished_blob (
  directory : &Path,
  file_name : &str,
  bytes     : &[u8],
) -> Result<BlobReference, SaveJournalError> {
  let path : PathBuf = directory . join (file_name);
  match read_exact_path_value (&path) {
    Ok (Some (existing)) if existing != bytes => {
      fs::remove_file (&path) . map_err (|error|
        io_error (&path, "remove incomplete unpublished blob", error))?;
      sync_directory (directory)?; },
    Ok (Some (_)) | Ok (None) => {},
    Err (reason) => return Err (SaveJournalError::Io {
      path,
      action: "validate unpublished blob" . into (),
      reason,
    }),
  }
  write_or_validate_blob (directory, file_name, bytes)
}

fn validate_blob (
  directory : &Path,
  reference : &BlobReference,
) -> Result<(), String> {
  let _ : Vec<u8> = read_blob_string_error (directory, reference)?;
  Ok (( ))
}

fn read_optional_blob (
  directory : &Path,
  reference : &Option<BlobReference>,
) -> Result<Option<Vec<u8>>, SaveJournalError> {
  match reference {
    Some (reference) => read_blob (directory, reference) . map (Some),
    None => Ok (None),
  }
}

fn read_optional_blob_string_error (
  directory : &Path,
  reference : &Option<BlobReference>,
) -> Result<Option<Vec<u8>>, String> {
  match reference {
    Some (reference) => read_blob_string_error (directory, reference) . map (Some),
    None => Ok (None),
  }
}

fn read_blob (
  directory : &Path,
  reference : &BlobReference,
) -> Result<Vec<u8>, SaveJournalError> {
  read_blob_string_error (directory, reference) . map_err (|reason|
    SaveJournalError::MalformedJournals (vec![MalformedSaveJournal {
      path: directory . join (&reference . file_name), reason,
    }]))
}

fn read_blob_string_error (
  directory : &Path,
  reference : &BlobReference,
) -> Result<Vec<u8>, String> {
  if reference . file_name . is_empty ()
  || reference . file_name . contains ('/')
  || reference . file_name . contains ('\\')
  {
    return Err ("recovery blob name is not a single file name" . into ()); }
  if reference . byte_len > MAX_BLOB_BYTES {
    return Err (format! ("recovery blob exceeds {} bytes", MAX_BLOB_BYTES)); }
  let path : PathBuf = directory . join (&reference . file_name);
  let bytes : Vec<u8> = read_bounded_file (&path, MAX_BLOB_BYTES)
    . map_err (|error| error . to_string ())?;
  if bytes . len () as u64 != reference . byte_len {
    return Err (format! ("recovery blob length is {}, expected {}",
      bytes . len (), reference . byte_len)); }
  if blake3::hash (&bytes) . to_hex () . to_string () != reference . blake3 {
    return Err ("recovery blob checksum mismatch" . into ()); }
  Ok (bytes)
}

fn read_bounded_file (
  path      : &Path,
  max_bytes : u64,
) -> Result<Vec<u8>, SaveJournalError> {
  let metadata : fs::Metadata = fs::symlink_metadata (path)
    . map_err (|error| io_error (path, "inspect file", error))?;
  if !metadata . file_type () . is_file () {
    return Err (SaveJournalError::Io {
      path: path . into (),
      action: "read file" . into (),
      reason: "path is not a regular file" . into (),
    }); }
  if !private_permissions_are_restrictive (&metadata) {
    return Err (SaveJournalError::Io {
      path: path . into (),
      action: "read private file" . into (),
      reason: "permissions grant group or other access" . into (),
    }); }
  if metadata . len () > max_bytes {
    return Err (SaveJournalError::Io {
      path: path . into (),
      action: "read file" . into (),
      reason: format! ("file exceeds {} bytes", max_bytes),
    }); }
  fs::read (path) . map_err (|error| io_error (path, "read file", error))
}

fn create_private_directory_all (path : &Path) -> Result<(), SaveJournalError> {
  if path . exists () {
    ensure_private_directory (path)?;
    return Ok (( )); }
  let mut missing : Vec<PathBuf> = Vec::new ();
  let mut cursor : &Path = path;
  while !cursor . exists () {
    missing . push (cursor . into ());
    cursor = cursor . parent () . ok_or_else (||
      SaveJournalError::InvalidRequest (format! (
        "private journal root {} has no existing ancestor",
        path . display ())))?;
  }
  for directory in missing . iter () . rev () {
    create_private_directory (directory)?;
    if let Some (parent) = directory . parent () { sync_directory (parent)?; }
  }
  Ok (( ))
}

#[cfg(unix)]
fn private_permissions_are_restrictive (metadata : &fs::Metadata) -> bool {
  metadata . permissions () . mode () & 0o077 == 0
}

#[cfg(not(unix))]
fn private_permissions_are_restrictive (_metadata : &fs::Metadata) -> bool {
  true
}

fn create_private_directory (path : &Path) -> Result<(), SaveJournalError> {
  match fs::create_dir (path) {
    Ok (( )) => {},
    Err (error) if error . kind () == std::io::ErrorKind::AlreadyExists => {},
    Err (error) => return Err (io_error (
      path, "create private directory", error)),
  }
  ensure_private_directory (path)
}

fn ensure_private_directory (path : &Path) -> Result<(), SaveJournalError> {
  let metadata : fs::Metadata = fs::symlink_metadata (path)
    . map_err (|error| io_error (path, "inspect private directory", error))?;
  if !metadata . file_type () . is_dir () {
    return Err (SaveJournalError::Io {
      path: path . into (),
      action: "use private directory" . into (),
      reason: "path is not a directory" . into (),
    }); }
  #[cfg(unix)]
  fs::set_permissions (path, fs::Permissions::from_mode (0o700))
    . map_err (|error| io_error (path, "restrict private directory", error))?;
  sync_directory (path)
}

fn write_new_file (
  path      : &Path,
  bytes     : &[u8],
  unix_mode : Option<u32>,
) -> Result<(), SaveJournalError> {
  let mut options : OpenOptions = OpenOptions::new ();
  options . write (true) . create_new (true);
  #[cfg(unix)]
  options . mode (unix_mode . unwrap_or (0o600));
  let mut file : File = options . open (path)
    . map_err (|error| io_error (path, "create file", error))?;
  file . write_all (bytes)
    . map_err (|error| io_error (path, "write file", error))?;
  file . sync_all ()
    . map_err (|error| io_error (path, "sync file", error))?;
  drop (file);
  Ok (( ))
}

fn sync_directory (path : &Path) -> Result<(), SaveJournalError> {
  #[cfg(unix)]
  {
    let directory : File = File::open (path)
      . map_err (|error| io_error (path, "open directory for sync", error))?;
    directory . sync_all ()
      . map_err (|error| io_error (path, "sync directory", error))?;
  }
  Ok (( ))
}

fn io_error (
  path   : &Path,
  action : &str,
  error  : std::io::Error,
) -> SaveJournalError {
  SaveJournalError::Io {
    path: path . into (), action: action . into (), reason: error . to_string (),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::process::{Command, Stdio};
  use tempfile::{TempDir, tempdir};

  const CRASH_WORKER_MODE : &str = "SKG_SAVE_JOURNAL_CRASH_WORKER_MODE";
  const CRASH_WORKER_FIXTURE : &str = "SKG_SAVE_JOURNAL_CRASH_WORKER_FIXTURE";

  struct Fixture {
    _temporary : TempDir,
    base       : PathBuf,
    state      : PathBuf,
    source     : PathBuf,
    first      : PathBuf,
    deleted    : PathBuf,
    last       : PathBuf,
  }

  impl Fixture {
    fn new () -> Self {
      let temporary : TempDir = tempdir () . unwrap ();
      let base : PathBuf = temporary . path () . to_path_buf ();
      let state : PathBuf = base . join ("private-state");
      let source : PathBuf = base . join ("source");
      fs::create_dir (&source) . unwrap ();
      let first : PathBuf = source . join ("a.skg");
      let deleted : PathBuf = source . join ("b.skg");
      let last : PathBuf = source . join ("c.skg");
      fs::write (&first, b"old-a") . unwrap ();
      fs::write (&deleted, b"old-b") . unwrap ();
      fs::write (&last, b"old-c") . unwrap ();
      Self {
        _temporary: temporary,
        base,
        state,
        source,
        first,
        deleted,
        last,
      }
    }

    fn request (&self) -> DurableSaveRequest {
      request_for_values (
        "save-operation", "request-base", &self . first,
        Some (b"old-a"), Some (b"new-a"), &self . deleted,
        Some (b"old-b"), None, &self . last,
        Some (b"old-c"), Some (b"new-c"))
    }

    fn store (&self) -> SaveJournalStore {
      SaveJournalStore::at_root (self . state . clone ())
    }

    fn assert_before_values (&self) {
      assert_eq! (fs::read (&self . first) . unwrap (), b"old-a");
      assert_eq! (fs::read (&self . deleted) . unwrap (), b"old-b");
      assert_eq! (fs::read (&self . last) . unwrap (), b"old-c");
    }

    fn assert_after_values (&self) {
      assert_eq! (fs::read (&self . first) . unwrap (), b"new-a");
      assert! (!self . deleted . exists ());
      assert_eq! (fs::read (&self . last) . unwrap (), b"new-c");
    }
  }

  #[test]
  fn refusal_retires_unapproved_effects_but_cannot_cancel_authorized_effects () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    let outcome : DurableSaveOutcome = DurableSaveOutcome {
      resulting_base_fingerprint: "unchanged" . into (),
      client_result: b"confirmation required" . to_vec (),
    };
    store . prepare (&request) . unwrap ();
    store . record_refused_outcome (
      &request . operation_id, &request . request_base_fingerprint, &outcome)
      . unwrap ();
    store . authorize (&request . operation_id, &request . request_base_fingerprint)
      . unwrap ();
    store . apply_authorized (&request . operation_id, &request . request_base_fingerprint)
      . unwrap ();
    let recovered : StartupSaveRecoveryReport = store . recover_all_unfinished () . unwrap ();
    assert! (!recovered . has_unresolved_operations);
    assert! (matches! (&recovered . operations [0] . status,
      SaveOperationStatus::Refused { outcome: recorded, .. } if recorded == &outcome));
    fixture . assert_before_values ();
    let mut authorized : DurableSaveRequest = request . clone ();
    authorized . operation_id = "second-operation" . into ();
    store . prepare (&authorized) . unwrap ();
    store . authorize (&authorized . operation_id, &authorized . request_base_fingerprint)
      . unwrap ();
    assert! (store . record_refused_outcome (
      &authorized . operation_id, &authorized . request_base_fingerprint, &outcome) . is_err ());
    store . recover_all_unfinished () . unwrap ();
    fixture . assert_after_values (); }

  #[test]
  fn complete_lifecycle_requires_authorization_and_a_distinct_commit () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    assert_eq! (
      store . prepare (&request) . unwrap () . status,
      SaveOperationStatus::PreparedUnAuthorized);
    assert_eq! (
      store . status (
        &request . operation_id, &request . request_base_fingerprint)
        . unwrap () . status,
      SaveOperationStatus::PreparedUnAuthorized);
    fixture . assert_before_values ();
    assert! (matches! (
      store . apply_authorized (
        &request . operation_id, &request . request_base_fingerprint),
      Err (SaveJournalError::WrongOperationState { .. })));
    assert!(matches! (
      store . authorize (
        &request . operation_id, &request . request_base_fingerprint)
        . unwrap () . status,
      SaveOperationStatus::Authorized {
        applied_path_count: 0, total_path_count: 3,
      }));
    fixture . assert_before_values ();
    assert_eq! (
      store . apply_authorized (
        &request . operation_id, &request . request_base_fingerprint)
        . unwrap () . status,
      SaveOperationStatus::AppliedAwaitingCommit);
    fixture . assert_after_values ();
    let outcome : DurableSaveOutcome = standard_outcome ();
    assert_eq! (
      store . record_committed_outcome (
        &request . operation_id, &request . request_base_fingerprint,
        &outcome) . unwrap () . status,
      SaveOperationStatus::Committed {
        outcome: outcome . clone (), delivery_acknowledged: false,
      });
    assert_eq! (
      store . acknowledge_delivery (
        &request . operation_id, &request . request_base_fingerprint)
        . unwrap () . status,
      SaveOperationStatus::Committed {
        outcome, delivery_acknowledged: true,
      });
  }

  #[test]
  fn acknowledged_completion_compacts_to_exact_outcome_and_marker () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    let outcome : DurableSaveOutcome = standard_outcome ();
    store . prepare (&request) . unwrap ();
    store . authorize (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . apply_authorized (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . record_committed_outcome (
      &request . operation_id, &request . request_base_fingerprint, &outcome)
      . unwrap ();
    let snapshot : SaveOperationSnapshot = store . acknowledge_delivery (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    assert_eq! (snapshot . status, SaveOperationStatus::Committed {
      outcome: outcome . clone (), delivery_acknowledged: true,
    });
    let directory : PathBuf = store . root () . join (format! (
      "operation-{}", operation_key (&request . operation_id)));
    assert! (directory . join ("completion.yaml") . is_file ());
    assert! (!directory . join ("record.yaml") . exists ());
    assert_eq! (fs::read (directory . join ("outcome.bin")) . unwrap (),
      outcome . client_result);
    let mut entries : Vec<String> = fs::read_dir (&directory) . unwrap ()
      . map (|entry| entry . unwrap () . file_name () . to_string_lossy () . into ())
      . collect ();
    entries . sort ();
    assert_eq! (entries, vec!["completion.yaml", "outcome.bin"]);
    let marker : LoadedCompletionMarker = load_completion_marker (&directory)
      . unwrap ();
    assert_eq! (marker . payload . cleanup_state, CompletionCleanupState::Complete);
    assert_eq! (marker . payload . terminal, CompletionTerminal::Committed);
    assert_eq! (marker . payload . operation_id, request . operation_id);
    assert_eq! (marker . payload . request_base_fingerprint,
      request . request_base_fingerprint);
    assert_eq! (marker . payload . interpretation_identity,
      request . interpretation_evidence . identity);
    assert_eq! (marker . payload . outcome, JournalOutcome {
      resulting_base_fingerprint: outcome . resulting_base_fingerprint,
      client_result: BlobReference {
        file_name: "outcome.bin" . into (), byte_len: 24,
        blake3: blake3::hash (b"rendered client response") . to_hex () . to_string (),
      },
    });
  }

  #[test]
  fn changed_request_is_refused_after_completion_compaction () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    store . prepare (&request) . unwrap ();
    store . authorize (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . apply_authorized (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . record_committed_outcome (
      &request . operation_id, &request . request_base_fingerprint,
      &standard_outcome ()) . unwrap ();
    store . acknowledge_delivery (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    let mut changed : DurableSaveRequest = request . clone ();
    changed . mutations [0] . after = Some (b"changed-after" . to_vec ());
    assert! (matches! (store . prepare (&changed),
      Err (SaveJournalError::OperationIdConflict { .. })));
    fixture . assert_after_values ();
  }

  #[test]
  fn direct_lookup_ignores_unrelated_malformed_directory_but_startup_audits_it () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    store . prepare (&request) . unwrap ();
    store . authorize (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . apply_authorized (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . record_committed_outcome (
      &request . operation_id, &request . request_base_fingerprint,
      &standard_outcome ()) . unwrap ();
    store . acknowledge_delivery (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    let unrelated : PathBuf = store . root () . join ("operation-unrelated");
    create_private_directory (&unrelated) . unwrap ();
    fs::write (unrelated . join ("record.yaml"), b"invalid: [") . unwrap ();
    assert!(matches! (store . prepare (&request) . unwrap () . status,
      SaveOperationStatus::Committed { delivery_acknowledged: true, .. }));
    assert!(matches! (store . status (
      &request . operation_id, &request . request_base_fingerprint),
      Ok (SaveOperationSnapshot {
        status: SaveOperationStatus::Committed {
          delivery_acknowledged: true, .. }, .. })));
    let report : SaveJournalLoadReport = store . load_all ();
    assert_eq! (report . malformed . len (), 1);
    assert!(matches! (store . recover_all_unfinished (),
      Err (SaveJournalError::MalformedJournals (_))));
  }

  #[test]
  fn valid_completion_marker_with_corrupt_predecessor_fails_closed () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    store . prepare (&request) . unwrap ();
    store . authorize (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . apply_authorized (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . record_committed_outcome (
      &request . operation_id, &request . request_base_fingerprint,
      &standard_outcome ()) . unwrap ();
    let directory : PathBuf = store . root () . join (format! (
      "operation-{}", operation_key (&request . operation_id)));
    let record : LoadedJournalRecord = load_record (&directory) . unwrap ();
    let marker : CompletionMarkerPayload = completion_marker_from_record (&record)
      . unwrap ();
    persist_completion_marker (&directory, &marker) . unwrap ();
    fs::write (directory . join ("record.yaml"), b"corrupt predecessor") . unwrap ();
    assert!(matches! (store . status (
      &request . operation_id, &request . request_base_fingerprint),
      Err (SaveJournalError::MalformedJournals (_))));
    assert!(matches! (store . recover_all_unfinished (),
      Err (SaveJournalError::MalformedJournals (_))));
    fixture . assert_after_values ();
  }

  #[test]
  fn unsupported_completion_marker_fails_closed () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    store . prepare (&request) . unwrap ();
    store . authorize (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . apply_authorized (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . record_committed_outcome (
      &request . operation_id, &request . request_base_fingerprint,
      &standard_outcome ()) . unwrap ();
    let directory : PathBuf = store . root () . join (format! (
      "operation-{}", operation_key (&request . operation_id)));
    let record : LoadedJournalRecord = load_record (&directory) . unwrap ();
    let mut payload : CompletionMarkerPayload = completion_marker_from_record (&record)
      . unwrap ();
    payload . format_version += 1;
    let payload_bytes : Vec<u8> = serde_yaml::to_string (&payload) . unwrap ()
      . into_bytes ();
    let envelope : CompletionMarkerEnvelope = CompletionMarkerEnvelope {
      payload, payload_blake3: blake3::hash (&payload_bytes) . to_hex () . to_string (),
    };
    fs::write (directory . join ("completion.yaml"),
      serde_yaml::to_string (&envelope) . unwrap ()) . unwrap ();
    assert!(matches! (store . status (
      &request . operation_id, &request . request_base_fingerprint),
      Err (SaveJournalError::MalformedJournals (_))));
    assert!(matches! (store . recover_all_unfinished (),
      Err (SaveJournalError::MalformedJournals (_))));
    fixture . assert_after_values ();
  }

  #[test]
  fn exact_duplicate_is_deduplicated_and_any_binding_change_conflicts () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    let first : SaveOperationSnapshot = store . prepare (&request) . unwrap ();
    assert_eq! (store . prepare (&request) . unwrap (), first);
    let mut different_fingerprint : DurableSaveRequest = request . clone ();
    different_fingerprint . request_base_fingerprint = "other-base" . into ();
    assert! (matches! (store . prepare (&different_fingerprint),
      Err (SaveJournalError::OperationIdConflict { .. })));
    let mut different_batch : DurableSaveRequest = request . clone ();
    different_batch . mutations [0] . after = Some (b"different" . to_vec ());
    assert! (matches! (store . prepare (&different_batch),
      Err (SaveJournalError::OperationIdConflict { .. })));
    fixture . assert_before_values ();
  }

  #[test]
  fn startup_does_not_apply_a_staged_but_un_authorized_operation () {
    let fixture : Fixture = Fixture::new ();
    run_crash_worker (&fixture, "after-staging");
    fixture . assert_before_values ();
    let report : StartupSaveRecoveryReport = fixture . store ()
      . recover_all_unfinished () . unwrap ();
    assert! (report . has_unresolved_operations);
    assert_eq! (report . operations . len (), 1);
    assert_eq! (report . operations [0] . status,
      SaveOperationStatus::PreparedUnAuthorized);
    fixture . assert_before_values ();
  }

  #[test]
  fn startup_resumes_a_crash_after_authorization_before_the_first_effect () {
    let fixture : Fixture = Fixture::new ();
    run_crash_worker (&fixture, "after-authorization");
    fixture . assert_before_values ();
    let report : StartupSaveRecoveryReport = fixture . store ()
      . recover_all_unfinished () . unwrap ();
    assert! (report . has_unresolved_operations);
    assert_eq! (report . operations [0] . status,
      SaveOperationStatus::AppliedAwaitingCommit);
    fixture . assert_after_values ();
  }

  #[test]
  fn startup_resumes_between_a_replacement_deletion_and_replacement () {
    for (mode, expected_deleted_exists, expected_last) in [
      ("after-first-effect", true, b"old-c" . as_slice ()),
      ("after-second-effect", false, b"old-c" . as_slice ()),
    ] {
      let fixture : Fixture = Fixture::new ();
      run_crash_worker (&fixture, mode);
      assert_eq! (fs::read (&fixture . first) . unwrap (), b"new-a");
      assert_eq! (fixture . deleted . exists (), expected_deleted_exists);
      assert_eq! (fs::read (&fixture . last) . unwrap (), expected_last);
      let report : StartupSaveRecoveryReport = fixture . store ()
        . recover_all_unfinished () . unwrap ();
      assert_eq! (report . operations [0] . status,
        SaveOperationStatus::AppliedAwaitingCommit);
      fixture . assert_after_values ();
    }
  }

  #[test]
  fn startup_recognizes_application_completed_before_commit () {
    let fixture : Fixture = Fixture::new ();
    run_crash_worker (&fixture, "after-application");
    fixture . assert_after_values ();
    let report : StartupSaveRecoveryReport = fixture . store ()
      . recover_all_unfinished () . unwrap ();
    assert_eq! (report . operations [0] . status,
      SaveOperationStatus::AppliedAwaitingCommit);
    fixture . assert_after_values ();
  }

  #[test]
  fn startup_finishes_completion_cleanup_after_each_published_boundary () {
    for mode in [
      "after-completion-marker-published",
      "after-completion-artifacts-cleaned",
      "after-completion-record-removed",
    ] {
      let fixture : Fixture = Fixture::new ();
      run_crash_worker (&fixture, mode);
      fixture . assert_after_values ();
      let report : StartupSaveRecoveryReport = fixture . store ()
        . recover_all_unfinished () . unwrap ();
      assert! (!report . has_unresolved_operations);
      assert_eq! (report . operations . len (), 1);
      assert_eq! (report . operations [0] . status,
        SaveOperationStatus::Committed {
          outcome: standard_outcome (), delivery_acknowledged: true,
        });
      let directory : PathBuf = fixture . store () . root () . join (format! (
        "operation-{}", operation_key ("save-operation")));
      let mut entries : Vec<String> = fs::read_dir (&directory) . unwrap ()
        . map (|entry| entry . unwrap () . file_name () . to_string_lossy () . into ())
        . collect ();
      entries . sort ();
      assert_eq! (entries, vec!["completion.yaml", "outcome.bin"]);
    }
  }

  #[test]
  fn compacted_old_retry_after_newer_save_preserves_newer_bytes () {
    let fixture : Fixture = Fixture::new ();
    run_crash_worker (&fixture, "after-commit");
    fixture . assert_after_values ();
    let store : SaveJournalStore = fixture . store ();
    store . acknowledge_delivery (
      "save-operation", "request-base") . unwrap ();
    let newer : DurableSaveRequest = request_for_values (
      "newer-save", "newer-request-base", &fixture . first,
      Some (b"new-a"), Some (b"newest-a"), &fixture . deleted,
      None, Some (b"newest-b"), &fixture . last,
      Some (b"new-c"), Some (b"newest-c"));
    store . prepare (&newer) . unwrap ();
    store . authorize (
      &newer . operation_id, &newer . request_base_fingerprint) . unwrap ();
    store . apply_authorized (
      &newer . operation_id, &newer . request_base_fingerprint) . unwrap ();
    store . record_committed_outcome (
      &newer . operation_id, &newer . request_base_fingerprint,
      &DurableSaveOutcome {
        resulting_base_fingerprint: "newest-result-base" . into (),
        client_result: b"newest response" . to_vec (),
      }) . unwrap ();
    let old_request : DurableSaveRequest = fixture . request ();
    let duplicate : SaveOperationSnapshot =
      store . prepare (&old_request) . unwrap ();
    assert_eq! (duplicate . status, SaveOperationStatus::Committed {
      outcome: standard_outcome (), delivery_acknowledged: true,
    });
    assert_eq! (fs::read (&fixture . first) . unwrap (), b"newest-a");
    assert_eq! (fs::read (&fixture . deleted) . unwrap (), b"newest-b");
    assert_eq! (fs::read (&fixture . last) . unwrap (), b"newest-c");
    store . recover_all_unfinished () . unwrap ();
    assert_eq! (fs::read (&fixture . first) . unwrap (), b"newest-a");
    assert_eq! (fs::read (&fixture . deleted) . unwrap (), b"newest-b");
    assert_eq! (fs::read (&fixture . last) . unwrap (), b"newest-c");
  }

  #[test]
  fn recovery_checks_the_whole_batch_before_resuming_any_remaining_path () {
    let fixture : Fixture = Fixture::new ();
    run_crash_worker (&fixture, "after-first-effect");
    fs::write (&fixture . last, b"external-c") . unwrap ();
    let error : SaveJournalError = fixture . store ()
      . recover_all_unfinished () . unwrap_err ();
    assert! (matches! (error,
      SaveJournalError::FilesystemConflicts { .. }));
    assert_eq! (fs::read (&fixture . first) . unwrap (), b"new-a");
    assert_eq! (fs::read (&fixture . deleted) . unwrap (), b"old-b");
    assert_eq! (fs::read (&fixture . last) . unwrap (), b"external-c");
  }

  #[test]
  fn corrupt_retained_evidence_fails_closed_without_touching_destinations () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    store . prepare (&request) . unwrap ();
    store . authorize (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    let directory : PathBuf = store . root () . join (format! (
      "operation-{}", operation_key (&request . operation_id)));
    fs::write (directory . join ("interpretation.bin"), b"corrupt") . unwrap ();
    let report : SaveJournalLoadReport = store . load_all ();
    assert_eq! (report . malformed . len (), 1);
    assert! (matches! (store . recover_all_unfinished (),
      Err (SaveJournalError::MalformedJournals (_))));
    fixture . assert_before_values ();
  }

  #[test]
  fn incomplete_stage_and_unpublished_outcome_are_rebuilt_from_retained_data () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    store . prepare (&request) . unwrap ();
    let operation_directory : PathBuf = store . root () . join (format! (
      "operation-{}", operation_key (&request . operation_id)));
    let loaded : LoadedJournalRecord = load_record (&operation_directory) . unwrap ();
    let first_stage : PathBuf = loaded . payload . mutations [0]
      . destination_stage_path . clone () . unwrap ();
    fs::write (&first_stage, b"partial stage") . unwrap ();
    store . authorize (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    store . apply_authorized (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    fs::write (operation_directory . join ("outcome.bin"), b"partial outcome")
      . unwrap ();
    let outcome : DurableSaveOutcome = standard_outcome ();
    assert_eq! (store . record_committed_outcome (
      &request . operation_id, &request . request_base_fingerprint,
      &outcome) . unwrap () . status,
      SaveOperationStatus::Committed {
        outcome, delivery_acknowledged: false,
      });
    fixture . assert_after_values ();
  }

  #[test]
  fn malformed_active_record_fails_closed_but_initial_temp_is_not_authority () {
    let fixture : Fixture = Fixture::new ();
    let store : SaveJournalStore = fixture . store ();
    let request : DurableSaveRequest = fixture . request ();
    store . prepare (&request) . unwrap ();
    let directory : PathBuf = store . root () . join (format! (
      "operation-{}", operation_key (&request . operation_id)));
    fs::write (directory . join ("record.yaml"), b"invalid: [") . unwrap ();
    assert! (matches! (store . recover_all_unfinished (),
      Err (SaveJournalError::MalformedJournals (_))));
    fixture . assert_before_values ();

    let clean : Fixture = Fixture::new ();
    create_private_directory_all (&clean . state) . unwrap ();
    let incomplete : PathBuf = clean . state . join (
      ".operation-unused-initial-publication.tmp");
    create_private_directory (&incomplete) . unwrap ();
    let report : SaveJournalLoadReport = clean . store () . load_all ();
    assert! (report . malformed . is_empty ());
    assert_eq! (report . incomplete_publications, vec![incomplete]);
    assert! (report . require_clean () . is_ok ());
  }

  #[cfg(unix)]
  #[test]
  fn journal_and_destination_staging_are_private () {
    let fixture : Fixture = Fixture::new ();
    let request : DurableSaveRequest = fixture . request ();
    fixture . store () . prepare (&request) . unwrap ();
    let root_mode : u32 = fs::metadata (&fixture . state) . unwrap ()
      . permissions () . mode () & 0o777;
    assert_eq! (root_mode, 0o700);
    let stage_directory : PathBuf = fixture . source . join (format! (
      ".skg-save-stage-{}", operation_key (&request . operation_id)));
    let stage_mode : u32 = fs::metadata (&stage_directory) . unwrap ()
      . permissions () . mode () & 0o777;
    assert_eq! (stage_mode, 0o700);
  }

  /// This test is selected in a child test process.  SIGKILL deliberately
  /// bypasses Rust unwinding, destructors, and the test harness's normal
  /// failure cleanup.
  #[test]
  fn durable_save_crash_process_worker () {
    let mode : String = match std::env::var (CRASH_WORKER_MODE) {
      Ok (mode) => mode,
      Err (_) => return,
    };
    let base : PathBuf = PathBuf::from (
      std::env::var_os (CRASH_WORKER_FIXTURE) . unwrap ());
    let state : PathBuf = base . join ("private-state");
    let source : PathBuf = base . join ("source");
    let request : DurableSaveRequest = request_for_values (
      "save-operation", "request-base", &source . join ("a.skg"),
      Some (b"old-a"), Some (b"new-a"), &source . join ("b.skg"),
      Some (b"old-b"), None, &source . join ("c.skg"),
      Some (b"old-c"), Some (b"new-c"));
    let store : SaveJournalStore = SaveJournalStore::at_root (state);
    store . prepare (&request) . unwrap ();
    if mode == "after-staging" { exit_without_cleanup (); }
    store . authorize (
      &request . operation_id, &request . request_base_fingerprint) . unwrap ();
    if mode == "after-authorization" { exit_without_cleanup (); }
    let crash_after : Option<usize> = match mode . as_str () {
      "after-first-effect" => Some (1),
      "after-second-effect" => Some (2),
      _ => None,
    };
    store . apply_authorized_with_progress_hook (
      &request . operation_id, &request . request_base_fingerprint,
      &mut |completed| {
        if crash_after == Some (completed) { exit_without_cleanup (); }
      }) . unwrap ();
    if mode == "after-application" { exit_without_cleanup (); }
    store . record_committed_outcome (
      &request . operation_id, &request . request_base_fingerprint,
      &standard_outcome ()) . unwrap ();
    if mode == "after-commit" { exit_without_cleanup (); }
    let completion_crash : bool = matches! (mode . as_str (),
      "after-completion-marker-published"
      | "after-completion-artifacts-cleaned"
      | "after-completion-record-removed");
    if completion_crash {
      store . acknowledge_delivery_with_progress_hook (
        &request . operation_id, &request . request_base_fingerprint,
        &mut |step| {
          if mode == format! ("after-{}", step) { exit_without_cleanup (); }
        }) . unwrap ();
    }
    panic! ("unknown crash-worker mode {:?}", mode);
  }

  fn request_for_values (
    operation_id             : &str,
    request_base_fingerprint : &str,
    first                    : &Path,
    first_before             : Option<&[u8]>,
    first_after              : Option<&[u8]>,
    deleted                  : &Path,
    deleted_before           : Option<&[u8]>,
    deleted_after            : Option<&[u8]>,
    last                     : &Path,
    last_before              : Option<&[u8]>,
    last_after               : Option<&[u8]>,
  ) -> DurableSaveRequest {
    DurableSaveRequest {
      operation_id: operation_id . into (),
      request_base_fingerprint: request_base_fingerprint . into (),
      interpretation_evidence: SaveInterpretationEvidence {
        identity: "config=/config; sources=public,private; format=v1" . into (),
        bytes: b"exact source and config interpretation evidence" . to_vec (),
      },
      mutations: vec![
        DurablePathMutation {
          path: first . into (),
          before: first_before . map (<[u8]>::to_vec),
          after: first_after . map (<[u8]>::to_vec),
        },
        DurablePathMutation {
          path: deleted . into (),
          before: deleted_before . map (<[u8]>::to_vec),
          after: deleted_after . map (<[u8]>::to_vec),
        },
        DurablePathMutation {
          path: last . into (),
          before: last_before . map (<[u8]>::to_vec),
          after: last_after . map (<[u8]>::to_vec),
        },
      ],
    }
  }

  fn standard_outcome () -> DurableSaveOutcome {
    DurableSaveOutcome {
      resulting_base_fingerprint: "result-base" . into (),
      client_result: b"rendered client response" . to_vec (),
    }
  }

  fn run_crash_worker (fixture : &Fixture, mode : &str) {
    let executable : PathBuf = std::env::current_exe () . unwrap ();
    let status : std::process::ExitStatus = Command::new (executable)
      . arg ("durable_save_crash_process_worker")
      . arg ("--nocapture")
      . env (CRASH_WORKER_MODE, mode)
      . env (CRASH_WORKER_FIXTURE, &fixture . base)
      . stdout (Stdio::null ())
      . stderr (Stdio::null ())
      . status () . unwrap ();
    assert! (!status . success (),
      "crash worker unexpectedly exited successfully for {}", mode);
  }

  #[cfg(unix)]
  fn exit_without_cleanup () -> ! {
    unsafe {
      libc::kill (libc::getpid (), libc::SIGKILL);
      libc::_exit (79); }
  }

  #[cfg(not(unix))]
  fn exit_without_cleanup () -> ! { std::process::abort () }
}
