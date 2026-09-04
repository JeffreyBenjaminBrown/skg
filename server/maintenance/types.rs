use crate::types::store_state::{GraphGeneration, ManifestRevision};

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt;
use std::path::PathBuf;

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct IncidentId (String);

impl IncidentId {
  pub fn new () -> Self { Self (uuid::Uuid::new_v4 () . to_string ()) }

  pub fn parse (value : &str) -> Result<Self, String> {
    uuid::Uuid::parse_str (value)
      . map (|uuid| Self (uuid . to_string ()))
      . map_err (|_| format! ("invalid incident UUID '{}'", value)) }

  pub fn as_str (&self) -> &str { &self . 0 }
}

impl fmt::Display for IncidentId {
  fn fmt (&self, formatter : &mut fmt::Formatter<'_>) -> fmt::Result {
    formatter . write_str (&self . 0) }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct CandidateId (String);

impl CandidateId {
  pub fn new () -> Self { Self (uuid::Uuid::new_v4 () . to_string ()) }

  pub fn parse (value : &str) -> Result<Self, String> {
    uuid::Uuid::parse_str (value)
      . map (|uuid| Self (uuid . to_string ()))
      . map_err (|_| format! ("invalid candidate UUID '{}'", value)) }

  pub fn as_str (&self) -> &str { &self . 0 }
}

impl fmt::Display for CandidateId {
  fn fmt (&self, formatter : &mut fmt::Formatter<'_>) -> fmt::Result {
    formatter . write_str (&self . 0) }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct MaintenanceEpoch (u64);

impl MaintenanceEpoch {
  pub const INITIAL : Self = Self (0);

  pub fn get (self) -> u64 { self . 0 }

  pub fn parse (value : &str) -> Result<Self, String> {
    value . parse::<u64> () . map (Self)
      . map_err (|_| format! ("invalid maintenance epoch '{}'", value)) }

  pub fn successor (self) -> Self {
    Self (self . 0 . checked_add (1)
      . expect ("maintenance epoch exhausted u64")) }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct ObservationSequence (u64);

impl ObservationSequence {
  pub const INITIAL : Self = Self (0);

  pub fn get (self) -> u64 { self . 0 }

  pub fn successor (self) -> Self {
    Self (self . 0 . checked_add (1)
      . expect ("observation sequence exhausted u64")) }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct PresentationGeneration (u64);

impl PresentationGeneration {
  pub const INITIAL : Self = Self (1);

  pub fn get (self) -> u64 { self . 0 }

  pub fn successor (self) -> Self {
    Self (self . 0 . checked_add (1)
      . expect ("presentation generation exhausted u64")) }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum MaintenanceOrigin {
  UnsolicitedObservation,
  ExplicitPartialReload,
  PendingReconciliation,
  Pull,
  FullRebuild,
  ConfigReplacement,
  Recovery,
}

impl MaintenanceOrigin {
  pub fn label (&self) -> &'static str {
    match self {
      Self::UnsolicitedObservation => "unsolicited-observation",
      Self::ExplicitPartialReload => "explicit-partial-reload",
      Self::PendingReconciliation => "pending-reconciliation",
      Self::Pull => "pull",
      Self::FullRebuild => "full-rebuild",
      Self::ConfigReplacement => "config-replacement",
      Self::Recovery => "recovery",
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum MaintenancePhase {
  PreparingArchive,
  AwaitingArchiveWaiver,
  ArchiveReady,
  RunningExternalMutation,
  FinalObservation,
  SelectingPartial,
  FullRebuildExclusive,
  Presenting,
  AwaitingScalarAuthorization,
  FinalizingArchive,
  BlockedInvalidAfterMutation,
  BlockedStoreHealth,
  AwaitingClient,
}

impl MaintenancePhase {
  pub fn label (&self) -> &'static str {
    match self {
      Self::PreparingArchive => "preparing-archive",
      Self::AwaitingArchiveWaiver => "awaiting-archive-waiver",
      Self::ArchiveReady => "archive-ready",
      Self::RunningExternalMutation => "running-external-mutation",
      Self::FinalObservation => "final-observation",
      Self::SelectingPartial => "selecting-partial",
      Self::FullRebuildExclusive => "full-rebuild-exclusive",
      Self::Presenting => "presenting",
      Self::AwaitingScalarAuthorization => "awaiting-scalar-authorization",
      Self::FinalizingArchive => "finalizing-archive",
      Self::BlockedInvalidAfterMutation => "blocked-invalid-after-mutation",
      Self::BlockedStoreHealth => "blocked-store-health",
      Self::AwaitingClient => "awaiting-client",
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum TerminalDisposition {
  Completed,
  MaintenanceAborted,
  FailedBeforeArchive,
  FailedAfterArchive,
  Dismissed,
}

impl TerminalDisposition {
  pub fn label (&self) -> &'static str {
    match self {
      Self::Completed => "completed",
      Self::MaintenanceAborted => "maintenance-aborted",
      Self::FailedBeforeArchive => "failed-before-archive",
      Self::FailedAfterArchive => "failed-after-archive",
      Self::Dismissed => "dismissed",
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum BufferKind {
  ContentView,
  NewEmptyContentView,
  SearchView,
  OverrideChoiceMenu,
  MetadataEditor,
  ForkConfirmation,
  ReloadSelector,
  RelationshipKindMenu,
  DiskConflict,
  IdStack,
  DerivedReport,
  DurableReport,
  RawSkgFile,
}

impl BufferKind {
  pub fn label (&self) -> &'static str {
    match self {
      Self::ContentView => "content-view",
      Self::NewEmptyContentView => "new-empty-content-view",
      Self::SearchView => "search-view",
      Self::OverrideChoiceMenu => "override-choice-menu",
      Self::MetadataEditor => "metadata-editor",
      Self::ForkConfirmation => "fork-confirmation",
      Self::ReloadSelector => "reload-selector",
      Self::RelationshipKindMenu => "relationship-kind-menu",
      Self::DiskConflict => "disk-conflict",
      Self::IdStack => "id-stack",
      Self::DerivedReport => "derived-report",
      Self::DurableReport => "durable-report",
      Self::RawSkgFile => "raw-skg-file",
    }
  }

  pub fn parse (value : &str) -> Result<Self, String> {
    match value {
      "content-view" => Ok (Self::ContentView),
      "new-empty-content-view" => Ok (Self::NewEmptyContentView),
      "search-view" => Ok (Self::SearchView),
      "override-choice-menu" => Ok (Self::OverrideChoiceMenu),
      "metadata-editor" => Ok (Self::MetadataEditor),
      "fork-confirmation" => Ok (Self::ForkConfirmation),
      "reload-selector" => Ok (Self::ReloadSelector),
      "relationship-kind-menu" => Ok (Self::RelationshipKindMenu),
      "disk-conflict" => Ok (Self::DiskConflict),
      "id-stack" => Ok (Self::IdStack),
      "derived-report" => Ok (Self::DerivedReport),
      "durable-report" => Ok (Self::DurableReport),
      "raw-skg-file" => Ok (Self::RawSkgFile),
      other => Err (format! ("unsupported buffer kind '{}'", other)),
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ViewDisposition {
  Interrupted,
  ReleasedUnimpacted,
  Refreshed,
  RetainedClean,
  ClosedDisposable,
  DetachedDerived,
  MaintenanceAborted,
  Failed (String),
}

impl ViewDisposition {
  pub fn label (&self) -> &str {
    match self {
      Self::Interrupted => "interrupted",
      Self::ReleasedUnimpacted => "released-unimpacted",
      Self::Refreshed => "refreshed",
      Self::RetainedClean => "retained-clean",
      Self::ClosedDisposable => "closed-disposable",
      Self::DetachedDerived => "detached-derived",
      Self::MaintenanceAborted => "maintenance-aborted",
      Self::Failed (_) => "failed",
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ViewSettlementRequirement {
  RetirementAck,
  ReleaseAck,
  ApplicationAck,
  CloseAck,
}

impl ViewSettlementRequirement {
  pub fn label (&self) -> &'static str {
    match self {
      Self::RetirementAck => "retirement-ack",
      Self::ReleaseAck => "release-ack",
      Self::ApplicationAck => "application-ack",
      Self::CloseAck => "close-ack",
    }
  }

  pub fn parse (value : &str) -> Result<Self, String> {
    match value {
      "retirement-ack" => Ok (Self::RetirementAck),
      "release-ack" => Ok (Self::ReleaseAck),
      "application-ack" => Ok (Self::ApplicationAck),
      "close-ack" => Ok (Self::CloseAck),
      other => Err (format! (
        "unsupported view settlement acknowledgement '{}'", other)),
    }
  }
}

/// Server-rendered text held as an uncommitted, replayable application offer.
/// Every resulting authority field must be echoed by the client before this
/// content can replace the retained forest.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ViewApplicationRecord {
  pub content                        : String,
  pub content_sha256                 : String,
  pub resulting_graph_generation     : u64,
  pub resulting_presentation_generation : u64,
  pub resulting_server_revision      : u64,
  pub resulting_application_token    : u64,
  pub warnings                       : Vec<String>,
}

/// The client proof that it installed one exact staged application offer.
/// Content itself is not echoed; its digest binds this acknowledgement to the
/// journaled bytes.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ViewApplicationAcknowledgement {
  pub content_sha256                     : String,
  pub resulting_graph_generation         : u64,
  pub resulting_presentation_generation  : u64,
  pub resulting_server_revision          : u64,
  pub resulting_application_token        : u64,
}

/// One durable promise for one buffer frozen in the maintenance census.
/// `acknowledged` is false until the editor proves the exact requested action;
/// an empty render queue is never a substitute for this inventory.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ViewSettlementRecord {
  pub buffer_id              : String,
  pub buffer_key             : Option<String>,
  pub kind                   : BufferKind,
  pub view_uri               : Option<String>,
  pub dirty                  : bool,
  pub impacted               : bool,
  pub parse_uncertain        : bool,
  pub uncertainty_reason     : Option<String>,
  pub observed_ids           : Vec<String>,
  pub resolved_primary_ids   : Vec<String>,
  #[serde(default)]
  pub base_graph_generation  : u64,
  #[serde(default)]
  pub base_presentation_generation : u64,
  pub base_server_revision   : u64,
  pub base_application_token : u64,
  pub planned_disposition    : ViewDisposition,
  pub requirement            : ViewSettlementRequirement,
  #[serde(default)]
  pub application            : Option<ViewApplicationRecord>,
  pub acknowledged           : bool,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum PendingReason {
  ValidDiskDifference,
  InvalidDisk,
  UnstableDisk,
  ObservationFailure,
}

impl PendingReason {
  pub fn label (&self) -> &'static str {
    match self {
      Self::ValidDiskDifference => "valid-disk-difference",
      Self::InvalidDisk => "invalid-disk",
      Self::UnstableDisk => "unstable-disk",
      Self::ObservationFailure => "observation-failure",
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum QueuedObservationReason {
  Startup,
  FilesystemEvent,
  ClientHint,
  WatcherGap,
  ExternalBatchEnded,
  SelectedGenerationAdvanced,
  SourceCatalogChanged,
  SaveFenceMismatch,
}

impl QueuedObservationReason {
  pub fn label (&self) -> &'static str {
    match self {
      Self::Startup => "startup",
      Self::FilesystemEvent => "filesystem-event",
      Self::ClientHint => "client-hint",
      Self::WatcherGap => "watcher-gap",
      Self::ExternalBatchEnded => "external-batch-ended",
      Self::SelectedGenerationAdvanced => "selected-generation-advanced",
      Self::SourceCatalogChanged => "source-catalog-changed",
      Self::SaveFenceMismatch => "save-fence-mismatch",
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ArchiveStatus {
  NotRequired,
  Preparing,
  UndoFailed { buffer_key : String, reason : String },
  UndoWaiverApproved { buffer_key : String, reason : String },
  Ready { manifest_sha256 : String },
  Finalized { manifest_sha256 : String },
  Incomplete { reason : String },
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CandidateSummary {
  pub id                   : CandidateId,
  pub base_graph_generation : GraphGeneration,
  pub base_manifest_revision : ManifestRevision,
  pub covered_sequence     : ObservationSequence,
  pub changed_primary_ids  : Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PendingDiskState {
  pub reason      : PendingReason,
  pub candidate   : Option<CandidateSummary>,
  pub details     : Vec<String>,
  pub offer_sent  : bool,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ServerEvidenceRecord {
  pub path             : PathBuf,
  pub bundle_sha256    : String,
  pub artifact_count   : u64,
  pub total_file_bytes : u64,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ClientEvidenceTransferRecord {
  pub server_bundle_sha256     : String,
  pub transfer_manifest_sha256 : String,
  pub artifact_bytes_sha256    : String,
  pub artifact_count           : u64,
  pub artifact_bytes           : u64,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SelectedStoreRecord {
  pub graph_generation   : GraphGeneration,
  pub manifest_revision  : ManifestRevision,
  pub tantivy_generation : u64,
  pub tantivy_outcome    : String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ScalarReleaseRecord {
  pub operation : String,
  pub pids      : Vec<String>,
  pub prompt    : String,
  pub approved  : bool,
}

/// The complete authority-bearing descriptor frozen when an incident begins.
/// Text remains off the bootstrap wire; its exact checksums bind the later
/// independently verified archive artifacts to this census.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FrozenBufferRecord {
  pub buffer_id               : String,
  pub kind                    : BufferKind,
  pub view_uri                : Option<String>,
  pub graph_generation        : u64,
  pub presentation_generation : u64,
  pub server_revision         : u64,
  pub application_token       : u64,
  pub dirty                   : bool,
  pub undo_required           : bool,
  pub last_fetched_sha256     : String,
  pub current_sha256          : String,
}

/// The exact partial-reload selector authorized when maintenance begins.
/// Paths retain the client's spelling for reporting; the server resolves and
/// validates them against its own source catalog before observing disk.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct MaintenanceTargets {
  pub paths : Vec<String>,
  pub ids   : Vec<String>,
}

/// Durable resolution of one user-supplied ID in an explicit partial reload.
/// A missing reason means the ID resolved against G0; the terminal wire calls
/// it acknowledged only after the entire incident completes successfully.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct MaintenanceIdOutcome {
  pub requested_id : String,
  pub pid          : Option<String>,
  pub reason       : Option<String>,
  pub paths        : Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ActiveMaintenance {
  pub incident_id       : IncidentId,
  pub epoch             : MaintenanceEpoch,
  pub origin            : MaintenanceOrigin,
  pub phase             : MaintenancePhase,
  pub candidate         : Option<CandidateSummary>,
  pub archive_status    : ArchiveStatus,
  #[serde(default)]
  pub started_at_utc    : String,
  #[serde(default)]
  pub archive_directory_name : String,
  #[serde(default)]
  pub archive_owner_session_id : String,
  #[serde(default)]
  pub archive_owner_client_kind : String,
  #[serde(default)]
  pub source_set        : String,
  #[serde(default = "initial_graph_generation")]
  pub g0_graph_generation : GraphGeneration,
  #[serde(default = "initial_manifest_revision")]
  pub g0_manifest_revision : ManifestRevision,
  #[serde(default)]
  pub registered_buffer_ids : Vec<String>,
  #[serde(default)]
  pub dirty_buffer_ids  : Vec<String>,
  #[serde(default)]
  pub undo_required_buffer_ids : Vec<String>,
  #[serde(default)]
  pub buffer_census     : BTreeMap<String, FrozenBufferRecord>,
  #[serde(default)]
  pub targets           : MaintenanceTargets,
  #[serde(default)]
  pub requested_id_outcomes : Vec<MaintenanceIdOutcome>,
  #[serde(default)]
  pub undo_waivers      : BTreeMap<String, String>,
  #[serde(default)]
  pub server_evidence   : Option<ServerEvidenceRecord>,
  #[serde(default)]
  pub client_evidence_transfer : Option<ClientEvidenceTransferRecord>,
  #[serde(default)]
  pub client_evidence_acknowledged : bool,
  #[serde(default)]
  pub selected_store    : Option<SelectedStoreRecord>,
  #[serde(default)]
  pub scalar_release    : Option<ScalarReleaseRecord>,
  #[serde(default)]
  pub view_settlements  : BTreeMap<String, ViewSettlementRecord>,
  #[serde(default)]
  pub blocking_reason   : Option<String>,
  #[serde(default)]
  pub suspended_phase   : Option<MaintenancePhase>,
  pub client_connected  : bool,
  pub terminal          : Option<TerminalDisposition>,
}

/// A fully resolved incident retained until the owning editor confirms that
/// it received and applied the terminal unlock instruction.  Keeping this in
/// the durable journal makes a lost terminal response exactly replayable.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TerminalMaintenance {
  pub incident_id       : IncidentId,
  pub epoch             : MaintenanceEpoch,
  pub disposition       : TerminalDisposition,
  pub archive_owner_session_id : String,
  pub archive_manifest_sha256 : Option<String>,
  pub registered_buffer_ids : Vec<String>,
  pub selected_store    : Option<SelectedStoreRecord>,
  #[serde(default)]
  pub requested_id_outcomes : Vec<MaintenanceIdOutcome>,
}

fn initial_graph_generation () -> GraphGeneration {
  GraphGeneration::INITIAL
}

fn initial_manifest_revision () -> ManifestRevision {
  ManifestRevision::INITIAL
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case", tag = "state", content = "details")]
pub enum CoordinatorState {
  Idle,
  Observing,
  Pending (PendingDiskState),
  Active (ActiveMaintenance),
  Terminal (TerminalMaintenance),
  BlockedStoreHealth { reason : String },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct StatePolicy {
  pub edits_allowed      : bool,
  pub skg_saves_allowed  : bool,
  pub queries_allowed    : bool,
  pub raw_saves_allowed  : bool,
  pub maintenance_locked : bool,
}

impl CoordinatorState {
  pub fn label (&self) -> &'static str {
    match self {
      Self::Idle => "idle",
      Self::Observing => "observing",
      Self::Pending (_) => "pending",
      Self::Active (_) => "active",
      Self::Terminal (_) => "terminal",
      Self::BlockedStoreHealth { .. } => "blocked-store-health",
    }
  }

  pub fn policy (&self) -> StatePolicy {
    match self {
      Self::Idle | Self::Observing
      | Self::Terminal (TerminalMaintenance {
          disposition: TerminalDisposition::Completed
            | TerminalDisposition::MaintenanceAborted
            | TerminalDisposition::FailedBeforeArchive
            | TerminalDisposition::Dismissed,
          ..
        }) => StatePolicy {
        edits_allowed: true,
        skg_saves_allowed: true,
        queries_allowed: true,
        raw_saves_allowed: true,
        maintenance_locked: false,
      },
      Self::Pending (_) => StatePolicy {
        edits_allowed: true,
        skg_saves_allowed: false,
        queries_allowed: true,
        raw_saves_allowed: true,
        maintenance_locked: false,
      },
      Self::Active (active) => match active . phase {
        MaintenancePhase::FullRebuildExclusive => StatePolicy {
          edits_allowed: false,
          skg_saves_allowed: false,
          queries_allowed: false,
          raw_saves_allowed: false,
          maintenance_locked: true,
        },
        MaintenancePhase::BlockedInvalidAfterMutation
        | MaintenancePhase::BlockedStoreHealth => StatePolicy {
          edits_allowed: false,
          skg_saves_allowed: false,
          queries_allowed: false,
          raw_saves_allowed: false,
          maintenance_locked: true,
        },
        _ => StatePolicy {
          edits_allowed: false,
          skg_saves_allowed: false,
          queries_allowed: true,
          raw_saves_allowed: false,
          maintenance_locked: true,
        },
      },
      Self::BlockedStoreHealth { .. } => StatePolicy {
        edits_allowed: false,
        skg_saves_allowed: false,
        queries_allowed: false,
        raw_saves_allowed: false,
        maintenance_locked: true,
      },
      Self::Terminal (_) => StatePolicy {
        edits_allowed: false,
        skg_saves_allowed: false,
        queries_allowed: false,
        raw_saves_allowed: false,
        maintenance_locked: true,
      },
    }
  }
}
