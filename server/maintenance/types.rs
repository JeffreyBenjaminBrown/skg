use crate::types::store_state::{GraphGeneration, ManifestRevision};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
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
pub enum ExternalMutationOutcome {
  Completed,
  Failed,
  Indeterminate,
}

impl ExternalMutationOutcome {
  pub fn label (&self) -> &'static str {
    match self {
      Self::Completed => "completed",
      Self::Failed => "failed",
      Self::Indeterminate => "indeterminate",
    }
  }

  pub fn parse (value : &str) -> Result<Self, String> {
    match value {
      "completed" => Ok (Self::Completed),
      "failed" => Ok (Self::Failed),
      "indeterminate" => Ok (Self::Indeterminate),
      other => Err (format! (
        "unsupported external mutation outcome '{}'", other)),
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ExternalMutationRecord {
  pub outcome : ExternalMutationOutcome,
  pub details : Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum MaintenancePhase {
  AwaitingLockedCensus,
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
      Self::AwaitingLockedCensus => "awaiting-locked-census",
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

/// How one frozen buffer's presentation obligation was discharged.  A
/// replacement editor which does not contain the buffer closes that
/// obligation through its complete census; it does not counterfeit the ACK
/// for an application which was never installed in that editor.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ViewSettlementResolution {
  #[default]
  Pending,
  ClientAcknowledged,
  CensusApplied,
  CensusAbsent,
}

impl ViewSettlementResolution {
  pub fn label (&self) -> &'static str {
    match self {
      Self::Pending => "pending",
      Self::ClientAcknowledged => "client-acknowledged",
      Self::CensusApplied => "census-applied",
      Self::CensusAbsent => "census-absent",
    }
  }
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
/// `acknowledged` is false until the editor proves the exact requested action
/// or a later complete census proves that the buffer is absent.  `resolution`
/// distinguishes those outcomes; an empty render queue is never a substitute
/// for this inventory.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ViewSettlementRecord {
  pub buffer_id              : String,
  pub buffer_key             : Option<String>,
  pub kind                   : BufferKind,
  pub view_uri               : Option<String>,
  #[serde(default)]
  pub origin_buffer_id       : Option<String>,
  #[serde(default)]
  pub origin_view_uri        : Option<String>,
  #[serde(default)]
  pub origin_application_token : Option<u64>,
  #[serde(default)]
  pub origin_location        : Option<String>,
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
  #[serde(default)]
  pub resolution             : ViewSettlementResolution,
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
  MaintenanceCompleted,
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
      Self::MaintenanceCompleted => "maintenance-completed",
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
  #[serde(default)]
  pub lifecycle               : String,
  #[serde(default)]
  pub disposable              : bool,
  #[serde(default)]
  pub continuation_id         : Option<String>,
  #[serde(default)]
  pub origin_buffer_id        : Option<String>,
  #[serde(default)]
  pub origin_view_uri         : Option<String>,
  #[serde(default)]
  pub origin_application_token : Option<u64>,
  #[serde(default)]
  pub origin_location         : Option<String>,
  pub view_uri                : Option<String>,
  #[serde(default)]
  pub recipe                  : String,
  #[serde(default)]
  pub root_ids                : Vec<String>,
  #[serde(default)]
  pub source_set              : String,
  pub graph_generation        : u64,
  pub presentation_generation : u64,
  pub server_revision         : u64,
  pub application_token       : u64,
  pub dirty                   : bool,
  #[serde(default)]
  pub logical_dirty           : bool,
  pub undo_required           : bool,
  #[serde(default)]
  pub maintenance_epoch       : Option<u64>,
  #[serde(default)]
  pub presentation_stale      : bool,
  #[serde(default)]
  pub search_stale            : bool,
  #[serde(default)]
  pub herald_bearing          : bool,
  pub last_fetched_sha256     : String,
  pub current_sha256          : String,
}

/// A retained view which the server has successfully produced during an
/// active incident, but whose concrete editor buffer does not exist yet.
/// The following incident-qualified client census binds this server-known
/// identity to the client's stable buffer ID.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PendingViewEnrollment {
  pub view_uri                : String,
  pub graph_generation        : u64,
  pub presentation_generation : u64,
  pub server_revision         : u64,
  pub application_token       : u64,
}

/// The exact origin-specific target set authorized when maintenance begins.
/// Paths retain the client's spelling for reporting; pull repository keys
/// bind client-local worktrees to server-verified groups of stable source
/// names without putting either side's absolute roots on the wire.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct MaintenanceTargets {
  pub paths : Vec<String>,
  pub ids   : Vec<String>,
  #[serde(default)]
  pub pull_repositories : BTreeMap<String, Vec<String>>,
}

pub fn pull_repository_key (sources : &[String]) -> String {
  let mut digest = Sha256::new ();
  digest . update (sources . join ("\0") . as_bytes ());
  format! ("{:x}", digest . finalize ())
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

/// The exact Git-presentation boundary paired with the disk observation which
/// produced G1.  Changes observed after this boundary are ordinary retained
/// presentation work, not a reason to reopen the semantic incident.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct MaintenancePresentationFence {
  pub candidate_observation_sequence : ObservationSequence,
  pub signature_blake3                : String,
  pub presentation_generation         : u64,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
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
  /// Immutable checksum of the initial archive.  `archive_status` later
  /// changes to the final-manifest checksum, so restart recovery must retain
  /// this separately in order to reverify the original archive.
  #[serde(default)]
  pub initial_archive_manifest_sha256 : Option<String>,
  /// The session currently authorized to drive the incident.  This starts as
  /// the archive producer and may move to a replacement editor only after the
  /// initial archive is durable.  Empty old journals fall back to the archive
  /// producer.
  #[serde(default)]
  pub controller_session_id : String,
  /// Report recovery never renews live authority issued by an earlier process.
  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub server_session_id : Option<String>,
  /// The original archive remains addressable after configuration replacement.
  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub archive_root_identity : Option<PathBuf>,
  /// A restarted process retires outstanding live offers, retaining their
  /// bytes as evidence without allowing them to affect its fresh views.
  #[serde(default, skip_serializing_if = "Option::is_none")]
  pub authority_retired_by_session : Option<String>,
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
  pub census_frozen     : bool,
  /// Version 1 could grant late results incident authority. Keep those exact
  /// already-existing obligations separately; new incidents never create them.
  #[serde(default)]
  pub legacy_census_obligations : Option<LegacyCensusObligations>,
  /// Retained for decoding known version 1 records and transitional readers.
  /// New incidents do not grow this inventory.
  #[serde(default)]
  pub presentation_buffer_census : BTreeMap<String, FrozenBufferRecord>,
  /// Version 1 server-known results whose buffer identity remains unresolved.
  #[serde(default)]
  pub pending_view_enrollments : BTreeMap<String, PendingViewEnrollment>,
  #[serde(default)]
  pub targets           : MaintenanceTargets,
  #[serde(default)]
  pub requested_id_outcomes : Vec<MaintenanceIdOutcome>,
  #[serde(default)]
  pub external_mutation : Option<ExternalMutationRecord>,
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
  pub presentation_fence : Option<MaintenancePresentationFence>,
  #[serde(default)]
  pub successor_observation_required : bool,
  #[serde(default)]
  pub force_full_rebuild_recovery : bool,
  #[serde(default)]
  pub scalar_release    : Option<ScalarReleaseRecord>,
  #[serde(default)]
  pub preselection_retirements : BTreeMap<String, ViewSettlementRecord>,
  #[serde(default)]
  pub view_settlements  : BTreeMap<String, ViewSettlementRecord>,
  #[serde(default)]
  pub blocking_reason   : Option<String>,
  #[serde(default)]
  pub suspended_phase   : Option<MaintenancePhase>,
  pub client_connected  : bool,
  pub terminal          : Option<TerminalDisposition>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct LegacyCensusObligations {
  pub presentation_census : BTreeMap<String, FrozenBufferRecord>,
  pub pending_view_enrollments : BTreeMap<String, PendingViewEnrollment>,
}

/// Graph publication ends the global admission barrier. These records retain
/// the incident's evidence and exact buffer obligations without owning another
/// graph transition. A terminal ACK is retained rather than deleting identity.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case", tag = "status", content = "details")]
pub enum CommittedIncident {
  Settling (ActiveMaintenance),
  Terminal {
    record : TerminalMaintenance,
    acknowledged : bool,
    /// Modern records retain the complete incident through terminal ACK.
    /// Known version 1 terminal records retain their existing archive proof.
    #[serde(default)]
    incident : Option<ActiveMaintenance>, },
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct IncidentSummary {
  pub incident_id : IncidentId,
  pub epoch : MaintenanceEpoch,
  pub phase : Option<MaintenancePhase>,
  pub selected_store : Option<SelectedStoreRecord>,
  pub disposition : Option<TerminalDisposition>,
  pub terminal_acknowledged : bool,
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
  #[serde(default)]
  pub controller_session_id : String,
  #[serde(default)]
  pub archive_directory_name : String,
  pub archive_manifest_sha256 : Option<String>,
  pub registered_buffer_ids : Vec<String>,
  pub selected_store    : Option<SelectedStoreRecord>,
  #[serde(default)]
  pub requested_id_outcomes : Vec<MaintenanceIdOutcome>,
}

impl ActiveMaintenance {
  pub fn controlling_session_id (&self) -> &str {
    if self . controller_session_id . is_empty () {
      &self . archive_owner_session_id
    } else { &self . controller_session_id }
  }

  /// Modern incidents settle exactly their frozen census. Legacy late results
  /// remain named obligations, never a license to enroll another result.
  pub fn presentation_census (&self) -> &BTreeMap<String, FrozenBufferRecord> {
    self . legacy_census_obligations . as_ref ()
      . map (|legacy| &legacy . presentation_census)
      . unwrap_or (&self . buffer_census)
  }

  pub fn presentation_buffer_ids (&self) -> Vec<String> {
    self . presentation_census () . keys () . cloned () . collect ()
  }

  pub fn presentation_buffer (
    &self,
    buffer_id : &str,
  ) -> Option<&FrozenBufferRecord> {
    self . presentation_census () . get (buffer_id)
  }
}

impl TerminalMaintenance {
  pub fn controlling_session_id (&self) -> &str {
    if self . controller_session_id . is_empty () {
      &self . archive_owner_session_id
    } else { &self . controller_session_id }
  }
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
        MaintenancePhase::BlockedInvalidAfterMutation => StatePolicy {
          edits_allowed: false,
          skg_saves_allowed: false,
          queries_allowed: true,
          raw_saves_allowed: false,
          maintenance_locked: true,
        },
        MaintenancePhase::BlockedStoreHealth => StatePolicy {
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
