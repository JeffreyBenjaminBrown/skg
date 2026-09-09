//! Selection of one already-proved immutable disk candidate.

use super::candidate::{ObservedDiskCandidate, revalidate_candidate};
use super::evidence::PublishedMaintenanceEvidence;
use super::types::{
  ArchiveStatus,
  CoordinatorState,
  IncidentId,
  MaintenanceEpoch,
  MaintenancePhase,
  SelectedStoreRecord,
  ServerEvidenceRecord,
};
use crate::context::{
  ContextComputation, MapToContent, MapToContainers,
  compute_context_types,
  content_maps_from_nodes,
  context_origin_types_for_graph,
  had_id_set_from_nodes,
  link_dests_from_nodes,
};
use crate::dbs::init::create_empty_tantivy_index;
use crate::dbs::tantivy::background_writer::{
  TantivyGenerationStatus,
  latest_tantivy_generation,
  wait_for_tantivy_generation,
};
use crate::dbs::tantivy::write::reconstruct_index_from_nodes;
use crate::maintenance::candidate::{config_identity, source_catalog_blake3};
use crate::runtime::{MutationControl, SelectedRuntimeSnapshot, ServerRuntime};
use crate::runtime::interactive_session::InteractiveSession;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SourceSetName, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;
use crate::save::{StoreUpdateOutcome, apply_define_nodes_to_stores, nodecompletes_from_graph};
use crate::source_sets::ActiveSourceSet;
use crate::types::store_state::{SelectedStoreState, StoreHealth};
use crate::telescope::invariants::{
  TelescopeViolation,
  report_telescope_violations,
  validate_all_telescopes,
};

use futures::executor::block_on;
use std::collections::HashSet;
use std::sync::{Arc, MutexGuard};
use arc_swap::ArcSwap;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CandidateSelectionOutcome {
  pub graph_generation   : u64,
  pub manifest_revision  : u64,
  pub tantivy_generation : u64,
  pub tantivy_outcome    : String,
  pub evidence           : PublishedMaintenanceEvidence,
}

pub enum CandidateSelectionResult {
  Selected (CandidateSelectionOutcome),
  DeferredForViewEnrollment,
}

/// Close every durable precondition and synchronously select a pending
/// candidate.  Keeping this one operation synchronous to its archive ACK means
/// a terminal client response can never outrun the exact Tantivy generation.
pub fn select_archived_candidate (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
) -> Result<CandidateSelectionResult, String> {
  let active = matching_archive_ready (runtime, incident_id, epoch)?;
  let summary = active . candidate . as_ref ()
    . ok_or_else (|| "this maintenance origin has no candidate to select"
      . to_string ())?;
  let candidate = runtime . candidate (&summary . id)
    . ok_or_else (|| format! (
      "candidate {} is not retained in this process", summary . id))?;
  let snapshot = runtime . selected_snapshot ();
  if let Err (reason) = validate_preselection (
      runtime, &active, &snapshot . env . config, &snapshot . selected,
      &candidate) . and_then (|_| revalidate_candidate (
        &snapshot . env . config, &candidate))
  {
    reschedule_superseded_candidate (runtime, &active, reason . clone ())?;
    return Err (format! (
      "candidate was superseded before evidence publication: {}; incident-specific observation was queued",
      reason));
  }

  let evidence = runtime . maintenance_evidence . publish_candidate (
    &active, &snapshot . env . config, &snapshot . selected, &candidate)?;
  let evidence_record = ServerEvidenceRecord {
    path: evidence . path . clone (),
    bundle_sha256: evidence . bundle_sha256 . clone (),
    artifact_count: evidence . artifact_count as u64,
    total_file_bytes: evidence . total_file_bytes,
  };
  runtime . transition_maintenance (|coordinator|
    coordinator . record_server_evidence (
      incident_id, epoch, evidence_record . clone ()))?;
  runtime . transition_maintenance (|coordinator|
    coordinator . transition (
      incident_id, epoch, MaintenancePhase::SelectingPartial))?;

  match block_on (select_stores (
      runtime, incident_id, epoch, candidate . clone ()))
  {
    Ok (record) => {
      runtime . retain_incident_snapshot (incident_id, &record)?;
      runtime . transition_maintenance (|coordinator|
        coordinator . store_selected (incident_id, epoch, record . clone ()))?;
      Ok (CandidateSelectionResult::Selected (CandidateSelectionOutcome {
        graph_generation: record . graph_generation . get (),
        manifest_revision: record . manifest_revision . get (),
        tantivy_generation: record . tantivy_generation,
        tantivy_outcome: record . tantivy_outcome,
        evidence,
      }))
    }
    Err (SelectionFailure::EnrollmentPending) =>
      Ok (CandidateSelectionResult::DeferredForViewEnrollment),
    Err (SelectionFailure::Superseded (reason)) => {
      reschedule_superseded_candidate (runtime, &active, reason . clone ())?;
      Err (format! (
        "candidate was superseded before mutation: {}; incident-specific observation was queued",
        reason))
    }
    Err (SelectionFailure::Stores { reason }) => {
      runtime . transition_maintenance (|coordinator|
        coordinator . block_store_health (
          incident_id, epoch, reason . clone ()))?;
      Err (format! (
        "candidate selection failed; the last published snapshot remains readable: {}", reason))
    }
  }
}

/// Rebuild every derived store from one already-proved complete candidate.
/// Candidate parsing, folding, validation, archive publication, and evidence
/// all precede worker effect authorization. Existing readers retain their
/// selected graph and Searcher while the replacement index is prepared.
pub fn rebuild_archived_candidate (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
) -> Result<CandidateSelectionResult, String> {
  let active = matching_archive_ready (runtime, incident_id, epoch)?;
  if active . origin != super::types::MaintenanceOrigin::FullRebuild
  && !active . force_full_rebuild_recovery
  {
    return Err ("exclusive rebuild selection requires recovery authority"
      . into ()); }
  let summary = active . candidate . as_ref ()
    . ok_or_else (|| "full rebuild has no complete candidate" . to_string ())?;
  let candidate = runtime . candidate (&summary . id)
    . ok_or_else (|| format! (
      "candidate {} is not retained in this process", summary . id))?;
  if !matches! (candidate . disk_fence,
      super::candidate::CandidateDiskFence::Complete)
  {
    return Err ("full rebuild requires a complete-disk candidate" . into ()); }
  let snapshot = runtime . selected_snapshot ();
  if let Err (reason) = validate_rebuild_preselection (
      runtime, &active, &snapshot . env . config, &snapshot . selected,
      &candidate) . and_then (|_| revalidate_candidate (
        &candidate . config, &candidate))
  {
    reschedule_superseded_candidate (runtime, &active, reason . clone ())?;
    return Err (format! (
      "full rebuild candidate was superseded before evidence publication: {}; incident-specific observation was queued",
      reason));
  }

  let evidence = runtime . maintenance_evidence . publish_candidate (
    &active, &candidate . config, &snapshot . selected, &candidate)?;
  let evidence_record = ServerEvidenceRecord {
    path: evidence . path . clone (),
    bundle_sha256: evidence . bundle_sha256 . clone (),
    artifact_count: evidence . artifact_count as u64,
    total_file_bytes: evidence . total_file_bytes,
  };
  runtime . transition_maintenance (|coordinator|
    coordinator . record_server_evidence (
      incident_id, epoch, evidence_record . clone ()))?;
  runtime . transition_maintenance (|coordinator|
    coordinator . transition (
      incident_id, epoch, MaintenancePhase::FullRebuildExclusive))?;

  match block_on (rebuild_stores (
      runtime, incident_id, epoch, candidate . clone ()))
  {
    Ok (record) => {
      runtime . retain_incident_snapshot (incident_id, &record)?;
      runtime . transition_maintenance (|coordinator|
        coordinator . store_rebuilt (incident_id, epoch, record . clone ()))?;
      Ok (CandidateSelectionResult::Selected (CandidateSelectionOutcome {
        graph_generation: record . graph_generation . get (),
        manifest_revision: record . manifest_revision . get (),
        tantivy_generation: record . tantivy_generation,
        tantivy_outcome: record . tantivy_outcome,
        evidence,
      }))
    }
    Err (SelectionFailure::EnrollmentPending) =>
      Ok (CandidateSelectionResult::DeferredForViewEnrollment),
    Err (SelectionFailure::Superseded (reason)) => {
      reschedule_superseded_candidate (runtime, &active, reason . clone ())?;
      Err (format! (
        "full rebuild candidate was superseded before mutation: {}; incident-specific observation was queued",
        reason))
    }
    Err (SelectionFailure::Stores { reason }) => {
      runtime . transition_maintenance (|coordinator|
        coordinator . block_store_health (
          incident_id, epoch, reason . clone ()))?;
      Err (format! (
        "full rebuild failed; the last published snapshot remains readable: {}", reason))
    }
  }
}

#[derive(Debug)]
enum SelectionFailure {
  EnrollmentPending,
  Superseded (String),
  Stores { reason : String },
}

fn reschedule_superseded_candidate (
  runtime : &ServerRuntime,
  active  : &super::types::ActiveMaintenance,
  reason  : String,
) -> Result<(), String> {
  runtime . transition_maintenance (|coordinator|
    coordinator . selection_superseded (
      &active . incident_id, active . epoch, reason . clone ()))?;
  if active . force_full_rebuild_recovery {
    return runtime . schedule_maintenance_final_observation (
      active . incident_id . clone (), active . epoch); }
  match active . origin {
    super::types::MaintenanceOrigin::ExplicitPartialReload =>
      runtime . schedule_maintenance_target_observation (
        active . incident_id . clone (), active . epoch),
    super::types::MaintenanceOrigin::PendingReconciliation
    | super::types::MaintenanceOrigin::Pull
    | super::types::MaintenanceOrigin::FullRebuild =>
      runtime . schedule_maintenance_final_observation (
        active . incident_id . clone (), active . epoch),
    ref origin => Err (format! (
      "maintenance origin '{}' has no candidate reobservation adapter",
      origin . label ())),
  }
}

/// One worker reservation. Drop deliberately does nothing: interruption after
/// authorization must leave the owner blocking competing store mutations.
struct SelectionMutation {
  control : MutationControl,
  before : Arc<SelectedRuntimeSnapshot>,
  authorized : bool,
  recovered : bool,
}

impl SelectionMutation {
  fn reserve (
    runtime : &ServerRuntime,
    incident_id : &IncidentId,
    epoch : MaintenanceEpoch,
    candidate : &ObservedDiskCandidate,
  ) -> Result<Self, SelectionFailure> {
    let before : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
    let control : MutationControl = runtime . reserve_mutation (
      format! ("maintenance/{}/{}", incident_id, epoch . get ()),
      candidate . summary . base_graph_generation,
      before . selected . manifest_revision)
      . map_err (SelectionFailure::Superseded)?;
    Ok (Self { control, before, authorized: false, recovered: false }) }

  fn authorize (&mut self) -> Result<(), SelectionFailure> {
    // Even a lost acknowledgement may have authorized the owner. An error
    // therefore takes the blocked path, never the prepared cancellation path.
    self . authorized = true;
    self . control . authorize () . map_err (store_failure) }

  fn complete (
    self,
    result : Result<SelectedStoreRecord, SelectionFailure>,
  ) -> Result<SelectedStoreRecord, SelectionFailure> {
    if result . is_ok () || !self . authorized || self . recovered {
      if let Err (error) = self . control . finish () {
        let reason : String = format! (
          "selection reservation could not finish: {}", error);
        return Err (self . block_failure (reason)); }
      return result; }
    let reason : String = match result {
      Err (SelectionFailure::Stores { reason })
      | Err (SelectionFailure::Superseded (reason)) => reason,
      Err (SelectionFailure::EnrollmentPending) =>
        "view enrollment changed after selection authorization" . into (),
      Ok (_) => unreachable! (), };
    Err (self . block_failure (reason)) }

  fn block_failure (&self, reason : String) -> SelectionFailure {
    let full_reason : String = match self . control . block (&reason) {
      Ok (( )) => reason,
      Err (error) => format! ("{}; owner block failed: {}", reason, error), };
    store_failure (full_reason) }

  /// Only a proved restoration of all worker effects reaches this boundary.
  /// Source files were never written by candidate selection; the original
  /// graph, manifest and actual Searcher remain valid throughout restoration.
  fn recover_g0 (
    &mut self,
    env : &mut SkgEnv,
    reason : &str,
  ) -> Result<(), String> {
    self . control . block (reason)?;
    *env = detached_writer_env (&self . before);
    self . control . recover (self . before . clone ())?;
    self . recovered = true;
    Ok (( )) }
}

fn store_failure (reason : String) -> SelectionFailure {
  SelectionFailure::Stores { reason } }

fn detached_writer_env (snapshot : &SelectedRuntimeSnapshot) -> SkgEnv {
  let mut env : SkgEnv = snapshot . env . clone ();
  env . in_rust_graph = Arc::new (ArcSwap::from (snapshot . selected . clone ()));
  env }

async fn rebuild_stores (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
  candidate   : Arc<ObservedDiskCandidate>,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  let mut mutation : SelectionMutation =
    SelectionMutation::reserve (runtime, incident_id, epoch, &candidate)?;
  let result : Result<SelectedStoreRecord, SelectionFailure> =
    rebuild_reserved_stores (
      runtime, incident_id, epoch, &candidate, &mut mutation) . await;
  mutation . complete (result) }

async fn rebuild_reserved_stores (
  runtime : &ServerRuntime,
  incident_id : &IncidentId,
  epoch : MaintenanceEpoch,
  candidate : &ObservedDiskCandidate,
  mutation : &mut SelectionMutation,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  if runtime . transition_maintenance (|coordinator|
      coordinator . defer_selection_for_view_enrollment (incident_id, epoch))
      . map_err (store_failure)?
  { return Err (SelectionFailure::EnrollmentPending); }
  let _write_guard : tokio::sync::MutexGuard<'static, ()> =
    crate::write_lock::acquire_graph_write_lock () . await;
  let mut env : MutexGuard<'_, SkgEnv> =
    runtime . lock_writer_env () . map_err (store_failure)?;
  let old_selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  validate_locked_rebuild (
    runtime, incident_id, epoch, &env . config, &old_selected, candidate)
    . map_err (SelectionFailure::Superseded)?;
  let nodes : Vec<NodeComplete> = nodecompletes_from_graph (&candidate . graph);
  let context : ContextComputation = {
    let had_id_set : HashSet<ID> = had_id_set_from_nodes (&nodes);
    let all_node_ids : HashSet<ID> = nodes . iter ()
      . map (|node| node . pid . clone ()) . collect ();
    let link_dests : HashSet<ID> = link_dests_from_nodes (&nodes);
    let (map_to_content, map_to_containers) : (MapToContent, MapToContainers) =
      content_maps_from_nodes (&nodes);
    compute_context_types (
      &had_id_set, &all_node_ids, &link_dests,
      &map_to_content, &map_to_containers) };
  let mut warnings : Vec<(ID, TelescopeViolation)> =
    validate_all_telescopes (&candidate . config, &candidate . graph);
  warnings . extend (candidate . load_violations . clone ());
  warnings . sort_by (|left, right| left . 0 . cmp (&right . 0));
  let source_catalog_changed : bool = source_catalog_blake3 (&env . config)
    != candidate . source_catalog_blake3;
  let mut interactive : MutexGuard<'_, InteractiveSession> =
    runtime . interactive . lock () . map_err (|_|
      store_failure ("interactive session poisoned" . into ()))?;
  let old_source_set : ActiveSourceSet = interactive . active_source_set . clone ();
  let replacement_source_set : ActiveSourceSet = ActiveSourceSet::named (
      &candidate . config, old_source_set . name . clone ())
    . or_else (|_| ActiveSourceSet::named (
      &candidate . config, SourceSetName::from ("all")))
    . map_err (|error| store_failure (format! (
      "replacement source-set is invalid: {}", error)))?;
  if old_source_set . name != replacement_source_set . name {
    tracing::warn! (
      old = %old_source_set . name . 0,
      new = %replacement_source_set . name . 0,
      "replacement config removed the active source-set; using exact fallback"); }
  mutation . authorize ()?;
  let replacement_tantivy : TantivyIndex = if candidate . config . tantivy_folder
      == env . config . tantivy_folder
  { env . tantivy_index . clone () }
  else {
    match create_empty_tantivy_index (&candidate . config . tantivy_folder) {
      Ok (index) => index,
      Err (error) => {
        // Different path spellings may still refer to overlapping index
        // directories. Prove old writer recovery even after creation fails.
        return fail_selection_and_restore (
          mutation, &mut env,
          format! ("replacement Tantivy index could not be created: {}", error)); } } };
  if let Err (error) = reconstruct_index_from_nodes (
      &nodes, &replacement_tantivy, &context . labels)
  {
    return fail_selection_and_restore (
      mutation, &mut env,
      format! ("Tantivy full reconstruction failed: {}", error)); }
  if source_catalog_changed {
    if let Err (error) = runtime . replace_observation_config (&candidate . config) {
      // replace_config builds both replacement watchers before swapping either,
      // but the service itself might have failed. Prove old watch installation
      // before treating the failed candidate as safely recoverable.
      runtime . replace_observation_config (&env . config)
        . map_err (|restore| store_failure (format! (
          "replacement source watches failed ({}); old source watches could not be restored ({})",
          error, restore)))?;
      return fail_selection_and_restore (
        mutation, &mut env,
        format! ("replacement source watches failed: {}", error)); } }
  if let Err (error) = runtime . transition_maintenance (|coordinator|
      coordinator . replace_full_rebuild_source_set (
        incident_id, epoch, replacement_source_set . name . 0 . clone ()))
  {
    if source_catalog_changed {
      runtime . replace_observation_config (&env . config)
        . map_err (|restore| store_failure (format! (
          "replacement source-set journal failed ({}); source-watch restoration failed ({})",
          error, restore)))?; }
    return fail_selection_and_restore (
      mutation, &mut env,
      format! ("replacement source-set could not be journaled: {}", error)); }
  let selected : Arc<SelectedStoreState> = Arc::new (
    old_selected . with_acknowledged_rebuild (
      (*candidate . graph) . clone (), candidate . manifest . clone ())
      . with_cyclic_roots (context . cyclic_roots)
      . with_searcher (replacement_tantivy . reader . searcher ()));
  let mut replacement_env : SkgEnv = detached_writer_env (&mutation . before);
  replacement_env . startup_warnings = Arc::new (warnings . clone ());
  replacement_env . config = (*candidate . config) . clone ();
  replacement_env . tantivy_index = replacement_tantivy;
  replacement_env . searcher = selected . searcher . clone ()
    . expect ("full-rebuild publication captured its Searcher");
  replacement_env . in_rust_graph . store (selected . clone ());
  runtime . publish_selected_from_env (&mutation . control, &replacement_env)
    . map_err (store_failure)?;
  *env = replacement_env;
  interactive . active_source_set = replacement_source_set;
  drop (interactive);
  if let Err (error) = report_telescope_violations (
      &warnings, &env . config . data_root)
  { tracing::warn! (%error, "could not write the full-rebuild telescope report"); }
  let record : SelectedStoreRecord = SelectedStoreRecord {
    graph_generation: selected . graph_generation,
    manifest_revision: selected . manifest_revision,
    tantivy_generation: latest_tantivy_generation ()
      . map (|generation| generation . get ()) . unwrap_or (0),
    tantivy_outcome: "synchronous-full-rebuild" . into (), };
  drop (env);
  drop (_write_guard);
  if source_catalog_changed {
    if let Err (error) = runtime . observe_git_presentation () {
      tracing::warn! (%error,
        "could not observe Git presentation after source-catalog replacement"); } }
  Ok (record) }

/// Restore writable derived state before relinquishing a failed selection.
/// Failure preserves the owner's old graph/Searcher and blocks future writes;
/// it does not make the old read snapshot unqueryable.
fn fail_selection_and_restore (
  mutation : &mut SelectionMutation,
  env : &mut SkgEnv,
  reason : String,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  let old_nodes : Vec<NodeComplete> =
    nodecompletes_from_graph (&mutation . before . selected . graph);
  if let Err (restore) = restore_g0 (
      &mutation . before . env . tantivy_index,
      &mutation . before . selected, &old_nodes)
  {
    return Err (store_failure (format! (
      "{}; G0 index restoration failed: {}; write authority remains blocked",
      reason, restore))); }
  mutation . recover_g0 (env, &reason) . map_err (|recovery|
    store_failure (format! ("{}; owner recovery failed: {}", reason, recovery)))?;
  Err (store_failure (format! ("{}; G0 index was restored", reason))) }

async fn select_stores (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
  candidate   : Arc<ObservedDiskCandidate>,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  if candidate . definitions . is_empty ()
     && graph_nodes (&candidate . graph) == graph_nodes (&candidate . base_graph)
  { return select_manifest_only (runtime, incident_id, epoch, candidate) . await; }
  let mut mutation : SelectionMutation =
    SelectionMutation::reserve (runtime, incident_id, epoch, &candidate)?;
  let result : Result<SelectedStoreRecord, SelectionFailure> =
    select_reserved_stores (
      runtime, incident_id, epoch, &candidate, &mut mutation) . await;
  mutation . complete (result) }

async fn select_reserved_stores (
  runtime : &ServerRuntime,
  incident_id : &IncidentId,
  epoch : MaintenanceEpoch,
  candidate : &ObservedDiskCandidate,
  mutation : &mut SelectionMutation,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  if runtime . transition_maintenance (|coordinator|
      coordinator . defer_selection_for_view_enrollment (incident_id, epoch))
      . map_err (store_failure)?
  { return Err (SelectionFailure::EnrollmentPending); }
  let _write_guard : tokio::sync::MutexGuard<'static, ()> =
    crate::write_lock::acquire_graph_write_lock () . await;
  let mut env : MutexGuard<'_, SkgEnv> =
    runtime . lock_writer_env () . map_err (store_failure)?;
  let old_selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  validate_locked_preselection (
    runtime, incident_id, epoch, &env . config, &old_selected, candidate)
    . map_err (SelectionFailure::Superseded)?;
  mutation . authorize ()?;
  let outcome : StoreUpdateOutcome = match apply_define_nodes_to_stores (
      candidate . definitions . clone (), &[], env . config . clone (),
      &env . tantivy_index, &env . in_rust_graph,
      false, Some (candidate . manifest . clone ()), &HashSet::new ()) . await
  {
    Ok (outcome) => outcome,
    Err (error) => return fail_selection_and_restore (
      mutation, &mut env, format! ("derived-store transition failed: {}", error)), };
  let terminal : TantivyGenerationStatus =
    wait_for_tantivy_generation (outcome . tantivy_generation);
  let tantivy_outcome : String = match terminal {
    TantivyGenerationStatus::Committed => "committed" . to_string (),
    TantivyGenerationStatus::Reconstructed (reason) =>
      format! ("reconstructed-after-incremental-failure: {}", reason),
    TantivyGenerationStatus::Failed (reason) => return fail_selection_and_restore (
      mutation, &mut env, format! (
        "Tantivy candidate generation {} failed: {}",
        outcome . tantivy_generation . get (), reason)),
    TantivyGenerationStatus::Pending => unreachable! (), };
  let selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  if selected . graph_generation != outcome . graph_generation
  || selected . manifest != candidate . manifest
  || graph_nodes (&selected . graph) != graph_nodes (&candidate . graph)
  || !healthy (&selected . tantivy_health)
  {
    return fail_selection_and_restore (mutation, &mut env,
      "store transition completed without the exact candidate publication" . into ()); }
  let record : SelectedStoreRecord = SelectedStoreRecord {
    graph_generation: selected . graph_generation,
    manifest_revision: selected . manifest_revision,
    tantivy_generation: outcome . tantivy_generation . get (),
    tantivy_outcome, };
  env . searcher = selected . searcher . clone ()
    . ok_or_else (|| store_failure (
      "candidate publication has no captured Searcher" . into ()))?;
  runtime . publish_selected_from_env (&mutation . control, &env)
    . map_err (store_failure)?;
  Ok (record) }

/// Select exact byte authority without inventing a graph or Tantivy
/// generation when the authorized disk observation folds to G0 exactly.
async fn select_manifest_only (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
  candidate   : Arc<ObservedDiskCandidate>,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  let mut mutation : SelectionMutation =
    SelectionMutation::reserve (runtime, incident_id, epoch, &candidate)?;
  let result : Result<SelectedStoreRecord, SelectionFailure> =
    select_reserved_manifest (runtime, incident_id, epoch, &candidate, &mut mutation)
      . await;
  mutation . complete (result) }

async fn select_reserved_manifest (
  runtime : &ServerRuntime,
  incident_id : &IncidentId,
  epoch : MaintenanceEpoch,
  candidate : &ObservedDiskCandidate,
  mutation : &mut SelectionMutation,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  if runtime . transition_maintenance (|coordinator|
      coordinator . defer_selection_for_view_enrollment (incident_id, epoch))
      . map_err (store_failure)?
  { return Err (SelectionFailure::EnrollmentPending); }
  let _write_guard : tokio::sync::MutexGuard<'static, ()> =
    crate::write_lock::acquire_graph_write_lock () . await;
  let env : MutexGuard<'_, SkgEnv> =
    runtime . lock_writer_env () . map_err (store_failure)?;
  let old_selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  validate_locked_preselection (
    runtime, incident_id, epoch, &env . config, &old_selected, candidate)
    . map_err (SelectionFailure::Superseded)?;
  mutation . authorize ()?;
  let selected : Arc<SelectedStoreState> = if old_selected . manifest == candidate . manifest {
    old_selected . clone ()
  } else {
    let selected : Arc<SelectedStoreState> = Arc::new (
      old_selected . with_semantically_equal_manifest (candidate . manifest . clone ()));
    env . in_rust_graph . store (selected . clone ());
    selected };
  runtime . publish_selected_from_env (&mutation . control, &env)
    . map_err (store_failure)?;
  Ok (SelectedStoreRecord {
    graph_generation: selected . graph_generation,
    manifest_revision: selected . manifest_revision,
    tantivy_generation: latest_tantivy_generation ()
      . map (|generation| generation . get ()) . unwrap_or (0),
    tantivy_outcome: "not-required-semantic-no-op" . into (), }) }

fn matching_archive_ready (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
) -> Result<super::types::ActiveMaintenance, String> {
  let coordinator = runtime . maintenance_snapshot ();
  let CoordinatorState::Active (active) = &coordinator . state else {
    return Err ("no maintenance incident is active" . into ()); };
  if &active . incident_id != incident_id || active . epoch != epoch {
    return Err ("maintenance selection envelope is stale" . into ()); }
  if active . phase != MaintenancePhase::ArchiveReady
  || !matches! (active . archive_status, ArchiveStatus::Ready { .. })
  {
    return Err (format! (
      "maintenance archive is not ready for selection ({:?})", active . phase)); }
  Ok (active . clone ())
}

fn validate_preselection (
  runtime   : &ServerRuntime,
  active    : &super::types::ActiveMaintenance,
  config    : &crate::types::misc::SkgConfig,
  selected  : &crate::types::store_state::SelectedStoreState,
  candidate : &ObservedDiskCandidate,
) -> Result<(), String> {
  if active . candidate . as_ref () != Some (&candidate . summary) {
    return Err ("active incident candidate identity changed" . into ()); }
  if active . g0_graph_generation != selected . graph_generation
  || active . g0_manifest_revision != selected . manifest_revision
  {
    return Err ("active incident G0 was superseded" . into ()); }
  if config_identity (config) != candidate . config_identity
  || source_catalog_blake3 (config) != candidate . source_catalog_blake3
  {
    return Err ("candidate configuration/source identity changed" . into ()); }
  let sequence = runtime . maintenance_snapshot ()
    . observation_sequence;
  if sequence != candidate . summary . covered_sequence {
    return Err (format! (
      "candidate covered observation {}, but current observation is {}",
      candidate . summary . covered_sequence . get (), sequence . get ())); }
  Ok (( ))
}

fn validate_rebuild_preselection (
  runtime   : &ServerRuntime,
  active    : &super::types::ActiveMaintenance,
  old_config : &crate::types::misc::SkgConfig,
  selected  : &crate::types::store_state::SelectedStoreState,
  candidate : &ObservedDiskCandidate,
) -> Result<(), String> {
  if active . candidate . as_ref () != Some (&candidate . summary) {
    return Err ("active full-rebuild candidate identity changed" . into ()); }
  if active . g0_graph_generation != selected . graph_generation
  || active . g0_manifest_revision != selected . manifest_revision
  {
    return Err ("active full-rebuild G0 was superseded" . into ()); }
  if config_identity (old_config) != candidate . config_identity
  || config_identity (&candidate . config) != candidate . config_identity
  {
    return Err (
      "full rebuild changed the identity of its governing configuration"
        . into ()); }
  if source_catalog_blake3 (&candidate . config)
      != candidate . source_catalog_blake3
  {
    return Err ("full-rebuild candidate source identity is inconsistent"
      . into ()); }
  let sequence = runtime . maintenance_snapshot ()
    . observation_sequence;
  if sequence != candidate . summary . covered_sequence {
    return Err (format! (
      "full-rebuild candidate covered observation {}, but current observation is {}",
      candidate . summary . covered_sequence . get (), sequence . get ())); }
  Ok (( ))
}

fn validate_locked_preselection (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
  config      : &crate::types::misc::SkgConfig,
  selected    : &crate::types::store_state::SelectedStoreState,
  candidate   : &ObservedDiskCandidate,
) -> Result<(), String> {
  let active = {
    let coordinator = runtime . maintenance_snapshot ();
    let CoordinatorState::Active (active) = &coordinator . state else {
      return Err ("maintenance ended before candidate selection" . into ()); };
    if &active . incident_id != incident_id || active . epoch != epoch
    || active . phase != MaintenancePhase::SelectingPartial
    {
      return Err ("maintenance authority changed before candidate selection"
        . into ()); }
    active . clone ()
  };
  validate_preselection (runtime, &active, config, selected, candidate)?;
  revalidate_candidate (config, candidate)
}

fn validate_locked_rebuild (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
  config      : &crate::types::misc::SkgConfig,
  selected    : &crate::types::store_state::SelectedStoreState,
  candidate   : &ObservedDiskCandidate,
) -> Result<(), String> {
  let active = {
    let coordinator = runtime . maintenance_snapshot ();
    let CoordinatorState::Active (active) = &coordinator . state else {
      return Err ("maintenance ended before full rebuild selection" . into ()); };
    if &active . incident_id != incident_id || active . epoch != epoch
    || (active . origin != super::types::MaintenanceOrigin::FullRebuild
        && !active . force_full_rebuild_recovery)
    || active . phase != MaintenancePhase::FullRebuildExclusive
    {
      return Err ("maintenance authority changed before full rebuild selection"
        . into ()); }
    active . clone ()
  };
  validate_rebuild_preselection (
    runtime, &active, config, selected, candidate)?;
  revalidate_candidate (&candidate . config, candidate)
}

fn restore_g0 (
  tantivy_index : &crate::types::misc::TantivyIndex,
  old_selected  : &crate::types::store_state::SelectedStoreState,
  old_nodes     : &[crate::types::nodes::complete::NodeComplete],
) -> Result<(), String> {
  let labels = context_origin_types_for_graph (
    &old_selected . graph, &old_selected . cyclic_roots);
  reconstruct_index_from_nodes (old_nodes, tantivy_index, &labels)
    . map_err (|error| format! ("Tantivy G0 reconstruction failed: {}", error))?;
  Ok (( ))
}

fn graph_nodes (
  graph : &crate::dbs::in_rust_graph::InRustGraph,
) -> Vec<crate::types::nodes::complete::NodeComplete> {
  let mut nodes = nodecompletes_from_graph (graph);
  nodes . sort_by (|left, right| left . pid . cmp (&right . pid));
  nodes
}

fn healthy (health : &StoreHealth) -> bool {
  matches! (health, StoreHealth::Healthy)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::consts::TANTIVY_WRITER_BUFFER_BYTES;
  use crate::dbs::in_rust_graph::InRustGraph;
  use crate::dbs::init::empty_in_ram_tantivy_index;
  use crate::types::misc::{SkgConfig, SourceName};
  use crate::types::nodes::complete::empty_node_complete;
  use std::collections::HashMap;
  use tantivy::{DocAddress, IndexWriter, TantivyDocument};
  use tantivy::schema::document::Value;
  use tempfile::{TempDir, tempdir};

  fn node (title : &str) -> NodeComplete {
    let mut node : NodeComplete = empty_node_complete ();
    node . pid = ID::new ("node");
    node . source = SourceName::from ("public");
    node . title = title . into ();
    node }

  fn fixture_runtime () -> (TempDir, ServerRuntime) {
    let directory : TempDir = tempdir () . unwrap ();
    let mut config : SkgConfig = SkgConfig::dummyFromSources (HashMap::new ());
    config . config_path = directory . path () . join ("config.toml");
    config . data_root = directory . path () . to_path_buf ();
    config . maintenance_archive_identity = directory . path () . join ("archive");
    let index : TantivyIndex = empty_in_ram_tantivy_index () . unwrap ();
    let nodes : Vec<NodeComplete> = vec![node ("old selected title")];
    reconstruct_index_from_nodes (&nodes, &index, &HashMap::new ()) . unwrap ();
    let selected : Arc<SelectedStoreState> = Arc::new (SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (&nodes), Default::default ())
      . with_searcher (index . reader . searcher ()));
    let runtime : ServerRuntime = ServerRuntime::new (SkgEnv {
      config,
      in_rust_graph: Arc::new (ArcSwap::from (selected)),
      searcher: index . reader . searcher (),
      tantivy_index: index,
      startup_warnings: Arc::new (Vec::new ()), }) . unwrap ();
    (directory, runtime) }

  fn fixture_mutation (runtime : &ServerRuntime) -> SelectionMutation {
    let before : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
    let control : MutationControl = runtime . reserve_mutation (
      // These fixtures exercise index restoration and reservation cleanup.
      // Incident admission itself has separate owner state-machine coverage.
      "selection-restoration-fixture", before . selected . graph_generation,
      before . selected . manifest_revision) . unwrap ();
    SelectionMutation { control, before, authorized: false, recovered: false } }

  fn selected_title (runtime : &ServerRuntime) -> String {
    let snapshot : Arc<SelectedRuntimeSnapshot> =
      runtime . query_lease () . unwrap () . snapshot;
    let document : TantivyDocument = snapshot . env . searcher
      . doc (DocAddress::new (0, 0)) . unwrap ();
    document . get_first (snapshot . env . tantivy_index . raw_title_field)
      . and_then (|value| value . as_str ()) . unwrap () . to_string () }

  #[test]
  fn deferred_worker_releases_prepared_reservation () {
    let (_directory, runtime) : (TempDir, ServerRuntime) = fixture_runtime ();
    let mutation : SelectionMutation = fixture_mutation (&runtime);
    assert! (matches! (
      mutation . complete (Err (SelectionFailure::EnrollmentPending)),
      Err (SelectionFailure::EnrollmentPending)));
    let next : SelectionMutation = fixture_mutation (&runtime);
    next . control . finish () . unwrap ();
    assert_eq! (selected_title (&runtime), "old selected title"); }

  #[test]
  fn failed_index_restoration_blocks_writes_but_preserves_selected_queries () {
    let (_directory, runtime) : (TempDir, ServerRuntime) = fixture_runtime ();
    let mut mutation : SelectionMutation = fixture_mutation (&runtime);
    let before : Arc<SelectedRuntimeSnapshot> = mutation . before . clone ();
    mutation . authorize () . unwrap ();
    reconstruct_index_from_nodes (
      &[node ("unpublished replacement")], &before . env . tantivy_index,
      &HashMap::new ()) . unwrap ();
    // A held index writer makes restoration fail at its real write boundary.
    // The selected Searcher still reads the old committed document.
    let _writer : IndexWriter = before . env . tantivy_index . index
      . writer (TANTIVY_WRITER_BUFFER_BYTES) . unwrap ();
    let result : Result<SelectedStoreRecord, SelectionFailure> = {
      let mut env : MutexGuard<'_, SkgEnv> = runtime . lock_writer_env () . unwrap ();
      fail_selection_and_restore (&mut mutation, &mut env,
        "candidate index preparation failed" . into ()) };
    assert! (mutation . complete (result) . is_err ());
    assert! (runtime . authority_failure () . unwrap ()
      . contains ("write authority remains blocked"));
    assert! (runtime . reserve_mutation ("competing",
      before . selected . graph_generation, before . selected . manifest_revision)
      . is_err ());
    assert! (Arc::ptr_eq (&runtime . selected_snapshot () . selected, &before . selected));
    assert_eq! (selected_title (&runtime), "old selected title"); }

  #[test]
  fn proved_index_restoration_recovers_then_releases_owner_reservation () {
    let (_directory, runtime) : (TempDir, ServerRuntime) = fixture_runtime ();
    let mut mutation : SelectionMutation = fixture_mutation (&runtime);
    let before : Arc<SelectedRuntimeSnapshot> = mutation . before . clone ();
    mutation . authorize () . unwrap ();
    reconstruct_index_from_nodes (
      &[node ("unpublished replacement")], &before . env . tantivy_index,
      &HashMap::new ()) . unwrap ();
    let result : Result<SelectedStoreRecord, SelectionFailure> = {
      let mut env : MutexGuard<'_, SkgEnv> = runtime . lock_writer_env () . unwrap ();
      fail_selection_and_restore (&mut mutation, &mut env,
        "candidate was refused after indexing" . into ()) };
    assert! (mutation . complete (result) . is_err ());
    assert! (runtime . authority_failure () . is_none ());
    let next : SelectionMutation = fixture_mutation (&runtime);
    next . control . finish () . unwrap ();
    assert! (Arc::ptr_eq (&runtime . selected_snapshot () . selected, &before . selected));
    assert_eq! (selected_title (&runtime), "old selected title"); }
}
