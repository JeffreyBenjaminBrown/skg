//! Selection of one already-proved immutable disk candidate.

use super::candidate::{ObservedDiskCandidate, revalidate_candidate};
use super::evidence::PublishedMaintenanceEvidence;
use super::types::{
  ArchiveStatus,
  CoordinatorState,
  IncidentId,
  MaintenanceEpoch,
  MaintenancePhase,
  QueuedObservationReason,
  SelectedStoreRecord,
  ServerEvidenceRecord,
};
use crate::context::context_origin_types_for_graph;
use crate::dbs::init::wipe_then_init_typedb_db;
use crate::dbs::tantivy::background_writer::{
  TantivyGenerationStatus,
  wait_for_tantivy_generation,
};
use crate::dbs::tantivy::write::reconstruct_index_from_nodes;
use crate::maintenance::candidate::{config_identity, source_catalog_blake3};
use crate::runtime::ServerRuntime;
use crate::save::{apply_define_nodes_to_stores, nodecompletes_from_graph};
use crate::types::store_state::StoreHealth;

use futures::executor::block_on;
use std::collections::HashSet;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CandidateSelectionOutcome {
  pub graph_generation   : u64,
  pub manifest_revision  : u64,
  pub tantivy_generation : u64,
  pub tantivy_outcome    : String,
  pub evidence           : PublishedMaintenanceEvidence,
}

/// Close every durable precondition and synchronously select a pending
/// candidate.  Keeping this one operation synchronous to its archive ACK means
/// a terminal client response can never outrun the exact Tantivy generation.
pub fn select_archived_candidate (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
) -> Result<CandidateSelectionOutcome, String> {
  let active = matching_archive_ready (runtime, incident_id, epoch)?;
  let summary = active . candidate . as_ref ()
    . ok_or_else (|| "this maintenance origin has no candidate to select"
      . to_string ())?;
  let candidate = runtime . candidate (&summary . id)
    . ok_or_else (|| format! (
      "candidate {} is not retained in this process", summary . id))?;
  let snapshot = runtime . selected_snapshot ();
  validate_preselection (
    runtime, &active, &snapshot . env . config, &snapshot . selected,
    &candidate)?;
  revalidate_candidate (&snapshot . env . config, &candidate)?;

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
      runtime . transition_maintenance (|coordinator|
        coordinator . store_selected (incident_id, epoch, record . clone ()))?;
      Ok (CandidateSelectionOutcome {
        graph_generation: record . graph_generation . get (),
        manifest_revision: record . manifest_revision . get (),
        tantivy_generation: record . tantivy_generation,
        tantivy_outcome: record . tantivy_outcome,
        evidence,
      })
    }
    Err (SelectionFailure::Superseded (reason)) => {
      runtime . transition_maintenance (|coordinator|
        coordinator . selection_superseded (
          incident_id, epoch, reason . clone ()))?;
      let _ = runtime . schedule_full_observation (
        QueuedObservationReason::SelectedGenerationAdvanced);
      Err (format! (
        "candidate was superseded before mutation: {}; exact observation was queued",
        reason))
    }
    Err (SelectionFailure::Stores { reason, queryable_g0 }) => {
      runtime . transition_maintenance (|coordinator|
        coordinator . block_store_health (
          incident_id, epoch, reason . clone ()))?;
      Err (if queryable_g0 {
        format! ("candidate selection failed; coherent G0 was restored: {}", reason)
      } else {
        format! ("candidate selection failed and stores are unqueryable: {}", reason)
      })
    }
  }
}

enum SelectionFailure {
  Superseded (String),
  Stores { reason : String, queryable_g0 : bool },
}

async fn select_stores (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
  candidate   : Arc<ObservedDiskCandidate>,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  let expected_generation = candidate . summary . base_graph_generation;
  let selection = runtime . generation_gate . begin_selection (
    expected_generation, false) . map_err (SelectionFailure::Superseded)?;
  let _write_guard = crate::write_lock::acquire_graph_write_lock () . await;
  let env = runtime . lock_writer_env () . map_err (|reason|
    SelectionFailure::Stores { reason, queryable_g0: false })?;
  let old_selected = env . in_rust_graph . load_full ();

  if let Err (reason) = validate_locked_preselection (
      runtime, incident_id, epoch, &env . config, &old_selected, &candidate)
  {
    drop (env);
    drop (_write_guard);
    selection . retain_generation ();
    return Err (SelectionFailure::Superseded (reason));
  }

  let old_nodes = nodecompletes_from_graph (&old_selected . graph);
  let outcome = match apply_define_nodes_to_stores (
      candidate . definitions . clone (), &[], env . config . clone (),
      &env . tantivy_index, &env . driver, &env . in_rust_graph,
      false, Some (candidate . manifest . clone ()), &HashSet::new ()) . await
  {
    Ok (outcome) => outcome,
    Err (error) => {
      let reason = format! ("derived-store transition failed: {}", error);
      let current = env . in_rust_graph . load_full ();
      let coherent_g0 = same_selected_identity (&current, &old_selected)
        && healthy (&current . typedb_health)
        && healthy (&current . tantivy_health);
      runtime . publish_selected_from_env (&env);
      drop (env);
      drop (_write_guard);
      if coherent_g0 {
        selection . retain_generation ();
      } else {
        selection . mark_unqueryable (reason . clone ()); }
      return Err (SelectionFailure::Stores { reason, queryable_g0: coherent_g0 });
    }
  };

  let terminal = wait_for_tantivy_generation (outcome . tantivy_generation);
  let tantivy_outcome = match terminal {
    TantivyGenerationStatus::Committed => "committed" . to_string (),
    TantivyGenerationStatus::Reconstructed (reason) =>
      format! ("reconstructed-after-incremental-failure: {}", reason),
    TantivyGenerationStatus::Failed (reason) => {
      let recovery = restore_g0 (
        &env . config, &env . driver, &env . tantivy_index,
        &old_selected, &old_nodes) . await;
      let (queryable_g0, full_reason) = match recovery {
        Ok (( )) => {
          env . in_rust_graph . store (old_selected . clone ());
          (true, format! (
            "Tantivy candidate generation {} failed ({}); complete G0 stores were reconstructed",
            outcome . tantivy_generation . get (), reason))
        }
        Err (recovery_reason) => {
          let poisoned = Arc::new (old_selected
            . with_typedb_poisoned (format! (
              "store restoration could not prove TypeDB: {}", recovery_reason))
            . with_tantivy_poisoned (format! (
              "candidate index failed: {}; restoration failed: {}",
              reason, recovery_reason)));
          env . in_rust_graph . store (poisoned);
          (false, format! (
            "Tantivy candidate generation {} failed ({}), and G0 restoration failed ({})",
            outcome . tantivy_generation . get (), reason, recovery_reason))
        }
      };
      runtime . publish_selected_from_env (&env);
      drop (env);
      drop (_write_guard);
      if queryable_g0 {
        selection . retain_generation ();
      } else {
        selection . mark_unqueryable (full_reason . clone ()); }
      return Err (SelectionFailure::Stores {
        reason: full_reason, queryable_g0,
      });
    }
    TantivyGenerationStatus::Pending => unreachable! (),
  };

  let selected = env . in_rust_graph . load_full ();
  if selected . graph_generation != outcome . graph_generation
  || selected . manifest != candidate . manifest
  || graph_nodes (&selected . graph) != graph_nodes (&candidate . graph)
  || !healthy (&selected . typedb_health)
  || !healthy (&selected . tantivy_health)
  {
    let reason = "store transition completed without the exact candidate publication"
      . to_string ();
    runtime . publish_selected_from_env (&env);
    drop (env);
    drop (_write_guard);
    selection . mark_unqueryable (reason . clone ());
    return Err (SelectionFailure::Stores {
      reason, queryable_g0: false,
    });
  }
  let record = SelectedStoreRecord {
    graph_generation: selected . graph_generation,
    manifest_revision: selected . manifest_revision,
    tantivy_generation: outcome . tantivy_generation . get (),
    tantivy_outcome,
  };
  runtime . publish_selected_from_env (&env);
  drop (env);
  drop (_write_guard);
  selection . publish (record . graph_generation)
    . map_err (|reason| SelectionFailure::Stores {
      reason, queryable_g0: false,
    })?;
  Ok (record)
}

fn matching_archive_ready (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
) -> Result<super::types::ActiveMaintenance, String> {
  let coordinator = runtime . maintenance . lock ()
    . map_err (|_| "maintenance coordinator poisoned" . to_string ())?;
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
  let sequence = runtime . maintenance . lock ()
    . map_err (|_| "maintenance coordinator poisoned" . to_string ())?
    . observation_sequence;
  if sequence != candidate . summary . covered_sequence {
    return Err (format! (
      "candidate covered observation {}, but current observation is {}",
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
    let coordinator = runtime . maintenance . lock ()
      . map_err (|_| "maintenance coordinator poisoned" . to_string ())?;
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

async fn restore_g0 (
  config        : &crate::types::misc::SkgConfig,
  driver        : &typedb_driver::TypeDBDriver,
  tantivy_index : &crate::types::misc::TantivyIndex,
  old_selected  : &crate::types::store_state::SelectedStoreState,
  old_nodes     : &[crate::types::nodes::complete::NodeComplete],
) -> Result<(), String> {
  wipe_then_init_typedb_db (config, driver, old_nodes) . await
    . map_err (|error| format! ("TypeDB G0 reconstruction failed: {}", error))?;
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

fn same_selected_identity (
  current : &crate::types::store_state::SelectedStoreState,
  old     : &crate::types::store_state::SelectedStoreState,
) -> bool {
  current . graph_generation == old . graph_generation
  && current . manifest_revision == old . manifest_revision
  && current . manifest == old . manifest
  && graph_nodes (&current . graph) == graph_nodes (&old . graph)
}

fn healthy (health : &StoreHealth) -> bool {
  matches! (health, StoreHealth::Healthy)
}
