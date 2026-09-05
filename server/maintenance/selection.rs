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
use crate::context::{
  compute_context_types,
  content_maps_from_nodes,
  context_origin_types_for_graph,
  had_id_set_from_nodes,
  link_dests_from_nodes,
};
use crate::dbs::init::wipe_then_init_typedb_db;
use crate::dbs::init::create_empty_tantivy_index;
use crate::dbs::tantivy::background_writer::{
  TantivyGenerationStatus,
  latest_tantivy_generation,
  wait_for_tantivy_writes_idle,
  wait_for_tantivy_generation,
};
use crate::dbs::tantivy::write::reconstruct_index_from_nodes;
use crate::maintenance::candidate::{config_identity, source_catalog_blake3};
use crate::runtime::ServerRuntime;
use crate::save::{apply_define_nodes_to_stores, nodecompletes_from_graph};
use crate::source_sets::ActiveSourceSet;
use crate::types::store_state::StoreHealth;
use crate::telescope::invariants::{
  report_telescope_violations,
  validate_all_telescopes,
};

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
  validate_rebuild_preselection (
    runtime, &active, &snapshot . env . config, &snapshot . selected,
    &candidate)?;
  revalidate_candidate (&candidate . config, &candidate)?;

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

/// Rebuild every derived store from one already-proved complete candidate.
/// Candidate parsing, folding, validation, archive publication, and evidence
/// all precede the exclusive gate; only this function spans destruction and
/// reconstruction.
pub fn rebuild_archived_candidate (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
) -> Result<CandidateSelectionOutcome, String> {
  let active = matching_archive_ready (runtime, incident_id, epoch)?;
  if active . origin != super::types::MaintenanceOrigin::FullRebuild {
    return Err ("exclusive rebuild selection requires a full-rebuild origin"
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
      incident_id, epoch, MaintenancePhase::FullRebuildExclusive))?;

  match block_on (rebuild_stores (
      runtime, incident_id, epoch, candidate . clone ()))
  {
    Ok (record) => {
      runtime . transition_maintenance (|coordinator|
        coordinator . store_rebuilt (incident_id, epoch, record . clone ()))?;
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
        "full rebuild candidate was superseded before mutation: {}; exact observation was queued",
        reason))
    }
    Err (SelectionFailure::Stores { reason, queryable_g0 }) => {
      runtime . transition_maintenance (|coordinator|
        coordinator . block_store_health (
          incident_id, epoch, reason . clone ()))?;
      Err (if queryable_g0 {
        format! ("full rebuild failed; coherent G0 was restored: {}", reason)
      } else {
        format! ("full rebuild failed and stores are unqueryable: {}", reason)
      })
    }
  }
}

enum SelectionFailure {
  Superseded (String),
  Stores { reason : String, queryable_g0 : bool },
}

async fn rebuild_stores (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
  candidate   : Arc<ObservedDiskCandidate>,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  let expected_generation = candidate . summary . base_graph_generation;
  let selection = runtime . generation_gate . begin_selection (
    expected_generation, true) . map_err (SelectionFailure::Superseded)?;
  let _write_guard = crate::write_lock::acquire_graph_write_lock () . await;
  wait_for_tantivy_writes_idle ();
  let mut env = runtime . lock_writer_env () . map_err (|reason|
    SelectionFailure::Stores { reason, queryable_g0: false })?;
  let old_selected = env . in_rust_graph . load_full ();
  if let Err (reason) = validate_locked_rebuild (
      runtime, incident_id, epoch, &env . config, &old_selected, &candidate)
  {
    drop (env);
    drop (_write_guard);
    selection . retain_generation ();
    return Err (SelectionFailure::Superseded (reason));
  }

  let old_nodes = nodecompletes_from_graph (&old_selected . graph);
  let nodes = nodecompletes_from_graph (&candidate . graph);
  let had_id_set = had_id_set_from_nodes (&nodes);
  let all_node_ids = nodes . iter () . map (|node| node . pid . clone ())
    . collect ();
  let link_dests = link_dests_from_nodes (&nodes);
  let (map_to_content, map_to_containers) = content_maps_from_nodes (&nodes);
  let context = compute_context_types (
    &had_id_set, &all_node_ids, &link_dests,
    &map_to_content, &map_to_containers);
  let mut warnings = validate_all_telescopes (
    &candidate . config, &candidate . graph);
  warnings . extend (candidate . load_violations . clone ());
  warnings . sort_by (|left, right| left . 0 . cmp (&right . 0));
  let source_catalog_changed = source_catalog_blake3 (&env . config)
    != candidate . source_catalog_blake3;
  let mut interactive = runtime . interactive . lock () . map_err (|_|
    SelectionFailure::Stores {
      reason: "interactive session poisoned" . into (),
      queryable_g0: false,
    })?;
  let old_source_set = interactive . active_source_set . clone ();
  let replacement_source_set = ActiveSourceSet::named (
      &candidate . config, old_source_set . name . clone ())
    . or_else (|_| ActiveSourceSet::named (
      &candidate . config,
      crate::types::misc::SourceSetName::from ("all")))
    . map_err (|error| SelectionFailure::Stores {
      reason: format! ("replacement source-set is invalid: {}", error),
      queryable_g0: true,
    })?;
  let source_set_changed = old_source_set . name != replacement_source_set . name;
  if source_set_changed {
    tracing::warn! (
      old = %old_source_set . name . 0,
      new = %replacement_source_set . name . 0,
      "replacement config removed the active source-set; using exact fallback");
  }

  if let Err (error) = wipe_then_init_typedb_db (
      &candidate . config, &env . driver, &nodes) . await
  {
    let reason = format! ("TypeDB full reconstruction failed: {}", error);
    return fail_rebuild_and_restore (
      runtime, selection, env, _write_guard, old_selected, old_nodes, reason)
      . await;
  }
  let replacement_tantivy = if candidate . config . tantivy_folder
      == env . config . tantivy_folder
  {
    env . tantivy_index . clone ()
  } else {
    match create_empty_tantivy_index (&candidate . config . tantivy_folder) {
      Ok (index) => index,
      Err (error) => {
        let reason = format! (
          "replacement Tantivy index could not be created: {}", error);
        return fail_rebuild_and_restore (
          runtime, selection, env, _write_guard,
          old_selected, old_nodes, reason) . await;
      }
    }
  };
  if let Err (error) = reconstruct_index_from_nodes (
      &nodes, &replacement_tantivy, &context . labels)
  {
    let reason = format! ("Tantivy full reconstruction failed: {}", error);
    return fail_rebuild_and_restore (
      runtime, selection, env, _write_guard, old_selected, old_nodes, reason)
      . await;
  }

  if source_catalog_changed {
    let sources = candidate . config . sources . values ()
      . map (|source| source . path . clone ()) . collect ();
    if let Err (error) = runtime . replace_observation_sources (sources) {
      let reason = format! ("replacement source watches failed: {}", error);
      return fail_rebuild_and_restore (
        runtime, selection, env, _write_guard, old_selected, old_nodes, reason)
        . await;
    }
  }
  if let Err (error) = runtime . transition_maintenance (|coordinator|
      coordinator . replace_full_rebuild_source_set (
        incident_id, epoch, replacement_source_set . name . 0 . clone ()))
  {
    if source_catalog_changed {
      let old_sources = env . config . sources . values ()
        . map (|source| source . path . clone ()) . collect ();
      let _ = runtime . replace_observation_sources (old_sources);
    }
    return fail_rebuild_and_restore (
      runtime, selection, env, _write_guard, old_selected, old_nodes,
      format! ("replacement source-set could not be journaled: {}", error))
      . await;
  }

  let selected = Arc::new (old_selected . with_acknowledged_rebuild (
      (*candidate . graph) . clone (), candidate . manifest . clone ())
    . with_cyclic_roots (context . cyclic_roots));
  env . startup_warnings = Arc::new (warnings . clone ());
  env . config = (*candidate . config) . clone ();
  env . tantivy_index = replacement_tantivy;
  env . in_rust_graph . store (selected . clone ());
  interactive . active_source_set = replacement_source_set . clone ();
  drop (interactive);
  runtime . publish_selected_from_env (&env);
  if let Err (error) = report_telescope_violations (
      &warnings, &env . config . data_root)
  {
    tracing::warn! (%error, "could not write the full-rebuild telescope report"); }
  let record = SelectedStoreRecord {
    graph_generation: selected . graph_generation,
    manifest_revision: selected . manifest_revision,
    tantivy_generation: latest_tantivy_generation ()
      . map (|generation| generation . get ()) . unwrap_or (0),
    tantivy_outcome: "synchronous-full-rebuild" . into (),
  };
  drop (env);
  drop (_write_guard);
  selection . publish (record . graph_generation)
    . map_err (|reason| SelectionFailure::Stores {
      reason, queryable_g0: false,
    })?;
  Ok (record)
}

async fn fail_rebuild_and_restore (
  runtime      : &ServerRuntime,
  selection    : crate::runtime::generation_gate::SelectionLease,
  env          : std::sync::MutexGuard<'_, crate::types::env::SkgEnv>,
  write_guard  : tokio::sync::MutexGuard<'static, ()>,
  old_selected : Arc<crate::types::store_state::SelectedStoreState>,
  old_nodes    : Vec<crate::types::nodes::complete::NodeComplete>,
  reason       : String,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  let recovery = restore_g0 (
    &env . config, &env . driver, &env . tantivy_index,
    &old_selected, &old_nodes) . await;
  let (queryable_g0, full_reason) = match recovery {
    Ok (( )) => {
      env . in_rust_graph . store (old_selected . clone ());
      (true, format! ("{}; complete G0 stores were reconstructed", reason))
    }
    Err (recovery_reason) => {
      let poisoned = Arc::new (old_selected
        . with_typedb_poisoned (format! (
          "full rebuild restoration could not prove TypeDB: {}",
          recovery_reason))
        . with_tantivy_poisoned (format! (
          "{}; G0 restoration failed: {}", reason, recovery_reason)));
      env . in_rust_graph . store (poisoned);
      (false, format! (
        "{}; complete G0 restoration failed: {}", reason, recovery_reason))
    }
  };
  runtime . publish_selected_from_env (&env);
  drop (env);
  drop (write_guard);
  if queryable_g0 {
    selection . retain_generation ();
  } else {
    selection . mark_unqueryable (full_reason . clone ()); }
  Err (SelectionFailure::Stores {
    reason: full_reason, queryable_g0,
  })
}

async fn select_stores (
  runtime     : &ServerRuntime,
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
  candidate   : Arc<ObservedDiskCandidate>,
) -> Result<SelectedStoreRecord, SelectionFailure> {
  if candidate . definitions . is_empty ()
     && graph_nodes (&candidate . graph)
        == graph_nodes (&candidate . base_graph)
  {
    return select_manifest_only (
      runtime, incident_id, epoch, candidate) . await;
  }
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

/// Select exact byte authority without inventing a graph or Tantivy
/// generation when the authorized disk observation folds to G0 exactly.
async fn select_manifest_only (
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
  let selected = if old_selected . manifest == candidate . manifest {
    old_selected . clone ()
  } else {
    let selected = Arc::new (old_selected . with_semantically_equal_manifest (
      candidate . manifest . clone ()));
    env . in_rust_graph . store (selected . clone ());
    selected
  };
  runtime . publish_selected_from_env (&env);
  let tantivy_generation = latest_tantivy_generation ()
    . map (|generation| generation . get ()) . unwrap_or (0);
  let record = SelectedStoreRecord {
    graph_generation: selected . graph_generation,
    manifest_revision: selected . manifest_revision,
    tantivy_generation,
    tantivy_outcome: "not-required-semantic-no-op" . into (),
  };
  drop (env);
  drop (_write_guard);
  selection . retain_generation ();
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
  let sequence = runtime . maintenance . lock ()
    . map_err (|_| "maintenance coordinator poisoned" . to_string ())?
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

fn validate_locked_rebuild (
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
      return Err ("maintenance ended before full rebuild selection" . into ()); };
    if &active . incident_id != incident_id || active . epoch != epoch
    || active . origin != super::types::MaintenanceOrigin::FullRebuild
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
