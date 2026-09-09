//! Small query facts are resolved in the owner publication. Index construction
//! and result delivery never run in the owner loop.

pub(crate) mod execution;
mod worker;

use crate::maintenance::coordinator::MaintenanceCoordinator;
use crate::maintenance::query_waits::{QueryWaitOutcomeKind, QueryWaitRecord,
  QueryWaitState, QueryWaitTarget, QueryWaitTargetOutcome,
  canonical_incident_operation_id};
use crate::maintenance::types::{ActiveMaintenance, CommittedIncident,
  CoordinatorState, TerminalDisposition, TerminalMaintenance};
use crate::runtime::SelectedRuntimeSnapshot;
use crate::types::misc::{SkgConfig, SkgfileSource, SourceCatalog, SourceName};

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

pub(crate) fn resolve_wait_targets (
  coordinator : &mut MaintenanceCoordinator,
  selected : Option<&SelectedRuntimeSnapshot>,
) -> Result<(), String> {
  // Copy only waits awaiting a target. Executing/ready/delivered waits already
  // own their exact outcome and must not adopt subsequent publications.
  let pending : Vec<QueryWaitRecord> = coordinator . query_waits . waits . values ()
    . filter (|wait| wait . resolved_target . is_none () && matches! (
      wait . state, QueryWaitState::Pending | QueryWaitState::Blocked { .. }))
    . cloned () . collect ();
  for wait in pending {
    let incident : Option<&ActiveMaintenance> = target_incident (coordinator, &wait . target);
    let outcome : Option<QueryWaitTargetOutcome> = match terminal_target_outcome (
      coordinator, &wait . target) {
      Some (outcome) => Some (outcome),
      None => match incident {
        Some (incident) => incident_outcome (incident, &wait, selected)?,
        None => None, },
    };
    if let Some (outcome) = outcome {
      coordinator . resolve_query_wait_target (&wait . operation_id, outcome)?;
    } else if incident . is_none () && matches! (
      wait . target, QueryWaitTarget::Candidate { .. })
      && !pending_candidate_matches (coordinator, &wait . target) {
      coordinator . supersede_query_wait_target (&wait . operation_id,
        "the named candidate is no longer awaiting reconciliation" . into ())?;
    }
  }
  Ok (( ))
}

fn target_incident<'a> (
  coordinator : &'a MaintenanceCoordinator,
  target : &QueryWaitTarget,
) -> Option<&'a ActiveMaintenance> {
  let matches = |incident : &&ActiveMaintenance| -> bool { match target {
    QueryWaitTarget::Incident { incident_id, epoch } =>
      &incident . incident_id == incident_id && &incident . epoch == epoch,
    QueryWaitTarget::Candidate { candidate_id } => incident . candidate
      . as_ref () . is_some_and (|candidate| &candidate . id == candidate_id),
  }};
  let active : Option<&ActiveMaintenance> = match &coordinator . state {
    CoordinatorState::Active (active) => Some (active), _ => None, };
  active . into_iter () . chain (coordinator . committed_incidents . values ()
    . filter_map (|incident| match incident {
      CommittedIncident::Settling (active) => Some (active),
      CommittedIncident::Terminal { incident, .. } => incident . as_ref (),
    })) . find (matches)
}

fn incident_outcome (
  incident : &ActiveMaintenance,
  wait : &QueryWaitRecord,
  selected : Option<&SelectedRuntimeSnapshot>,
) -> Result<Option<QueryWaitTargetOutcome>, String> {
  let outcome : QueryWaitOutcomeKind = if let Some (record) = &incident . selected_store {
    if let Some (selected) = selected . filter (|selected|
      record . graph_generation == selected . selected . graph_generation
      && record . manifest_revision == selected . selected . manifest_revision) {
      let config_snapshot : String = encode_config (&selected . env . config)?;
      let source_catalog_snapshot : String = serde_yaml::to_string (
        &selected . env . config . sources) . map_err (|error| error . to_string ())?;
      if record . graph_generation == incident . g0_graph_generation {
        QueryWaitOutcomeKind::NoChange {
          graph_generation: record . graph_generation . get (),
          manifest_revision: record . manifest_revision . get (),
          source_set: wait . recipe . source_set . clone (),
          config_snapshot, source_catalog_snapshot,
          cyclic_root_ids: selected . selected . cyclic_roots . clone (), }
      } else {
        QueryWaitOutcomeKind::Published {
          graph_generation: record . graph_generation . get (),
          manifest_revision: record . manifest_revision . get (),
          source_set: wait . recipe . source_set . clone (),
          config_snapshot, source_catalog_snapshot,
          cyclic_root_ids: selected . selected . cyclic_roots . clone (), }
      }
    } else { QueryWaitOutcomeKind::Blocked {
      reason: "the exact target snapshot requires historical recovery" . into (), } }
  } else if let Some (reason) = &incident . blocking_reason {
    QueryWaitOutcomeKind::Blocked { reason: reason . clone (), }
  } else { return Ok (None); };
  Ok (Some (QueryWaitTargetOutcome {
    incident_id: incident . incident_id . clone (), epoch: incident . epoch,
    candidate_id: incident . candidate . as_ref () . map (|candidate| candidate . id . clone ()),
    operation_id: canonical_incident_operation_id (&incident . incident_id, incident . epoch),
    outcome, }))
}

fn terminal_target_outcome (
  coordinator : &MaintenanceCoordinator,
  target : &QueryWaitTarget,
) -> Option<QueryWaitTargetOutcome> {
  let terminal : Option<(&TerminalMaintenance, Option<&ActiveMaintenance>)> =
    match &coordinator . state {
      CoordinatorState::Terminal (record) => Some ((record, None)), _ => None, };
  terminal . into_iter () . chain (coordinator . committed_incidents . values ()
    . filter_map (|incident| match incident {
      CommittedIncident::Terminal { record, incident, .. } => Some ((record, incident . as_ref ())),
      _ => None, })) . find_map (|(record, incident)| {
      let matches : bool = match target {
        QueryWaitTarget::Incident { incident_id, epoch } =>
          &record . incident_id == incident_id && &record . epoch == epoch,
        QueryWaitTarget::Candidate { candidate_id } => incident
          . and_then (|active| active . candidate . as_ref ())
          . is_some_and (|candidate| &candidate . id == candidate_id), };
      if !matches || record . selected_store . is_some () { return None; }
      let outcome : QueryWaitOutcomeKind = match record . disposition {
        TerminalDisposition::FailedBeforeArchive | TerminalDisposition::FailedAfterArchive =>
          QueryWaitOutcomeKind::Failed {
            reason: "target maintenance failed before selecting a query snapshot" . into (), },
        TerminalDisposition::Completed => QueryWaitOutcomeKind::Blocked {
          reason: "completed target has no retained query snapshot" . into (), },
        _ => QueryWaitOutcomeKind::Cancelled {
          reason: "target maintenance ended without selecting a query snapshot" . into (), }, };
      Some (QueryWaitTargetOutcome {
        incident_id: record . incident_id . clone (), epoch: record . epoch,
        candidate_id: incident . and_then (|active| active . candidate . as_ref ())
          . map (|candidate| candidate . id . clone ()),
        operation_id: canonical_incident_operation_id (&record . incident_id, record . epoch),
        outcome, })
    })
}

fn pending_candidate_matches (
  coordinator : &MaintenanceCoordinator,
  target : &QueryWaitTarget,
) -> bool {
  match (&coordinator . state, target) {
    (CoordinatorState::Pending (pending), QueryWaitTarget::Candidate { candidate_id }) =>
      pending . candidate . as_ref () . is_some_and (|candidate| &candidate . id == candidate_id),
    // An observation has not yet settled whether the candidate is superseded.
    (CoordinatorState::Observing, _) => true,
    _ => false,
  }
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct RetainedQueryConfig {
  format_version : u32,
  config : SkgConfig,
  config_path : PathBuf,
  data_root : PathBuf,
  archive_identity : PathBuf,
  sources : Vec<RetainedQuerySource>,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct RetainedQuerySource {
  source : SkgfileSource,
  configured_path : PathBuf,
  directory_identity : Option<PathBuf>,
}

pub(crate) fn encode_config (
  config : &SkgConfig,
) -> Result<String, String> {
  let retained : RetainedQueryConfig = RetainedQueryConfig {
    format_version: 1, config: config . clone (),
    config_path: config . config_path . clone (), data_root: config . data_root . clone (),
    archive_identity: config . maintenance_archive_identity . clone (),
    sources: config . sources . values () . map (|source| RetainedQuerySource {
      source: source . clone (),
      configured_path: config . sources . configured_path (&source . name)
        . unwrap_or (&source . path) . to_path_buf (),
      directory_identity: config . sources . directory_identity (&source . name)
        . map (|path| path . to_path_buf ()), }) . collect (), };
  serde_yaml::to_string (&retained) . map_err (|error| error . to_string ())
}

pub(crate) fn decode_config (
  bytes : &str,
) -> Result<SkgConfig, String> {
  let retained : RetainedQueryConfig = serde_yaml::from_str (bytes)
    . map_err (|error| error . to_string ())?;
  if retained . format_version != 1 {
    return Err ("unsupported retained query configuration version" . into ()); }
  let mut config : SkgConfig = retained . config;
  config . config_path = retained . config_path;
  config . data_root = retained . data_root;
  config . maintenance_archive_identity = retained . archive_identity;
  config . sources = SourceCatalog::default ();
  for retained in retained . sources {
    let mut source : SkgfileSource = retained . source . clone ();
    let name : SourceName = source . name . clone ();
    source . path = retained . configured_path;
    if config . sources . insert (name . clone (), source) . is_some () {
      return Err ("duplicate source in retained query configuration" . into ()); }
    *config . sources . get_mut (&name) . expect ("inserted source") = retained . source;
    if let Some (identity) = retained . directory_identity {
      config . sources . set_directory_identity (name, identity); }
  }
  Ok (config)
}

#[cfg(test)]
pub(super) mod tests {
  use super::*;
  use crate::maintenance::query_waits::{QueryWaitDestination, QueryWaitRecipe};
  use crate::maintenance::types::{MaintenanceOrigin, SelectedStoreRecord};
  use crate::types::store_state::{GraphGeneration, ManifestRevision};
  use uuid::Uuid;

  pub(crate) fn wait_for (incident : &ActiveMaintenance) -> QueryWaitRecord {
    QueryWaitRecord::new (Uuid::new_v4 () . to_string (), QueryWaitRecipe {
      terms: "needle" . into (), regex: false, body: true, operators: false,
      ugly_choice: None, source_set: "all" . into (), config_snapshot: "accepted config" . into (),
      source_catalog_snapshot: "accepted sources" . into (),
      config_file_blake3: "a" . repeat (64), source_catalog_blake3: "b" . repeat (64),
    }, QueryWaitDestination {
      view_uri: "search:wait:test" . into (), client_buffer_id: Some ("buffer" . into ()),
      base_graph_generation: 1, base_presentation_generation: 1,
      base_server_revision: 1, base_application_token: 1, base_content_sha256: "c" . repeat (64),
    }, QueryWaitTarget::Incident {
      incident_id: incident . incident_id . clone (), epoch: incident . epoch,
    }) . unwrap ()
  }

  #[test]
  fn retained_configuration_preserves_order_ownership_and_runtime_path_facts () {
    let mut config : SkgConfig = SkgConfig::dummyFromSources (Default::default ());
    config . config_path = "/private/data/skgconfig.toml" . into ();
    config . data_root = "/private/data" . into ();
    config . maintenance_archive_identity = "/private/archive" . into ();
    for (name, owned) in [("z", false), ("a", true)] {
      let name : SourceName = SourceName::from (name);
      config . sources . insert (name . clone (), SkgfileSource {
        name: name . clone (), abbreviation: Some ("label" . into ()),
        path: format! ("configured/{}", name) . into (), user_owns_it: owned, });
      config . sources . get_mut (&name) . unwrap () . path = format! ("/resolved/{}", name) . into ();
      config . sources . set_directory_identity (name . clone (), format! ("/canonical/{}", name) . into ());
    }
    let encoded : String = encode_config (&config) . unwrap ();
    let restored : SkgConfig = decode_config (&encoded) . unwrap ();
    assert! (restored == config);
    assert_eq! (encode_config (&restored) . unwrap (), encoded);
  }

  #[test]
  fn target_failure_and_cancellation_have_distinct_terminal_outcomes () {
    for (disposition, expected) in [
      (TerminalDisposition::FailedBeforeArchive, "failed"),
      (TerminalDisposition::MaintenanceAborted, "cancelled"),
    ] {
      let mut coordinator : MaintenanceCoordinator = MaintenanceCoordinator::new ();
      let incident : ActiveMaintenance = coordinator . begin (MaintenanceOrigin::FullRebuild, None) . unwrap ();
      let wait : QueryWaitRecord = wait_for (&incident);
      coordinator . register_query_wait (wait . clone ()) . unwrap ();
      coordinator . finish (&incident . incident_id, incident . epoch, disposition) . unwrap ();
      resolve_wait_targets (&mut coordinator, None) . unwrap ();
      assert_eq! (coordinator . query_waits . get (&wait . operation_id) . unwrap () . state . label (), expected);
    }
  }

  #[test]
  fn target_selection_never_falls_forward_to_an_unrelated_graph () {
    let mut coordinator : MaintenanceCoordinator = MaintenanceCoordinator::new ();
    let incident : ActiveMaintenance = coordinator . begin (MaintenanceOrigin::FullRebuild, None) . unwrap ();
    let wait : QueryWaitRecord = wait_for (&incident);
    coordinator . register_query_wait (wait . clone ()) . unwrap ();
    let CoordinatorState::Active (active) : &mut CoordinatorState = &mut coordinator . state else { unreachable! (); };
    active . selected_store = Some (SelectedStoreRecord {
      graph_generation: GraphGeneration::INITIAL, manifest_revision: ManifestRevision::INITIAL,
      tantivy_generation: 1, tantivy_outcome: "committed" . into (), });
    resolve_wait_targets (&mut coordinator, None) . unwrap ();
    let stored : &QueryWaitRecord = coordinator . query_waits . get (&wait . operation_id) . unwrap ();
    assert! (matches! (stored . state, QueryWaitState::Blocked { .. }));
    assert! (stored . resolved_target . is_none ());
  }
}

#[cfg(test)]
mod recovery_tests;
