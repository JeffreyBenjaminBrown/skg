//! Resume exact staged results before executing a retained recipe again.

use super::decode_config;
use crate::maintenance::journal::MaintenanceJournalStore;
use crate::maintenance::query_artifacts::{QueryArtifact, QueryArtifactStore};
use crate::maintenance::query_waits::{QueryWaitFreshness, QueryWaitPublication,
  QueryWaitRecipe, QueryWaitRecord, QueryWaitResult, QueryWaitState};
use crate::runtime::{SelectedRuntimeSnapshot, ServerRuntime};
use crate::types::misc::SkgConfig;
use crate::serve::handlers::text_search::wait_result;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::sync::{Arc, Weak};
use std::sync::atomic::Ordering;
use std::sync::mpsc::{Receiver, SyncSender, sync_channel};

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ResultPayload {
  format_version : u32,
  recipe : QueryWaitRecipe,
  target : QueryWaitPublication,
  content : String,
  warnings : Vec<String>,
  presentation_generation : u64,
}

enum ExecutionFailure {
  Blocked (String),
  Failed (String),
}

impl ServerRuntime {
  pub(crate) fn start_query_wait_worker (
    self : &Arc<Self>,
  ) -> Result<(), String> {
    if self . query_worker_started . swap (true, Ordering::AcqRel) { return Ok (( )); }
    let (wake, events) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    if let Err (error) = self . owner . subscribe_query_waits (wake) {
      self . query_worker_started . store (false, Ordering::Release);
      return Err (error); }
    let runtime : Weak<ServerRuntime> = Arc::downgrade (self);
    std::thread::spawn (move || {
      while events . recv () . is_ok () {
        let Some (runtime) : Option<Arc<ServerRuntime>> = runtime . upgrade () else { break; };
        runtime . execute_query_waits ();
      }
    });
    Ok (( ))
  }

  fn execute_query_waits (
    &self,
  ) {
    let waits : Vec<QueryWaitRecord> = self . maintenance_snapshot () . query_waits
      . waits . into_values () . filter (|wait| wait . state == QueryWaitState::Executing) . collect ();
    for wait in waits {
      let result : Result<QueryWaitResult, ExecutionFailure> = self . prepare_query_result (&wait);
      // A late cancellation wins. Artifact bytes can remain as evidence; they
      // cannot turn a cancelled or superseded operation back into a ready one.
      let _ : Result<bool, String> = self . transition_maintenance (|coordinator| {
        let record : &mut QueryWaitRecord = coordinator . query_waits . get_mut (&wait . operation_id)?;
        if record . state != QueryWaitState::Executing || record . resolved_target != wait . resolved_target {
          return Ok (false); }
        match &result {
          Ok (result) => record . mark_result_ready (result . clone ()),
          Err (ExecutionFailure::Failed (reason)) => record . fail (reason . clone ()),
          Err (ExecutionFailure::Blocked (reason)) => {
            record . state = QueryWaitState::Blocked { reason: reason . clone (), };
            Ok (true) },
        }
      });
    }
  }

  fn prepare_query_result (
    &self,
    wait : &QueryWaitRecord,
  ) -> Result<QueryWaitResult, ExecutionFailure> {
    let store : QueryArtifactStore = artifact_store (wait) . map_err (ExecutionFailure::Blocked)?;
    let (artifact, payload) : (QueryArtifact, ResultPayload) =
      if let Some (artifact) = store . find (&wait . operation_id) . map_err (ExecutionFailure::Blocked)? {
        let payload : ResultPayload = read_payload (&store, &artifact, wait) . map_err (ExecutionFailure::Blocked)?;
        (artifact, payload)
      } else {
        let snapshot : Arc<SelectedRuntimeSnapshot> = self . query_wait_snapshot (wait) . map_err (ExecutionFailure::Blocked)?;
        let (content, warnings) : (String, Vec<String>) = wait_result::execute (
          &snapshot . env, &wait . recipe) . map_err (ExecutionFailure::Failed)?;
        drop (snapshot);
        let payload : ResultPayload = ResultPayload {
          format_version: 1, recipe: wait . recipe . clone (),
          target: wait . resolved_target . clone () . expect ("acquired query target"), content, warnings,
          presentation_generation: self . interactive . lock ()
            . map_err (|_| ExecutionFailure::Blocked ("query presentation state is unavailable" . into ()))?
            . collateral_scheduler . presentation_generation (), };
        let bytes : String = serde_yaml::to_string (&payload)
          . map_err (|error| ExecutionFailure::Blocked (error . to_string ()))?;
        let artifact : QueryArtifact = store . stage (&wait . operation_id, &bytes)
          . map_err (ExecutionFailure::Blocked)?;
        (artifact, payload)
      };
    Ok (result_metadata (artifact, &payload))
  }

  pub(crate) fn read_query_wait_result (
    &self,
    wait : &QueryWaitRecord,
  ) -> Result<String, String> {
    let result : &QueryWaitResult = wait . result . as_ref () . ok_or ("query result is not staged")?;
    let artifact : QueryArtifact = QueryArtifact {
      operation_id: wait . operation_id . clone (), path: result . artifact_path . clone (),
      bytes: result . artifact_bytes, sha256: result . artifact_sha256 . clone (), };
    let payload : ResultPayload = read_payload (&artifact_store (wait)?, &artifact, wait)?;
    if result_metadata (artifact, &payload) != *result {
      return Err ("query result metadata disagrees with its verified artifact" . into ()); }
    Ok (payload . content)
  }
}

fn artifact_store (
  wait : &QueryWaitRecord,
) -> Result<QueryArtifactStore, String> {
  let config : SkgConfig = decode_config (&wait . recipe . config_snapshot)?;
  Ok (QueryArtifactStore::alongside (&MaintenanceJournalStore::for_config (&config . config_path)))
}

fn read_payload (
  store : &QueryArtifactStore,
  artifact : &QueryArtifact,
  wait : &QueryWaitRecord,
) -> Result<ResultPayload, String> {
  let payload : ResultPayload = serde_yaml::from_str (&store . read (artifact)?)
    . map_err (|error| error . to_string ())?;
  if payload . format_version != 1 || payload . recipe != wait . recipe
  || Some (&payload . target) != wait . resolved_target . as_ref () {
    return Err ("staged query artifact disagrees with its accepted recipe or target" . into ()); }
  Ok (payload)
}

fn result_metadata (
  artifact : QueryArtifact,
  payload : &ResultPayload,
) -> QueryWaitResult {
  let content_sha256 : String = format! ("{:x}", Sha256::digest (payload . content . as_bytes ()));
  QueryWaitResult {
    artifact_path: artifact . path, artifact_bytes: artifact . bytes, artifact_sha256: artifact . sha256,
    content_sha256, warnings: payload . warnings . clone (),
    presentation_generation: payload . presentation_generation,
    graph_generation: payload . target . graph_generation,
    manifest_revision: payload . target . manifest_revision, source_set: payload . target . source_set . clone (),
    // Transport derives current/stale/pending status from the current owner
    // publication immediately before delivery, without rewriting this result.
    freshness: QueryWaitFreshness::Current,
  }
}

#[cfg(test)]
pub(super) mod tests {
  use super::*;
  use crate::dbs::in_rust_graph::InRustGraph;
  use crate::maintenance::candidate::source_catalog_blake3;
  use crate::maintenance::types::{CoordinatorState, MaintenanceOrigin,
    MaintenancePhase, SelectedStoreRecord, ServerEvidenceRecord};
  use crate::runtime::query_waits::{encode_config, execution::indexed_query_snapshot};
  use crate::runtime::SelectedRuntimeSnapshot;
  use crate::types::env::{GraphReadSnapshot, SkgEnv};
  use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
  use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
  use crate::types::store_state::SelectedStoreState;
  use std::collections::BTreeSet;

  pub(crate) fn fixture () -> (tempfile::TempDir, Arc<ServerRuntime>, String) {
    fixture_with_recipe ("needle", false)
  }

  fn fixture_with_recipe (
    terms : &str,
    regex : bool,
  ) -> (tempfile::TempDir, Arc<ServerRuntime>, String) {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let mut config : SkgConfig = SkgConfig::dummyFromSources (Default::default ());
    config . config_path = temp . path () . join ("skgconfig.toml");
    config . data_root = temp . path () . to_path_buf ();
    config . maintenance_archive_identity = temp . path () . join ("archive");
    std::fs::write (&config . config_path, "# retained query fixture\n") . unwrap ();
    let source : SourceName = SourceName::from ("owned");
    config . sources . insert (source . clone (), SkgfileSource {
      name: source . clone (), abbreviation: None, path: temp . path () . join ("owned"), user_owns_it: true, });
    std::fs::create_dir_all (config . sources . get (&source) . unwrap () . path . clone ()) . unwrap ();
    let mut node : NodeComplete = empty_node_complete ();
    node . pid = ID::from ("needle"); node . title = "Retained needle" . into (); node . source = source;
    let selected : SelectedStoreState = SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (&[node]), Default::default ());
    let snapshot : Arc<SelectedRuntimeSnapshot> = indexed_query_snapshot (GraphReadSnapshot {
      config: config . clone (), selected: selected . graph_base (), cyclic_roots: BTreeSet::new (), }) . unwrap ();
    let runtime : Arc<ServerRuntime> = Arc::new (ServerRuntime::new (snapshot . env . clone ()) . unwrap ());
    let (incident, operation_id) = runtime . transition_maintenance (|coordinator| {
      let incident = coordinator . begin (MaintenanceOrigin::FullRebuild, None)?;
      if let CoordinatorState::Active (active) = &mut coordinator . state {
        active . server_session_id = Some (runtime . server_session_id () . into ()); }
      coordinator . archive_ready (&incident . incident_id, incident . epoch, "initial" . into ())?;
      coordinator . record_server_evidence (&incident . incident_id, incident . epoch, ServerEvidenceRecord {
        path: "intentionally-unavailable-evidence" . into (), bundle_sha256: "bundle" . into (),
        artifact_count: 1, total_file_bytes: 2, })?;
      coordinator . transition (&incident . incident_id, incident . epoch, MaintenancePhase::FullRebuildExclusive)?;
      let mut wait : QueryWaitRecord = crate::runtime::query_waits::tests::wait_for (&incident);
      wait . recipe . terms = terms . into ();
      wait . recipe . regex = regex;
      wait . recipe . config_snapshot = encode_config (&config)?;
      wait . recipe . source_catalog_snapshot = serde_yaml::to_string (&config . sources) . unwrap ();
      wait . recipe . source_catalog_blake3 = source_catalog_blake3 (&config);
      let operation_id : String = wait . operation_id . clone ();
      coordinator . register_query_wait (wait)?;
      Ok ((incident, operation_id))
    }) . unwrap ();
    let control = runtime . reserve_mutation (
      format! ("maintenance/{}/{}", incident . incident_id, incident . epoch . get ()),
      snapshot . selected . graph_generation, snapshot . selected . manifest_revision) . unwrap ();
    control . authorize () . unwrap ();
    let mut env : SkgEnv = snapshot . env . clone ();
    let next : SelectedStoreState = snapshot . selected . with_acknowledged_rebuild (
      (*snapshot . selected . graph) . clone (), Default::default ())
      . with_searcher (env . searcher . clone ());
    env . in_rust_graph = Arc::new (arc_swap::ArcSwap::from_pointee (next));
    runtime . publish_selected_from_env (&control, &env) . unwrap ();
    runtime . transition_maintenance (|coordinator| coordinator . store_rebuilt (
      &incident . incident_id, incident . epoch, SelectedStoreRecord {
        graph_generation: env . in_rust_graph . load_full () . graph_generation,
        manifest_revision: env . in_rust_graph . load_full () . manifest_revision,
        tantivy_generation: 1, tantivy_outcome: "committed" . into (), })) . unwrap ();
    control . finish () . unwrap ();
    (temp, runtime, operation_id)
  }

  #[test]
  fn orphan_query_result_resumes_exact_bytes_after_restart_without_rerendering () {
    let (_temp, runtime, operation_id) = fixture ();
    let wait : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits . get (&operation_id) . unwrap () . clone ();
    let prepared : QueryWaitResult = runtime . prepare_query_result (&wait) . ok () . expect ("staged query result");
    let env : SkgEnv = runtime . selected_snapshot () . env . clone ();
    drop (runtime);
    let recovered : ServerRuntime = ServerRuntime::new (env) . unwrap ();
    assert! (recovered . query_wait_snapshot (&wait) . is_err ());
    recovered . execute_query_waits ();
    let ready : QueryWaitRecord = recovered . maintenance_snapshot () . query_waits . get (&operation_id) . unwrap () . clone ();
    assert_eq! (ready . state, QueryWaitState::Ready);
    assert_eq! (ready . result, Some (prepared));
    let content : String = recovered . read_query_wait_result (&ready) . unwrap ();
    assert! (content . contains ("Retained needle"));
    recovered . execute_query_waits ();
    assert_eq! (recovered . read_query_wait_result (&ready) . unwrap (), content);
  }

  #[test]
  fn query_worker_distinguishes_query_failure_from_missing_target_recovery () {
    let (_temp, runtime, operation_id) = fixture_with_recipe ("[", true);
    runtime . execute_query_waits ();
    assert! (matches! (runtime . maintenance_snapshot () . query_waits
      . get (&operation_id) . unwrap () . state, QueryWaitState::Failed { .. }));
    let (_temp, runtime, operation_id) = fixture ();
    let env : SkgEnv = runtime . selected_snapshot () . env . clone ();
    drop (runtime);
    let recovered : ServerRuntime = ServerRuntime::new (env) . unwrap ();
    recovered . execute_query_waits ();
    let blocked : QueryWaitRecord = recovered . maintenance_snapshot () . query_waits
      . get (&operation_id) . unwrap () . clone ();
    assert! (matches! (blocked . state, QueryWaitState::Blocked { .. }));
    assert! (blocked . result . is_none ());
  }

  #[test]
  fn cancelled_query_never_adopts_an_orphan_artifact () {
    let (_temp, runtime, operation_id) = fixture ();
    let wait : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits
      . get (&operation_id) . unwrap () . clone ();
    assert! (runtime . prepare_query_result (&wait) . is_ok ());
    runtime . transition_maintenance (|coordinator| coordinator
      . cancel_query_wait (&operation_id, "user cancelled" . into ())) . unwrap ();
    runtime . execute_query_waits ();
    let cancelled : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits
      . get (&operation_id) . unwrap () . clone ();
    assert! (matches! (cancelled . state, QueryWaitState::Cancelled { .. }));
    assert! (cancelled . result . is_none ());
  }

  #[test]
  fn query_worker_wakes_from_publication_and_delivers_only_one_result () {
    let (_temp, runtime, operation_id) = fixture ();
    runtime . start_query_wait_worker () . unwrap ();
    runtime . start_query_wait_worker () . unwrap ();
    let deadline : std::time::Instant = std::time::Instant::now () + std::time::Duration::from_secs (3);
    loop {
      let wait : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits . get (&operation_id) . unwrap () . clone ();
      if wait . state == QueryWaitState::Ready {
        assert! (runtime . read_query_wait_result (&wait) . unwrap () . contains ("Retained needle"));
        break; }
      assert! (std::time::Instant::now () < deadline, "query worker remained {:?}", wait . state);
      std::thread::sleep (std::time::Duration::from_millis (5));
    }
    let first : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits
      . get (&operation_id) . unwrap () . clone ();
    let mut destination = first . destination . clone ();
    destination . view_uri . push_str (":second");
    let second : QueryWaitRecord = QueryWaitRecord::new (
      uuid::Uuid::new_v4 () . to_string (), first . recipe, destination, first . target) . unwrap ();
    runtime . transition_maintenance (|coordinator|
      coordinator . register_query_wait (second . clone ())) . unwrap ();
    let deadline : std::time::Instant = std::time::Instant::now () + std::time::Duration::from_secs (3);
    loop {
      let ready : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits
        . get (&second . operation_id) . unwrap () . clone ();
      if ready . state == QueryWaitState::Ready { break; }
      assert! (std::time::Instant::now () < deadline, "publication did not wake query worker");
      std::thread::sleep (std::time::Duration::from_millis (5));
    }
  }
}
