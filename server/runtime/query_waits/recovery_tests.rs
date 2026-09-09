use super::super::ServerRuntime;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::init::empty_in_ram_tantivy_index;
use crate::dbs::tantivy::search::{search_index, SearchOptions};
use crate::dbs::tantivy::write::reconstruct_index_from_nodes;
use crate::maintenance::candidate::{config_file_blake3, config_identity,
  source_catalog_blake3, CandidateDiskFence, ObservedDiskCandidate,
  SemanticChangeEvidence, SemanticNodeEvidence};
use crate::maintenance::evidence::{MaintenanceEvidenceStore,
  PublishedMaintenanceEvidence};
use crate::maintenance::journal::MaintenanceJournalStore;
use crate::maintenance::query_waits::{QueryWaitDestination, QueryWaitPublication,
  QueryWaitRecipe, QueryWaitRecord, QueryWaitState, QueryWaitTarget,
  canonical_incident_operation_id};
use crate::maintenance::MaintenanceCoordinator;
use crate::maintenance::types::{ActiveMaintenance, ArchiveStatus, CandidateSummary,
  CommittedIncident, CoordinatorState, MaintenanceOrigin, MaintenancePhase,
  ServerEvidenceRecord, SelectedStoreRecord};
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::{empty_node_complete, NodeComplete};
use crate::types::store_state::{PathDigest, SelectedPathManifest,
  SelectedStoreState};
use crate::runtime::SelectedRuntimeSnapshot;

use arc_swap::ArcSwap;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tempfile::TempDir;

struct RecoveryFixture {
  _temporary : TempDir,
  config    : SkgConfig,
  current   : SelectedStoreState,
  runtime   : ServerRuntime,
  wait      : QueryWaitRecord,
  evidence_header : PathBuf,
}

fn node (title : &str) -> NodeComplete {
  let mut node : NodeComplete = empty_node_complete ();
  node . pid = ID::from ("query-node");
  node . title = title . into ();
  node
}

fn runtime_for (
  config   : &SkgConfig,
  selected : SelectedStoreState,
) -> ServerRuntime {
  let index : TantivyIndex = empty_in_ram_tantivy_index () . unwrap ();
  let nodes : Vec<NodeComplete> = selected . graph . nodes . values ()
    . map (|node| {
      let mut complete : NodeComplete = empty_node_complete ();
      complete . pid = node . pid . clone ();
      complete . title = node . title . clone ();
      complete
    }) . collect ();
  reconstruct_index_from_nodes (&nodes, &index, &HashMap::new ()) . unwrap ();
  let selected : SelectedStoreState = selected . with_searcher (
    index . reader . searcher ());
  ServerRuntime::new (SkgEnv {
    config: config . clone (),
    in_rust_graph: Arc::new (ArcSwap::from (Arc::new (selected))),
    searcher: index . reader . searcher (),
    tantivy_index: index,
    startup_warnings: Arc::new (Vec::new ()),
  }) . unwrap ()
}

fn fixture () -> RecoveryFixture {
  let temporary : TempDir = tempfile::tempdir () . unwrap ();
  let config_path : PathBuf = temporary . path () . join ("skgconfig.toml");
  fs::write (&config_path, b"query recovery fixture") . unwrap ();
  let mut config : SkgConfig = SkgConfig::dummyFromSources (HashMap::new ());
  config . config_path = config_path;
  config . data_root = temporary . path () . to_path_buf ();
  config . maintenance_archive_identity = temporary . path () . join ("archive");

  let g0_node : NodeComplete = node ("G0");
  let g1_node : NodeComplete = node ("G1 historical");
  let g2_node : NodeComplete = node ("G2 current");
  let g0_path : PathBuf = "/selected/g0.skg" . into ();
  let g1_path : PathBuf = "/selected/g1.skg" . into ();
  let g2_path : PathBuf = "/selected/g2.skg" . into ();
  let g0_bytes : Vec<u8> = b"g0 selected bytes" . to_vec ();
  let g1_bytes : Vec<u8> = b"g1 selected bytes" . to_vec ();
  let g2_bytes : Vec<u8> = b"g2 selected bytes" . to_vec ();
  let g0_manifest : SelectedPathManifest = BTreeMap::from ([
    (g0_path, PathDigest::of_bytes (&g0_bytes))]);
  let g1_manifest : SelectedPathManifest = BTreeMap::from ([
    (g1_path . clone (), PathDigest::of_bytes (&g1_bytes))]);
  let g2_manifest : SelectedPathManifest = BTreeMap::from ([
    (g2_path, PathDigest::of_bytes (&g2_bytes))]);
  let mut g0 : SelectedStoreState = SelectedStoreState::initial (
    InRustGraph::from_nodecompletes (&[g0_node . clone ()]), g0_manifest);
  g0 . cyclic_roots = BTreeSet::from ([ID::from ("query-node")]);
  let g1_graph : InRustGraph = InRustGraph::from_nodecompletes (&[g1_node . clone ()]);
  let g2_graph : InRustGraph = InRustGraph::from_nodecompletes (&[g2_node]);
  let g1 : SelectedStoreState = g0 . with_acknowledged_rebuild (
    g1_graph, g1_manifest . clone ());
  let current : SelectedStoreState = g1 . with_acknowledged_rebuild (
    g2_graph, g2_manifest);

  let node_id : ID = ID::from ("query-node");
  let summary : CandidateSummary = CandidateSummary {
    id: crate::maintenance::CandidateId::new (),
    base_graph_generation: g0 . graph_generation,
    base_manifest_revision: g0 . manifest_revision,
    covered_sequence: crate::maintenance::ObservationSequence::INITIAL,
    changed_primary_ids: vec![node_id . to_string ()],
  };
  let candidate : ObservedDiskCandidate = ObservedDiskCandidate {
    summary: summary . clone (),
    config_identity: config_identity (&config),
    config_file_blake3: config_file_blake3 (&config),
    source_catalog_blake3: source_catalog_blake3 (&config),
    config: Arc::new (config . clone ()),
    manifest: g1_manifest . clone (),
    base_graph: g0 . graph . clone (),
    graph: g1 . graph . clone (),
    definitions: Vec::new (),
    added_primary_ids: BTreeSet::new (),
    deleted_primary_ids: BTreeSet::new (),
    modified_primary_ids: BTreeSet::from ([node_id . clone ()]),
    evidence: BTreeMap::from ([(node_id . clone (),
      SemanticChangeEvidence {
        before: Some (SemanticNodeEvidence::from (&g0_node)),
        after: Some (SemanticNodeEvidence::from (&g1_node)),
        diff: "historical query fixture" . into (),
      })]),
    selected_bytes: BTreeMap::from ([(g1_path, g1_bytes)]),
    warnings: Vec::new (),
    load_violations: Vec::new (),
    disk_fence: CandidateDiskFence::Complete,
  };
  let mut seed : MaintenanceCoordinator = MaintenanceCoordinator::new ();
  let mut active : ActiveMaintenance = seed . begin_for_client (
    MaintenanceOrigin::FullRebuild, Some (summary), "old-client" . into ())
    . unwrap ();
  let evidence_store : MaintenanceEvidenceStore =
    MaintenanceEvidenceStore::alongside (&MaintenanceJournalStore::for_config (
      &config . config_path));
  let publication : PublishedMaintenanceEvidence = evidence_store . publish_candidate (
    &active, &config, &g0, &candidate) . unwrap ();
  active . phase = MaintenancePhase::Presenting;
  active . archive_status = ArchiveStatus::Ready {
    manifest_sha256: "archive" . into () };
  active . initial_archive_manifest_sha256 = Some ("archive" . into ());
  active . server_session_id = Some ("old-server" . into ());
  active . selected_store = Some (SelectedStoreRecord {
    graph_generation: g1 . graph_generation,
    manifest_revision: g1 . manifest_revision,
    tantivy_generation: 1,
    tantivy_outcome: "fixture" . into (),
  });
  active . server_evidence = Some (ServerEvidenceRecord {
    path: publication . path . clone (),
    bundle_sha256: publication . bundle_sha256 . clone (),
    artifact_count: publication . artifact_count as u64,
    total_file_bytes: publication . total_file_bytes as u64,
  });
  seed . state = CoordinatorState::Idle;
  seed . committed_incidents . insert (
    active . incident_id . clone (), CommittedIncident::Settling (active . clone ())); 

  let config_snapshot : String = super::encode_config (&config) . unwrap ();
  let source_catalog_snapshot : String = serde_yaml::to_string (
    &config . sources) . unwrap ();
  let operation_id : String = uuid::Uuid::new_v4 () . to_string ();
  let mut wait : QueryWaitRecord = QueryWaitRecord::new (
    operation_id, QueryWaitRecipe {
      terms: "G1 historical" . into (), regex: false, body: false,
      operators: false, ugly_choice: None, source_set: "all" . into (),
      config_snapshot: config_snapshot . clone (),
      source_catalog_snapshot: source_catalog_snapshot . clone (),
      config_file_blake3: config_file_blake3 (&config),
      source_catalog_blake3: source_catalog_blake3 (&config),
    }, QueryWaitDestination {
      view_uri: "search:recovery" . into (), client_buffer_id: None,
      base_graph_generation: 0, base_presentation_generation: 0,
      base_server_revision: 0, base_application_token: 0,
      base_content_sha256: "a" . repeat (64),
    }, QueryWaitTarget::Incident {
      incident_id: active . incident_id . clone (), epoch: active . epoch,
    }) . unwrap ();
  wait . state = QueryWaitState::Executing;
  wait . resolved_target = Some (QueryWaitPublication {
    operation_id: canonical_incident_operation_id (
      &active . incident_id, active . epoch),
    graph_generation: g1 . graph_generation . get (),
    manifest_revision: g1 . manifest_revision . get (),
    source_set: "all" . into (), config_snapshot,
    source_catalog_snapshot,
    cyclic_root_ids: BTreeSet::from ([ID::from ("query-node")]),
  });
  let runtime : ServerRuntime = runtime_for (&config, current . clone ());
  runtime . transition_maintenance (|coordinator| {
    *coordinator = seed . clone ();
    Ok (())
  }) . unwrap ();
  let evidence_header : PathBuf = publication . path . join ("header.yaml");
  RecoveryFixture { _temporary: temporary, config, current, runtime, wait,
                    evidence_header }
}

#[test]
fn recovers_exact_historical_g1_after_current_g2_and_rejects_tampering () {
  let RecoveryFixture { _temporary, config, current, runtime, wait,
                        evidence_header } = fixture ();
  let recovered : Arc<SelectedRuntimeSnapshot> = runtime
    . query_wait_snapshot (&wait) . unwrap ();
  assert_eq! (recovered . selected . graph_generation . get (), 2);
  assert_eq! (recovered . selected . manifest_revision . get (), 2);
  assert_eq! (recovered . selected . graph . get (&ID::from ("query-node"))
    . map (|node| node . title . clone ()), Some ("G1 historical" . into ()));
  assert! (recovered . env . config == config);
  let expected_manifest : SelectedPathManifest = BTreeMap::from ([
    (PathBuf::from ("/selected/g1.skg"),
     PathDigest::of_bytes (b"g1 selected bytes"))]);
  assert_eq! (*recovered . selected . manifest, expected_manifest);
  assert_eq! (recovered . selected . cyclic_roots,
              BTreeSet::from ([ID::from ("query-node")]));
  assert! (!search_index (&recovered . env . tantivy_index,
    &recovered . env . searcher, "G1 historical", &SearchOptions::default ())
    . unwrap () . 0 . is_empty ());
  let current_selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  assert_eq! (current_selected . selected . graph_generation . get (), 3);
  assert_ne! (*recovered . selected . manifest, *current_selected . selected . manifest);
  drop (runtime);
  let restarted : ServerRuntime = runtime_for (&config, current);
  let restarted_recovered : Arc<SelectedRuntimeSnapshot> = restarted
    . query_wait_snapshot (&wait) . unwrap ();
  assert_eq! (restarted_recovered . selected . graph_generation . get (), 2);
  fs::write (&evidence_header, b"tampered evidence") . unwrap ();
  assert! (restarted . query_wait_snapshot (&wait) . is_err ());
}
