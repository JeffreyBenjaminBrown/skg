//! Recover one named query target without selecting it as the current store.

use super::{decode_config, target_incident};
use crate::consts::TANTIVY_WRITER_BUFFER_BYTES;
use crate::context::context_origin_types_for_graph;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::init::empty_in_ram_tantivy_index;
use crate::dbs::tantivy::write::add_documents_to_tantivy_writer;
use crate::maintenance::candidate::source_catalog_blake3;
use crate::maintenance::evidence::{MaintenanceEvidenceBundle, PublishedMaintenanceEvidence,
  ReconstructedEvidence, reconstruct_evidence};
use crate::maintenance::query_waits::{QueryWaitPublication, QueryWaitRecord, QueryWaitState};
use crate::maintenance::types::{ActiveMaintenance, SelectedStoreRecord};
use crate::maintenance::coordinator::MaintenanceCoordinator;
use crate::runtime::incident_recovery::{graph_from_evidence, validate_report_evidence};
use crate::runtime::{SelectedRuntimeSnapshot, ServerRuntime};
use crate::save::nodecompletes_from_graph;
use crate::types::env::{GraphReadSnapshot, SkgEnv};
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::tantivy::NodeTantivy;
use crate::types::store_state::{SelectedGraphBase, SelectedStoreState};

use arc_swap::ArcSwap;
use std::collections::HashMap;
use std::sync::Arc;
use tantivy::IndexWriter;

impl ServerRuntime {
  pub(crate) fn query_wait_snapshot (
    &self,
    wait : &QueryWaitRecord,
  ) -> Result<Arc<SelectedRuntimeSnapshot>, String> {
    if wait . state != QueryWaitState::Executing {
      return Err ("query target may only be acquired during explicit execution" . into ()); }
    let target : &QueryWaitPublication = wait . resolved_target . as_ref ()
      . ok_or ("query wait has no resolved target")?;
    let config : SkgConfig = decode_config (&target . config_snapshot)?;
    if source_catalog_blake3 (&config) != wait . recipe . source_catalog_blake3 {
      return Err ("query source interpretation changed during reconciliation; choose a new explicit query" . into ()); }
    let coordinator : MaintenanceCoordinator = self . maintenance_snapshot ();
    let incident : &ActiveMaintenance = target_incident (&coordinator, &wait . target)
      . ok_or ("query target has no retained incident evidence")?;
    let record : &SelectedStoreRecord = incident . selected_store . as_ref ()
      . ok_or ("query target incident has no selected store")?;
    if target . operation_id != super::canonical_incident_operation_id (
      &incident . incident_id, incident . epoch)
    || target . graph_generation != record . graph_generation . get ()
    || target . manifest_revision != record . manifest_revision . get () {
      return Err ("query target disagrees with its retained incident outcome" . into ()); }
    let current : Arc<SelectedRuntimeSnapshot> = self . selected_snapshot ();
    // A restart can reuse initial numeric counters. The process identity is
    // necessary before treating those counters as an exact live publication.
    if incident . server_session_id . as_deref () == Some (self . server_session_id ())
    && current . selected . graph_generation == record . graph_generation
    && current . selected . manifest_revision == record . manifest_revision
    && current . env . config == config
    && current . selected . cyclic_roots == target . cyclic_root_ids {
      return Ok (current); }
    drop (current);
    if let Ok (snapshot) = self . incident_snapshot (&incident . incident_id) {
      if snapshot . selected . graph_generation == record . graph_generation
      && snapshot . selected . manifest_revision == record . manifest_revision
      && snapshot . config == config && snapshot . cyclic_roots == target . cyclic_root_ids {
        return indexed_query_snapshot ((*snapshot) . clone ()); }
    }
    let (bundle, publication) : (MaintenanceEvidenceBundle, PublishedMaintenanceEvidence) =
      self . maintenance_evidence . load (&incident . incident_id)?;
    validate_report_evidence (incident, &bundle, &publication)?;
    if source_catalog_blake3 (&config) != bundle . header . source_catalog_blake3 {
      return Err ("query configuration disagrees with its exact evidence" . into ()); }
    let reconstructed : ReconstructedEvidence = reconstruct_evidence (&bundle . recovery)?;
    let graph : InRustGraph = graph_from_evidence (&reconstructed . g1_nodes)?;
    indexed_query_snapshot (GraphReadSnapshot {
      config, cyclic_roots: target . cyclic_root_ids . clone (),
      selected: SelectedGraphBase {
        graph: Arc::new (graph), manifest: Arc::new (reconstructed . g1_manifest),
        graph_generation: record . graph_generation, manifest_revision: record . manifest_revision, },
    })
  }
}

pub(crate) fn indexed_query_snapshot (
  semantic : GraphReadSnapshot,
) -> Result<Arc<SelectedRuntimeSnapshot>, String> {
  let index : TantivyIndex = empty_in_ram_tantivy_index ()
    . map_err (|error| error . to_string ())?;
  let nodes : Vec<NodeTantivy> = nodecompletes_from_graph (&semantic . selected . graph)
    . iter () . map (NodeTantivy::from) . collect ();
  let context : HashMap<ID, String> = context_origin_types_for_graph (
    &semantic . selected . graph, &semantic . cyclic_roots);
  let mut writer : IndexWriter = index . index . writer (TANTIVY_WRITER_BUFFER_BYTES)
    . map_err (|error| error . to_string ())?;
  // This index is private to this bounded query; it needs no global writer lock.
  add_documents_to_tantivy_writer (&nodes, &mut writer, &index, &context)
    . map_err (|error| error . to_string ())?;
  writer . commit () . map_err (|error| error . to_string ())?;
  drop (writer);
  index . reader . reload () . map_err (|error| error . to_string ())?;
  let mut selected : SelectedStoreState = SelectedStoreState::initial (
    InRustGraph::new (), Default::default ());
  selected . graph = semantic . selected . graph;
  selected . manifest = semantic . selected . manifest;
  selected . graph_generation = semantic . selected . graph_generation;
  selected . manifest_revision = semantic . selected . manifest_revision;
  selected . cyclic_roots = semantic . cyclic_roots;
  selected . searcher = Some (index . reader . searcher ());
  let selected : Arc<SelectedStoreState> = Arc::new (selected);
  let env : SkgEnv = SkgEnv {
    config: semantic . config, searcher: index . reader . searcher (),
    tantivy_index: index, in_rust_graph: Arc::new (ArcSwap::from (selected . clone ())),
    startup_warnings: Arc::new (Vec::new ()), };
  Ok (Arc::new (SelectedRuntimeSnapshot { env, selected }))
}
