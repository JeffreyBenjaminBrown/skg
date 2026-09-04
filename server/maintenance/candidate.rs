//! Immutable disk candidates built without touching any selected store.

use crate::dbs::filesystem::multiple_nodes::{
  distinct_id_claim_conflicts,
  fold_grouped_sections,
};
use crate::dbs::filesystem::one_node::{
  parse_nodefs_bytes,
  validate_pid_matches_filename,
};
use crate::dbs::filesystem::source_files::{
  selected_direct_source_files,
  selected_path_digest_manifest,
};
use crate::dbs::in_rust_graph::{
  InRustGraph,
  override_invariants::error_unless_override_invariants_hold,
};
use crate::maintenance::{
  CandidateId,
  CandidateSummary,
  ObservationSequence,
};
use crate::save::nodecompletes_from_graph;
use crate::types::misc::{ID, MSV, MemberAtSource, SkgConfig, SourceName};
use crate::types::nodes::complete::{FileProperty, NodeComplete};
use crate::types::nodes::fs::NodeFS;
use crate::types::save::{DefineNode, DeleteNode, SaveNode};
use crate::types::store_state::{
  PathDigest,
  SelectedPathManifest,
  SelectedStoreState,
};

use serde::{Deserialize, Serialize};
use similar::TextDiff;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct ObservedDiskCandidate {
  pub summary          : CandidateSummary,
  pub config_identity  : PathBuf,
  pub source_catalog_blake3 : String,
  pub manifest         : SelectedPathManifest,
  /// Immutable G0 needed after G1 publication for exact two-generation view
  /// impact resolution.  Durable recovery uses the evidence DTO projection;
  /// this field intentionally remains runtime-only.
  pub base_graph       : Arc<InRustGraph>,
  pub graph            : Arc<InRustGraph>,
  pub definitions      : Vec<DefineNode>,
  pub added_primary_ids : BTreeSet<ID>,
  pub deleted_primary_ids : BTreeSet<ID>,
  pub modified_primary_ids : BTreeSet<ID>,
  pub evidence         : BTreeMap<ID, SemanticChangeEvidence>,
  pub selected_bytes   : BTreeMap<PathBuf, Vec<u8>>,
  pub warnings         : Vec<String>,
}

#[derive(Clone, Debug)]
pub enum DiskObservation {
  ByteEquivalent,
  SemanticallyEqual {
    manifest       : SelectedPathManifest,
    selected_bytes : BTreeMap<PathBuf, Vec<u8>>,
  },
  Valid (Arc<ObservedDiskCandidate>),
  Invalid {
    details : Vec<String>,
  },
  Unstable {
    details : Vec<String>,
  },
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SemanticChangeEvidence {
  pub before : Option<SemanticNodeEvidence>,
  pub after  : Option<SemanticNodeEvidence>,
  pub diff   : String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SemanticNodeEvidence {
  pub format_version : u32,
  pub pid             : String,
  pub source          : String,
  pub title           : String,
  pub ugly_telescope  : bool,
  pub extra_ids       : Vec<String>,
  pub body            : Option<String>,
  pub aliases         : Option<Vec<EvidenceMember>>,
  pub contains        : Vec<EvidenceMember>,
  pub subscribes_to   : Option<Vec<EvidenceMember>>,
  pub hides_from_its_subscriptions : Option<Vec<EvidenceMember>>,
  pub overrides_view_of : Option<Vec<EvidenceMember>>,
  pub file_properties : Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct EvidenceMember {
  pub source : String,
  pub value  : String,
}

pub fn observe_complete_disk (
  config            : &SkgConfig,
  selected          : &SelectedStoreState,
  covered_sequence  : ObservationSequence,
) -> DiskObservation {
  match observe_complete_disk_inner (config, selected, covered_sequence) {
    Ok (result) => result,
    Err (error) => DiskObservation::Invalid {
      details: vec![error],
    },
  }
}

fn observe_complete_disk_inner (
  config            : &SkgConfig,
  selected          : &SelectedStoreState,
  covered_sequence  : ObservationSequence,
) -> Result<DiskObservation, String> {
  let captured = capture_selected_corpus (config)?;
  if captured . manifest == selected . manifest {
    return Ok (DiskObservation::ByteEquivalent); }

  let (nodes, violations) = fold_grouped_sections (
    captured . sections_by_pid,
    captured . pid_order,
    config)
    . map_err (|error| format! ("disk corpus does not fold: {}", error))?;
  let conflicts = distinct_id_claim_conflicts (&nodes);
  if !conflicts . is_empty () {
    return Ok (DiskObservation::Invalid { details: vec![format! (
      "disk corpus has IDs claimed by multiple nodes: {:?}", conflicts)] }); }
  let graph = InRustGraph::from_nodecompletes (&nodes);
  if let Err (error) = error_unless_override_invariants_hold (config, &graph) {
    return Ok (DiskObservation::Invalid { details: vec![format! (
      "disk corpus violates override invariants: {}", error)] }); }

  let final_manifest = selected_path_digest_manifest (config)
    . map_err (|error| format! ("final disk stability read failed: {}", error))?;
  if final_manifest != captured . manifest {
    return Ok (DiskObservation::Unstable { details: vec![
      "selected disk bytes changed while the candidate was being folded"
        . into (),
    ] }); }

  let before_nodes = nodes_by_pid (nodecompletes_from_graph (&selected . graph));
  let after_nodes = nodes_by_pid (nodes);
  if before_nodes == after_nodes {
    return Ok (DiskObservation::SemanticallyEqual {
      manifest: captured . manifest,
      selected_bytes: captured . selected_bytes,
    }); }

  let changes = classify_changes (&before_nodes, &after_nodes);
  let changed_primary_ids : Vec<String> = changes . all () . into_iter ()
    . map (|pid| pid . to_string ()) . collect ();
  let evidence = changes . all () . into_iter () . map (|pid| {
    let before = before_nodes . get (&pid) . map (SemanticNodeEvidence::from);
    let after = after_nodes . get (&pid) . map (SemanticNodeEvidence::from);
    let before_yaml = semantic_yaml (before . as_ref ());
    let after_yaml = semantic_yaml (after . as_ref ());
    let diff = TextDiff::from_lines (&before_yaml, &after_yaml)
      . unified_diff () . context_radius (3)
      . header ("before.semantic.yaml", "after.semantic.yaml")
      . to_string ();
    (pid, SemanticChangeEvidence { before, after, diff })
  }) . collect ();
  let definitions = graph_delta (&before_nodes, &after_nodes);
  let summary = CandidateSummary {
    id: CandidateId::new (),
    base_graph_generation: selected . graph_generation,
    base_manifest_revision: selected . manifest_revision,
    covered_sequence,
    changed_primary_ids,
  };
  Ok (DiskObservation::Valid (Arc::new (ObservedDiskCandidate {
    summary,
    config_identity: config_identity (config),
    source_catalog_blake3: source_catalog_blake3 (config),
    manifest: captured . manifest,
    base_graph: selected . graph . clone (),
    graph: Arc::new (graph),
    definitions,
    added_primary_ids: changes . added,
    deleted_primary_ids: changes . deleted,
    modified_primary_ids: changes . modified,
    evidence,
    selected_bytes: captured . selected_bytes,
    warnings: violations . into_iter () . map (|(pid, warning)|
      format! ("{}: {}", pid, warning)) . collect (),
  })))
}

pub fn revalidate_candidate (
  config    : &SkgConfig,
  candidate : &ObservedDiskCandidate,
) -> Result<(), String> {
  if config_identity (config) != candidate . config_identity {
    return Err ("candidate belongs to a different configuration" . into ()); }
  if source_catalog_blake3 (config) != candidate . source_catalog_blake3 {
    return Err ("candidate source catalog no longer matches" . into ()); }
  let actual = selected_path_digest_manifest (config)
    . map_err (|error| format! ("candidate revalidation failed: {}", error))?;
  if actual != candidate . manifest {
    return Err ("candidate disk bytes no longer match its exact manifest"
      . into ()); }
  Ok (( ))
}

pub fn config_identity (config : &SkgConfig) -> PathBuf {
  config . config_path . canonicalize ()
    . unwrap_or_else (|_| config . config_path . clone ())
}

pub fn source_catalog_blake3 (config : &SkgConfig) -> String {
  let bytes = serde_yaml::to_string (&config . sources)
    . expect ("source catalog serialization is infallible");
  blake3::hash (bytes . as_bytes ()) . to_hex () . to_string ()
}

struct CapturedCorpus {
  sections_by_pid : HashMap<ID, Vec<(SourceName, NodeFS)>>,
  pid_order       : Vec<ID>,
  manifest        : SelectedPathManifest,
  selected_bytes  : BTreeMap<PathBuf, Vec<u8>>,
}

fn capture_selected_corpus (
  config : &SkgConfig,
) -> Result<CapturedCorpus, String> {
  let selected = selected_direct_source_files (config)
    . map_err (|error| format! ("could not enumerate source corpus: {}", error))?;
  let mut sections_by_pid = HashMap::new ();
  let mut manifest = SelectedPathManifest::new ();
  let mut selected_bytes = BTreeMap::new ();
  for pid in &selected . pid_order {
    for file in selected . by_pid . get (pid) . into_iter () . flatten () {
      let bytes = fs::read (&file . path) . map_err (|error| format! (
        "could not read {}: {}", file . path . display (), error))?;
      let node = parse_nodefs_bytes (&bytes, &file . path)
        . map_err (|error| format! ("could not parse {}: {}",
          file . path . display (), error))?;
      validate_pid_matches_filename (&node, &file . path)
        . map_err (|error| format! ("invalid {}: {}",
          file . path . display (), error))?;
      manifest . insert (
        file . path . clone (), PathDigest::of_bytes (&bytes));
      selected_bytes . insert (file . path . clone (), bytes);
      sections_by_pid . entry (pid . clone ()) . or_insert_with (Vec::new)
        . push ((file . source . clone (), node));
    }
  }
  Ok (CapturedCorpus {
    sections_by_pid,
    pid_order: selected . pid_order,
    manifest,
    selected_bytes,
  })
}

#[derive(Default)]
struct ChangedNodes {
  added    : BTreeSet<ID>,
  deleted  : BTreeSet<ID>,
  modified : BTreeSet<ID>,
}

impl ChangedNodes {
  fn all (&self) -> BTreeSet<ID> {
    self . added . iter () . chain (&self . deleted)
      . chain (&self . modified) . cloned () . collect ()
  }
}

fn classify_changes (
  before : &BTreeMap<ID, NodeComplete>,
  after  : &BTreeMap<ID, NodeComplete>,
) -> ChangedNodes {
  let mut result = ChangedNodes::default ();
  for pid in before . keys () . chain (after . keys ()) {
    match (before . get (pid), after . get (pid)) {
      (None, Some (_)) => { result . added . insert (pid . clone ()); }
      (Some (_), None) => { result . deleted . insert (pid . clone ()); }
      (Some (old), Some (new)) if old != new => {
        result . modified . insert (pid . clone ()); }
      _ => {}
    }
  }
  result
}

fn graph_delta (
  before : &BTreeMap<ID, NodeComplete>,
  after  : &BTreeMap<ID, NodeComplete>,
) -> Vec<DefineNode> {
  let changes = classify_changes (before, after);
  changes . all () . into_iter () . filter_map (|pid|
    match after . get (&pid) {
      Some (node) => Some (DefineNode::Save (SaveNode (node . clone ()))),
      None => before . get (&pid) . map (|node|
        DefineNode::Delete (DeleteNode {
          id: pid,
          source: node . source . clone (),
        })),
    }) . collect ()
}

fn nodes_by_pid (
  nodes : Vec<NodeComplete>,
) -> BTreeMap<ID, NodeComplete> {
  nodes . into_iter () . map (|node| (node . pid . clone (), node))
    . collect ()
}

fn semantic_yaml (evidence : Option<&SemanticNodeEvidence>) -> String {
  evidence . map (|value| serde_yaml::to_string (value)
    . expect ("semantic evidence serialization is infallible"))
    . unwrap_or_default ()
}

fn evidence_members<T : ToString> (
  members : &[MemberAtSource<T>],
) -> Vec<EvidenceMember> {
  members . iter () . map (|member| EvidenceMember {
    source: member . source . to_string (),
    value: member . member . to_string (),
  }) . collect ()
}

fn evidence_msv<T : ToString> (
  values : &MSV<MemberAtSource<T>>,
) -> Option<Vec<EvidenceMember>> {
  match values {
    MSV::Unspecified => None,
    MSV::Specified (members) => Some (evidence_members (members)),
  }
}

impl From<&NodeComplete> for SemanticNodeEvidence {
  fn from (node : &NodeComplete) -> Self {
    Self {
      format_version: 1,
      pid: node . pid . to_string (),
      source: node . source . to_string (),
      title: node . title . clone (),
      ugly_telescope: node . ugly_telescope,
      extra_ids: node . extra_ids . iter () . map (ToString::to_string)
        . collect (),
      body: node . body . clone (),
      aliases: evidence_msv (&node . aliases),
      contains: evidence_members (&node . contains),
      subscribes_to: evidence_msv (&node . subscribes_to),
      hides_from_its_subscriptions:
        evidence_msv (&node . hides_from_its_subscriptions),
      overrides_view_of: evidence_msv (&node . overrides_view_of),
      file_properties: node . misc . iter () . map (|property| match property {
        FileProperty::Had_ID_Before_Import => "had-id-before-import",
        FileProperty::Was_Overloaded => "was-overloaded",
      } . to_string ()) . collect (),
    }
  }
}
