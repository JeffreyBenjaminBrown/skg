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
  SourceFile,
  selected_direct_source_files,
  selected_path_digest_manifest,
  select_source_file_candidates_for_pid,
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
use crate::telescope::fold::fold_telescope_collecting_warnings;
use crate::telescope::invariants::{
  TelescopeViolation,
  validate_all_telescopes,
};
use crate::telescope::types::Telescope;

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
  /// Load-time warnings which cannot be recovered from the folded graph
  /// alone (for example ignored foreign PID collisions).
  pub load_violations  : Vec<(ID, TelescopeViolation)>,
  pub disk_fence       : CandidateDiskFence,
}

/// Exact disk authority rechecked at the point of selection.  A complete
/// observation owns the whole selected corpus; an explicit partial reload
/// owns every possible direct source path for only its resolved telescopes.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CandidateDiskFence {
  Complete,
  Targeted (BTreeMap<PathBuf, Option<PathDigest>>),
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
  match observe_complete_disk_inner (
      config, selected, covered_sequence, false)
  {
    Ok (result) => result,
    Err (error) => DiskObservation::Invalid {
      details: vec![error],
    },
  }
}

/// Observe the complete disk for an already-authorized maintenance origin.
/// Unlike unsolicited observation, exact byte equality and semantic equality
/// still return a candidate: the incident must finalize its archive and reach
/// one durable terminal outcome even when the external operation was a no-op.
pub fn observe_complete_maintenance_disk (
  config            : &SkgConfig,
  selected          : &SelectedStoreState,
  covered_sequence  : ObservationSequence,
) -> DiskObservation {
  match observe_complete_disk_inner (
      config, selected, covered_sequence, true)
  {
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
  retain_noop_candidate : bool,
) -> Result<DiskObservation, String> {
  let captured = capture_selected_corpus (config)?;
  if captured . manifest == selected . manifest && !retain_noop_candidate {
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
    if retain_noop_candidate {
      return Ok (DiskObservation::Valid (complete_candidate (
        config, selected, covered_sequence,
        captured . manifest, captured . selected_bytes,
        Arc::new (graph), violations))); }
    return Ok (DiskObservation::SemanticallyEqual {
      manifest: captured . manifest,
      selected_bytes: captured . selected_bytes,
    }); }

  Ok (DiskObservation::Valid (complete_candidate (
    config, selected, covered_sequence,
    captured . manifest, captured . selected_bytes, Arc::new (graph),
    violations)))
}

fn complete_candidate (
  config           : &SkgConfig,
  selected         : &SelectedStoreState,
  covered_sequence : ObservationSequence,
  manifest         : SelectedPathManifest,
  selected_bytes   : BTreeMap<PathBuf, Vec<u8>>,
  graph            : Arc<InRustGraph>,
  load_violations  : Vec<(ID, TelescopeViolation)>,
) -> Arc<ObservedDiskCandidate> {
  let before_nodes = nodes_by_pid (nodecompletes_from_graph (&selected . graph));
  let after_nodes = nodes_by_pid (nodecompletes_from_graph (&graph));

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
  let mut all_violations = validate_all_telescopes (config, &graph);
  all_violations . extend (load_violations . clone ());
  all_violations . sort_by (|left, right| left . 0 . cmp (&right . 0));
  let warnings = all_violations . iter () . map (|(pid, warning)|
    format! ("{}: {}", pid, warning)) . collect ();
  Arc::new (ObservedDiskCandidate {
    summary,
    config_identity: config_identity (config),
    source_catalog_blake3: source_catalog_blake3 (config),
    manifest,
    base_graph: selected . graph . clone (),
    graph,
    definitions,
    added_primary_ids: changes . added,
    deleted_primary_ids: changes . deleted,
    modified_primary_ids: changes . modified,
    evidence,
    selected_bytes,
    warnings,
    load_violations,
    disk_fence: CandidateDiskFence::Complete,
  })
}

/// Observe just the named primary telescopes while retaining every unrelated
/// G0 node and selected-path digest.  This is the candidate builder for an
/// explicit path/ID reload: unrelated worktree changes remain pending rather
/// than entering the authorized transition accidentally.
pub fn observe_targeted_disk (
  config           : &SkgConfig,
  selected         : &SelectedStoreState,
  covered_sequence : ObservationSequence,
  targets          : &BTreeSet<ID>,
) -> DiskObservation {
  match observe_targeted_disk_inner (
      config, selected, covered_sequence, targets)
  {
    Ok (result) => result,
    Err (error) => DiskObservation::Invalid { details: vec![error] },
  }
}

fn observe_targeted_disk_inner (
  config           : &SkgConfig,
  selected         : &SelectedStoreState,
  covered_sequence : ObservationSequence,
  targets          : &BTreeSet<ID>,
) -> Result<DiskObservation, String> {
  // An explicit request containing only unknown IDs deliberately resolves to
  // an empty target set.  It still gets a no-op candidate so its durable
  // archive can reach a terminal result carrying per-ID rejection reasons.
  let captured = capture_targeted_telescopes (config, targets)?;
  let mut manifest = selected . manifest . clone ();
  for target in targets {
    for source in config . sources . values () {
      manifest . remove (&source . path . join (format! ("{}.skg", target))); }
  }
  for (path, bytes) in &captured . selected_bytes {
    manifest . insert (path . clone (), PathDigest::of_bytes (bytes)); }
  let before_nodes = nodes_by_pid (nodecompletes_from_graph (&selected . graph));
  let mut after_nodes = before_nodes . clone ();
  let stale_target_ids : BTreeSet<ID> = targets . iter () . flat_map (|pid|
    before_nodes . get (pid) . into_iter ()
      . flat_map (|node| node . all_ids () . cloned ())) . collect ();
  let mut replacement_extra_ids = HashMap::new ();
  for (pid, sections) in &captured . sections_by_pid {
    for (_, node) in sections {
      for extra in &node . extra_ids {
        replacement_extra_ids . insert (extra . clone (), pid . clone ()); }
    }
  }
  let resolve = |id : &ID| -> ID {
    replacement_extra_ids . get (id) . cloned ()
      . or_else (|| if stale_target_ids . contains (id) {
        None
      } else {
        selected . graph . pid_of (id)
      })
      . unwrap_or_else (|| id . clone ())
  };
  let mut warnings = captured . warnings;
  for pid in targets {
    after_nodes . remove (pid);
    let Some (sections) = captured . sections_by_pid . get (pid) else {
      continue; };
    let telescope = Telescope::try_new (
      pid . clone (), sections . clone (), config)
      . map_err (|error| format! ("telescope {} is invalid: {}", pid, error))?;
    let (node, fold_warnings) = fold_telescope_collecting_warnings (
      telescope, &resolve)
      . map_err (|error| format! ("telescope {} cannot fold: {}", pid, error))?;
    warnings . extend (fold_warnings . into_iter ()
      . map (|warning| format! ("{}: {}", pid, warning)));
    after_nodes . insert (pid . clone (), node);
  }
  let extra_ids_changed = targets . iter () . any (|pid| {
    let before = before_nodes . get (pid)
      . map (|node| &node . extra_ids[..]) . unwrap_or (&[]);
    let after = after_nodes . get (pid)
      . map (|node| &node . extra_ids[..]) . unwrap_or (&[]);
    before != after
  });
  if extra_ids_changed {
    // Extra IDs can be authored as relationship anchors anywhere in the
    // corpus.  Their replacement therefore keeps the established complete-
    // corpus fallback instead of pretending an untouched normalized node is
    // enough to recompute every alias resolution.
    return observe_complete_disk_inner (
      config, selected, covered_sequence, true);
  }
  let after_values : Vec<NodeComplete> =
    after_nodes . values () . cloned () . collect ();
  let conflicts = distinct_id_claim_conflicts (&after_values);
  if !conflicts . is_empty () {
    return Ok (DiskObservation::Invalid { details: vec![format! (
      "targeted disk candidate has IDs claimed by multiple nodes: {:?}",
      conflicts)] }); }
  let graph = InRustGraph::from_nodecompletes (&after_values);
  if let Err (error) = error_unless_override_invariants_hold (config, &graph) {
    return Ok (DiskObservation::Invalid { details: vec![format! (
      "targeted disk candidate violates override invariants: {}", error)] }); }
  if targeted_path_fence (config, targets)? != captured . path_fence {
    return Ok (DiskObservation::Unstable { details: vec![
      "targeted disk bytes changed while the candidate was being folded"
        . into (),
    ] }); }

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
    manifest,
    base_graph: selected . graph . clone (),
    graph: Arc::new (graph),
    definitions,
    added_primary_ids: changes . added,
    deleted_primary_ids: changes . deleted,
    modified_primary_ids: changes . modified,
    evidence,
    selected_bytes: captured . selected_bytes,
    warnings,
    load_violations: Vec::new (),
    disk_fence: CandidateDiskFence::Targeted (captured . path_fence),
  })))
}

struct CapturedTargetedTelescopes {
  sections_by_pid : HashMap<ID, Vec<(SourceName, NodeFS)>>,
  selected_bytes  : BTreeMap<PathBuf, Vec<u8>>,
  path_fence      : BTreeMap<PathBuf, Option<PathDigest>>,
  warnings        : Vec<String>,
}

fn capture_targeted_telescopes (
  config  : &SkgConfig,
  targets : &BTreeSet<ID>,
) -> Result<CapturedTargetedTelescopes, String> {
  let mut sections_by_pid = HashMap::new ();
  let mut selected_bytes = BTreeMap::new ();
  let mut path_fence = BTreeMap::new ();
  let mut warnings = Vec::new ();
  for pid in targets {
    let mut candidates = Vec::new ();
    let mut path_bytes = BTreeMap::new ();
    for source_name in config . ordered_sources () {
      let source = config . sources . get (&source_name)
        . expect ("ordered source exists");
      let path = source . path . join (format! ("{}.skg", pid));
      match fs::symlink_metadata (&path) {
        Ok (metadata) if metadata . file_type () . is_file () => {
          let bytes = fs::read (&path) . map_err (|error| format! (
            "could not read {}: {}", path . display (), error))?;
          path_fence . insert (
            path . clone (), Some (PathDigest::of_bytes (&bytes)));
          path_bytes . insert (path . clone (), bytes);
          candidates . push (SourceFile { source: source_name, path });
        }
        Ok (_) => { path_fence . insert (path, None); }
        Err (error) if error . kind () == std::io::ErrorKind::NotFound => {
          path_fence . insert (path, None); }
        Err (error) => return Err (format! (
          "could not inspect {}: {}", path . display (), error)),
      }
    }
    let (selected_files, collision) =
      select_source_file_candidates_for_pid (config, pid, candidates);
    if let Some (collision) = collision {
      warnings . push (format! (
        "WARNING: owned telescope {} was retained; ignored same-ID non-owned file(s): {}",
        collision . pid,
        collision . losers . iter ()
          . map (|file| file . path . display () . to_string ())
          . collect::<Vec<_>> () . join (", ")));
    }
    for file in selected_files {
      let bytes = path_bytes . remove (&file . path)
        . expect ("selected target path was captured");
      let node = parse_nodefs_bytes (&bytes, &file . path)
        . map_err (|error| format! (
          "could not parse {}: {}", file . path . display (), error))?;
      validate_pid_matches_filename (&node, &file . path)
        . map_err (|error| format! (
          "invalid {}: {}", file . path . display (), error))?;
      selected_bytes . insert (file . path . clone (), bytes);
      sections_by_pid . entry (pid . clone ()) . or_insert_with (Vec::new)
        . push ((file . source, node));
    }
  }
  Ok (CapturedTargetedTelescopes {
    sections_by_pid, selected_bytes, path_fence, warnings,
  })
}

fn targeted_path_fence (
  config  : &SkgConfig,
  targets : &BTreeSet<ID>,
) -> Result<BTreeMap<PathBuf, Option<PathDigest>>, String> {
  let mut result = BTreeMap::new ();
  for pid in targets {
    for source in config . sources . values () {
      let path = source . path . join (format! ("{}.skg", pid));
      match fs::symlink_metadata (&path) {
        Ok (metadata) if metadata . file_type () . is_file () => {
          let bytes = fs::read (&path) . map_err (|error| format! (
            "could not read {}: {}", path . display (), error))?;
          result . insert (path, Some (PathDigest::of_bytes (&bytes)));
        }
        Ok (_) => { result . insert (path, None); }
        Err (error) if error . kind () == std::io::ErrorKind::NotFound => {
          result . insert (path, None); }
        Err (error) => return Err (format! (
          "could not inspect {}: {}", path . display (), error)),
      }
    }
  }
  Ok (result)
}

pub fn revalidate_candidate (
  config    : &SkgConfig,
  candidate : &ObservedDiskCandidate,
) -> Result<(), String> {
  if config_identity (config) != candidate . config_identity {
    return Err ("candidate belongs to a different configuration" . into ()); }
  if source_catalog_blake3 (config) != candidate . source_catalog_blake3 {
    return Err ("candidate source catalog no longer matches" . into ()); }
  match &candidate . disk_fence {
    CandidateDiskFence::Complete => {
      let actual = selected_path_digest_manifest (config)
        . map_err (|error| format! (
          "candidate revalidation failed: {}", error))?;
      if actual != candidate . manifest {
        return Err ("candidate disk bytes no longer match its exact manifest"
          . into ()); }
    }
    CandidateDiskFence::Targeted (expected) => {
      let targets : BTreeSet<ID> = expected . keys () . filter_map (|path|
        path . file_stem () . and_then (|stem| stem . to_str ())
          . map (ID::from)) . collect ();
      let actual = targeted_path_fence (config, &targets)?;
      if &actual != expected {
        return Err (
          "candidate target bytes no longer match its exact path fence"
            . into ()); }
    }
  }
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_with_manifest;
  use crate::types::misc::{SkgfileSource, SourceCatalog};
  use tempfile::tempdir;

  #[test]
  fn targeted_candidate_changes_only_named_telescopes_and_fences_their_paths () {
    let temporary = tempdir () . unwrap ();
    let source_path = temporary . path () . join ("owned");
    fs::create_dir (&source_path) . unwrap ();
    let a_path = source_path . join ("A.skg");
    let b_path = source_path . join ("B.skg");
    fs::write (&a_path, "pid: A\ntitle: old A\n") . unwrap ();
    fs::write (&b_path, "pid: B\ntitle: old B\n") . unwrap ();
    let source_name = SourceName::from ("owned");
    let mut sources = SourceCatalog::default ();
    sources . insert (source_name . clone (), SkgfileSource {
      name: source_name, abbreviation: None,
      path: source_path, user_owns_it: true,
    });
    let config = SkgConfig::dummyFromSources (
      sources . iter () . map (|(name, source)|
        (name . clone (), source . clone ())) . collect ());
    let loaded = read_all_skg_files_with_manifest (&config) . unwrap ();
    let selected = SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (&loaded . nodes), loaded . manifest);

    fs::write (&a_path, "pid: A\ntitle: new A\n") . unwrap ();
    fs::write (&b_path, "pid: B\ntitle: unrelated disk B\n") . unwrap ();
    let observation = observe_targeted_disk (
      &config, &selected, ObservationSequence::INITIAL,
      &BTreeSet::from ([ID::from ("A")]));
    let DiskObservation::Valid (candidate) = observation else {
      panic! ("targeted semantic change did not produce a candidate"); };
    assert_eq! (candidate . graph . nodes . get (&ID::from ("A"))
      . unwrap () . title, "new A");
    assert_eq! (candidate . graph . nodes . get (&ID::from ("B"))
      . unwrap () . title, "old B");
    assert_eq! (candidate . manifest . get (&a_path),
      Some (&PathDigest::of_bytes (b"pid: A\ntitle: new A\n")));
    assert_eq! (candidate . manifest . get (&b_path),
      selected . manifest . get (&b_path));
    assert! (matches! (
      candidate . disk_fence, CandidateDiskFence::Targeted (_)));
    assert! (revalidate_candidate (&config, &candidate) . is_ok ());

    fs::write (&a_path, "pid: A\ntitle: newer A\n") . unwrap ();
    assert! (revalidate_candidate (&config, &candidate) . is_err ());
  }

  #[test]
  fn targeted_extra_id_change_uses_the_complete_corpus_fallback () {
    let temporary = tempdir () . unwrap ();
    let source_path = temporary . path () . join ("owned");
    fs::create_dir (&source_path) . unwrap ();
    let a_path = source_path . join ("A.skg");
    let b_path = source_path . join ("B.skg");
    fs::write (&a_path, "pid: A\ntitle: old A\n") . unwrap ();
    fs::write (&b_path, "pid: B\ntitle: old B\n") . unwrap ();
    let source_name = SourceName::from ("owned");
    let mut entries = HashMap::new ();
    entries . insert (source_name . clone (), SkgfileSource {
      name: source_name, abbreviation: None,
      path: source_path, user_owns_it: true,
    });
    let config = SkgConfig::dummyFromSources (entries);
    let loaded = read_all_skg_files_with_manifest (&config) . unwrap ();
    let selected = SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (&loaded . nodes), loaded . manifest);

    fs::write (&a_path,
      "pid: A\ntitle: new A\nextra_ids:\n- new-alias\n") . unwrap ();
    fs::write (&b_path, "pid: B\ntitle: newly observed B\n") . unwrap ();
    let observation = observe_targeted_disk (
      &config, &selected, ObservationSequence::INITIAL,
      &BTreeSet::from ([ID::from ("A")]));
    let DiskObservation::Valid (candidate) = observation else {
      panic! ("extra-ID replacement did not produce a candidate"); };
    assert! (matches! (candidate . disk_fence, CandidateDiskFence::Complete));
    assert_eq! (candidate . graph . nodes . get (&ID::from ("B"))
      . unwrap () . title, "newly observed B");
  }

  #[test]
  fn targeted_semantic_no_op_still_produces_an_explicit_candidate () {
    let temporary = tempdir () . unwrap ();
    let source_path = temporary . path () . join ("owned");
    fs::create_dir (&source_path) . unwrap ();
    let a_path = source_path . join ("A.skg");
    fs::write (&a_path, "pid: A\ntitle: unchanged\n") . unwrap ();
    let source_name = SourceName::from ("owned");
    let mut entries = HashMap::new ();
    entries . insert (source_name . clone (), SkgfileSource {
      name: source_name, abbreviation: None,
      path: source_path, user_owns_it: true,
    });
    let config = SkgConfig::dummyFromSources (entries);
    let loaded = read_all_skg_files_with_manifest (&config) . unwrap ();
    let selected = SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (&loaded . nodes), loaded . manifest);

    let empty = observe_targeted_disk (
      &config, &selected, ObservationSequence::INITIAL, &BTreeSet::new ());
    let DiskObservation::Valid (empty) = empty else {
      panic! ("empty resolved target set did not produce a no-op candidate"); };
    assert! (empty . definitions . is_empty ());
    assert_eq! (empty . manifest, selected . manifest);

    // The explicit operation still needs one candidate identity and the
    // archive/presentation lifecycle even when the selected bytes are exact.
    let exact = observe_targeted_disk (
      &config, &selected, ObservationSequence::INITIAL,
      &BTreeSet::from ([ID::from ("A")]));
    let DiskObservation::Valid (exact) = exact else {
      panic! ("byte-identical explicit target did not produce a candidate"); };
    assert! (exact . definitions . is_empty ());
    assert! (exact . summary . changed_primary_ids . is_empty ());
    assert_eq! (exact . manifest, selected . manifest);

    fs::write (&a_path, "pid: A\n\ntitle: unchanged\n") . unwrap ();
    let reformatted = observe_targeted_disk (
      &config, &selected, ObservationSequence::INITIAL,
      &BTreeSet::from ([ID::from ("A")]));
    let DiskObservation::Valid (reformatted) = reformatted else {
      panic! ("semantic no-op explicit target did not produce a candidate"); };
    assert! (reformatted . definitions . is_empty ());
    assert! (reformatted . summary . changed_primary_ids . is_empty ());
    assert_ne! (reformatted . manifest, selected . manifest);
  }

  #[test]
  fn complete_maintenance_observation_retains_exact_and_semantic_noops () {
    let temporary = tempdir () . unwrap ();
    let source_path = temporary . path () . join ("owned");
    fs::create_dir (&source_path) . unwrap ();
    let a_path = source_path . join ("A.skg");
    fs::write (&a_path, "pid: A\ntitle: unchanged\n") . unwrap ();
    let source_name = SourceName::from ("owned");
    let mut entries = HashMap::new ();
    entries . insert (source_name . clone (), SkgfileSource {
      name: source_name, abbreviation: None,
      path: source_path, user_owns_it: true,
    });
    let config = SkgConfig::dummyFromSources (entries);
    let loaded = read_all_skg_files_with_manifest (&config) . unwrap ();
    let selected = SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (&loaded . nodes), loaded . manifest);

    let exact = observe_complete_maintenance_disk (
      &config, &selected, ObservationSequence::INITIAL);
    let DiskObservation::Valid (exact) = exact else {
      panic! ("maintenance byte equality did not produce a candidate"); };
    assert! (exact . definitions . is_empty ());
    assert_eq! (exact . manifest, selected . manifest);
    assert! (matches! (exact . disk_fence, CandidateDiskFence::Complete));

    fs::write (&a_path, "pid: A\n\ntitle: unchanged\n") . unwrap ();
    let reformatted = observe_complete_maintenance_disk (
      &config, &selected, ObservationSequence::INITIAL);
    let DiskObservation::Valid (reformatted) = reformatted else {
      panic! ("maintenance semantic equality did not produce a candidate"); };
    assert! (reformatted . definitions . is_empty ());
    assert! (reformatted . summary . changed_primary_ids . is_empty ());
    assert_ne! (reformatted . manifest, selected . manifest);
  }
}
