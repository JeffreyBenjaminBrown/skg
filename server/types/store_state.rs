//! Identities and atomically published state for the selected graph corpus.
//!
//! A graph snapshot, its generation, and the exact disk-byte manifest which
//! selected it describe one fact.  They therefore live in one ArcSwap value;
//! readers must not combine a graph from one publication with bookkeeping
//! from another.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::tantivy::background_writer::TantivyGeneration;
use crate::types::misc::ID;

use std::collections::{BTreeMap, BTreeSet};
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;

use serde::{Deserialize, Serialize};
use tantivy::Searcher;

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct GraphGeneration (u64);

impl GraphGeneration {
  pub const INITIAL : Self = Self (1);

  pub fn get (self) -> u64 { self . 0 }

  pub fn successor (self) -> Self {
    Self (self . 0 . checked_add (1)
      . expect ("graph generation exhausted u64")) }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct ManifestRevision (u64);

impl ManifestRevision {
  pub const INITIAL : Self = Self (1);

  pub fn get (self) -> u64 { self . 0 }

  pub fn successor (self) -> Self {
    Self (self . 0 . checked_add (1)
      . expect ("manifest revision exhausted u64")) }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct PathDigest ([u8; 32]);

impl PathDigest {
  pub fn of_bytes (bytes : &[u8]) -> Self {
    Self (*blake3::hash (bytes) . as_bytes ()) }

  pub fn to_hex (self) -> String {
    blake3::Hash::from_bytes (self . 0) . to_hex () . to_string () }
}

pub type SelectedPathManifest = BTreeMap<PathBuf, PathDigest>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SelectedPathValue {
  Present (PathDigest),
  Absent, }

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PathIndexState {
  /// Initialization and explicit synchronous rebuilds have no background
  /// generation; ordinary transitions name the exact committed generation.
  Acknowledged { tantivy_generation : Option<TantivyGeneration> },
  SelectedAwaitingIndex { tantivy_generation : TantivyGeneration }, }

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SelectedPathOutcome {
  pub value            : SelectedPathValue,
  pub graph_generation : GraphGeneration,
  pub index_state      : PathIndexState, }

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StoreHealth {
  Healthy,
  Poisoned (String), }

/// One immutable publication.  `Deref` preserves the graph handle's familiar
/// read API, but callers which need generational correctness retain the whole
/// selected state rather than extracting the graph alone.
#[derive(Clone, Debug)]
pub struct SelectedStoreState {
  pub graph             : Arc<InRustGraph>,
  /// Absent only in graph-only fixture/build state. Live publications require it.
  pub searcher          : Option<Searcher>,
  pub graph_generation  : GraphGeneration,
  pub manifest_revision : ManifestRevision,
  pub manifest          : Arc<SelectedPathManifest>,
  pub path_outcomes     : BTreeMap<PathBuf, SelectedPathOutcome>,
  pub cyclic_roots      : BTreeSet<ID>,

  pub tantivy_health    : StoreHealth, }

/// Semantic proof inputs outlive rendering without retaining index readers.
/// Both potentially large values are shared with their original publication.
#[derive(Clone, Debug)]
pub struct SelectedGraphBase {
  pub graph             : Arc<InRustGraph>,
  pub graph_generation  : GraphGeneration,
  pub manifest_revision : ManifestRevision,
  pub manifest          : Arc<SelectedPathManifest>, }

impl SelectedStoreState {
  pub fn graph_base
  ( &self,
  ) -> SelectedGraphBase {
    SelectedGraphBase {
      graph: self . graph . clone (),
      graph_generation: self . graph_generation,
      manifest_revision: self . manifest_revision,
      manifest: self . manifest . clone (), } }

  pub fn initial (
    graph    : InRustGraph,
    manifest : SelectedPathManifest,
  ) -> Self {
    let graph_generation = GraphGeneration::INITIAL;
    let path_outcomes = manifest . iter () . map ( |(path, digest)|
      (path . clone (), SelectedPathOutcome {
        value: SelectedPathValue::Present (*digest),
        graph_generation,
        index_state: PathIndexState::Acknowledged {
          tantivy_generation: None },
      })) . collect ();
    Self {
      graph: Arc::new (graph),
      searcher: None,
      graph_generation,
      manifest_revision: ManifestRevision::INITIAL,
      manifest: Arc::new (manifest),
      path_outcomes,
      cyclic_roots: BTreeSet::new (),

      tantivy_health: StoreHealth::Healthy, } }

  pub fn with_searcher (
    mut self,
    searcher : Searcher,
  ) -> Self {
    self . searcher = Some (searcher);
    self }

  pub fn with_graph_preserving_disk_selection (
    &self,
    graph : InRustGraph,
  ) -> Self {
    let mut next = self . clone ();
    next . graph = Arc::new (graph);
    next . searcher = None;
    next . graph_generation = self . graph_generation . successor ();
    next }

  /// Select byte-different disk which folds to the existing semantic graph.
  /// Derived stores and graph generation stay fixed; only the exact selected
  /// byte authority advances.
  pub fn with_semantically_equal_manifest (
    &self,
    manifest : SelectedPathManifest,
  ) -> Self {
    let mut next = self . clone ();
    next . manifest_revision = self . manifest_revision . successor ();
    next . manifest = Arc::new (manifest);
    next
  }

  pub fn with_acknowledged_rebuild (
    &self,
    graph    : InRustGraph,
    manifest : SelectedPathManifest,
  ) -> Self {
    let graph_generation = self . graph_generation . successor ();
    let path_outcomes = manifest . iter () . map ( |(path, digest)|
      (path . clone (), SelectedPathOutcome {
        value: SelectedPathValue::Present (*digest),
        graph_generation,
        index_state: PathIndexState::Acknowledged {
          tantivy_generation: None },
      })) . collect ();
    Self {
      graph: Arc::new (graph),
      searcher: None,
      graph_generation,
      manifest_revision: self . manifest_revision . successor (),
      manifest: Arc::new (manifest),
      path_outcomes,
      cyclic_roots: self . cyclic_roots . clone (),

      tantivy_health: StoreHealth::Healthy, } }

  pub fn with_selected_transition (
    &self,
    graph              : InRustGraph,
    manifest           : SelectedPathManifest,
    tantivy_generation : TantivyGeneration,
  ) -> Self {
    let graph_generation = self . graph_generation . successor ();
    let mut path_outcomes = self . path_outcomes . clone ();
    let all_paths : BTreeSet<PathBuf> = self . manifest . keys () . cloned ()
      . chain (manifest . keys () . cloned ())
      . collect ();
    for path in all_paths {
      let before = self . manifest . get (&path);
      let after = manifest . get (&path);
      if before == after { continue; }
      path_outcomes . insert (path, SelectedPathOutcome {
        value: after . copied ()
          . map (SelectedPathValue::Present)
          . unwrap_or (SelectedPathValue::Absent),
        graph_generation,
        index_state: PathIndexState::SelectedAwaitingIndex {
          tantivy_generation }, }); }
    Self {
      graph: Arc::new (graph),
      searcher: None,
      graph_generation,
      manifest_revision: self . manifest_revision . successor (),
      manifest: Arc::new (manifest),
      path_outcomes,
      cyclic_roots: self . cyclic_roots . clone (),

      tantivy_health: self . tantivy_health . clone (), } }

  pub fn with_tantivy_terminal (
    &self,
    generation : TantivyGeneration,
    failure    : Option<String>,
  ) -> Self {
    let mut next = self . clone ();
    for outcome in next . path_outcomes . values_mut () {
      if outcome . index_state == (PathIndexState::SelectedAwaitingIndex {
           tantivy_generation: generation })
      {
        if failure . is_none () {
          outcome . index_state = PathIndexState::Acknowledged {
            tantivy_generation: Some (generation) }; }
      }}
    if let Some (reason) = failure {
      next . tantivy_health = StoreHealth::Poisoned (reason); }
    next }



  pub fn with_tantivy_poisoned (&self, reason : String) -> Self {
    let mut next = self . clone ();
    next . tantivy_health = StoreHealth::Poisoned (reason);
    next }

  pub fn with_cyclic_roots (
    &self,
    cyclic_roots : BTreeSet<ID>,
  ) -> Self {
    let mut next = self . clone ();
    next . cyclic_roots = cyclic_roots;
    next
  }
}

impl Deref for SelectedStoreState {
  type Target = InRustGraph;

  fn deref (&self) -> &Self::Target { &self . graph }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn initial_manifest_and_graph_share_one_generation () {
    let path = PathBuf::from ("/source/n.skg");
    let digest = PathDigest::of_bytes (b"pid: n\n");
    let state = SelectedStoreState::initial (
      InRustGraph::new (),
      BTreeMap::from ([(path . clone (), digest)]));
    assert_eq! (state . graph_generation, GraphGeneration::INITIAL);
    assert_eq! (state . manifest_revision, ManifestRevision::INITIAL);
    assert_eq! (state . manifest . get (&path), Some (&digest));
    assert_eq! (
      state . path_outcomes . get (&path) . unwrap () . graph_generation,
      state . graph_generation);
    assert_eq! (digest . to_hex () . len (), 64); }

  #[test]
  fn semantic_noop_advances_only_manifest_revision () {
    let old_path = PathBuf::from ("/source/n.skg");
    let new_path = PathBuf::from ("/source/m.skg");
    let state = SelectedStoreState::initial (
      InRustGraph::new (),
      BTreeMap::from ([(old_path, PathDigest::of_bytes (b"old"))]));
    let next = state . with_semantically_equal_manifest (
      BTreeMap::from ([(new_path, PathDigest::of_bytes (b"new"))]));
    assert_eq! (next . graph_generation, state . graph_generation);
    assert_eq! (
      next . manifest_revision, state . manifest_revision . successor ());
    assert! (Arc::ptr_eq (&next . graph, &state . graph));
  }
}
