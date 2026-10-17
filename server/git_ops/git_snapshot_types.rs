use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Graphnode;

use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GitSnapshotKind {
  Head,
  Index,
  Worktree,
}

#[derive(Clone, Debug, Default)]
pub struct GraphSnapshot {
  /// One entry per TELESCOPE (keyed by pid, sections folded).
  pub nodes     : HashMap<ID, Graphnode>,
  /// id -> claiming pid -> skgrepos whose section for that pid
  /// claims the id (as its pid or among its extra_ids). One pid
  /// claiming an id from several skgrepos is the normal telescope
  /// shape; TWO pids claiming one id is the duplicate-ID VIOLATION
  /// the report warns about.
  pub id_claims : HashMap<ID, BTreeMap<ID, BTreeSet<SkgrepoName>>>,
}

impl GraphSnapshot {
  /// Repos holding any claim on this id, across claiming pids.
  pub fn skgrepos_claiming_skgid (
    &self,
    skgid : &ID,
  ) -> BTreeSet<SkgrepoName> {
    self . id_claims . get (skgid)
      . map ( |by_pid| by_pid . values () . flatten ()
              . cloned () . collect () )
      . unwrap_or_default () }
}
