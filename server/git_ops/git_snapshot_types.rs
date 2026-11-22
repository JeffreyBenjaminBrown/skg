//! The types of a graph read from git ('git_snapshot.rs'):
//! 'GraphSnapshot' and which tree it was read from ('GitSnapshotKind').

use crate::types::misc::ID;
use crate::types::nodes::complete::Graphnode;

use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GitSnapshotKind {
  Head,
  Worktree,
  /// A tree of the skgrepo's gitrepo, which no commit need hold.
  Tree (git2::Oid),
}

#[derive(Clone, Debug, Default)]
pub struct GraphSnapshot {
  /// One entry per telescope (keyed by pid, sections folded).
  pub nodes     : HashMap<ID, Graphnode>,
}
