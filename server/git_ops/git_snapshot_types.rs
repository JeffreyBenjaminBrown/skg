use crate::types::misc::ID;
use crate::types::nodes::complete::Graphnode;

use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GitSnapshotKind {
  Head,
  Worktree,
}

#[derive(Clone, Debug, Default)]
pub struct GraphSnapshot {
  /// One entry per TELESCOPE (keyed by pid, sections folded).
  pub nodes     : HashMap<ID, Graphnode>,
}
