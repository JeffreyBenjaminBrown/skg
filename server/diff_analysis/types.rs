use crate::types::misc::{ID, SourceName};
use crate::types::nodes::complete::NodeComplete;

use std::collections::{BTreeSet, HashMap};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SnapshotKind {
  Head,
  Index,
  Worktree,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DiffSelection {
  pub include_staged   : bool,
  pub include_unstaged : bool,
}

#[derive(Clone, Debug)]
pub struct SnapshotPair {
  pub before : GraphSnapshot,
  pub after  : GraphSnapshot,
}

#[derive(Clone, Debug)]
pub struct ChangedSnapshotPair {
  pub pair          : SnapshotPair,
  pub affected_pids : BTreeSet<ID>,
}

#[derive(Clone, Debug, Default)]
pub struct GraphSnapshot {
  pub nodes      : HashMap<ID, NodeComplete>,
  pub id_sources : HashMap<ID, BTreeSet<SourceName>>,
}

#[derive(Clone, Debug, Default)]
pub struct DiffReport {
  pub duplicate_ids : Vec<DuplicateIDReport>,
  pub titles        : HashMap<ID, String>,
  pub buckets       : Vec<NodeBucket>,
}

#[derive(Clone, Debug)]
pub struct DuplicateIDReport {
  pub id             : ID,
  pub before_sources : BTreeSet<SourceName>,
  pub after_sources  : BTreeSet<SourceName>,
  pub title          : String,
}

#[derive(Clone, Debug)]
pub struct NodeBucket {
  pub name  : &'static str,
  pub nodes : Vec<NodeDiffReport>,
}

#[derive(Clone, Debug)]
pub struct NodeDiffReport {
  pub pid                 : ID,
  pub source              : SourceForReport,
  pub title               : String,
  pub title_diff          : Option<Vec<TextDiffLine>>,
  pub body_diff           : Option<Vec<TextDiffLine>>,
  pub source_change       : Option<(SourceName, SourceName)>,
  pub value_set_diffs     : Vec<ValueSetDiff>,
  pub relationship_diffs  : Vec<RelationshipDiff>,
  pub contained_list_diff : Option<Vec<ListDiffItem>>,
}

#[derive(Clone, Debug)]
pub enum SourceForReport {
  Before (SourceName),
  After  (SourceName),
}

#[derive(Clone, Debug)]
pub struct ValueSetDiff {
  pub name   : &'static str,
  pub lost   : Vec<String>,
  pub gained : Vec<String>,
}

#[derive(Clone, Debug)]
pub struct RelationshipDiff {
  pub role      : &'static str,
  pub lost      : Vec<ID>,
  pub gained    : Vec<ID>,
  pub unchanged : Vec<ID>,
}

#[derive(Clone, Debug)]
pub enum TextDiffLine {
  Unchanged (String),
  Removed   (String),
  Added     (String),
}

#[derive(Clone, Debug)]
pub enum ListDiffItem {
  Unchanged (ID),
  Removed   (ID),
  Added     (ID),
}
