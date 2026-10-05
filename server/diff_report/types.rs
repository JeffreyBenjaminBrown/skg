use crate::types::misc::{ID, SkgRepoName};
use crate::types::nodes::complete::Graphnode;

use std::collections::{BTreeMap, BTreeSet, HashMap};

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
  /// One entry per TELESCOPE (keyed by pid, sections folded).
  pub nodes     : HashMap<ID, Graphnode>,
  /// id -> claiming pid -> skgrepos whose section for that pid
  /// claims the id (as its pid or among its extra_ids). One pid
  /// claiming an id from several skgrepos is the normal telescope
  /// shape; TWO pids claiming one id is the duplicate-ID VIOLATION
  /// the report warns about.
  pub id_claims : HashMap<ID, BTreeMap<ID, BTreeSet<SkgRepoName>>>,
}

impl GraphSnapshot {
  /// Repos holding any claim on this id, across claiming pids.
  pub fn skgrepos_claiming_skgid (
    &self,
    skgid : &ID,
  ) -> BTreeSet<SkgRepoName> {
    self . id_claims . get (skgid)
      . map ( |by_pid| by_pid . values () . flatten ()
              . cloned () . collect () )
      . unwrap_or_default () }
}

#[derive(Clone, Debug, Default)]
pub struct DiffReport {
  pub duplicate_ids : Vec<DuplicateIDReport>,
  pub titles        : HashMap<ID, String>,
  pub buckets       : Vec<NodeBucket>,
  /// Ids the worktree references though they exist in no skgrepo,
  /// investigated in git history (diff_report/vanished.rs).
  pub vanished      : Vec<VanishedNodeReport>,
}

#[derive(Clone, Debug)]
pub struct VanishedNodeReport {
  pub skgid        : ID,
  pub sightings : Vec<VanishedNodeSighting>, // empty means: never in any skgrepo's git history
}

#[derive(Clone, Debug)]
pub struct VanishedNodeSighting {
  pub home_skgrepo : SkgRepoName,
  pub last_present : CommitStamp,
  pub vanished_at  : Option<CommitStamp>, // its first-parent descendant, which lacks the file
  pub title        : String,             // the node's title when last present
  pub outbound     : Vec<(&'static str, Vec<ID>)>, // its own nonempty relation lists then
  pub inbound      : Vec<(ID, &'static str)>,      // who referred to it then, and how (links included)
}

#[derive(Clone, Debug)]
pub struct CommitStamp {
  pub short_sha : String,
  pub date      : String, // YYYY-MM-DD, UTC
  pub summary   : String,
}

#[derive(Clone, Debug)]
pub struct DuplicateIDReport {
  pub skgid           : ID,
  pub before_skgrepos : BTreeSet<SkgRepoName>,
  pub after_skgrepos  : BTreeSet<SkgRepoName>,
  pub title           : String,
}

#[derive(Clone, Debug)]
pub struct NodeBucket {
  pub name  : &'static str,
  pub nodes : Vec<NodeDiffReport>,
}

#[derive(Clone, Debug)]
pub struct NodeDiffReport {
  pub pid                 : ID,
  pub home_skgrepo        : RepoForReport,
  pub title               : String,
  pub title_diff          : Option<Vec<TextDiffLine>>,
  pub body_diff           : Option<Vec<TextDiffLine>>,
  pub skgrepo_change      : Option<(SkgRepoName, SkgRepoName)>,
  pub value_set_diffs     : Vec<ValueSetDiff>,
  pub relationship_diffs  : Vec<RelationshipDiff>,
  pub contained_list_diff : Option<Vec<ListDiffItem>>,
}

#[derive(Clone, Debug)]
pub enum RepoForReport {
  Before (SkgRepoName),
  After  (SkgRepoName),
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
