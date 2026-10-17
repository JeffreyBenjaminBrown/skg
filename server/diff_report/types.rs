use crate::types::misc::{ID, SkgrepoName};

use std::collections::{BTreeSet, HashMap};

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
  pub home_skgrepo : SkgrepoName,
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
  pub before_skgrepos : BTreeSet<SkgrepoName>,
  pub after_skgrepos  : BTreeSet<SkgrepoName>,
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
  pub skgrepo_change      : Option<(SkgrepoName, SkgrepoName)>,
  pub value_set_diffs     : Vec<ValueSetDiff>,
  pub relationship_diffs  : Vec<RelationshipDiff>,
  pub content_list_diff : Option<Vec<ListDiffItem>>,
}

#[derive(Clone, Debug)]
pub enum RepoForReport {
  Before (SkgrepoName),
  After  (SkgrepoName),
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
