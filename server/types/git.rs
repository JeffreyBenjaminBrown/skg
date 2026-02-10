/// Git-related types for diff view functionality.

use crate::types::list::Diff_Item;
use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::SkgNode;

use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::str::FromStr;

//
// Types
//

/// Represents changes for an entire source directory.
/// (I assume each source is a separate git repo,
/// but the code might work even if some are part of the same repo.)
#[derive(Debug, Clone)]
pub struct SourceDiff {
  pub is_git_repo: bool,
  pub skgnode_diffs: HashMap<PathBuf, SkgnodeDiff>,
  /// Nodes that existed in HEAD but not in worktree (deleted files).
  /// Loaded from git HEAD.
  pub deleted_nodes: HashMap<ID, SkgNode>,
}

/// All the diff info for a single .skg file.
#[derive(Debug, Clone)]
pub struct SkgnodeDiff {
  // TODO ? Since we keep the skgnode around for deleted nodes already,
  // why not just do that for everything, and dispense with the node_changes field?
  pub status: GitDiffStatus,
  pub node_changes: Option<NodeChanges>,
  pub head_node: Option<SkgNode>, // only for deleted files
}

/// A single entry representing a changed file.
#[derive(Debug, Clone)]
pub struct PathDiffStatus {
  pub path   : PathBuf,
  pub status : GitDiffStatus,
}

/// Status of a file in the git diff.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitDiffStatus {
  Added,    // File was added (not in HEAD)
  Modified, // File was modified
  Deleted,  // File was deleted (in HEAD but not on disk)
}

/// Represents changes to a single SkgNode.
#[derive(Debug, Clone, Default)]
pub struct NodeChanges {
  pub text_changed  : bool,
  pub aliases_diff  : Vec<Diff_Item<String>>,
  pub ids_diff      : Vec<Diff_Item<ID>>,
  pub contains_diff : Vec<Diff_Item<ID>>,
}

/// Indicates how a node differs between the current state and HEAD.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeDiffStatus {
  New,         // did not exist in HEAD
  NewHere,     // existed in HEAD but not in this relationship
  Removed,     // existed in HEAD and is no longer in the worktree
  RemovedHere, // exists in worktree but no longer in this relationship
  NotInGit,    // its source is not a git repo
}

/// Diff status for scaffold fields (Alias, ID) in git diff view mode.
/// Indicates how a field value differs between the current state and HEAD.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldDiffStatus {
  New,      // Value was not in the list in HEAD
  Removed,  // Value was in the list in HEAD, now gone
}

//
// Implementations
//

impl SourceDiff {
  pub fn new_not_git_repo () -> Self {
    SourceDiff {
      is_git_repo: false,
      skgnode_diffs: HashMap::new(),
      deleted_nodes: HashMap::new() }}

  pub fn skgnode_diff_for_pid (
    &self,
    pid : &ID,
  ) -> Option<&SkgnodeDiff> {
    let file_path : PathBuf =
      PathBuf::from( format!( "{}.skg", pid.0 ) );
    self.skgnode_diffs.get( &file_path ) }}

impl NodeDiffStatus {
  /// Single source of truth for NodeDiffStatus <-> client string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, NodeDiffStatus)] = &[
    ("new",          NodeDiffStatus::New),
    ("new-here",     NodeDiffStatus::NewHere),
    ("removed",      NodeDiffStatus::Removed),
    ("removed-here", NodeDiffStatus::RemovedHere),
    ("not-in-git",   NodeDiffStatus::NotInGit),
  ];

  /// String representation as used in client metadata.
  pub fn repr_in_client ( &self ) -> &'static str {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(_, nd)| nd == self )
      .map ( |(s, _)| *s )
      .expect ( "REPRS_IN_CLIENT should cover all NodeDiffStatus variants" ) }

  /// Parse a client string to a NodeDiffStatus.
  pub fn from_client_string ( s: &str ) -> Option<NodeDiffStatus> {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(cs, _)| *cs == s )
      .map ( |(_, nd)| *nd ) }
}

impl fmt::Display for NodeDiffStatus {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    write!(f, "{}", self.repr_in_client()) } }

impl FromStr for NodeDiffStatus {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    Self::from_client_string ( s )
      .ok_or_else ( || format! ( "Unknown NodeDiffStatus value: {}", s ) ) } }

impl FieldDiffStatus {
  /// Single source of truth for FieldDiffStatus <-> client string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, FieldDiffStatus)] = &[
    ("new",        FieldDiffStatus::New),
    ("removed",    FieldDiffStatus::Removed),
  ];

  /// String representation as used in client metadata.
  pub fn repr_in_client ( &self ) -> &'static str {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(_, fd)| fd == self )
      .map ( |(s, _)| *s )
      .expect ( "REPRS_IN_CLIENT should cover all FieldDiffStatus variants" ) }

  /// Parse a client string to a FieldDiffStatus.
  pub fn from_client_string ( s: &str ) -> Option<FieldDiffStatus> {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(cs, _)| *cs == s )
      .map ( |(_, fd)| *fd ) }
}

impl fmt::Display for FieldDiffStatus {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    write!(f, "{}", self.repr_in_client()) } }

impl FromStr for FieldDiffStatus {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    Self::from_client_string ( s )
      .ok_or_else ( || format! ( "Unknown FieldDiffStatus value: {}", s ) ) } }

//
// Functions
//

/// Look up the NodeChanges for a TrueNode from source_diffs.
/// Returns None if not in diff view, the source is not a git repo,
/// or there are no recorded changes for this node's .skg file.
pub fn node_changes_for_truenode<'a> (
  source_diffs : &'a Option<HashMap<SourceName, SourceDiff>>,
  pid          : &ID,
  source       : &SourceName,
) -> Option<&'a NodeChanges> {
  source_diffs.as_ref().and_then(
    |diffs| {
      let sourcediff : &SourceDiff = diffs.get( source ) ?;
      if !sourcediff.is_git_repo { return None; }
      sourcediff.skgnode_diff_for_pid( pid ) ?
        .node_changes.as_ref() } ) }
