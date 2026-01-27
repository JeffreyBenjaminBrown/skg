/// Git-related types for diff view functionality.

use crate::types::list::ListDiff;
use crate::types::misc::ID;

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
  pub file_diffs: HashMap<PathBuf, FileDiff>,
}

/// All the diff info for a single .skg file.
#[derive(Debug, Clone)]
pub struct FileDiff {
  pub status: DiffStatus,
  pub node_changes: Option<NodeChanges>,
}

/// A single entry representing a changed file.
#[derive(Debug, Clone)]
pub struct PathDiffStatus {
  pub path   : PathBuf,
  pub status : DiffStatus,
}

/// Status of a file in the git diff.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiffStatus {
  Added,    // File was added (not in HEAD)
  Modified, // File was modified
  Deleted,  // File was deleted (in HEAD but not on disk)
}

/// Represents changes to a single SkgNode.
#[derive(Debug, Clone, Default)]
pub struct NodeChanges {
  /// True if title or body changed
  pub text_changed: bool,
  /// Changes to the aliases list
  pub aliases_diff: ListDiff<String>,
  /// Changes to the ids list
  pub ids_diff: ListDiff<ID>,
}

/// Indicates how a node differs between the current state and HEAD.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeDiff {
  New,         // did not exist in HEAD
  NewHere,     // existed in HEAD but not in this relationship
  Removed,     // existed in HEAD and is no longer in the worktree
  RemovedHere, // exists in worktree but no longer in this relationship
  NotInGit,    // its source is not a git repo
}

/// Diff status for scaffold fields (Alias, ID) in git diff view mode.
/// Indicates how a field value differs between the current state and HEAD.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldDiff {
  New,      // Value was not in the list in HEAD
  Removed,  // Value was in the list in HEAD, now gone
  NotInGit, // Source is not a git repo
}

//
// Implementations
//

impl SourceDiff {
  pub fn new_not_git_repo () -> Self {
    SourceDiff {
      is_git_repo: false,
      file_diffs: HashMap::new() }}}

impl NodeDiff {
  /// Single source of truth for NodeDiff <-> client string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, NodeDiff)] = &[
    ("new",          NodeDiff::New),
    ("new-here",     NodeDiff::NewHere),
    ("removed",      NodeDiff::Removed),
    ("removed-here", NodeDiff::RemovedHere),
    ("not-in-git",   NodeDiff::NotInGit),
  ];

  /// String representation as used in client metadata.
  pub fn repr_in_client ( &self ) -> &'static str {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(_, nd)| nd == self )
      .map ( |(s, _)| *s )
      .expect ( "REPRS_IN_CLIENT should cover all NodeDiff variants" ) }

  /// Parse a client string to a NodeDiff.
  pub fn from_client_string ( s: &str ) -> Option<NodeDiff> {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(cs, _)| *cs == s )
      .map ( |(_, nd)| *nd ) }
}

impl fmt::Display for NodeDiff {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    write!(f, "{}", self.repr_in_client()) } }

impl FromStr for NodeDiff {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    Self::from_client_string ( s )
      .ok_or_else ( || format! ( "Unknown NodeDiff value: {}", s ) ) } }

impl FieldDiff {
  /// Single source of truth for FieldDiff <-> client string bijection.
  const REPRS_IN_CLIENT: &'static [(&'static str, FieldDiff)] = &[
    ("new",        FieldDiff::New),
    ("removed",    FieldDiff::Removed),
    ("not-in-git", FieldDiff::NotInGit),
  ];

  /// String representation as used in client metadata.
  pub fn repr_in_client ( &self ) -> &'static str {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(_, fd)| fd == self )
      .map ( |(s, _)| *s )
      .expect ( "REPRS_IN_CLIENT should cover all FieldDiff variants" ) }

  /// Parse a client string to a FieldDiff.
  pub fn from_client_string ( s: &str ) -> Option<FieldDiff> {
    Self::REPRS_IN_CLIENT.iter()
      .find ( |(cs, _)| *cs == s )
      .map ( |(_, fd)| *fd ) }
}

impl fmt::Display for FieldDiff {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>
  ) -> fmt::Result {
    write!(f, "{}", self.repr_in_client()) } }

impl FromStr for FieldDiff {
  type Err = String;

  fn from_str (
    s : &str
  ) -> Result<Self, Self::Err> {
    Self::from_client_string ( s )
      .ok_or_else ( || format! ( "Unknown FieldDiff value: {}", s ) ) } }
