/// Utilities for phantom node lookup in git diff view.
/// A phantom is a display-only placeholder for a removed node.

use crate::dbs::filesystem::one_node::skgnode_from_pid_and_source;

use std::collections::HashMap;
use std::path::PathBuf;

use super::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff};
use super::misc::{ID, SkgConfig, SourceName};
use super::nodes::complete::NodeComplete;

/// Unified title lookup for phantom nodes.
/// Lookup order: source_diffs deleted_nodes → map → disk → fallback.
pub fn title_for_phantom (
  id           : &ID,
  source       : &SourceName,
  source_diffs : Option<&HashMap<SourceName, SourceDiff>>,
  map          : &HashMap<ID, NodeComplete>,
  config       : &SkgConfig,
) -> String {
  source_diffs
    . and_then( |diffs| diffs . get (source) )
    . and_then( |sd| sd . deleted_nodes . get (id) )
    . map( |n| n . title . clone() )
    . or_else( || map . get (id) . map( |n| n . title . clone() ))
    . or_else( || skgnode_from_pid_and_source(
                    config, id . clone(), source )
                  . ok() . map( |n| n . title ) )
    . unwrap_or_else( || format!( "TITLE NOT FOUND for ID {}", id . 0 )) }

/// Diff axes for a phantom node, for use by the save / rerender pipeline.
/// A phantom always has membership.unstaged = '-' (it's missing from
/// this parent's worktree contains list). Existence is '-' on the
/// unstaged side iff the file is also gone in the worktree (recorded in
/// SourceDiff.deleted_nodes); otherwise existence axes are empty.
pub fn phantom_axes (
  id           : &ID,
  source       : &SourceName,
  source_diffs : Option<&HashMap<SourceName, SourceDiff>>,
) -> (ExistenceAxes, MembershipAxes) {
  let file_gone : bool = source_diffs
    . and_then ( |diffs| diffs . get (source) )
    . map ( |sd| sd . deleted_nodes . contains_key (id) )
    . unwrap_or (false);
  let existence : ExistenceAxes =
    if file_gone {
      ExistenceAxes { staged: None, unstaged: Some (Sign::Minus) }
    } else {
      ExistenceAxes::default ()
    };
  let membership : MembershipAxes =
    MembershipAxes { staged: None, unstaged: Some (Sign::Minus) };
  (existence, membership) }

/// Find the source for a node by checking which source directory
/// contains its .skg file on disk. Returns None if not found in any source.
pub fn source_from_disk (
  id     : &ID,
  config : &SkgConfig,
) -> Option<SourceName> {
  let filename : String = format!( "{}.skg", id . 0 );
  for (source_name, source_config) in &config . sources {
    let path : PathBuf =
      PathBuf::from( &source_config . path ) . join (&filename);
    if path . exists() {
      return Some( source_name . clone() ); }}
  None }
