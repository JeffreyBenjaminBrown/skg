/// Utilities for phantom node lookup in git diff view.
/// A phantom is a display-only placeholder for a removed node.

use crate::dbs::filesystem::one_node::skgnode_from_pid_and_source;

use std::collections::HashMap;
use std::path::PathBuf;

use super::git::{SourceDiff, NodeDiffStatus};
use super::misc::{ID, SkgConfig, SourceName};
use super::skgnode::SkgNode;

/// Unified source lookup for phantom nodes.
/// Lookup order: existing children → deleted_id_src_map → map → disk.
pub fn source_for_phantom (
  id                 : &ID,
  existing_children  : &HashMap<ID, SourceName>,
  deleted_id_src_map : &HashMap<ID, SourceName>,
  map                : &HashMap<ID, SkgNode>,
  config             : &SkgConfig,
) -> Result<SourceName, String> {
  existing_children.get( id ).cloned()
    .or_else( || deleted_id_src_map.get( id ).cloned() )
    .or_else( || map.get( id ).map( |n| n.source.clone() ))
    .or_else( || source_from_disk( id, config ) )
    .ok_or_else( || format!(
      "source_for_phantom: no source for phantom {}", id.0 )) }

/// Unified title lookup for phantom nodes.
/// Lookup order: source_diffs deleted_nodes → map → disk → fallback.
pub fn title_for_phantom (
  id           : &ID,
  source       : &SourceName,
  source_diffs : Option<&HashMap<SourceName, SourceDiff>>,
  map          : &HashMap<ID, SkgNode>,
  config       : &SkgConfig,
) -> String {
  source_diffs
    .and_then( |diffs| diffs.get( source ) )
    .and_then( |sd| sd.deleted_nodes.get( id ) )
    .map( |n| n.title.clone() )
    .or_else( || map.get( id ).map( |n| n.title.clone() ))
    .or_else( || skgnode_from_pid_and_source(
                    config, id.clone(), source )
                  .ok().map( |n| n.title ) )
    .unwrap_or_else( || format!( "TITLE NOT FOUND for ID {}", id.0 )) }

/// Unified diff status for phantom nodes.
/// Returns Removed if source_diffs[source].deleted_nodes has id,
/// else RemovedHere.
pub fn phantom_diff_status (
  id           : &ID,
  source       : &SourceName,
  source_diffs : Option<&HashMap<SourceName, SourceDiff>>,
) -> NodeDiffStatus {
  if source_diffs
    .and_then( |diffs| diffs.get( source ) )
    .map( |sd| sd.deleted_nodes.contains_key( id ) )
    .unwrap_or( false )
  { NodeDiffStatus::Removed } else { NodeDiffStatus::RemovedHere } }

/// Find the source for a node by checking which source directory
/// contains its .skg file on disk. Returns None if not found in any source.
pub fn source_from_disk (
  id     : &ID,
  config : &SkgConfig,
) -> Option<SourceName> {
  let filename : String = format!( "{}.skg", id.0 );
  for (source_name, source_config) in &config.sources {
    let path : PathBuf =
      PathBuf::from( &source_config.path ).join( &filename );
    if path.exists() {
      return Some( source_name.clone() ); }}
  None }
