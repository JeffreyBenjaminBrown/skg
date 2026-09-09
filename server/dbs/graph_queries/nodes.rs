use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SourceName};

use std::collections::{BTreeSet, HashSet};

/// Resolve either a primary or extra ID within the supplied snapshot.
pub fn pid_and_source_from_id (
  graph : &InRustGraph,
  id : &ID,
) -> Option<(ID, SourceName)> {
  graph . pid_and_source (id) }

/// Return existing primary IDs. Extra IDs are resolved by 'pid_of';
/// they do not make a second primary node exist.
pub fn which_ids_exist (
  graph : &InRustGraph,
  ids : &BTreeSet<String>,
) -> HashSet<String> {
  ids . iter () . filter ( |id| graph . nodes . contains_key (&ID::from (id . as_str ())))
    . cloned () . collect () }
