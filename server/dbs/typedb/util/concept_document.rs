use typedb_driver::answer::concept_document::{Node, Leaf};

use crate::types::misc::ID;
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;


/// Extract ID from a TypeDB ConceptDocument Node.
/// Expects a Leaf containing a Concept.
pub fn extract_id_from_node (
  node : & Node
) -> Option < ID > {
  if let Node::Leaf ( Some (leaf) ) = node {
    if let Leaf::Concept (concept) = leaf {
      return Some ( ID (
        extract_payload_from_typedb_string_rep (
          & concept . to_string () )) ); }}
  None }

/// Extract ID from a map node at a specific key.
pub fn extract_id_from_map (
  node : & Node,
  key  : &str
) -> Option < ID > {
  if let Node::Map (inner_map) = node {
    inner_map . get (key)
      . and_then (extract_id_from_node)
  } else { None }}

