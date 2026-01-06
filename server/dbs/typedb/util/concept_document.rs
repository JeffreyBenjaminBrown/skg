use typedb_driver::answer::concept_document::{Node, Leaf};

use crate::types::misc::ID;
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;


/// Extract ID from a TypeDB ConceptDocument Node.
/// Expects a Leaf containing a Concept.
pub fn extract_id_from_node (
  node : & Node
) -> Option < ID > {
  if let Node::Leaf ( Some ( leaf ) ) = node {
    if let Leaf::Concept ( concept ) = leaf {
      return Some ( ID (
        extract_payload_from_typedb_string_rep (
          & concept . to_string () )) ); }}
  None }

/// Extract ID from a map node at a specific key.
pub fn extract_id_from_map (
  node : & Node,
  key  : &str
) -> Option < ID > {
  if let Node::Map ( inner_map ) = node {
    inner_map . get ( key )
      . and_then ( extract_id_from_node )
  } else { None }}

/// Build a disjunction clause from items using a custom formatter.
/// Returns a string like "{format(item1)} or {format(item2)} or ..."
pub fn build_disjunction<I, F>(
  items       : I,
  format_item : F
) -> String
where
  I : IntoIterator,
  F : Fn(I::Item) -> String,
{
  items.into_iter()
    .map(format_item)
    .collect::<Vec<_>>()
    .join(" or ") }

/// Build a TypeDB disjunction clause for matching multiple IDs.
/// Returns a string like "{$var == "id1";} or {$var == "id2";} or ..."
pub fn build_id_disjunction (
  ids      : &[ID],
  var_name : &str
) -> String {
  build_disjunction(
    ids.iter(),
    |id| format!("{{${} == \"{}\";}}", var_name, id.0) ) }

/// Build a TypeDB disjunction clause for matching by "has id" attribute.
/// Returns a string like "{$var has id "id1";} or {$var has id "id2";} or ..."
pub fn build_has_id_disjunction<S: AsRef<str>>(
  ids      : &[S],
  var_name : &str
) -> String {
  build_disjunction(
    ids.iter(),
    |id| format!("{{${} has id \"{}\";}}", var_name, id.as_ref()) ) }
