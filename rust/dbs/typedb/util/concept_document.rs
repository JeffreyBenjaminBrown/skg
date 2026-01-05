use typedb_driver::answer::concept_document::{Node, Leaf};

use crate::types::ID;
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

/// Build a TypeDB disjunction clause for matching multiple IDs
/// Returns a string like "{$var == "id1";} or {$var == "id2";} or ..."
pub fn build_id_disjunction (
  ids      : &[ID],
  var_name : &str
) -> String {
  ids
  . iter ()
  . map ( | id |
            format! ( "{{${} == \"{}\";}}",
                         var_name, id . 0 ) )
  . collect::< Vec < _ > > ()
  . join ( " or " )
}
