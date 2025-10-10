use std::collections::HashSet;
use futures::StreamExt;
use std::error::Error;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::types::ID;
use crate::typedb::util::extract_payload_from_typedb_string_rep;


/* Searches containerward recursively until reaching the first node
which is either uncontained or multiply contained. Returns that node's ID.
So for instance, if the input is uncontained, it just returns the input.
.
PITFALL: This just takes the last element of the path
returned by `path_containerward_to_end_cycle_and_or_branches`,
throwing away the rest of the information.
*/
pub async fn climb_containerward_and_fetch_rootish_context (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < ID, Box<dyn Error> > {
  let ( path, _cycle_node, _multi_containers )
    : ( Vec<ID>, Option<ID>, HashSet<ID> )
    = path_containerward_to_end_cycle_and_or_branches (
      db_name, driver, node ). await ?;
  path . last () . ok_or_else ( || {
      // This should never happen,
      // since the path always includes at least the input node.
      std::io::Error::new (
        std::io::ErrorKind::InvalidData,
        format!(
          "Empty path from path_containerward_to_end_cycle_and_or_branches for node '{}'",
          node )) . into ()
    } ) . cloned ()
}

/* Follows a path via a single directed binary relation
  until reaching a cycle and/or branches.
The path begins with the input node.
Following the specified relationship,
  each time we find a single related node,
  we append it to the path.
The process can end in three ways:
1 - If no related node is found, we return the path and exit.
    The option and the set are both null.
2 - If at any point multiple related nodes are found,
    the choice of 'next in path' has no answer,
    so they are added to the set,
    nothing is added to the path, and the function returns.
3 - If at any point the related node is
    equal to one already in the path, we have found a cycle.
    That ID becomes the option, the path is not extended,
    and the function returns.
Note that 2 and 3 can coincide. That is the only case
  in which are all three outputs non-null. */
pub async fn path_to_end_cycle_and_or_branches (
  db_name     : &str,
  driver      : &TypeDBDriver,
  node        : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str
) -> Result <
    ( Vec<ID>,    // The path. Its first node is the input.
      Option<ID>, // If the path cycles, this is the first repeated node.
      HashSet<ID> // If the path forks, these are the fork's branches.
    ), Box<dyn Error> > {

  let mut path : Vec<ID> = vec![ node.clone () ];
  let mut path_set : HashSet<ID> =
    // path and path_set have the same nodes.
    HashSet::from ( [ node.clone() ] );
  let mut current_node : ID = node.clone ();
  loop {
    let related_nodes : HashSet<ID> =
      find_related_nodes (
        db_name, driver, &current_node,
        relation, input_role, output_role ). await ?;
    if related_nodes.is_empty () {
      // No related node found, so this is the end.
      return Ok (( path, None, HashSet::new () ));
    } else {
      let cycle_node : Option<ID> =
        // 'Some' if the related node has been seen already.
        related_nodes.iter ()
          .find ( |&c| path_set.contains ( c ) )
          .cloned ();
      if ( related_nodes.len () == 1
           && cycle_node.is_none () ) {
        // Add the related node to the path and continue.
        let next_node : ID =
          related_nodes . into_iter() . next() . unwrap();
        path.push ( next_node.clone () );
        path_set.insert ( next_node.clone () );
        current_node = next_node;
      } else { // We are at a fork, or a cycle, or both.
        return Ok ((
          path,
          cycle_node,
          ( if related_nodes.len () == 1 {
            HashSet::new () }
            else {related_nodes} ) ));
        }} }}

/// See path_to_end_cycle_and_or_branches.
/// This is the case that searches sourceward.
pub async fn path_containerward_to_end_cycle_and_or_branches (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < ( Vec<ID>,
                Option<ID>,
                HashSet<ID>
), Box<dyn Error> > {
  path_to_end_cycle_and_or_branches (
    db_name,
    driver,
    node,
    "contains",
    "contained",
    "container"
  ). await }

/// See path_to_end_cycle_and_or_branches.
/// This is the case that searches containerward.
pub async fn path_sourceward_to_end_cycle_and_or_branches (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < ( Vec<ID>,
                Option<ID>,
                HashSet<ID>
), Box<dyn Error> > {
  path_to_end_cycle_and_or_branches (
    db_name,
    driver,
    node,
    "hyperlinks_to",
    "dest",
    "source"
  ). await }

/// Runs a single TypeDB query.
/// Returns the containing nodes' IDs.
pub async fn find_containers_of (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < HashSet<ID>, Box<dyn Error> > {
  find_related_nodes (
    db_name,
    driver,
    node,
    "contains",
    "contained",
    "container"
  ). await }

/// Runs a single TypeDB query.
/// Returns the IDs of nodes that link to the input node.
pub async fn find_links_to (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < HashSet<ID>, Box<dyn Error> > {
  find_related_nodes (
    db_name,
    driver,
    node,
    "hyperlinks_to",
    "dest",
    "source"
  ). await }

/// Generalized function to find related nodes via a specified relationship.
/// Returns the IDs of nodes in the `output_role` position related to the input node.
pub async fn find_related_nodes (
  db_name     : &str,
  driver      : &TypeDBDriver,
  node        : &ID,
  relation    : &str,
  input_role  : &str,
  output_role : &str
) -> Result < HashSet<ID>, Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction (
      db_name, TransactionType::Read
    ). await ?;
  let output_id_var = format!("{}_id", output_role);
  let match_clause = format!( r#" match
                     ${} isa node, has id ${};
                  {{ ${} isa node, has id "{}"; }} or
                  {{ ${} isa node;
                     $e isa extra_id, has id "{}";
                     $extra_rel isa has_extra_id ( node:     ${},
                                                   extra_id: $e ); }};"#,
                   output_role, output_id_var,
                   input_role, node,
                   input_role, node, input_role );
  let relationship_and_select = format!( r#"
                     $rel isa {} ( {}: ${},
                                   {}: ${} );
                     select ${};"#,
                     relation, input_role, input_role,
                     output_role, output_role,
                     output_id_var );
  let query = format!("{}{}", match_clause, relationship_and_select);

  let answer : QueryAnswer = tx.query ( query ) . await?;
  let mut stream = answer.into_rows ();
  let mut related_nodes : HashSet<ID> = HashSet::new ();
  while let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row.get (&output_id_var) ? {
      related_nodes.insert ( ID (
        extract_payload_from_typedb_string_rep (
          &concept . to_string () )) ); }}
  Ok (related_nodes) }
