/// DEPRECATED.
/// Once it's complete, use super::robust instead.

use std::collections::HashSet;
use futures::StreamExt;
use std::error::Error;
use std::io;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::types::ID;
use super::util::extract_payload_from_typedb_string_rep;


/* Returns the last container in the path from the input,
to its container, to its container's container, etc.
until either no container is found
or one is found that repeats one earlier in the path.
In the first case, the returned value is in fact the root.
In the latter case the duplicate is not included. */
pub async fn find_rootish_container (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < ID, Box<dyn Error> > {

  path_to_rootish_container ( db_name, driver, node )
    . await ?
    . last ()
    . ok_or_else ( || {
      // Last can't actually fail, because the path always includes the starting node, but Rust doesn't know that.
      io::Error::new (
        io::ErrorKind::InvalidData,
        format!( "Empty path containerward from node '{}'", node )
      ) . into ()
    } )
    . cloned () }

/* Runs a series of TypeDB queries.
Returns a path through the graph, starting with the input,
and hopefully ending with its root container.
If it is a member of a cycle, then it has no root container,
so the path ends just before repeating.
If any member of the path is multiply contained,
one branch is chosen in an undefined manner,
as described in the comment for `find_container_of`.
.
TODO : The path ought to end at the first such multiplicity,
reporting the full set of containers there. */
pub async fn path_to_rootish_container (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < Vec<ID>,
              Box<dyn Error> > {

  let mut path : Vec<ID> =
    vec! [ node.clone () ];
  let mut seen : HashSet<ID> =
    HashSet::from ( [ node.clone() ] );
  let mut current_node : ID =
    node.clone ();
  loop {
    // TODO: This is inefficient. Better to use a single TypeDB query.
    match find_container_of (
      db_name, driver, &current_node) . await {
      Ok (container_id) => {
        if seen.contains ( &container_id ) {
          // It's a duplicate. Return without re-including duplicate.
          break; }
        else { // Found a container. Add it to `path` and `seen`.
          path.push ( container_id.clone () );
          seen.insert ( container_id.clone () );
          current_node = container_id; } },
      Err(_) => {
        // No container found, so this is the root.
        // TODO: Distinguish between no container found and query failed.
        break; } } }
  Ok (path) }

pub async fn find_container_of (
  // Runs a single TypeDB query.
  // Returns the containing node's ID.
  // TODO | PITFALL: If the node is multiply contained,
  // this silently ignores all but the first container found.
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < ID, Box<dyn Error> > {

  let tx : Transaction = driver.transaction (
    db_name, TransactionType::Read ). await ?;
  let answer : QueryAnswer = tx.query (
    format!( r#" match
                   $container isa node, has id $container_id;
                   $contained isa node, has id "{}";
                   $rel isa contains ( container: $container,
                                       contained: $contained );
                 select $container_id;"#,
                 node ) ) . await?;
  let mut stream = answer.into_rows ();
  if let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row.get ("container_id") ? {
      return Ok ( ID (
        extract_payload_from_typedb_string_rep (
          &concept . to_string () )) ); }}
  Err ( format! ( "No container found for node with ID '{}'",
                   node ). into() ) }
