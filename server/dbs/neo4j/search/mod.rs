pub mod contains_from_pids;
pub mod count_relationships;
pub mod hidden_in_subscribee_content;

use std::collections::HashSet;
use std::error::Error;
use neo4rs::Graph;

use crate::types::misc::{ID, SourceName};

/// Returns true if the from_role is the arrow's tail (outgoing).
/// This replaces role-string gymnastics in the TypeQL query builder.
fn cypher_direction (
  relation   : &str,
  input_role : &str
) -> Result < bool, Box<dyn Error> > {
  // true  = input is the tail (from-node):  (input)-[r]->(output)
  // false = input is the head (to-node):    (output)-[r]->(input)
  match ( relation, input_role ) {
    ( "contains",     "container" )   => Ok ( true ),
    ( "contains",     "contained" )   => Ok ( false ),
    ( "textlinks_to", "source" )      => Ok ( true ),
    ( "textlinks_to", "dest" )        => Ok ( false ),
    ( "subscribes",   "subscriber" )  => Ok ( true ),
    ( "subscribes",   "subscribee" )  => Ok ( false ),
    ( "hides",        "hider" )       => Ok ( true ),
    ( "hides",        "hidden" )      => Ok ( false ),
    ( "overrides",    "replacement" ) => Ok ( true ),
    ( "overrides",    "replaced" )    => Ok ( false ),
    _ => Err ( format! (
      "Unknown relation/role pair: '{}/{}'",
      relation, input_role ) . into () ) }}

/// Searches containerward recursively until reaching the first node
/// which is either uncontained or multiply contained.
/// Returns that node's ID.
pub async fn climb_containerward_and_fetch_rootish_context (
  graph : &Graph,
  node  : &ID
) -> Result < ID, Box<dyn Error> > {
  let ( path, _cycle_node, _multi_containers )
    : ( Vec<ID>, Option<ID>, HashSet<ID> )
    = path_containerward_to_end_cycle_and_or_branches (
      graph, node ). await ?;
  path . last () . ok_or_else ( || {
      std::io::Error::new (
        std::io::ErrorKind::InvalidData,
        format!(
          "Empty path from path_containerward_to_end_cycle_and_or_branches for node '{}'",
          node )) . into ()
    } ) . cloned ()
}

/// See path_to_end_cycle_and_or_branches.
/// This is the case that searches containerward.
pub async fn path_containerward_to_end_cycle_and_or_branches (
  graph : &Graph,
  node  : &ID
) -> Result < ( Vec<ID>,
                Option<ID>,
                HashSet<ID>
), Box<dyn Error> > {
  path_to_end_cycle_and_or_branches (
    graph,
    node,
    "contains",
    "contained",
  ). await }

/// See path_to_end_cycle_and_or_branches.
/// This is the case that searches sourceward.
pub async fn path_sourceward_to_end_cycle_and_or_branches (
  graph : &Graph,
  node  : &ID
) -> Result < ( Vec<ID>,
                Option<ID>,
                HashSet<ID>
), Box<dyn Error> > {
  path_to_end_cycle_and_or_branches (
    graph,
    node,
    "textlinks_to",
    "dest",
  ). await }

/// Runs a single query.
/// Returns the containing nodes' IDs.
pub async fn find_containers_of (
  graph : &Graph,
  node  : &ID
) -> Result < HashSet<ID>, Box<dyn Error> > {
  find_related_nodes (
    graph,
    & [ node . clone () ],
    "contains",
    "contained",
  ). await }

/// Returns the IDs of nodes that link to the input node.
pub(super) async fn find_links_to (
  graph : &Graph,
  node  : &ID
) -> Result < HashSet<ID>, Box<dyn Error> > {
  find_related_nodes (
    graph,
    & [ node . clone () ],
    "textlinks_to",
    "dest",
  ). await }

/* Follows a path via a single directed binary relation
  until reaching a cycle and/or branches.
The path begins with the input node.
Following the specified relationship,
  each time we find exactly one 'related node'
  (meaning a node related in the specified manner),
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
pub(super) async fn path_to_end_cycle_and_or_branches (
  graph      : &Graph,
  node       : &ID,
  relation   : &str,
  input_role : &str,
) -> Result <
    ( Vec<ID>,
      Option<ID>,
      HashSet<ID>
    ), Box<dyn Error> > {
  let mut path : Vec<ID> = vec![ node.clone () ];
  let mut path_set : HashSet<ID> =
    HashSet::from ( [ node.clone() ] );
  let mut current_node : ID = node.clone ();
  loop {
    let related_nodes : HashSet<ID> =
      find_related_nodes (
        graph, & [ current_node . clone () ],
        relation, input_role ). await ?;
    if related_nodes.is_empty () {
      return Ok (( path, None, HashSet::new () ));
    } else {
      let cycle_node : Option<ID> =
        related_nodes.iter ()
          .find ( |&c| path_set.contains ( c ) )
          .cloned ();
      if ( related_nodes.len () == 1
           && cycle_node.is_none () ) {
        let next_node : ID =
          related_nodes . into_iter() . next() . unwrap();
        path.push ( next_node.clone () );
        path_set.insert ( next_node.clone () );
        current_node = next_node;
      } else {
        return Ok ((
          path,
          cycle_node,
          ( if related_nodes.len () == 1 {
            HashSet::new () }
            else {related_nodes} ) ));
        }} }}

/// Generalized function to find related nodes via a specified relationship.
/// Returns the IDs of nodes in the output role position
/// related to any of the input nodes.
/// Input IDs are resolved via both Node.id and IdAlias.id.
pub async fn find_related_nodes (
  graph      : &Graph,
  nodes      : &[ID],
  relation   : &str,
  input_role : &str,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  if nodes . is_empty () {
    return Ok ( HashSet::new () ); }
  let input_is_tail : bool =
    cypher_direction ( relation, input_role ) ?;
  // Build the query: resolve input IDs, traverse relationship
  let cypher : String = format! ( "\
    UNWIND $ids AS input_id \
    OPTIONAL MATCH (direct:Node {{id: input_id}}) \
    OPTIONAL MATCH (alias:IdAlias {{id: input_id}}) \
    WITH coalesce(direct.id, alias.primary_id) AS resolved_pid \
    WHERE resolved_pid IS NOT NULL \
    MATCH (input:Node {{id: resolved_pid}}) \
    MATCH {}-[:{}]->{} \
    RETURN DISTINCT output.id AS output_id",
    if input_is_tail { "(input)" }     else { "(output:Node)" },
    relation,
    if input_is_tail { "(output:Node)" } else { "(input)" } );
  let id_strings : Vec<String> =
    nodes . iter () . map ( |id| id.0.clone () ) . collect ();
  let mut result_stream =
    graph . execute (
      neo4rs::query ( &cypher )
      . param ( "ids", id_strings )
    ) . await ?;
  let mut related_nodes : HashSet<ID> = HashSet::new ();
  while let Some ( row ) = result_stream . next () . await ? {
    let output_id : String = row . get ( "output_id" ) ?;
    related_nodes . insert ( ID ( output_id ) ); }
  Ok (related_nodes) }

/// Runs a single query to get both PID and source.
/// Returns None if not found.
pub async fn pid_and_source_from_id (
  graph : &Graph,
  skgid : &ID
) -> Result < Option<(ID, SourceName)>, Box<dyn Error> > {
  let mut result_stream =
    graph . execute (
      neo4rs::query ( "\
        OPTIONAL MATCH (direct:Node {id: $id}) \
        OPTIONAL MATCH (alias:IdAlias {id: $id}) \
        WITH coalesce(direct.id, alias.primary_id) AS pid \
        WHERE pid IS NOT NULL \
        MATCH (n:Node {id: pid}) \
        RETURN n.id AS primary_id, n.source AS source" )
      . param ( "id", skgid . as_str () )
    ) . await ?;
  if let Some ( row ) = result_stream . next () . await ? {
    let primary_id : String = row . get ( "primary_id" ) ?;
    let source : String     = row . get ( "source" ) ?;
    Ok ( Some (( ID ( primary_id ),
                 SourceName::from ( source ) )) ) }
  else { Ok (None) } }
