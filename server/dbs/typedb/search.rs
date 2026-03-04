pub mod all_graphnodestats;
pub mod contains_from_pids;
pub mod hidden_in_subscribee_content;

use futures::StreamExt;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer, ConceptDocument,
           concept_document::{Node, Leaf}},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::dbs::typedb::util::ConceptRowStream;
use crate::dbs::typedb::util::concept_document::{build_id_disjunction, extract_id_from_node};
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;
use crate::types::misc::{ID, SourceName};
use crate::types::viewnode::ContainerwardPathStats;

/// Compute containerward path stats for multiple nodes at once.
/// Level-by-level traversal: each round issues one TypeDB query
/// per frontier node (in parallel via join_all), then advances.
/// Convergent paths merge (shared containers queried only once).
///
/// PITFALL: Uses per-node queries rather than a single bulk query
/// with N-way disjunction, because TypeDB's disjunction scaling
/// is non-linear (see all_graphnodestats.rs).
pub async fn containerward_path_stats_bulk (
  db_name : &str,
  driver  : &TypeDBDriver,
  nodes   : &[ID],
) -> Result < HashMap<ID, ContainerwardPathStats>,
              Box<dyn Error> > {
  struct TrackedPath {
    original : ID,
    depth    : usize,
    visited  : HashSet<ID>,
  }
  let mut result : HashMap<ID, ContainerwardPathStats> =
    HashMap::new ();
  let mut frontier : Vec<(ID, TrackedPath)> =
    nodes . iter ()
    . map ( |id| {
      let visited : HashSet<ID> =
        HashSet::from ( [ id . clone () ] );
      ( id . clone (),
        TrackedPath {
          original : id . clone (),
          depth    : 0,
          visited } ) } )
    . collect ();
  while ! frontier . is_empty () {
    // Deduplicate frontier IDs so each distinct node
    // is queried only once (multiple tracked paths may
    // have converged onto the same frontier node).
    let unique_frontier_ids : Vec<ID> = {
      let mut seen : HashSet<ID> = HashSet::new ();
      frontier . iter ()
        . filter_map ( |(id, _)| {
          if seen . insert ( id . clone () ) {
            Some ( id . clone () )
          } else { None } } )
        . collect () };
    let futures : Vec<_> =
      unique_frontier_ids . iter ()
      . map ( |pid|
        find_container_ids_of_pid ( db_name, driver, pid ) )
      . collect ();
    let results : Vec < Result < HashSet<ID>,
                                 Box<dyn Error> > > =
      futures::future::join_all (futures) . await;
    let mut containers_map : HashMap<ID, HashSet<ID>> =
      HashMap::new ();
    for ( id, r ) in unique_frontier_ids . into_iter ()
                     . zip ( results )
    { containers_map . insert (
        id, r . unwrap_or_default () ); }
    let mut next_frontier : Vec<(ID, TrackedPath)> =
      Vec::new ();
    for (current_id, tracked) in frontier {
      let containers : HashSet<ID> =
        containers_map . get (& current_id)
        . cloned ()
        . unwrap_or_default ();
      if containers . is_empty () {
        // Root: no containers.
        result . insert (
          tracked . original,
          ContainerwardPathStats {
            length : tracked . depth,
            forks  : 1,
            cycles : false } );
      } else if containers . len () > 1 {
        // Fork: multiple containers.
        let cycles : bool =
          containers . iter ()
          . any ( |c| tracked . visited . contains (c) );
        result . insert (
          tracked . original,
          ContainerwardPathStats {
            length : tracked . depth,
            forks  : containers . len (),
            cycles } );
      } else {
        // Exactly one container.
        let container : ID =
          containers . into_iter () . next () . unwrap ();
        if tracked . visited . contains (& container) {
          // Cycle detected.
          result . insert (
            tracked . original,
            ContainerwardPathStats {
              length : tracked . depth,
              forks  : 1,
              cycles : true } );
        } else {
          // Advance: continue climbing.
          let mut next_visited : HashSet<ID> =
            tracked . visited;
          next_visited . insert ( container . clone () );
          next_frontier . push ((
            container,
            TrackedPath {
              original : tracked . original,
              depth    : tracked . depth + 1,
              visited  : next_visited } )); } } }
    frontier = next_frontier; }
  Ok (result) }

/// Fast container lookup by primary ID.
/// Assumes the input is a primary ID (not an extra ID).
/// Skips the extra_id `or` pattern used by find_related_nodes,
/// which is the dominant cost in that query.
async fn find_container_ids_of_pid (
  db_name : &str,
  driver  : &TypeDBDriver,
  pid     : &ID,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let mut stream : ConceptRowStream = {
    let answer : QueryAnswer = tx . query (
      format! ( r#"match
        $contained isa node, has id "{}";
        $container isa node, has id $container_id;
        $rel isa contains ( container: $container,
                            contained: $contained );
        select $container_id;"#,
        pid ) ) . await ?;
    answer } . into_rows ();
  let mut result : HashSet<ID> = HashSet::new ();
  while let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row . get ("container_id") ? {
      result . insert ( ID (
        extract_payload_from_typedb_string_rep (
          & concept . to_string () )) ); } }
  Ok (result) }

/// See path_to_end_cycle_and_or_branches.
/// This is the case that searches sourceward.
pub async fn path_containerward_to_end_cycle_and_or_branches (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < ( Vec<ID>,    // the path, starting with the input
                Option<ID>, // the first repeated node, if any
                HashSet<ID> // branches, if any
), Box<dyn Error> > {
  path_to_end_cycle_and_or_branches (
    db_name,
    driver,
    node,
    "contains",
    "contained",
    "container"
  ) . await }

/// See path_to_end_cycle_and_or_branches.
/// This is the case that searches containerward.
pub async fn path_sourceward_to_end_cycle_and_or_branches (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < ( Vec<ID>,    // the path, starting with the input
                Option<ID>, // the first repeated node, if any
                HashSet<ID> // branches, if any
), Box<dyn Error> > {
  path_to_end_cycle_and_or_branches (
    db_name,
    driver,
    node,
    "textlinks_to",
    "dest",
    "source"
  ) . await }

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

  let mut path : Vec<ID> = vec![ node . clone () ];
  let mut path_set : HashSet<ID> =
    // path and path_set have the same nodes.
    HashSet::from ( [ node . clone() ] );
  let mut current_node : ID = node . clone ();
  loop {
    let related_nodes : HashSet<ID> =
      find_related_nodes (
        db_name, driver, & [ current_node . clone () ],
        relation, input_role, output_role ) . await ?;
    if related_nodes . is_empty () {
      // No related node found, so this is the end.
      return Ok (( path, None, HashSet::new () ));
    } else {
      let cycle_node : Option<ID> =
        // 'Some' if the related node has been seen already.
        related_nodes . iter ()
          . find ( |&c| path_set . contains (c) )
          . cloned ();
      if ( related_nodes . len () == 1
           && cycle_node . is_none () ) {
        // Add the related node to the path and continue.
        let next_node : ID =
          related_nodes . into_iter() . next() . unwrap();
        path . push ( next_node . clone () );
        path_set . insert ( next_node . clone () );
        current_node = next_node;
      } else { // We are at a fork, or a cycle, or both.
        return Ok ((
          path,
          cycle_node,
          ( if related_nodes . len () == 1 {
            HashSet::new () }
            else {related_nodes} ) ));
        }} }}

/// Generalized function to find related nodes via a specified relationship.
/// Returns the IDs of nodes in the `output_role` position
/// related to any of the input nodes.
pub async fn find_related_nodes (
  db_name     : &str,
  driver      : &TypeDBDriver,
  nodes       : &[ID],
  relation    : &str,
  input_role  : &str,
  output_role : &str
) -> Result < HashSet<ID>, Box<dyn Error> > {
  if nodes . is_empty () {
    return Ok ( HashSet::new () ); }
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let input_id_var : String =
    format!("{}_id", input_role);
  let output_id_var : String =
    format!("{}_id", output_role);
  let mut stream : ConceptRowStream = {
    let answer : QueryAnswer = tx . query ( {
      let input_disjunction : String =
        build_id_disjunction ( nodes, &input_id_var );
      let match_clause : String =
        format!( r#" match
                       ${} isa node, has id ${};
                    {{ ${} isa node, has id ${}; }} or
                    {{ ${} isa node;
                       $e isa extra_id, has id ${};
                       $extra_rel isa has_extra_id ( node:     ${},
                                                     extra_id: $e ); }};
                    {};"#,
                     output_role, output_id_var,
                     input_role, input_id_var,
                     input_role, input_id_var, input_role,
                     input_disjunction );
      let relationship_and_select : String = format!( r#"
                       $rel isa {} ( {}: ${},
                                     {}: ${} );
                       select ${};"#,
                       relation, input_role, input_role,
                       output_role, output_role,
                       output_id_var );
      let query : String = format!(
        "{}{}", match_clause, relationship_and_select);
      query } ) . await?;
    answer } . into_rows ();
  let mut related_nodes : HashSet<ID> = HashSet::new ();
  while let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row . get (&output_id_var) ? {
      related_nodes . insert ( ID (
        extract_payload_from_typedb_string_rep (
          &concept . to_string () )) ); }}
  Ok (related_nodes) }

/// Runs a single TypeDB query to get both PID and source.
/// Returns None if not found.
pub async fn pid_and_source_from_id (
  db_name : &str,
  driver  : &TypeDBDriver,
  skgid  : &ID
) -> Result < Option<(ID, SourceName)>, Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = {
    let answer : QueryAnswer = tx . query ( {
      let query : String = format! (
        r#"match
          $node isa node,
                has id $primary_id,
                has source $source;
          {{ $node has id "{}"; }} or
          {{ $e   isa     extra_id, has id "{}";
             $rel isa has_extra_id ( node: $node,
                                     extra_id: $e ); }} ;
          fetch {{
            "primary_id": $primary_id,
            "source": $source
          }};"#,
        skgid,
        skgid );
      query } ) . await ?;
    answer } {
      if let Some (doc_result) = stream . next () . await {
        let doc : ConceptDocument = doc_result ?;
        if let Some ( Node::Map ( ref map ) ) = doc . root {
          let primary_id_opt : Option < ID > =
            map . get ("primary_id")
            . and_then (extract_id_from_node);
          let source_opt : Option < SourceName > =
            map . get ("source")
            . and_then ( | node : & Node | {
              if let Node::Leaf ( Some (leaf) ) = node {
                if let Leaf::Concept (concept) = leaf {
                  return Some ( SourceName::from (
                    extract_payload_from_typedb_string_rep (
                      & concept . to_string () ) ) ); }}
              None } );
          if let ( Some (pid), Some (source) )
            = ( primary_id_opt, source_opt )
          { return Ok ( Some ( ( pid, source ) ) ); }} }}
  Ok (None) }
