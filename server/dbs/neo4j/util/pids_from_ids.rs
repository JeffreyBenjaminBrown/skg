use std::error::Error;
use std::collections::HashMap;
use neo4rs::Graph;
use ego_tree::{NodeRef, NodeMut, NodeId, Tree};

use crate::types::unchecked_viewnode::{UncheckedViewNode, UncheckedViewNodeKind};
use crate::types::misc::ID;

/// Collect all IDs, batch-lookup their PIDs in Neo4j, then replace.
pub async fn replace_ids_with_pids(
  forest  : &mut Tree<UncheckedViewNode>,
  root_id : NodeId,
  graph   : &Graph,
) -> Result<(), Box<dyn Error>> {
  let mut ids_to_lookup: Vec<ID> = Vec::new();
  collect_ids_in_tree( forest.root(),
                       &mut ids_to_lookup );
  let pid_map: HashMap<ID, Option<ID>> =
    pids_from_ids( graph, &ids_to_lookup
    ). await?;
  if let Some(root_mut) = forest.get_mut(root_id) {
    assign_pids_throughout_tree_from_map(
      root_mut, &pid_map); }
  Ok(( )) }

/// Collect IDs for bulk PID lookup
pub fn collect_ids_in_tree (
  node_ref : NodeRef < UncheckedViewNode >,
  ids_to_lookup : & mut Vec < ID >
) {
  if let UncheckedViewNodeKind::True ( t ) =
    &node_ref . value () . kind
  { if let Some ( id ) = &t . id_opt
    { ids_to_lookup . push ( id . clone () ); }}
  for child in node_ref . children () {
    collect_ids_in_tree (
      child,
      ids_to_lookup ); } }

pub fn assign_pids_throughout_tree_from_map (
  mut node_ref : NodeMut < UncheckedViewNode >,
  pid_map : & HashMap < ID, Option < ID > >
) {
  if let UncheckedViewNodeKind::True(t)
    = &mut node_ref . value() . kind
    { let pid_opt : Option < ID > = t . id_opt . as_ref ()
        . and_then ( |id| pid_map . get ( id ) )
        . and_then ( |opt| opt . clone () );
      if let Some ( pid ) = pid_opt {
        t . id_opt = Some ( pid ); }}
  { for child_treeid in {
      let treeid : NodeId = node_ref . id ();
      let child_treeids : Vec < NodeId > = {
        let tree : &Tree<UncheckedViewNode> = node_ref . tree ();
        tree . get ( treeid ) . unwrap ()
          . children () . map ( | child | child . id () )
          . collect () };
      child_treeids } {
      if let Some ( child_mut )
        = node_ref . tree () . get_mut ( child_treeid )
      { assign_pids_throughout_tree_from_map (
        child_mut, pid_map ); }} }}

/// PURPOSE: Run one Neo4j query to look up PIDs for multiple IDs.
/// Return a HashMap mapping each ID to its PID (or None if not found).
pub async fn pids_from_ids (
  graph    : &Graph,
  node_ids : &[ID]
) -> Result < HashMap < ID, Option < ID > >, Box < dyn Error > > {
  if node_ids . is_empty () {
    return Ok ( HashMap::new () ); }
  let mut result : HashMap < ID, Option < ID > > =
    HashMap::new ();
  for node_id in node_ids {
    result . insert ( node_id . clone (), None ); }
  let id_strings : Vec<String> =
    node_ids . iter () . map ( |id| id.0.clone () ) . collect ();
  let mut result_stream =
    graph . execute (
      neo4rs::query ( "\
        UNWIND $ids AS input_id \
        OPTIONAL MATCH (direct:Node {id: input_id}) \
        OPTIONAL MATCH (alias:IdAlias {id: input_id}) \
        WITH input_id, coalesce(direct.id, alias.primary_id) AS pid \
        RETURN input_id AS queried_id, pid AS primary_id" )
      . param ( "ids", id_strings )
    ) . await ?;
  while let Some ( row ) = result_stream . next () . await ? {
    let queried_id : String = row . get ( "queried_id" ) ?;
    let primary_id : Option<String> = row . get ( "primary_id" ) ?;
    result . insert (
      ID ( queried_id ),
      primary_id . map ( ID ) ); }
  Ok ( result ) }
