/// Partition a subscribee's content based on what the subscriber hides.
///
/// Given subscriber R and subscribee E, returns two sets:
/// - visible: what E contains that R does NOT hide
/// - hidden: what E contains that R hides

use std::collections::HashSet;
use std::error::Error;
use neo4rs::Graph;

use crate::types::misc::ID;
use crate::dbs::neo4j::search::find_related_nodes;

/// Partition E's direct content into visible and hidden portions,
/// based on what R hides.
///
/// Returns (visible, hidden) where:
/// - visible = E_content - R_hides
/// - hidden = E_content ∩ R_hides
pub async fn partition_subscribee_content_for_subscriber (
  graph          : &Graph,
  subscriber_pid : &ID,
  subscribee_pid : &ID,
) -> Result < ( HashSet < ID >,
               HashSet < ID > ),
             Box < dyn Error > > {
  let subscriber_hides : HashSet < ID > =
    what_node_hides (
      graph, subscriber_pid ) . await ?;
  let subscribee_content : HashSet < ID > =
    what_nodes_contain (
      graph, & [ subscribee_pid . clone () ] ) . await ?;
  Ok (( { let visible : HashSet < ID > =
            subscribee_content . iter ()
            . filter ( | id | ! subscriber_hides . contains ( id ) )
            . cloned () . collect ();
          visible },
        { let hidden : HashSet < ID > =
            subscribee_content . iter ()
            . filter ( | id | subscriber_hides . contains ( id ) )
            . cloned () . collect ();
          hidden } )) }

/// Returns all IDs that the subscriber hides.
pub async fn what_node_hides (
  graph          : &Graph,
  subscriber_pid : &ID,
) -> Result < HashSet < ID >, Box < dyn Error > > {
  find_related_nodes (
    graph,
    & [ subscriber_pid . clone () ],
    "hides",
    "hider" ) . await }

/// Returns the union of all subscribees' direct content.
/// Uses a single batched query.
pub async fn what_nodes_contain (
  graph           : &Graph,
  subscribee_pids : &[ID],
) -> Result < HashSet < ID >, Box < dyn Error > > {
  find_related_nodes (
    graph, subscribee_pids,
    "contains",
    "container" ) . await }
