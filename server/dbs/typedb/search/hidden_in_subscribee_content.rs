/// Partition a subscribee's content based on what the subscriber hides.
///
/// Given subscriber R and subscribee E, returns two sets:
/// - visible: what E contains that R does NOT hide
/// - hidden: what E contains that R hides

use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

use crate::types::misc::ID;
use crate::dbs::typedb::search::find_related_nodes;

/// Partition E's direct content into visible and hidden portions,
/// based on what R hides_from_its_subscriptions.
///
/// Returns (visible, hidden) where:
/// - visible = E_content - R_hides
/// - hidden = E_content ∩ R_hides
pub async fn partition_subscribee_content_for_subscriber (
  db_name        : &str,
  driver         : &TypeDBDriver,
  subscriber_pid : &ID,
  subscribee_pid : &ID,
) -> Result < ( HashSet < ID >,   // visible
               HashSet < ID > ),  // hidden
             Box < dyn Error > > {
  let subscriber_hides : HashSet < ID > =
    what_node_hides (
      db_name, driver, subscriber_pid ) . await ?;
  let subscribee_content : HashSet < ID > =
    what_nodes_contain (
      db_name, driver, & [ subscribee_pid . clone () ] ) . await ?;
  Ok (( { let visible : HashSet < ID > =
            subscribee_content . iter () //  (rust-mode--indent-line)
            . filter ( | id | ! subscriber_hides . contains ( id ) )
            . cloned () . collect ();
          visible },
        { let hidden : HashSet < ID > =
            subscribee_content . iter ()
            . filter ( | id | subscriber_hides . contains ( id ) )
            . cloned () . collect ();
          hidden } )) }

/// Returns all IDs that the subscriber hides_from_its_subscriptions.
pub async fn what_node_hides (
  db_name        : &str,
  driver         : &TypeDBDriver,
  subscriber_pid : &ID,
) -> Result < HashSet < ID >, Box < dyn Error > > {
  find_related_nodes (
    db_name, driver,
    & [ subscriber_pid . clone () ],
    "hides_from_its_subscriptions",
    "hider",
    "hidden" ) . await }

/// Returns the union of all subscribees' direct content.
/// Uses a single batched query.
pub async fn what_nodes_contain (
  db_name         : &str,
  driver          : &TypeDBDriver,
  subscribee_pids : &[ID],
) -> Result < HashSet < ID >, Box < dyn Error > > {
  find_related_nodes (
    db_name, driver, subscribee_pids,
    "contains",
    "container",
    "contained" ) . await }
