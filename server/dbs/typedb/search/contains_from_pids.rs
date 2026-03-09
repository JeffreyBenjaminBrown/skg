use std::error::Error;
use std::collections::{HashMap, HashSet};
use typedb_driver::{
  answer::{QueryAnswer, ConceptDocument},
  answer::concept_document::Node,
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use futures::stream::{self, StreamExt};

use crate::types::misc::ID;
use crate::dbs::typedb::util::concept_document::extract_id_from_map;


/// PURPOSE: Find all 'contains' relations among a collection of nodes.
/// Sends one query per PID, bounded by TYPEDB_CONCURRENT_TRANSACTIONS.
/// Takes a vector of IDs (PIDs) and returns two maps:
/// 1. container_to_contents: Map from each container to the set of nodes it contains
/// 2. content_to_containers: Map from each contained node to the set of containers
/// If a value (set) is empty, the corresponding key is omitted from the map.
pub async fn contains_from_pids (
  db_name : &str,
  driver  : &TypeDBDriver,
  pids : &[ID]
) -> Result <
    ( HashMap < ID, HashSet < ID > >, // maps container to contents
      HashMap < ID, HashSet < ID > >), // maps content to containers
  Box < dyn Error > > {
  if pids . is_empty () {
    return Ok (( HashMap::new (), HashMap::new () )); }
  let pid_set : HashSet<ID> =
    pids . iter () . cloned () . collect ();
  let results : Vec < Result <
      (ID, Vec<ID>, Vec<ID>),
      Box<dyn Error> > > =
    stream::iter ( pids . iter ()
      . map ( |pid| contains_for_one_pid (
                db_name, driver, pid )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  let mut container_to_contents : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  let mut content_to_containers : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  for result in results {
    let (node_id, contained_ids, container_ids)
      : (ID, Vec<ID>, Vec<ID>) = result ?;
    for contained_id in contained_ids {
      // Only track if contained_id is in our input set.
      if pid_set . contains (&contained_id) {
        container_to_contents
          . entry ( node_id . clone () )
          . or_insert_with (HashSet::new)
          . insert (contained_id); }}
    for container_id in container_ids {
      // Only track if container_id is in our input set.
      if pid_set . contains (&container_id) {
        content_to_containers
          . entry ( node_id . clone () )
          . or_insert_with (HashSet::new)
          . insert (container_id); }}}
  Ok (( container_to_contents,
        content_to_containers )) }

/// Query contains and contained_by for a single PID.
async fn contains_for_one_pid (
  db_name : &str,
  driver  : &TypeDBDriver,
  pid     : &ID,
) -> Result < (ID, Vec<ID>, Vec<ID>), Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) = {
    let answer : QueryAnswer = tx . query ( format! (
      r#"match
           $node isa node, has id "{}";
           fetch {{
             "contains": [
               match
                 $child isa node, has id $contained_id;
                 $rel isa contains ( container: $node,
                                     contained: $child );
               fetch {{ "contained_id": $contained_id }};
             ],
             "contained_by": [
               match
                 $parent isa node, has id $container_id;
                 $rel2 isa contains ( container: $parent,
                                      contained: $node );
               fetch {{ "container_id": $container_id }};
             ]
           }};"#,
      pid ) ) . await ?;
    answer }
  { if let Some (doc_result) = stream . next () . await {
      let doc : ConceptDocument = doc_result ?;
      if let Some ( Node::Map ( ref map ) ) = doc . root {
        let mut contained_ids : Vec<ID> = Vec::new ();
        let mut container_ids : Vec<ID> = Vec::new ();
        if let Some ( Node::List (contains_list) ) =
          map . get ("contains")
        { for item in contains_list {
            if let Some (id) =
              extract_id_from_map ( item, "contained_id" )
            { contained_ids . push (id); }}}
        if let Some ( Node::List (contained_by_list) ) =
          map . get ("contained_by")
        { for item in contained_by_list {
            if let Some (id) =
              extract_id_from_map ( item, "container_id" )
            { container_ids . push (id); }}}
        return Ok (( pid . clone (),
                     contained_ids,
                     container_ids )); }}}
  Ok (( pid . clone (), Vec::new (), Vec::new () )) }
