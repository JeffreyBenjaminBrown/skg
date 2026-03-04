/// PURPOSE: Fetch all graph-node statistics for a set of PIDs
/// in a SINGLE TypeDB query with nested subqueries.
///
/// Replaces 6 separate queries (count_containers, count_contents,
/// count_link_sources, has_subscribes, has_overrides, contains_from_pids)
/// with one combined fetch.
///
/// PITFALL: Assumes input IDs are primary IDs, not extra IDs.
/// This is always true for the graphnodestats path, which collects
/// PIDs from the already-built viewnode tree.

use crate::dbs::typedb::util::concept_document::{
  build_id_disjunction,
  extract_id_from_node,
  extract_id_from_map};
use crate::types::misc::ID;

use futures::StreamExt;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::{
  answer::{QueryAnswer, ConceptDocument},
  answer::concept_document::Node,
  Transaction,
  TransactionType,
  TypeDBDriver,
};

/// Everything 'set_graphnodestats_in_forest' needs from TypeDB,
/// fetched in a single round-trip.
pub struct AllGraphNodeStats {
  pub num_containers : HashMap < ID, usize >,
  pub num_contents   : HashMap < ID, usize >,
  pub num_links_in   : HashMap < ID, usize >,
  pub has_subscribes : HashSet < ID >,
  pub has_overrides  : HashSet < ID >,
  pub container_to_contents : HashMap < ID, HashSet < ID > >,
  pub content_to_containers : HashMap < ID, HashSet < ID > >,
}

pub async fn fetch_all_graphnodestats (
  db_name : &str,
  driver  : &TypeDBDriver,
  pids    : &[ID],
) -> Result < AllGraphNodeStats, Box<dyn Error> > {
  if pids . is_empty () {
    return Ok ( AllGraphNodeStats {
      num_containers : HashMap::new (),
      num_contents   : HashMap::new (),
      num_links_in   : HashMap::new (),
      has_subscribes : HashSet::new (),
      has_overrides  : HashSet::new (),
      container_to_contents : HashMap::new (),
      content_to_containers : HashMap::new (),
    }); }
  let pid_set : HashSet < ID > =
    pids . iter () . cloned () . collect ();
  let mut num_containers : HashMap < ID, usize > = HashMap::new ();
  let mut num_contents   : HashMap < ID, usize > = HashMap::new ();
  let mut num_links_in   : HashMap < ID, usize > = HashMap::new ();
  let mut has_subscribes : HashSet < ID > = HashSet::new ();
  let mut has_overrides  : HashSet < ID > = HashSet::new ();
  let mut container_to_contents : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  let mut content_to_containers : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  for pid in pids {
    num_containers . insert ( pid . clone (), 0 );
    num_contents   . insert ( pid . clone (), 0 );
    num_links_in   . insert ( pid . clone (), 0 ); }
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let disjunction_clauses : String =
    build_id_disjunction ( pids, "node_id" );
  let query : String =
    format! ( r#"match
        $node isa node, has id $node_id;
        {};
        fetch {{
          "node_id": $node_id,
          "containers": [
            match
              $c1 isa node, has id $c1id;
              $rel1 isa contains ( container: $c1,
                                   contained: $node );
            fetch {{ "id": $c1id }};
          ],
          "contents": [
            match
              $c2 isa node, has id $c2id;
              $rel2 isa contains ( container: $node,
                                   contained: $c2 );
            fetch {{ "id": $c2id }};
          ],
          "link_sources": [
            match
              $s isa node, has id $sid;
              $rel3 isa textlinks_to ( source: $s,
                                       dest:   $node );
            fetch {{ "id": $sid }};
          ],
          "subscribes_related": [
            match
              $sub isa node, has id $subid;
              {{ $rel4 isa subscribes ( subscriber: $node,
                                        subscribee: $sub ); }} or
              {{ $rel4 isa subscribes ( subscriber: $sub,
                                        subscribee: $node ); }};
            fetch {{ "id": $subid }};
          ],
          "overrides_related": [
            match
              $ov isa node, has id $ovid;
              {{ $rel5 isa overrides_view_of ( replacement: $node,
                                               replaced:    $ov ); }} or
              {{ $rel5 isa overrides_view_of ( replacement: $ov,
                                               replaced:    $node ); }};
            fetch {{ "id": $ovid }};
          ]
        }};"#,
      disjunction_clauses );
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) =
    tx . query (query) . await ?
  { while let Some (doc_result) = stream . next () . await {
      let doc : ConceptDocument = doc_result ?;
      if let Some ( Node::Map ( ref map ) ) = doc . root {
        let node_id_opt : Option < ID > =
          map . get ("node_id")
          . and_then (extract_id_from_node);
        if let Some (node_id) = node_id_opt {
          if let Some ( Node::List (list) ) =
            map . get ("containers")
          { num_containers . insert ( node_id . clone (), list . len () );
            for item in list {
              if let Some (cid) = extract_id_from_map ( item, "id" ) {
                if pid_set . contains (&cid) {
                  content_to_containers
                    . entry ( node_id . clone () )
                    . or_insert_with (HashSet::new)
                    . insert (cid); }}}}
          if let Some ( Node::List (list) ) =
            map . get ("contents")
          { num_contents . insert ( node_id . clone (), list . len () );
            for item in list {
              if let Some (cid) = extract_id_from_map ( item, "id" ) {
                if pid_set . contains (&cid) {
                  container_to_contents
                    . entry ( node_id . clone () )
                    . or_insert_with (HashSet::new)
                    . insert (cid); }}}}
          if let Some ( Node::List (list) ) =
            map . get ("link_sources")
          { num_links_in . insert ( node_id . clone (), list . len () ); }
          if let Some ( Node::List (list) ) =
            map . get ("subscribes_related")
          { if ! list . is_empty () {
              has_subscribes . insert ( node_id . clone () ); }}
          if let Some ( Node::List (list) ) =
            map . get ("overrides_related")
          { if ! list . is_empty () {
              has_overrides . insert ( node_id . clone () ); }}
        }}}}
  Ok ( AllGraphNodeStats {
    num_containers,
    num_contents,
    num_links_in,
    has_subscribes,
    has_overrides,
    container_to_contents,
    content_to_containers,
  }) }
