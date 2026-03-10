/// PURPOSE: Fetch all graph-node statistics for a set of PIDs.
///
/// Runs one TypeDB query PER PID, all in parallel.
/// Each individual query is fast (~15ms) because it matches
/// exactly one node. The old approach used a single query with
/// an N-way disjunction, which scaled very non-linearly
/// (1 PID: 15ms, 24 PIDs: 4300ms).
///
/// PITFALL: Assumes input IDs are primary IDs, not extra IDs.
/// This is always true for the graphnodestats path, which collects
/// PIDs from the already-built viewnode tree.

use crate::dbs::typedb::util::concept_document::extract_id_from_map;
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::GraphNodeStats;

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

/// Everything 'set_graphnodestats_in_forest' needs from TypeDB.
pub struct AllGraphNodeStats {
  pub num_containers : HashMap < ID, usize >,
  pub num_contents   : HashMap < ID, usize >,
  pub num_links_in   : HashMap < ID, usize >,
  pub has_subscribes : HashSet < ID >,
  pub has_overrides  : HashSet < ID >,
  pub container_to_contents : HashMap < ID, HashSet < ID > >,
  pub content_to_containers : HashMap < ID, HashSet < ID > >,
}

impl AllGraphNodeStats {
  pub fn empty () -> AllGraphNodeStats {
    AllGraphNodeStats {
      num_containers        : HashMap::new (),
      num_contents          : HashMap::new (),
      num_links_in          : HashMap::new (),
      has_subscribes        : HashSet::new (),
      has_overrides         : HashSet::new (),
      container_to_contents : HashMap::new (),
      content_to_containers : HashMap::new (),
    } } }

/// Extract GraphNodeStats for a single PID
/// from AllGraphNodeStats and an optional disk SkgNode.
pub fn graphnodestats_for_pid (
  pid     : &ID,
  stats   : &AllGraphNodeStats,
  skgnode : Option<&SkgNode>,
) -> GraphNodeStats {
  let aliasing : bool =
    skgnode
    . map ( |n| ! n . aliases . or_default () . is_empty () )
    . unwrap_or (false);
  let extra_ids : bool =
    skgnode
    . map ( |n| ! n . extra_ids . is_empty () )
    . unwrap_or (false);
  GraphNodeStats {
    aliasing,
    extraIDs      : extra_ids,
    overriding    : stats . has_overrides  . contains (pid),
    subscribing   : stats . has_subscribes . contains (pid),
    numContainers : stats . num_containers . get (pid) . copied (),
    numContents   : stats . num_contents   . get (pid) . copied (),
    numLinksIn    : stats . num_links_in   . get (pid) . copied (),
    containerwardPath : None,
  } }

/// Stats for a single PID, returned by 'fetch_one_pid_stats'.
struct OnePidStats {
  pid            : ID,
  num_containers : usize,
  num_contents   : usize,
  num_links_in   : usize,
  subscribes     : bool,
  overrides      : bool,
  container_ids  : Vec < ID >,
  content_ids    : Vec < ID >,
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
  let futures : Vec < _ > =
    pids . iter ()
    . map ( |pid| fetch_one_pid_stats ( db_name, driver, pid ) )
    . collect ();
  let results : Vec < Result < OnePidStats, Box < dyn Error > > > =
    futures::future::join_all (futures) . await;
  let mut num_containers : HashMap < ID, usize > = HashMap::new ();
  let mut num_contents   : HashMap < ID, usize > = HashMap::new ();
  let mut num_links_in   : HashMap < ID, usize > = HashMap::new ();
  let mut has_subscribes : HashSet < ID > = HashSet::new ();
  let mut has_overrides  : HashSet < ID > = HashSet::new ();
  let mut container_to_contents : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  let mut content_to_containers : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  for result in results {
    let s : OnePidStats = result ?;
    num_containers . insert ( s . pid . clone (), s . num_containers );
    num_contents   . insert ( s . pid . clone (), s . num_contents );
    num_links_in   . insert ( s . pid . clone (), s . num_links_in );
    if s . subscribes {
      has_subscribes . insert ( s . pid . clone () ); }
    if s . overrides {
      has_overrides . insert ( s . pid . clone () ); }
    for cid in s . container_ids {
      if pid_set . contains (&cid) {
        content_to_containers
          . entry ( s . pid . clone () )
          . or_insert_with (HashSet::new)
          . insert (cid); }}
    for cid in s . content_ids {
      if pid_set . contains (&cid) {
        container_to_contents
          . entry ( s . pid . clone () )
          . or_insert_with (HashSet::new)
          . insert (cid); }}}
  Ok ( AllGraphNodeStats {
    num_containers,
    num_contents,
    num_links_in,
    has_subscribes,
    has_overrides,
    container_to_contents,
    content_to_containers,
  }) }

async fn fetch_one_pid_stats (
  db_name : &str,
  driver  : &TypeDBDriver,
  pid     : &ID,
) -> Result < OnePidStats, Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  let query : String =
    format! ( r#"match
        $node isa node, has id "{}";
        fetch {{
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
      pid );
  let mut container_ids : Vec < ID > = Vec::new ();
  let mut content_ids   : Vec < ID > = Vec::new ();
  let mut num_containers : usize = 0;
  let mut num_contents   : usize = 0;
  let mut num_links_in   : usize = 0;
  let mut subscribes     : bool = false;
  let mut overrides      : bool = false;
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) =
    tx . query (query) . await ?
  { while let Some (doc_result) = stream . next () . await {
      let doc : ConceptDocument = doc_result ?;
      if let Some ( Node::Map ( ref map ) ) = doc . root {
        if let Some ( Node::List (list) ) =
          map . get ("containers")
        { num_containers = list . len ();
          for item in list {
            if let Some (cid) = extract_id_from_map ( item, "id" ) {
              container_ids . push (cid); }}}
        if let Some ( Node::List (list) ) =
          map . get ("contents")
        { num_contents = list . len ();
          for item in list {
            if let Some (cid) = extract_id_from_map ( item, "id" ) {
              content_ids . push (cid); }}}
        if let Some ( Node::List (list) ) =
          map . get ("link_sources")
        { num_links_in = list . len (); }
        if let Some ( Node::List (list) ) =
          map . get ("subscribes_related")
        { subscribes = ! list . is_empty (); }
        if let Some ( Node::List (list) ) =
          map . get ("overrides_related")
        { overrides = ! list . is_empty (); }}}}
  Ok ( OnePidStats {
    pid : pid . clone (),
    num_containers,
    num_contents,
    num_links_in,
    subscribes,
    overrides,
    container_ids,
    content_ids,
  }) }
