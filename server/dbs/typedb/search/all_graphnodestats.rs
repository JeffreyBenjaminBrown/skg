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

use crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS;
use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::dbs::typedb::util::concept_document::extract_id_from_map;
use crate::types::misc::ID;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{GraphNodeStats, NodeContainRels, NodeLinksourceRels};

use futures::stream::{self, StreamExt};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::{
  answer::{QueryAnswer, ConceptDocument},
  answer::concept_document::Node,
  Transaction,
  TransactionType,
  TypeDBDriver,
};

/// Everything 'set_graphnodestats_in_viewforest' needs from TypeDB.
pub struct AllGraphNodeStats {
  pub num_containers               : HashMap < ID, usize >,
  pub num_contents                 : HashMap < ID, usize >,
  pub num_links_in_from_containers : HashMap < ID, usize >,
  pub num_links_in_from_leaves     : HashMap < ID, usize >,
  pub has_subscribes               : HashSet < ID >,
  pub has_overrides                : HashSet < ID >,
  pub container_to_contents        : HashMap < ID, HashSet < ID > >,
  pub content_to_containers        : HashMap < ID, HashSet < ID > >,
}

impl AllGraphNodeStats {
  pub fn empty () -> AllGraphNodeStats {
    AllGraphNodeStats {
      num_containers               : HashMap::new (),
      num_contents                 : HashMap::new (),
      num_links_in_from_containers : HashMap::new (),
      num_links_in_from_leaves     : HashMap::new (),
      has_subscribes               : HashSet::new (),
      has_overrides                : HashSet::new (),
      container_to_contents        : HashMap::new (),
      content_to_containers        : HashMap::new (),
    } } }

/// Extract GraphNodeStats for a single PID
/// from AllGraphNodeStats and an optional disk NodeComplete.
pub fn graphnodestats_for_pid (
  pid     : &ID,
  stats   : &AllGraphNodeStats,
  nodecomplete : Option<&NodeComplete>,
) -> GraphNodeStats {
  let aliasing : bool =
    nodecomplete
    . map ( |n| ! n . aliases . or_default () . is_empty () )
    . unwrap_or (false);
  let extra_ids : bool =
    nodecomplete
    . map ( |n| ! n . extra_ids . is_empty () )
    . unwrap_or (false);
  let contain_rels : Option<NodeContainRels> =
    stats . num_containers . get (pid) . copied ()
    . map ( |containers| {
      let contents : usize =
        stats . num_contents . get (pid) . copied ()
        . unwrap_or (0);
      NodeContainRels { containers, contents }} );
  let from_containers : usize =
    stats . num_links_in_from_containers
    . get (pid) . copied () . unwrap_or (0);
  let from_leaves : usize =
    stats . num_links_in_from_leaves
    . get (pid) . copied () . unwrap_or (0);
  let linksource_rels : Option<NodeLinksourceRels> =
    Some ( NodeLinksourceRels {
      sources_with_content    : from_containers,
      sources_without_content : from_leaves } );
  GraphNodeStats {
    aliasing,
    extraIDs       : extra_ids,
    overriding     : stats . has_overrides  . contains (pid),
    subscribing    : stats . has_subscribes . contains (pid),
    containRels    : contain_rels,
    linksourceRels : linksource_rels, }}

/// Stats for a single PID, returned by 'fetch_one_pid_stats'.
struct OnePidStats {
  pid                          : ID,
  num_containers               : usize,
  num_contents                 : usize,
  num_links_in_from_containers : usize,
  num_links_in_from_leaves     : usize,
  subscribes                   : bool,
  overrides                    : bool,
  container_ids                : Vec < ID >,
  content_ids                  : Vec < ID >,
}

/// Dispatcher: routes to 'fetch_all_graphnodestats_in_rust' when the
/// in-Rust graph is initialized, otherwise to
/// 'fetch_all_graphnodestats_from_typedb'. In the running server the
/// in-Rust graph path is always taken; the TypeDB path is exercised only by
/// tests that bypass 'init_global_handle_for_first_time_or_panic'.
pub async fn fetch_all_graphnodestats (
  db_name : &str,
  driver  : &TypeDBDriver,
  pids    : &[ID],
) -> Result < AllGraphNodeStats, Box<dyn Error> > {
  if pids . is_empty () {
    return Ok ( AllGraphNodeStats::empty() ); }
  let pid_set : HashSet < ID > =
    pids . iter () . cloned () . collect ();
  if let Some (graph_snap) = snapshot_global () {
    return Ok ( fetch_all_graphnodestats_in_rust (
      &graph_snap, pids, &pid_set ) ); }
  fetch_all_graphnodestats_from_typedb (
    db_name, driver, pids, &pid_set ) . await }

/// TypeDB-backed implementation. Exercised only by tests that bypass
/// 'init_global_handle_for_first_time_or_panic'; in the running server the dispatcher routes
/// to 'fetch_all_graphnodestats_in_rust' instead. Uses
/// 'buffer_unordered (TYPEDB_CONCURRENT_TRANSACTIONS)' for consistency
/// with the rest of the TypeDB module.
async fn fetch_all_graphnodestats_from_typedb (
  db_name : &str,
  driver  : &TypeDBDriver,
  pids    : &[ID],
  pid_set : &HashSet<ID>,
) -> Result < AllGraphNodeStats, Box<dyn Error> > {
  let results : Vec < Result < OnePidStats, Box < dyn Error > > > =
    stream::iter ( pids . iter ()
      . map ( |pid| fetch_one_pid_stats ( db_name, driver, pid ) ) )
    . buffer_unordered ( TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  let mut num_containers : HashMap < ID, usize > = HashMap::new ();
  let mut num_contents   : HashMap < ID, usize > = HashMap::new ();
  let mut num_links_in_from_containers : HashMap < ID, usize > =
    HashMap::new ();
  let mut num_links_in_from_leaves : HashMap < ID, usize > =
    HashMap::new ();
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
    num_links_in_from_containers . insert (
      s . pid . clone (), s . num_links_in_from_containers );
    num_links_in_from_leaves . insert (
      s . pid . clone (), s . num_links_in_from_leaves );
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
    num_links_in_from_containers,
    num_links_in_from_leaves,
    has_subscribes,
    has_overrides,
    container_to_contents,
    content_to_containers,
  }) }

/// In-Rust-graph implementation. Computes every field from NodeRust and
/// the inverse indexes — no TypeDB round-trips.
///
/// PITFALL: 'has_subscribes' and 'has_overrides' are "in either
/// role": a node qualifies if it's on /either/ side of a
/// subscribes / overrides relation.
/// Matches the TypeDB version's behaviour.
fn fetch_all_graphnodestats_in_rust (
  graph   : &InRustGraph,
  pids    : &[ID],
  pid_set : &HashSet<ID>,
) -> AllGraphNodeStats {
  let mut num_containers               : HashMap<ID, usize> = HashMap::new ();
  let mut num_contents                 : HashMap<ID, usize> = HashMap::new ();
  let mut num_links_in_from_containers : HashMap<ID, usize> = HashMap::new ();
  let mut num_links_in_from_leaves     : HashMap<ID, usize> = HashMap::new ();
  let mut has_subscribes               : HashSet<ID> = HashSet::new ();
  let mut has_overrides                : HashSet<ID> = HashSet::new ();
  let mut container_to_contents
    : HashMap<ID, HashSet<ID>> = HashMap::new ();
  let mut content_to_containers
    : HashMap<ID, HashSet<ID>> = HashMap::new ();
  for pid in pids {
    let node_opt = graph . nodes . get (pid);
    // num_containers: size of inverse-contains set.
    let n_containers : usize =
      graph . contained_by . get (pid)
      . map ( |s| s . len () ) . unwrap_or (0);
    num_containers . insert ( pid . clone (), n_containers );
    // num_contents: length of outbound contains.
    let n_contents : usize =
      node_opt . map ( |n| n . contains . len () ) . unwrap_or (0);
    num_contents . insert ( pid . clone (), n_contents );
    // Link sources: partition textlinks_in by whether source has
    // non-empty contains.
    let (from_containers, from_leaves) : (usize, usize) =
      if let Some (sources) = graph . textlinks_in . get (pid) {
        let mut with    : usize = 0;
        let mut without : usize = 0;
        for src_pid in sources {
          let src_has_content : bool =
            graph . nodes . get (src_pid)
            . map ( |n| ! n . contains . is_empty () )
            . unwrap_or (false);
          if src_has_content { with += 1; } else { without += 1; } }
        (with, without) }
      else { (0, 0) };
    num_links_in_from_containers . insert ( pid . clone (),
                                            from_containers );
    num_links_in_from_leaves     . insert ( pid . clone (),
                                            from_leaves );
    // has_subscribes / has_overrides: either role.
    let out_sub : bool =
      node_opt . map ( |n| ! n . subscribes_to
                        . or_default () . is_empty () )
      . unwrap_or (false);
    let in_sub  : bool =
      graph . subscribers_of . get (pid)
      . map ( |s| ! s . is_empty () ) . unwrap_or (false);
    if out_sub || in_sub { has_subscribes
                           . insert ( pid . clone () ); }
    let out_ov : bool =
      node_opt . map ( |n| ! n . overrides_view_of
                        . or_default () . is_empty () )
      . unwrap_or (false);
    let in_ov  : bool =
      graph . replacements_of . get (pid)
      . map ( |s| ! s . is_empty () ) . unwrap_or (false);
    if out_ov || in_ov { has_overrides . insert ( pid . clone () ); }
    // container_to_contents[pid] = contains ∩ pid_set.
    // n.contains carries raw IDs; map each to its corresponding pid
    // (which might be itself) before the set-membership test
    // so references that are actually extra_ids of pids in pid_set
    // are matched.
    if let Some (n) = node_opt {
      let intersected : HashSet<ID> =
        n . contains . iter ()
        . map ( |cid| graph . pid_of (cid)
                 . unwrap_or_else ( || cid . clone () ) )
        . filter ( |pid| pid_set . contains (pid) )
        . collect ();
      if ! intersected . is_empty () {
        container_to_contents . insert ( pid . clone (),
                                         intersected ); }}
    // content_to_containers[pid] = contained_by ∩ pid_set
    if let Some (containers) = graph . contained_by . get (pid) {
      let intersected : HashSet<ID> =
        containers . iter ()
        . filter ( |cid| pid_set . contains (*cid) )
        . cloned () . collect ();
      if ! intersected . is_empty () {
        content_to_containers . insert ( pid . clone (),
                                         intersected ); }}}
  AllGraphNodeStats {
    num_containers,
    num_contents,
    num_links_in_from_containers,
    num_links_in_from_leaves,
    has_subscribes,
    has_overrides,
    container_to_contents,
    content_to_containers,
  } }

async fn fetch_one_pid_stats (
  db_name : &str,
  driver  : &TypeDBDriver,
  pid     : &ID,
) -> Result < OnePidStats, Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;
  // TODO for efficiency : container_link_sources joins textlinks_to with contains, producing one row per content of each source. We only need whether the source has *any* content. `select $sid; distinct;` collapses the duplicates, but TypeDB still does the full join internally — there is no per-row short-circuit (limit inside nested sub-fetches is a parse error in TypeDB 3.4). If a future TypeDB version adds per-source existence checks, replace that sub-fetch.
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
          "container_link_sources": [
            match
              $s isa node, has id $sid;
              $rel3 isa textlinks_to ( source: $s,
                                       dest:   $node );
              $sc isa node;
              $screl isa contains ( container: $s,
                                    contained: $sc );
            select $sid;
            distinct;
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
  let mut num_containers               : usize = 0;
  let mut num_contents                 : usize = 0;
  let mut num_links_in_total           : usize = 0;
  let mut num_links_in_from_containers : usize = 0;
  let mut subscribes                   : bool = false;
  let mut overrides                    : bool = false;
  if let QueryAnswer::ConceptDocumentStream ( _, mut stream )
    = tx . query (query) . await ?
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
        { num_links_in_total = list . len (); }
        if let Some ( Node::List (list) ) =
          map . get ("container_link_sources")
        { num_links_in_from_containers = list . len (); }
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
    num_links_in_from_containers,
    num_links_in_from_leaves
      : num_links_in_total - num_links_in_from_containers,
    subscribes,
    overrides,
    container_ids,
    content_ids,
  }) }
