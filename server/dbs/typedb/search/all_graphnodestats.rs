/// PURPOSE: Fetch all graph-node statistics for a set of PIDs from the
/// in-Rust graph: directional member counts for the five relations, the
/// alias / extra-id counts, and the "surprising links" =a(b,c)= split.
/// These feed the uniform-herald token grammar (server/herald_tokens.rs).
///
/// PITFALL: Assumes input IDs are primary IDs, not extra IDs. Always
/// true on the graphnodestats path (PIDs come from the built viewnode
/// tree).
///
/// The old TypeDB fallback was retired with the uniform-heralds rewrite:
/// the directional counts and the on-demand surprising-links re-parse
/// have no reasonable TypeQL form, and the running server always has the
/// in-Rust graph. When the global graph handle is absent (a few tests
/// that bypass init), stats are simply empty -- views still render,
/// without heralds.

use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;
use crate::types::nodes::complete::NodeComplete;
use crate::types::textlinks::{replace_each_link_with_its_label,
                              textlinks_from_text};
use crate::types::viewnode::{GraphNodeStats, RelationCounts};

use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Everything 'set_graphnodestats_in_viewforest' needs from the graph.
pub struct AllGraphNodeStats {
  pub counts                : HashMap < ID, RelationCounts >,
  pub container_to_contents : HashMap < ID, HashSet < ID > >,
  pub content_to_containers : HashMap < ID, HashSet < ID > >,
}

impl AllGraphNodeStats {
  pub fn empty () -> AllGraphNodeStats {
    AllGraphNodeStats {
      counts                : HashMap::new (),
      container_to_contents : HashMap::new (),
      content_to_containers : HashMap::new (),
    } } }

/// Extract GraphNodeStats for a single PID from AllGraphNodeStats and
/// an optional disk NodeComplete (the source of the alias / extra-id
/// counts).
pub fn graphnodestats_for_pid (
  pid          : &ID,
  stats        : &AllGraphNodeStats,
  nodecomplete : Option<&NodeComplete>,
) -> GraphNodeStats {
  let aliases : usize =
    nodecomplete
    . map ( |n| n . aliases . or_default () . len () )
    . unwrap_or (0);
  let extra_ids : usize =
    nodecomplete
    . map ( |n| n . extra_ids . len () )
    . unwrap_or (0);
  GraphNodeStats {
    aliases,
    extra_ids,
    rels : stats . counts . get (pid) . cloned (), }}

/// Dispatcher kept for signature compatibility; the driver is now
/// unused (the TypeDB path was retired). Routes to the in-Rust graph,
/// or empty stats when no graph handle is installed.
pub async fn fetch_all_graphnodestats (
  db_name : &str,
  driver  : &TypeDBDriver,
  pids    : &[ID],
) -> Result < AllGraphNodeStats, Box<dyn Error> > {
  fetch_all_graphnodestats_with_source_set (
    db_name, driver, pids, None ) . await }

pub async fn fetch_all_graphnodestats_with_source_set (
  _db_name : &str,
  _driver  : &TypeDBDriver,
  pids     : &[ID],
  active   : Option<&ActiveSourceSet>,
) -> Result < AllGraphNodeStats, Box<dyn Error> > {
  if pids . is_empty () {
    return Ok ( AllGraphNodeStats::empty() ); }
  let pid_set : HashSet < ID > =
    pids . iter () . cloned () . collect ();
  match snapshot_global () {
    Some (graph_snap) =>
      Ok ( fetch_all_graphnodestats_in_rust (
        &graph_snap, pids, &pid_set, active ) ),
    None =>
      // No in-Rust graph (tests that bypass init); render without stats.
      Ok ( AllGraphNodeStats::empty () ), } }

/// In-Rust-graph implementation. Every field is computed from NodeRust
/// and the inverse indexes -- no TypeDB round-trips.
///
/// Edge-level gating (render-and-gating, 5_plan.org): counts and the
/// container/content maps use the gated accessors
/// ('outbound_pids_for_relation_gated' / 'inbound_pids_for_relation_gated'),
/// not the raw NodeRust lists / inverse indexes -- a membership
/// recorded at a level outside 'active' must not inflate a count or
/// appear in these maps, in either direction, even when the member
/// NODE itself is active (still checked separately via
/// 'pid_source_is_active', matching every other render surface's
/// two-part gate).
fn fetch_all_graphnodestats_in_rust (
  graph   : &InRustGraph,
  pids    : &[ID],
  pid_set : &HashSet<ID>,
  active  : Option<&ActiveSourceSet>,
) -> AllGraphNodeStats {
  let mut counts : HashMap<ID, RelationCounts> = HashMap::new ();
  let mut container_to_contents
    : HashMap<ID, HashSet<ID>> = HashMap::new ();
  let mut content_to_containers
    : HashMap<ID, HashSet<ID>> = HashMap::new ();
  for pid in pids {
    // Inbound counts: gated partners, further source-filtered.
    let inbound_count = | relation : NodeRelation | -> usize {
      graph . inbound_pids_for_relation_gated (pid, relation, active)
      . iter ()
      . filter ( |p| pid_source_is_active (graph, active, p) )
      . count () };
    // Outbound counts: gated partners, further source-filtered.
    let outbound_count = | relation : NodeRelation | -> usize {
      graph . outbound_pids_for_relation_gated (pid, relation, active)
      . iter ()
      . filter ( |p| pid_source_is_active (graph, active, p) )
      . count () };
    let containers : usize = inbound_count (NodeRelation::Contains);
    let contents : usize = outbound_count (NodeRelation::Contains);
    let hiders : usize =
      inbound_count (NodeRelation::HidesFromItsSubscriptions);
    let hides : usize =
      outbound_count (NodeRelation::HidesFromItsSubscriptions);
    let subscribers : usize = inbound_count (NodeRelation::Subscribes);
    let subscribees : usize = outbound_count (NodeRelation::Subscribes);
    let overriders : usize = inbound_count (NodeRelation::OverridesViewOf);
    let overrides_out : usize =
      outbound_count (NodeRelation::OverridesViewOf);
    let (link_total, link_surprising, link_with_content) : (usize, usize, usize) =
      link_split (graph, active, pid);
    counts . insert ( pid . clone (), RelationCounts {
      containers, contents, hiders, hides,
      subscribers, subscribees, overriders, overrides_out,
      link_total, link_surprising, link_with_content } );
    // container_to_contents[pid] = (pid's gated contents) ∩ pid_set.
    { let intersected : HashSet<ID> =
        graph . outbound_pids_for_relation_gated (
          pid, NodeRelation::Contains, active )
        . into_iter ()
        . filter ( |p| pid_set . contains (p) )
        . filter ( |p| pid_source_is_active (graph, active, p) )
        . collect ();
      if ! intersected . is_empty () {
        container_to_contents . insert ( pid . clone (),
                                         intersected ); }}
    // content_to_containers[pid] = (pid's gated containers) ∩ pid_set.
    { let intersected : HashSet<ID> =
        graph . inbound_pids_for_relation_gated (
          pid, NodeRelation::Contains, active )
        . into_iter ()
        . filter ( |p| pid_set . contains (p) )
        . filter ( |p| pid_source_is_active (graph, active, p) )
        . collect ();
      if ! intersected . is_empty () {
        content_to_containers . insert ( pid . clone (),
                                         intersected ); }}}
  AllGraphNodeStats {
    counts,
    container_to_contents,
    content_to_containers,
  } }

/// The inbound textlink split =a(b,c)= for one node (option A: re-parse
/// each source's title+body on demand). a = total active inbound link
/// sources; c = of those, sources with their own content; b = of those,
/// sources that differ from this node, are bodyless AND contentless, and
/// EVERY label they use to link here is "surprising" -- the source's
/// normalized title is something other than the (normalized) link label.
fn link_split (
  graph  : &InRustGraph,
  active : Option<&ActiveSourceSet>,
  pid    : &ID,
) -> (usize, usize, usize) {
  let sources : Vec<ID> = match graph . textlinks_in . get (pid) {
    Some (s) => s . iter ()
      . filter ( |src| pid_source_is_active (graph, active, src) )
      . cloned () . collect (),
    None => return (0, 0, 0), };
  let mut total       : usize = 0;
  let mut surprising  : usize = 0;
  let mut with_content : usize = 0;
  for src_pid in &sources {
    total += 1;
    let src = match graph . nodes . get (src_pid) {
      Some (n) => n, None => continue, };
    if ! src . contains . is_empty () {
      with_content += 1;
      continue; } // c-bucket: sources with content are never "surprising"
    // Contentless. Surprising iff it differs from this node, is
    // bodyless, and every label it uses to link here differs from its
    // normalized title.
    let differs   : bool = src_pid != pid;
    let bodyless  : bool = src . body . is_none ();
    if differs && bodyless {
      let src_text : String = format! (
        "{} {}", src . title, src . body . as_deref () . unwrap_or ("") );
      let labels_here : Vec<String> =
        textlinks_from_text (& src_text) . into_iter ()
        . filter ( |tl| graph . pid_of (& tl . id) . as_ref () == Some (pid) )
        . map ( |tl| tl . label )
        . collect ();
      let norm_title : String = normalize_for_compare (& src . title);
      let all_surprising : bool =
        ! labels_here . is_empty ()
        && labels_here . iter () . all (
          |l| normalize_for_compare (l) != norm_title );
      if all_surprising { surprising += 1; } } }
  (total, surprising, with_content) }

/// Normalize text for the surprising-links title/label comparison:
/// replace each link with its label, then trim and lowercase (matching
/// the existing Tantivy/search practice).
fn normalize_for_compare (
  text : &str,
) -> String {
  replace_each_link_with_its_label (text) . trim () . to_lowercase () }

fn pid_source_is_active (
  graph  : &InRustGraph,
  active : Option<&ActiveSourceSet>,
  pid    : &ID,
) -> bool {
  match active {
    None => true,
    Some (active) if active . is_all () => true,
    Some (active) =>
      graph . nodes . get (pid)
      . map ( |node| active . contains_source (&node . source) )
      . unwrap_or (false), } }
