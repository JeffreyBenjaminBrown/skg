use crate::dbs::graph_queries::relations::{
  contains_from_pids, pid_source_is_active, visible_related_pids};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole};
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;
use crate::types::textlinks::{replace_each_link_with_its_label, textlinks_from_text};
use crate::types::viewnode::{GraphNodeStats, RelationCounts};

use std::collections::{HashMap, HashSet};

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
/// an optional selected NodeComplete (the source of the alias / extra-id
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

/// Directional counts, shared containment maps, and link classification
/// derived from one selected graph. Input IDs are primary IDs.
pub fn fetch_all_graphnodestats (
  graph : &InRustGraph,
  pids : &[ID],
  active : Option<&ActiveSourceSet>,
) -> AllGraphNodeStats {
  let counts : HashMap<ID, RelationCounts> = pids . iter () . map ( |pid| {
    let inbound_count = |relation : NodeRelation| -> usize {
      visible_related_pids (graph, pid,
        RelationRole::new (relation, BinaryRolePosition::Second), active) . len () };
    let outbound_count = |relation : NodeRelation| -> usize {
      visible_related_pids (graph, pid,
        RelationRole::new (relation, BinaryRolePosition::First), active) . len () };
    let (link_total, link_surprising, link_with_content) : (usize, usize, usize) =
      link_split (graph, active, pid);
    (pid . clone (), RelationCounts {
      containers : inbound_count (NodeRelation::Contains),
      contents : outbound_count (NodeRelation::Contains),
      hiders : inbound_count (NodeRelation::HidesFromItsSubscriptions),
      hides : outbound_count (NodeRelation::HidesFromItsSubscriptions),
      subscribers : inbound_count (NodeRelation::Subscribes),
      subscribees : outbound_count (NodeRelation::Subscribes),
      overriders : inbound_count (NodeRelation::OverridesViewOf),
      overrides_out : outbound_count (NodeRelation::OverridesViewOf),
      link_total, link_surprising, link_with_content, }) }) . collect ();
  let (container_to_contents, content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>) =
    contains_from_pids (graph, pids, active);
  AllGraphNodeStats { counts, container_to_contents, content_to_containers } }

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
  if !pid_source_is_active (graph, active, pid) { return (0, 0, 0); }
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
    let src : &NodeRust = match graph . nodes . get (src_pid) {
      Some (n) => n, None => continue, };
    if ! visible_related_pids (graph, src_pid, RelationRole::CONTAINER, active)
      . is_empty () {
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
