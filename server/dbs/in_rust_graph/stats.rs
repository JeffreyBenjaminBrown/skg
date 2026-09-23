/// PURPOSE: Fetch all graph-node statistics for a set of PIDs from the
/// in-Rust graph: directional member counts for the five relations, the
/// alias / extra-id / true-property counts, and the interesting-link subset.
/// These feed the uniform-herald token grammar (server/herald_tokens.rs).
///
/// PITFALL: Assumes input IDs are primary IDs, not extra IDs. Always
/// true on the graphnodestats path (PIDs come from the built viewnode
/// tree).
///
/// Statistics are computed directly from the explicit graph snapshot. The
/// directional counts and link facts are graph-native
/// operations used by the uniform-herald grammar.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{GraphNodeStats, RelationCounts};

use std::collections::{HashMap, HashSet};
use std::error::Error;

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
/// an optional disk NodeComplete (the source of the alias / extra-id /
/// property counts).
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
  let properties : usize =
    nodecomplete
    . map ( |n| n . misc . iter () . copied ()
      . collect::<HashSet<_>> () . len () )
    . unwrap_or (0);
  GraphNodeStats {
    aliases,
    extra_ids,
    properties,
    rels : stats . counts . get (pid) . cloned (), }}

/// Compute graph-node statistics without I/O.
pub fn fetch_all_graphnodestats (
  graph : &InRustGraph,
  pids    : &[ID],
) -> Result < AllGraphNodeStats, Box<dyn Error> > {
  fetch_all_graphnodestats_with_source_set (
    graph, pids, None ) }

pub fn fetch_all_graphnodestats_with_source_set (
  graph    : &InRustGraph,
  pids     : &[ID],
  active   : Option<&ActiveSourceSet>,
) -> Result < AllGraphNodeStats, Box<dyn Error> > {
  if pids . is_empty () {
    return Ok ( AllGraphNodeStats::empty() ); }
  let pid_set : HashSet < ID > =
    pids . iter () . cloned () . collect ();
  Ok ( fetch_all_graphnodestats_in_rust (
    graph, pids, &pid_set, active ) ) }

/// In-Rust-graph implementation. Every field is computed from NodeRust
/// and the inverse indexes, without I/O.
///
/// relSource gating (render-and-gating, 5_plan.org): counts and the
/// container/content maps use the gated accessors
/// ('outbound_pids_for_relation_gated' / 'inbound_pids_for_relation_gated'),
/// not the raw NodeRust lists / inverse indexes -- a membership
/// recorded at a source outside 'active' must not inflate a count or
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
  let mut link_source_facts : HashMap<ID, (HashSet<ID>, bool)> = HashMap::new ();
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
      . collect::<HashSet<_>> () . len () };
    // Outbound counts: gated partners, further source-filtered.
    let outbound_count = | relation : NodeRelation | -> usize {
      graph . outbound_pids_for_relation_gated (pid, relation, active)
      . iter ()
      . filter ( |p| pid_source_is_active (graph, active, p) )
      . collect::<HashSet<_>> () . len () };
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
    let link_sources : HashSet<ID> =
      graph . inbound_pids_for_relation_gated (
        pid, NodeRelation::TextlinksTo, active )
      . into_iter ()
      . filter (|source| pid_source_is_active (graph, active, source))
      . collect ();
    let link_total : usize = link_sources . len ();
    let link_interesting : usize = link_sources . iter ()
      . filter (|source| {
        let facts : &(HashSet<ID>, bool) =
          link_source_facts . entry ((*source) . clone ())
          . or_insert_with (|| link_facts_for_source (graph, active, source));
        facts . 1 })
      . count ();
    let link_targets : usize =
      link_source_facts . entry (pid . clone ())
      . or_insert_with (|| link_facts_for_source (graph, active, pid))
      . 0 . len ();
    counts . insert ( pid . clone (), RelationCounts {
      containers, contents, hiders, hides,
      subscribers, subscribees, overriders, overrides_out,
      link_total, link_interesting, link_targets } );
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

/// Distinct visible resolved link targets and the source's interestingness.
/// The graph already parsed title and body into textlinks_to.
fn link_facts_for_source (
  graph  : &InRustGraph,
  active : Option<&ActiveSourceSet>,
  pid    : &ID,
) -> (HashSet<ID>, bool) {
  if ! pid_source_is_active (graph, active, pid) {
    return (HashSet::new (), false); }
  let Some (node) = graph . nodes . get (pid) else {
    return (HashSet::new (), false); };
  let targets : HashSet<ID> =
    graph . outbound_pids_for_relation_gated (
      pid, NodeRelation::TextlinksTo, active )
    . into_iter ()
    . filter (|target| pid_source_is_active (graph, active, target))
    . collect ();
  let has_body : bool = node . body . as_ref ()
    . is_some_and (|body| ! body . trim () . is_empty ());
  let has_content : bool = graph . outbound_pids_for_relation_gated (
      pid, NodeRelation::Contains, active )
    . iter ()
    . any (|member| pid_source_is_active (graph, active, member));
  let interesting : bool = has_body || has_content || targets . len () > 1;
  (targets, interesting) }

pub(crate) fn link_source_is_interesting (
  graph  : &InRustGraph,
  active : Option<&ActiveSourceSet>,
  pid    : &ID,
) -> bool {
  link_facts_for_source (graph, active, pid) . 1 }

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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::nodes::complete::{FileProperty, empty_node_complete};
  use crate::types::misc::{RelPartner, SourceName};
  use crate::dbs::filesystem::not_nodes::load_config;
  use crate::source_sets::SourceSetName;

  fn node (
    id    : &str,
    title : &str,
    body  : Option<&str>,
  ) -> NodeComplete {
    NodeComplete {
      pid : ID::from (id),
      title : title . to_string (),
      body : body . map (str::to_string),
      .. empty_node_complete () } }

  #[test]
  fn links_count_distinct_resolved_identities_and_interesting_sources () {
    let mut target : NodeComplete = node ("target", "A different title", None);
    target . extra_ids = vec![ ID::from ("old-target") ];
    let mut with_content : NodeComplete =
      node ("with-content", "[[id:target][x]]", None);
    with_content . contains = vec![ RelPartner::at_relSource (
      SourceName::from ("main"), ID::from ("target")) ];
    let nodes : Vec<NodeComplete> = vec![
      target,
      node ("other", "Another distinct target", None),
      node ("repeated", "[[id:target][x]] [[id:old-target][y]]", None),
      node ("with-body", "[[id:target][x]]", Some ("body text")),
      with_content,
      node ("two-targets", "[[id:target][x]] [[id:other][y]]", None),
      node ("with-self-link", "[[id:target][x]] [[id:with-self-link][self]]", None),
      node ("with-dangling", "[[id:target][x]] [[id:missing][z]]", None) ];
    let graph : InRustGraph = InRustGraph::from_nodecompletes (&nodes);
    let pids : Vec<ID> = nodes . iter () . map (|n| n . pid . clone ()) . collect ();
    let stats : AllGraphNodeStats = fetch_all_graphnodestats (
      &graph, &pids) . unwrap ();
    let target_counts : &RelationCounts = stats . counts . get (&ID::from ("target")) . unwrap ();
    assert_eq! (target_counts . link_total, 6);
    assert_eq! (target_counts . link_interesting, 4);
    assert_eq! (stats . counts [&ID::from ("repeated")] . link_targets, 1);
    assert_eq! (stats . counts [&ID::from ("two-targets")] . link_targets, 2);
    assert_eq! (stats . counts [&ID::from ("with-self-link")] . link_targets, 2);
    assert_eq! (stats . counts [&ID::from ("with-dangling")] . link_targets, 1);
  }

  #[test]
  fn inactive_content_and_targets_do_not_make_a_link_source_interesting () {
    let config = load_config (
      "tests/source_sets/fixtures/skgconfig.toml") . unwrap ();
    let active : ActiveSourceSet = ActiveSourceSet::named (
      &config, SourceSetName::from ("public")) . unwrap ();
    let mut source : NodeComplete = node (
      "source", "[[id:dest][d]] [[id:private-target][p]]", None);
    source . source = SourceName::from ("public");
    source . contains = vec![RelPartner::at_relSource (
      SourceName::from ("private"), ID::from ("visible-child"))];
    let mut dest : NodeComplete = node ("dest", "destination", None);
    dest . source = SourceName::from ("public");
    let mut child : NodeComplete = node ("visible-child", "child", None);
    child . source = SourceName::from ("public");
    let mut private_target : NodeComplete = node (
      "private-target", "private target", None);
    private_target . source = SourceName::from ("private");
    let graph : InRustGraph = InRustGraph::from_nodecompletes (
      &[source, dest, child, private_target]);
    let stats : AllGraphNodeStats = fetch_all_graphnodestats_with_source_set (
      &graph, &[ID::from ("source"), ID::from ("dest")],
      Some (&active)) . unwrap ();
    assert_eq! (stats . counts [&ID::from ("dest")] . link_total, 1);
    assert_eq! (stats . counts [&ID::from ("dest")] . link_interesting, 0);
    assert_eq! (stats . counts [&ID::from ("source")] . link_targets, 1);
  }

  #[test]
  fn property_count_is_the_number_of_distinct_true_properties () {
    let pid = ID::from ("node");
    let node = NodeComplete {
      pid : pid . clone (),
      misc : vec![
        FileProperty::Had_ID_Before_Import,
        FileProperty::Had_ID_Before_Import,
        FileProperty::NoSearchMatching ],
      .. empty_node_complete () };
    let result = graphnodestats_for_pid (
      &pid, &AllGraphNodeStats::empty (), Some (&node));
    assert_eq! (result . properties, 2);
  }
}
