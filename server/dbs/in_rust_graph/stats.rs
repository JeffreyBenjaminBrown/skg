/// PURPOSE: Fetch all graphnode statistics for a set of PIDs from the
/// in-Rust graph: directional member counts for the five relations, the
/// alias / extra-id / true-flag counts, and the substantive-mentioner subset.
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
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::ID;
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{GraphnodeStats, RelationCounts};

use std::collections::{HashMap, HashSet};
use std::error::Error;

/// Everything 'set_graphnodestats_in_viewforest' needs from the graph.
pub struct AllGraphnodeStats {
  pub counts                : HashMap < ID, RelationCounts >,
  pub container_to_contents : HashMap < ID, HashSet < ID > >,
  pub content_to_containers : HashMap < ID, HashSet < ID > >,
}

impl AllGraphnodeStats {
  pub fn empty () -> AllGraphnodeStats {
    AllGraphnodeStats {
      counts                : HashMap::new (),
      container_to_contents : HashMap::new (),
      content_to_containers : HashMap::new (),
    } } }

/// Extract GraphnodeStats for a single PID from AllGraphnodeStats and
/// an optional disk Graphnode (the skgrepo of the alias / extra-id /
/// flag counts).
pub fn graphnodestats_for_pid (
  pid          : &ID,
  stats        : &AllGraphnodeStats,
  graphnode : Option<&Graphnode>,
) -> GraphnodeStats {
  let aliases : usize =
    graphnode
    . map ( |n| n . aliases . or_default () . len () )
    . unwrap_or (0);
  let extra_ids : usize =
    graphnode
    . map ( |n| n . extra_ids . len () )
    . unwrap_or (0);
  let flags : usize =
    graphnode
    . map ( |n| n . flags . iter () . copied ()
      . collect::<HashSet<_>> () . len () )
    . unwrap_or (0);
  GraphnodeStats {
    aliases,
    extra_ids,
    flags,
    rels : stats . counts . get (pid) . cloned (), }}

/// Compute graphnode statistics without I/O.
pub fn fetch_all_graphnodestats (
  graph : &InRustGraph,
  pids    : &[ID],
) -> Result < AllGraphnodeStats, Box<dyn Error> > {
  fetch_all_graphnodestats_with_skgrepo_set (
    graph, pids, None ) }

pub fn fetch_all_graphnodestats_with_skgrepo_set (
  graph    : &InRustGraph,
  pids     : &[ID],
  restriction : Option<&SkgrepoRestriction>,
) -> Result < AllGraphnodeStats, Box<dyn Error> > {
  if pids . is_empty () {
    return Ok ( AllGraphnodeStats::empty() ); }
  let pid_set : HashSet < ID > =
    pids . iter () . cloned () . collect ();
  Ok ( fetch_all_graphnodestats_in_rust (
    graph, pids, &pid_set, restriction ) ) }

/// In-Rust-graph implementation. Every field is computed from GraphnodeInRust
/// and the recorderward relmaps, without I/O.
///
/// relRepo gating (render-and-gating, 5_plan.org): counts and the
/// container/content maps use the gated accessors
/// ('outbound_pids_for_relation_gated' / 'inbound_pids_for_relation_gated'),
/// not the raw GraphnodeInRust lists / recorderward relmaps -- a membership
/// recorded at a skgrepo outside 'unrestricted' must not inflate a count or
/// appear in these maps, in either direction, even when the member
/// NODE itself is unrestricted (still checked separately via
/// 'pid_repo_is_unrestricted', matching every other render surface's
/// two-part gate).
fn fetch_all_graphnodestats_in_rust (
  graph   : &InRustGraph,
  pids    : &[ID],
  pid_set : &HashSet<ID>,
  restriction : Option<&SkgrepoRestriction>,
) -> AllGraphnodeStats {
  let mut counts : HashMap<ID, RelationCounts> = HashMap::new ();
  let mut mentioner_link_facts : HashMap<ID, (HashSet<ID>, bool)> = HashMap::new ();
  let mut container_to_contents
    : HashMap<ID, HashSet<ID>> = HashMap::new ();
  let mut content_to_containers
    : HashMap<ID, HashSet<ID>> = HashMap::new ();
  for pid in pids {
    // Inbound counts: gated partners, further repo-filtered.
    let inbound_count = | relation : NodeRelation | -> usize {
      graph . inbound_pids_for_relation_gated (pid, relation, restriction)
      . iter ()
      . filter ( |p| pid_skgrepo_is_unrestricted (graph, restriction, p) )
      . collect::<HashSet<_>> () . len () };
    // Outbound counts: gated partners, further repo-filtered.
    let outbound_count = | relation : NodeRelation | -> usize {
      graph . outbound_pids_for_relation_gated (pid, relation, restriction)
      . iter ()
      . filter ( |p| pid_skgrepo_is_unrestricted (graph, restriction, p) )
      . collect::<HashSet<_>> () . len () };
    let containers : usize = inbound_count (NodeRelation::Contains);
    let contents : usize = outbound_count (NodeRelation::Contains);
    let hiders : usize =
      inbound_count (NodeRelation::HidesFromSubs);
    let hides : usize =
      outbound_count (NodeRelation::HidesFromSubs);
    let subscribers : usize = inbound_count (NodeRelation::SubscribesTo);
    let subscribees : usize = outbound_count (NodeRelation::SubscribesTo);
    let overriders : usize = inbound_count (NodeRelation::Overrides);
    let overrides_out : usize =
      outbound_count (NodeRelation::Overrides);
    let mentioners : HashSet<ID> =
      graph . inbound_pids_for_relation_gated (
        pid, NodeRelation::LinksTo, restriction )
      . into_iter ()
      . filter (|mentioner| pid_skgrepo_is_unrestricted (graph, restriction, mentioner))
      . collect ();
    let link_total : usize = mentioners . len ();
    let link_substantive : usize = mentioners . iter ()
      . filter (|mentioner| {
        let facts : &(HashSet<ID>, bool) =
          mentioner_link_facts . entry ((*mentioner) . clone ())
          . or_insert_with (|| link_facts_for_mentioner (graph, restriction, mentioner));
        facts . 1 })
      . count ();
    let link_targets : usize =
      mentioner_link_facts . entry (pid . clone ())
      . or_insert_with (|| link_facts_for_mentioner (graph, restriction, pid))
      . 0 . len ();
    counts . insert ( pid . clone (), RelationCounts {
      containers, contents, hiders, hides,
      subscribers, subscribees, overriders, overrides_out,
      link_total, link_substantive, link_targets } );
    // container_to_contents[pid] = (pid's gated contents) ∩ pid_set.
    { let intersected : HashSet<ID> =
        graph . outbound_pids_for_relation_gated (
          pid, NodeRelation::Contains, restriction )
        . into_iter ()
        . filter ( |p| pid_set . contains (p) )
        . filter ( |p| pid_skgrepo_is_unrestricted (graph, restriction, p) )
        . collect ();
      if ! intersected . is_empty () {
        container_to_contents . insert ( pid . clone (),
                                         intersected ); }}
    // content_to_containers[pid] = (pid's gated containers) ∩ pid_set.
    { let intersected : HashSet<ID> =
        graph . inbound_pids_for_relation_gated (
          pid, NodeRelation::Contains, restriction )
        . into_iter ()
        . filter ( |p| pid_set . contains (p) )
        . filter ( |p| pid_skgrepo_is_unrestricted (graph, restriction, p) )
        . collect ();
      if ! intersected . is_empty () {
        content_to_containers . insert ( pid . clone (),
                                         intersected ); }}}
  AllGraphnodeStats {
    counts,
    container_to_contents,
    content_to_containers,
  } }

/// Distinct visible resolved link targets, and whether the mentioner is substantive.
/// The graph already parsed title and body into linksTo.
fn link_facts_for_mentioner (
  graph  : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
  pid    : &ID,
) -> (HashSet<ID>, bool) {
  if ! pid_skgrepo_is_unrestricted (graph, restriction, pid) {
    return (HashSet::new (), false); }
  let Some (node) = graph . nodes . get (pid) else {
    return (HashSet::new (), false); };
  let targets : HashSet<ID> =
    graph . outbound_pids_for_relation_gated (
      pid, NodeRelation::LinksTo, restriction )
    . into_iter ()
    . filter (|target| pid_skgrepo_is_unrestricted (graph, restriction, target))
    . collect ();
  let has_body : bool = node . body . as_ref ()
    . is_some_and (|body| ! body . trim () . is_empty ());
  let has_content : bool = graph . outbound_pids_for_relation_gated (
      pid, NodeRelation::Contains, restriction )
    . iter ()
    . any (|member| pid_skgrepo_is_unrestricted (graph, restriction, member));
  let substantive : bool = has_body || has_content || targets . len () > 1;
  (targets, substantive) }

pub(crate) fn mentioner_is_substantive (
  graph  : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
  pid    : &ID,
) -> bool {
  link_facts_for_mentioner (graph, restriction, pid) . 1 }

fn pid_skgrepo_is_unrestricted (
  graph  : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
  pid    : &ID,
) -> bool {
  match restriction {
    None => true,
    Some (restriction) if restriction . is_all () => true,
    Some (restriction) =>
      graph . nodes . get (pid)
      . map ( |node| restriction . contains_skgrepo (&node . home_skgrepo) )
      . unwrap_or (false), } }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::nodes::complete::{Flag, empty_graphnode};
  use crate::types::misc::{RelPartner, SkgRepoName};
  use crate::dbs::filesystem::not_nodes::load_config;
  use crate::skgrepo_sets::SkgRepoSetName;

  fn node (
    skgid : &str,
    title : &str,
    body  : Option<&str>,
  ) -> Graphnode {
    Graphnode {
      pid : ID::from (skgid),
      title : title . to_string (),
      body : body . map (str::to_string),
      .. empty_graphnode () } }

  #[test]
  fn links_count_distinct_resolved_identities_and_substantive_mentioners () {
    let mut target : Graphnode = node ("target", "A different title", None);
    target . extra_ids = vec![ ID::from ("old-target") ];
    let mut with_content : Graphnode =
      node ("with-content", "[[id:target][x]]", None);
    with_content . contains = vec![ RelPartner::at_relRepo (
      SkgRepoName::from ("main"), ID::from ("target")) ];
    let nodes : Vec<Graphnode> = vec![
      target,
      node ("other", "Another distinct target", None),
      node ("repeated", "[[id:target][x]] [[id:old-target][y]]", None),
      node ("with-body", "[[id:target][x]]", Some ("body text")),
      with_content,
      node ("two-targets", "[[id:target][x]] [[id:other][y]]", None),
      node ("with-self-link", "[[id:target][x]] [[id:with-self-link][self]]", None),
      node ("with-dangling", "[[id:target][x]] [[id:missing][z]]", None) ];
    let graph : InRustGraph = InRustGraph::from_graphnodes (&nodes);
    let pids : Vec<ID> = nodes . iter () . map (|n| n . pid . clone ()) . collect ();
    let stats : AllGraphnodeStats = fetch_all_graphnodestats (
      &graph, &pids) . unwrap ();
    let target_counts : &RelationCounts = stats . counts . get (&ID::from ("target")) . unwrap ();
    assert_eq! (target_counts . link_total, 6);
    assert_eq! (target_counts . link_substantive, 4);
    assert_eq! (stats . counts [&ID::from ("repeated")] . link_targets, 1);
    assert_eq! (stats . counts [&ID::from ("two-targets")] . link_targets, 2);
    assert_eq! (stats . counts [&ID::from ("with-self-link")] . link_targets, 2);
    assert_eq! (stats . counts [&ID::from ("with-dangling")] . link_targets, 1);
  }

  #[test]
  fn restricted_content_and_targets_do_not_make_a_mentioner_substantive () {
    let config = load_config (
      "tests/repo_sets/fixtures/skgconfig.toml") . unwrap ();
    let restriction : SkgrepoRestriction = SkgrepoRestriction::named (
      &config, SkgRepoSetName::from ("public")) . unwrap ();
    let mut mentioner : Graphnode = node (
      "mentioner", "[[id:target][d]] [[id:private-target][p]]", None);
    mentioner . home_skgrepo = SkgRepoName::from ("public");
    mentioner . contains = vec![RelPartner::at_relRepo (
      SkgRepoName::from ("private"), ID::from ("visible-child"))];
    let mut target : Graphnode = node ("target", "destination", None);
    target . home_skgrepo = SkgRepoName::from ("public");
    let mut child : Graphnode = node ("visible-child", "child", None);
    child . home_skgrepo = SkgRepoName::from ("public");
    let mut private_target : Graphnode = node (
      "private-target", "private target", None);
    private_target . home_skgrepo = SkgRepoName::from ("private");
    let graph : InRustGraph = InRustGraph::from_graphnodes (
      &[mentioner, target, child, private_target]);
    let stats : AllGraphnodeStats = fetch_all_graphnodestats_with_skgrepo_set (
      &graph, &[ID::from ("mentioner"), ID::from ("target")],
      Some (&restriction)) . unwrap ();
    assert_eq! (stats . counts [&ID::from ("target")] . link_total, 1);
    assert_eq! (stats . counts [&ID::from ("target")] . link_substantive, 0);
    assert_eq! (stats . counts [&ID::from ("mentioner")] . link_targets, 1);
  }

  #[test]
  fn flag_count_is_the_number_of_distinct_true_flags () {
    let pid = ID::from ("node");
    let node = Graphnode {
      pid : pid . clone (),
      flags : vec![
        Flag::Had_ID_Before_Import,
        Flag::Had_ID_Before_Import,
        Flag::NoSearchMatching ],
      .. empty_graphnode () };
    let result = graphnodestats_for_pid (
      &pid, &AllGraphnodeStats::empty (), Some (&node));
    assert_eq! (result . flags, 2);
  }
}
