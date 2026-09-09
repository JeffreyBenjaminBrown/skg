use crate::dbs::graph_queries::all_graphnodestats::{
  AllGraphNodeStats, fetch_all_graphnodestats, graphnodestats_for_pid};
use crate::dbs::graph_queries::ancestry::{
  AncestryTree, ancestry_by_id_from_ids, full_containerward_ancestry};
use crate::dbs::graph_queries::nodes::{pid_and_source_from_id, which_ids_exist};
use crate::dbs::graph_queries::paths::{
  PathToFirstNonlinearity, containerward_path_stats_bulk,
  path_containerward_to_first_nonlinearity, paths_to_first_nonlinearities};
use crate::dbs::graph_queries::pids_from_ids::{
  collect_ids_in_tree, pids_from_ids, replace_ids_with_pids};
use crate::dbs::graph_queries::relations::{
  OUTBOUND_RELATIONSHIP_TYPES, contains_from_pids,
  find_related_nodes, input_role_from_names, visible_related_pids};
use crate::dbs::graph_queries::subscriptions::partition_subscribee_content_for_subscriber;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use crate::source_sets::ActiveSourceSet;
use crate::types::maybe_placed_viewnode::MpViewnode;
use crate::types::misc::{ID, MSV, MemberAtSource, SourceName, SourceSetName};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
use crate::types::viewnode::{ContainerwardPathStats, GraphNodeStats};

use ego_tree::{NodeId, Tree};
use std::collections::{BTreeSet, HashMap, HashSet};

#[test]
fn all_relation_roles_resolve_extra_ids_and_follow_the_selected_direction () {
  let mut owner : NodeComplete = node ("owner");
  let mut member : NodeComplete = node ("member");
  owner . extra_ids = vec![id ("old-owner")];
  member . extra_ids = vec![id ("old-member")];
  owner . contains = vec![edge ("public", "old-member")];
  owner . subscribes_to = MSV::Specified (vec![edge ("public", "old-member")]);
  owner . hides_from_its_subscriptions = MSV::Specified (vec![edge ("public", "old-member")]);
  owner . overrides_view_of = MSV::Specified (vec![edge ("public", "old-member")]);
  owner . body = Some ("[[id:old-member][member link]]" . to_string ());
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[owner, member]);
  for (relation, first, second) in OUTBOUND_RELATIONSHIP_TYPES {
    let forward : RelationRole = input_role_from_names (relation, first, second) . unwrap ();
    let backward : RelationRole = input_role_from_names (relation, second, first) . unwrap ();
    assert_eq! (find_related_nodes (&graph, &[id ("old-owner")], forward, None),
                HashSet::from ([id ("member")]), "forward {}", relation);
    assert_eq! (find_related_nodes (&graph, &[id ("old-member")], backward, None),
                HashSet::from ([id ("owner")]), "backward {}", relation);
    assert_eq! (forward . relation . relation_name (), *relation);
    assert_eq! (forward . opposite_role (), backward);
    assert! (input_role_from_names (relation, first, first) . is_none ()); }
  assert! (input_role_from_names ("unknown", "first", "second") . is_none ()); }

#[test]
fn ordered_members_and_containment_maps_apply_edge_and_home_visibility () {
  let mut owner : NodeComplete = node ("owner");
  let mut private : NodeComplete = node ("private-node");
  private . source = SourceName::from ("private");
  owner . contains = vec![edge ("public", "z"), edge ("private", "b"),
    edge ("public", "a"), edge ("public", "private-node"), edge ("public", "missing")];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (
    &[owner, node ("z"), node ("b"), node ("a"), private]);
  let active : ActiveSourceSet = public ();
  assert_eq! (visible_related_pids (
    &graph, &id ("owner"), RelationRole::CONTAINER, Some (&active)),
    vec![id ("z"), id ("a")]);
  let (outbound, inbound) : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>) =
    contains_from_pids (&graph, &[id ("owner"), id ("a"), id ("b"), id ("private-node")],
      Some (&active));
  assert_eq! (outbound, HashMap::from ([(id ("owner"), HashSet::from ([id ("a")]))]));
  assert_eq! (inbound, HashMap::from ([(id ("a"), HashSet::from ([id ("owner")]))])); }

#[test]
fn primary_existence_and_extra_id_resolution_are_distinct_and_negative_is_final () {
  let mut owner : NodeComplete = node ("owner");
  owner . extra_ids = vec![id ("old-owner")];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[owner]);
  let ids : Vec<ID> = vec![id ("owner"), id ("old-owner"), id ("missing")];
  let map : HashMap<ID, Option<ID>> = pids_from_ids (&graph, &ids);
  assert_eq! (map [&id ("owner")], Some (id ("owner")));
  assert_eq! (map [&id ("old-owner")], Some (id ("owner")));
  assert_eq! (map [&id ("missing")], None);
  assert_eq! (pid_and_source_from_id (&graph, &id ("old-owner")),
              Some ((id ("owner"), SourceName::from ("public"))));
  assert_eq! (which_ids_exist (&graph,
    &ids . iter () . map ( |id| id . 0 . clone ()) . collect ()),
    HashSet::from (["owner" . to_string ()])); }

#[test]
fn tree_id_replacement_preserves_unknown_ids_and_other_subtrees () {
  let mut owner : NodeComplete = node ("owner");
  owner . extra_ids = vec![id ("old-owner")];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[owner]);
  let mut tree : Tree<MpViewnode> = org_to_uninterpreted_nodes (
    "* (skg (node (id old-owner) (source public))) first\n\
     ** (skg (node (id missing) (source public))) missing\n\
     * (skg (node (id old-owner) (source public))) second\n") . unwrap () . 0;
  let root : NodeId = tree . root () . first_child () . unwrap () . id ();
  replace_ids_with_pids (&graph, &mut tree, root);
  let mut ids : Vec<ID> = Vec::new ();
  collect_ids_in_tree (tree . root (), &mut ids);
  assert_eq! (ids, vec![id ("owner"), id ("missing"), id ("old-owner")]); }

#[test]
fn ancestry_uses_breadth_first_repeats_and_depth_limits () {
  let graph : InRustGraph = diamond ();
  let ancestry : AncestryTree = full_containerward_ancestry (&graph, &id ("n"), 10, None);
  assert_eq! (ancestry, AncestryTree::Inner (id ("n"), vec![
    AncestryTree::Inner (id ("a"), vec![AncestryTree::Root (id ("root"))]),
    AncestryTree::Inner (id ("b"), vec![AncestryTree::Repeated (id ("root"))]), ]));
  assert_eq! (full_containerward_ancestry (&graph, &id ("n"), 2, None),
    AncestryTree::Inner (id ("n"), vec![
      AncestryTree::DepthTruncated (id ("a")),
      AncestryTree::DepthTruncated (id ("b")), ]));
  let map : HashMap<ID, AncestryTree> = ancestry_by_id_from_ids (
    &graph, &[id ("n"), id ("missing")], 10, None);
  assert_eq! (map [&id ("missing")], AncestryTree::Root (id ("missing")));
  assert_eq! (map [&id ("n")], ancestry); }

#[test]
fn hidden_edges_cannot_shape_ancestry_repeats_or_forks () {
  let mut a : NodeComplete = node ("a");
  let mut b : NodeComplete = node ("b");
  let mut root : NodeComplete = node ("root");
  a . contains = vec![edge ("public", "n")];
  b . contains = vec![edge ("private", "n")];
  root . contains = vec![edge ("public", "a"), edge ("public", "b")];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[a, b, root, node ("n")]);
  let active : ActiveSourceSet = public ();
  assert_eq! (full_containerward_ancestry (&graph, &id ("n"), 10, Some (&active)),
    AncestryTree::Inner (id ("n"), vec![
      AncestryTree::Inner (id ("a"), vec![AncestryTree::Root (id ("root"))]), ]));
  let paths : Vec<PathToFirstNonlinearity> = paths_to_first_nonlinearities (
    &graph, &id ("n"), RelationRole::CONTAINER, Some (&active));
  assert_eq! (paths . len (), 1);
  assert_eq! (paths [0] . path, vec![id ("a"), id ("root")]);
  assert! (paths [0] . branches . is_empty ()); }

#[test]
fn immediate_forks_and_cycles_agree_with_bulk_path_statistics () {
  let graph : InRustGraph = diamond ();
  let fork : PathToFirstNonlinearity = path_containerward_to_first_nonlinearity (
    &graph, &id ("n"), None);
  assert! (fork . path . is_empty ());
  assert_eq! (fork . branches, HashSet::from ([id ("a"), id ("b")]));
  let paths : Vec<PathToFirstNonlinearity> = paths_to_first_nonlinearities (
    &graph, &id ("n"), RelationRole::CONTAINER, None);
  assert_eq! (paths . len (), 2);
  assert_eq! (paths [0] . path, vec![id ("a"), id ("root")]);
  assert_eq! (paths [1] . path, vec![id ("b"), id ("root")]);
  let stats : HashMap<ID, ContainerwardPathStats> = containerward_path_stats_bulk (
    &graph, &[id ("n"), id ("a"), id ("b"), id ("root")], None);
  assert_eq! (stats [&id ("n")] . length, 0);
  assert_eq! (stats [&id ("n")] . forks, 2);
  assert_eq! (stats [&id ("a")] . length, 1);
  assert_eq! (stats [&id ("b")] . length, 1);
  assert_eq! (stats [&id ("root")] . length, 0);
  let mut loop_node : NodeComplete = node ("loop");
  loop_node . contains = vec![edge ("public", "loop")];
  let loop_graph : InRustGraph = InRustGraph::from_nodecompletes (&[loop_node]);
  let stats : HashMap<ID, ContainerwardPathStats> = containerward_path_stats_bulk (
    &loop_graph, &[id ("loop")], None);
  assert! (stats [&id ("loop")] . cycles);
  assert_eq! (full_containerward_ancestry (&loop_graph, &id ("loop"), 10, None),
    AncestryTree::Inner (id ("loop"), vec![AncestryTree::Repeated (id ("loop"))])); }

#[test]
fn subscription_partition_uses_visible_hide_edges () {
  let mut subscriber : NodeComplete = node ("subscriber");
  let mut subscribee : NodeComplete = node ("subscribee");
  subscriber . hides_from_its_subscriptions = MSV::Specified (vec![edge ("private", "member")]);
  subscribee . contains = vec![edge ("public", "member")];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (
    &[subscriber, subscribee, node ("member")]);
  let active : ActiveSourceSet = public ();
  assert_eq! (partition_subscribee_content_for_subscriber (
    &graph, &id ("subscriber"), &id ("subscribee"), Some (&active)),
    (HashSet::from ([id ("member")]), HashSet::new ()));
  assert_eq! (partition_subscribee_content_for_subscriber (
    &graph, &id ("subscriber"), &id ("subscribee"), None),
    (HashSet::new (), HashSet::from ([id ("member")]))); }

#[test]
fn link_classification_and_relation_counts_do_not_reveal_private_content () {
  let mut source : NodeComplete = node ("source");
  let mut target : NodeComplete = node ("target");
  source . title = "tag [[id:old-target][linked name]]" . to_string ();
  source . contains = vec![edge ("private", "child")];
  source . aliases = MSV::Specified (vec![MemberAtSource::at_source (
    SourceName::from ("public"), "nickname" . to_string ())]);
  target . extra_ids = vec![id ("old-target")];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (
    &[source . clone (), target, node ("child")]);
  let active : ActiveSourceSet = public ();
  let visible : AllGraphNodeStats = fetch_all_graphnodestats (
    &graph, &[id ("target"), id ("source"), id ("child")], Some (&active));
  assert_eq! (visible . counts [&id ("target")] . link_total, 1);
  assert_eq! (visible . counts [&id ("target")] . link_surprising, 1);
  assert_eq! (visible . counts [&id ("target")] . link_with_content, 0);
  assert_eq! (visible . counts [&id ("source")] . contents, 0);
  let all : AllGraphNodeStats = fetch_all_graphnodestats (
    &graph, &[id ("target"), id ("source")], None);
  assert_eq! (all . counts [&id ("target")] . link_with_content, 1);
  assert_eq! (all . counts [&id ("target")] . link_surprising, 0);
  let stats : GraphNodeStats = graphnodestats_for_pid (&id ("source"), &all, Some (&source));
  assert_eq! (stats . aliases, 1); }

fn diamond (
) -> InRustGraph {
  let mut a : NodeComplete = node ("a");
  let mut b : NodeComplete = node ("b");
  let mut root : NodeComplete = node ("root");
  a . contains = vec![edge ("public", "n")];
  b . contains = vec![edge ("public", "n")];
  root . contains = vec![edge ("public", "a"), edge ("public", "b")];
  InRustGraph::from_nodecompletes (&[a, b, root, node ("n")]) }

fn id (
  value : &str,
) -> ID {
  ID::from (value) }

fn node (
  value : &str,
) -> NodeComplete {
  let mut node : NodeComplete = empty_node_complete ();
  node . pid = id (value);
  node . title = format! ("{} title", value);
  node . source = SourceName::from ("public");
  node }

fn edge (
  source : &str,
  member : &str,
) -> MemberAtSource<ID> {
  MemberAtSource::at_source (SourceName::from (source), id (member)) }

fn public (
) -> ActiveSourceSet {
  ActiveSourceSet { name : SourceSetName::from ("public"),
    sources : BTreeSet::from ([SourceName::from ("public")]) } }
