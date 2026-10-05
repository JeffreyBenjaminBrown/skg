use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  NodeRelation,
  RelationRole,
};
use skg::types::misc::{ID, MSV, RelPartner, RelationshipMemberKey, SkgRepoName, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};

fn node (
  pid       : &str,
  extra_ids : &[&str],
  subscribes: &[&str],
  hides     : &[&str],
  overrides : &[&str],
) -> Graphnode {
  let mut node : Graphnode =
    empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . home_skgrepo = SkgRepoName::from ("main");
  node . extra_ids =
    extra_ids . iter () . map ( |skgid| ID::from (*skgid) ) . collect ();
  node . subscribesTo =
    if subscribes . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      subscribes . iter () . map ( |skgid| ID::from (*skgid) ) . collect ())) };
  node . hidesFromSubs =
    if hides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      hides . iter () . map ( |skgid| ID::from (*skgid) ) . collect ())) };
  node . overrides =
    if overrides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      overrides . iter () . map ( |skgid| ID::from (*skgid) ) . collect ())) };
  node }

fn node_with_all_relations (
  pid       : &str,
  contains  : &[&str],
  subscribes: &[&str],
  hides     : &[&str],
  overrides : &[&str],
  links : &[&str],
) -> Graphnode {
  let mut result : Graphnode =
    node (pid, &[], subscribes, hides, overrides);
  result . contains = rel_partners_at_relRepo (
    &result . home_skgrepo,
    contains . iter () . map (|skgid| ID::from (*skgid)) . collect ());
  if ! links . is_empty () {
    result . body = Some (links . iter ()
      . map (|skgid| format! ("[[id:{}][{}]]", skgid, skgid))
      . collect::<Vec<String>> () . join (" ")); }
  result
}

fn skgid_set (
  skgids : &[&str],
) -> std::collections::HashSet<ID> {
  skgids . iter () . map (|skgid| ID::from (*skgid)) . collect ()
}

#[test]
fn relation_accessors_return_both_membership_directions () {
  let graph : InRustGraph =
    InRustGraph::from_graphnodes (&[
      node ("recorder", &[], &["subscribee-alias"], &["hidden"], &["overridden"]),
      node ("subscribee", &["subscribee-alias"], &[], &[], &[]),
      node ("subscriber", &[], &["recorder"], &[], &[]),
      node ("hidden", &[], &[], &[], &[]),
      node ("hider", &[], &[], &["recorder"], &[]),
      node ("overridden", &[], &[], &[], &[]),
      node ("overrider", &[], &[], &[], &["recorder"]),
    ]);

  assert_eq!(
    graph . other_member_pids (
      &ID::from ("recorder"),
      RelationRole::new (NodeRelation::SubscribesTo, BinaryRolePosition::First)),
    vec![ID::from ("subscribee")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("recorder"),
      RelationRole::new (NodeRelation::SubscribesTo, BinaryRolePosition::Second)),
    vec![ID::from ("subscriber")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("recorder"),
      RelationRole::new (
        NodeRelation::HidesFromSubs, BinaryRolePosition::First)),
    vec![ID::from ("hidden")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("recorder"),
      RelationRole::new (
        NodeRelation::HidesFromSubs, BinaryRolePosition::Second)),
    vec![ID::from ("hider")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("recorder"),
      RelationRole::new (NodeRelation::Overrides, BinaryRolePosition::First)),
    vec![ID::from ("overridden")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("recorder"),
      RelationRole::new (NodeRelation::Overrides, BinaryRolePosition::Second)),
    vec![ID::from ("overrider")] ); }

#[test]
fn stored_outbound_accessor_retains_unresolved_raw_members () {
  let mut recorder : Graphnode = node (
    "recorder", &[], &[], &[], &[]);
  recorder . contains = vec! [
    RelPartner {
      member : ID::from ("known-extra"),
      relRepo : SkgRepoName::from ("main"), },
    RelPartner {
      member : ID::from ("absent-raw"),
      relRepo : SkgRepoName::from ("main"), },
  ];
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    recorder,
    node ("known", &["known-extra"], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . outbound_rel_partners_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, None ),
    vec! [
      RelPartner {
        member : ID::from ("known-extra"),
        relRepo : SkgRepoName::from ("main"), },
      RelPartner {
        member : ID::from ("absent-raw"),
        relRepo : SkgRepoName::from ("main"), },
    ] );
  assert_eq! (
    graph . outbound_skgids_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, None ),
    vec![ ID::from ("known-extra"), ID::from ("absent-raw") ] );
  assert_eq! (
    graph . outbound_pids_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, None ),
    vec![ID::from ("known")] ); }

#[test]
fn relationship_member_key_canonicalizes_only_resolved_skgids () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node ("known", &["known-extra"], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . relationship_member_key (&ID::from ("known-extra")),
    RelationshipMemberKey::ResolvedPid (ID::from ("known")));
  assert_eq! (
    graph . relationship_member_key (&ID::from ("absent-raw")),
    RelationshipMemberKey::UnresolvedRawId (ID::from ("absent-raw"))); }

#[test]
fn update_relevant_neighborhood_includes_each_ordinary_relation_both_ways () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node_with_all_relations (
      "seed", &["contained"], &["subscribee"], &["hidden"], &[],
      &["link-dest", "dangling-link"]),
    node_with_all_relations ("contained", &[], &[], &[], &[], &[]),
    node_with_all_relations ("container", &["seed"], &[], &[], &[], &[]),
    node_with_all_relations ("subscribee", &[], &[], &[], &[], &[]),
    node_with_all_relations ("subscriber", &[], &["seed"], &[], &[], &[]),
    node_with_all_relations ("hidden", &[], &[], &[], &[], &[]),
    node_with_all_relations ("hider", &[], &[], &["seed"], &[], &[]),
    node_with_all_relations ("link-dest", &[], &[], &[], &[], &[]),
    node_with_all_relations ("link-source", &[], &[], &[], &[], &["seed"]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood ([ID::from ("seed")]),
    skgid_set (&[
      "seed", "contained", "container", "subscribee", "subscriber",
      "hidden", "hider", "link-dest", "link-source", "dangling-link",
    ]));
}

#[test]
fn update_relevant_neighborhood_walks_override_directions_independently () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node_with_all_relations ("above-3", &[], &[], &[], &["above-2"], &[]),
    node_with_all_relations ("above-2", &[], &[], &[], &["seed"], &[]),
    node_with_all_relations ("above-branch", &[], &[], &[], &["seed"], &[]),
    node_with_all_relations ("seed", &[], &[], &[], &["below-1"], &[]),
    node_with_all_relations ("below-1", &[], &[], &[], &["below-2"], &[]),
    node_with_all_relations ("below-2", &[], &[], &[], &["below-1"], &[]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood ([ID::from ("seed")]),
    skgid_set (&[
      "seed", "above-2", "above-3", "above-branch", "below-1", "below-2",
    ]));
}

#[test]
fn update_relevant_neighborhood_neither_reverses_nor_mixes_paths () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node_with_all_relations ("above", &[], &[], &[], &["seed"], &[]),
    node_with_all_relations ("side-of-above", &["above"], &[], &[], &[], &[]),
    node_with_all_relations ("seed", &["ordinary"], &[], &[], &["below"], &[]),
    node_with_all_relations ("ordinary", &[], &[], &[], &["mixed"], &[]),
    node_with_all_relations ("below", &[], &[], &[], &[], &[]),
    node_with_all_relations ("reversal", &[], &[], &[], &["below"], &[]),
    node_with_all_relations ("two-hop", &["ordinary"], &[], &[], &[], &[]),
    node_with_all_relations ("mixed", &[], &[], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood ([ID::from ("seed")]),
    skgid_set (&["seed", "above", "ordinary", "below"]));
}

#[test]
fn update_relevant_neighborhood_canonicalizes_aliases_and_retains_unknowns () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node ("known", &["alias"], &[], &[], &[]),
    node_with_all_relations (
      "points-at-unknown", &["unknown"], &[], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood (
      [ID::from ("alias"), ID::from ("unknown")]),
    skgid_set (&["known", "unknown", "points-at-unknown"]));
}
