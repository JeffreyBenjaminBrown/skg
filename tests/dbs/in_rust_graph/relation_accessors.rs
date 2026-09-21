use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  NodeRelation,
  RelationRole,
};
use skg::types::misc::{ID, MSV, RelPartner, RelationshipMemberKey, SourceName, rel_partners_at_relSource};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};

fn node (
  pid       : &str,
  extra_ids : &[&str],
  subscribes: &[&str],
  hides     : &[&str],
  overrides : &[&str],
) -> NodeComplete {
  let mut node : NodeComplete =
    empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . source = SourceName::from ("main");
  node . extra_ids =
    extra_ids . iter () . map ( |id| ID::from (*id) ) . collect ();
  node . subscribes_to =
    if subscribes . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relSource (
      &node . source,
      subscribes . iter () . map ( |id| ID::from (*id) ) . collect ())) };
  node . hides_from_its_subscriptions =
    if hides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relSource (
      &node . source,
      hides . iter () . map ( |id| ID::from (*id) ) . collect ())) };
  node . overrides_view_of =
    if overrides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relSource (
      &node . source,
      overrides . iter () . map ( |id| ID::from (*id) ) . collect ())) };
  node }

fn node_with_all_relations (
  pid       : &str,
  contains  : &[&str],
  subscribes: &[&str],
  hides     : &[&str],
  overrides : &[&str],
  textlinks : &[&str],
) -> NodeComplete {
  let mut result : NodeComplete =
    node (pid, &[], subscribes, hides, overrides);
  result . contains = rel_partners_at_relSource (
    &result . source,
    contains . iter () . map (|id| ID::from (*id)) . collect ());
  if ! textlinks . is_empty () {
    result . body = Some (textlinks . iter ()
      . map (|id| format! ("[[id:{}][{}]]", id, id))
      . collect::<Vec<String>> () . join (" ")); }
  result
}

fn id_set (
  ids : &[&str],
) -> std::collections::HashSet<ID> {
  ids . iter () . map (|id| ID::from (*id)) . collect ()
}

#[test]
fn relation_accessors_return_both_membership_directions () {
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (&[
      node ("owner", &[], &["subscribee-alias"], &["hidden"], &["overridden"]),
      node ("subscribee", &["subscribee-alias"], &[], &[], &[]),
      node ("subscriber", &[], &["owner"], &[], &[]),
      node ("hidden", &[], &[], &[], &[]),
      node ("hider", &[], &[], &["owner"], &[]),
      node ("overridden", &[], &[], &[], &[]),
      node ("overrider", &[], &[], &[], &["owner"]),
    ]);

  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (NodeRelation::Subscribes, BinaryRolePosition::First)),
    vec![ID::from ("subscribee")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (NodeRelation::Subscribes, BinaryRolePosition::Second)),
    vec![ID::from ("subscriber")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (
        NodeRelation::HidesFromItsSubscriptions, BinaryRolePosition::First)),
    vec![ID::from ("hidden")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (
        NodeRelation::HidesFromItsSubscriptions, BinaryRolePosition::Second)),
    vec![ID::from ("hider")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (NodeRelation::OverridesViewOf, BinaryRolePosition::First)),
    vec![ID::from ("overridden")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (NodeRelation::OverridesViewOf, BinaryRolePosition::Second)),
    vec![ID::from ("overrider")] ); }

#[test]
fn stored_outbound_accessor_retains_unresolved_raw_members () {
  let mut owner : NodeComplete = node (
    "owner", &[], &[], &[], &[]);
  owner . contains = vec! [
    RelPartner {
      member : ID::from ("known-extra"),
      relSource : SourceName::from ("main"), },
    RelPartner {
      member : ID::from ("absent-raw"),
      relSource : SourceName::from ("main"), },
  ];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[
    owner,
    node ("known", &["known-extra"], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . outbound_rel_partners_for_relation_gated (
      &ID::from ("owner"), NodeRelation::Contains, None ),
    vec! [
      RelPartner {
        member : ID::from ("known-extra"),
        relSource : SourceName::from ("main"), },
      RelPartner {
        member : ID::from ("absent-raw"),
        relSource : SourceName::from ("main"), },
    ] );
  assert_eq! (
    graph . outbound_ids_for_relation_gated (
      &ID::from ("owner"), NodeRelation::Contains, None ),
    vec![ ID::from ("known-extra"), ID::from ("absent-raw") ] );
  assert_eq! (
    graph . outbound_pids_for_relation_gated (
      &ID::from ("owner"), NodeRelation::Contains, None ),
    vec![ID::from ("known")] ); }

#[test]
fn relationship_member_key_canonicalizes_only_resolved_ids () {
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[
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
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[
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
    id_set (&[
      "seed", "contained", "container", "subscribee", "subscriber",
      "hidden", "hider", "link-dest", "link-source", "dangling-link",
    ]));
}

#[test]
fn update_relevant_neighborhood_walks_override_directions_independently () {
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[
    node_with_all_relations ("above-3", &[], &[], &[], &["above-2"], &[]),
    node_with_all_relations ("above-2", &[], &[], &[], &["seed"], &[]),
    node_with_all_relations ("above-branch", &[], &[], &[], &["seed"], &[]),
    node_with_all_relations ("seed", &[], &[], &[], &["below-1"], &[]),
    node_with_all_relations ("below-1", &[], &[], &[], &["below-2"], &[]),
    node_with_all_relations ("below-2", &[], &[], &[], &["below-1"], &[]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood ([ID::from ("seed")]),
    id_set (&[
      "seed", "above-2", "above-3", "above-branch", "below-1", "below-2",
    ]));
}

#[test]
fn update_relevant_neighborhood_neither_reverses_nor_mixes_paths () {
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[
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
    id_set (&["seed", "above", "ordinary", "below"]));
}

#[test]
fn update_relevant_neighborhood_canonicalizes_aliases_and_retains_unknowns () {
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[
    node ("known", &["alias"], &[], &[], &[]),
    node_with_all_relations (
      "points-at-unknown", &["unknown"], &[], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood (
      [ID::from ("alias"), ID::from ("unknown")]),
    id_set (&["known", "unknown", "points-at-unknown"]));
}
