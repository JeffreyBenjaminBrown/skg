use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  NodeRelation,
  RelationRole,
};
use skg::types::misc::{ID, MSV, MemberAtSource, RelationshipMemberKey, SourceName, members_at_source};
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
    else { MSV::Specified ( members_at_source (
      &node . source,
      subscribes . iter () . map ( |id| ID::from (*id) ) . collect ())) };
  node . hides_from_its_subscriptions =
    if hides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( members_at_source (
      &node . source,
      hides . iter () . map ( |id| ID::from (*id) ) . collect ())) };
  node . overrides_view_of =
    if overrides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( members_at_source (
      &node . source,
      overrides . iter () . map ( |id| ID::from (*id) ) . collect ())) };
  node }

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
    MemberAtSource {
      member : ID::from ("known-extra"),
      source : SourceName::from ("main"), },
    MemberAtSource {
      member : ID::from ("absent-raw"),
      source : SourceName::from ("main"), },
  ];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[
    owner,
    node ("known", &["known-extra"], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . outbound_members_at_sources_for_relation_gated (
      &ID::from ("owner"), NodeRelation::Contains, None ),
    vec! [
      MemberAtSource {
        member : ID::from ("known-extra"),
        source : SourceName::from ("main"), },
      MemberAtSource {
        member : ID::from ("absent-raw"),
        source : SourceName::from ("main"), },
    ] );
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
