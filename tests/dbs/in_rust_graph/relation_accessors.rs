use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::relation_accessors::{
  NodeRelation,
  RelationRole,
};
use skg::types::misc::{ID, MSV, SourceName};
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
    else { MSV::Specified (
      subscribes . iter () . map ( |id| ID::from (*id) ) . collect ()) };
  node . hides_from_its_subscriptions =
    if hides . is_empty () { MSV::Unspecified }
    else { MSV::Specified (
      hides . iter () . map ( |id| ID::from (*id) ) . collect ()) };
  node . overrides_view_of =
    if overrides . is_empty () { MSV::Unspecified }
    else { MSV::Specified (
      overrides . iter () . map ( |id| ID::from (*id) ) . collect ()) };
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
      RelationRole::new (NodeRelation::Subscribes, "subscriber") . unwrap ()),
    vec![ID::from ("subscribee")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (NodeRelation::Subscribes, "subscribee") . unwrap ()),
    vec![ID::from ("subscriber")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (
        NodeRelation::HidesFromItsSubscriptions, "hider") . unwrap ()),
    vec![ID::from ("hidden")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (
        NodeRelation::HidesFromItsSubscriptions, "hidden") . unwrap ()),
    vec![ID::from ("hider")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (NodeRelation::OverridesViewOf, "overrider") . unwrap ()),
    vec![ID::from ("overridden")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("owner"),
      RelationRole::new (NodeRelation::OverridesViewOf, "overridden") . unwrap ()),
    vec![ID::from ("overrider")] ); }
