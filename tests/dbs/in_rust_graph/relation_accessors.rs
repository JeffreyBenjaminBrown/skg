use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  GnidPresence,
  NodeRelation,
  RelationRole,
};
use skg::types::misc::{ID, MSV, RelPartner, RelationshipMemberKey, SkgrepoName, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName};

use std::collections::BTreeSet;

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
  node . home_skgrepo = SkgrepoName::from ("main");
  node . extra_ids =
    extra_ids . iter () . map ( |gnid| ID::from (*gnid) ) . collect ();
  node . subscribesTo =
    if subscribes . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      subscribes . iter () . map ( |gnid| ID::from (*gnid) ) . collect ())) };
  node . hidesFromSubs =
    if hides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      hides . iter () . map ( |gnid| ID::from (*gnid) ) . collect ())) };
  node . overrides =
    if overrides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      overrides . iter () . map ( |gnid| ID::from (*gnid) ) . collect ())) };
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
    contains . iter () . map (|gnid| ID::from (*gnid)) . collect ());
  if ! links . is_empty () {
    result . body = Some (links . iter ()
      . map (|gnid| format! ("[[id:{}][{}]]", gnid, gnid))
      . collect::<Vec<String>> () . join (" ")); }
  result
}

fn gnid_set (
  gnids : &[&str],
) -> std::collections::HashSet<ID> {
  gnids . iter () . map (|gnid| ID::from (*gnid)) . collect ()
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
      relRepo : SkgrepoName::from ("main"), },
    RelPartner {
      member : ID::from ("absent-raw"),
      relRepo : SkgrepoName::from ("main"), },
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
        relRepo : SkgrepoName::from ("main"), },
      RelPartner {
        member : ID::from ("absent-raw"),
        relRepo : SkgrepoName::from ("main"), },
    ] );
  assert_eq! (
    graph . outbound_gnids_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, None ),
    vec![ ID::from ("known-extra"), ID::from ("absent-raw") ] );
  assert_eq! (
    graph . outbound_pids_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, None ),
    vec![ID::from ("known")] );
  assert_eq! (
    graph . outbound_member_keys_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, None ),
    vec! [
      RelPartner {
        member : RelationshipMemberKey::ResolvedPid (ID::from ("known")),
        relRepo : SkgrepoName::from ("main"), },
      RelPartner {
        member : RelationshipMemberKey::UnresolvedRawId (ID::from ("absent-raw")),
        relRepo : SkgrepoName::from ("main"), },
    ] ); }

#[test]
fn relationship_member_key_canonicalizes_only_resolved_gnids () {
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
    gnid_set (&[
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
    gnid_set (&[
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
    gnid_set (&["seed", "above", "ordinary", "below"]));
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
    gnid_set (&["known", "unknown", "points-at-unknown"]));
}

// A node the restriction excludes is restricted, by its pid or an extra
// ID; without a restriction, or under 'all', it is present; a gnid
// no node answers to is absent, whatever the restriction.
#[test]
fn gnid_presence_gated_tells_hidden_from_absent () {
  let mut private : Graphnode = node ("private", &["private-extra"], &[], &[], &[]);
  private . home_skgrepo = SkgrepoName::from ("private");
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node ("public", &[], &[], &[], &[]), private ]);
  let only_main : SkgrepoRestriction = SkgrepoRestriction {
    name     : SkgrepoSetName ("main" . to_string ()),
    skgrepos : BTreeSet::from ([ SkgrepoName::from ("main") ]) };
  let all : SkgrepoRestriction = SkgrepoRestriction {
    name     : SkgrepoSetName ("all" . to_string ()),
    skgrepos : BTreeSet::new () };
  let presence = |gnid : &str, restriction : Option<&SkgrepoRestriction>|
    graph . gnid_presence_gated (&ID::from (gnid), restriction);
  assert_eq! ( presence ("public", Some (&only_main)), GnidPresence::Present );
  assert_eq! ( presence ("private", Some (&only_main)), GnidPresence::Restricted );
  assert_eq! ( presence ("private-extra", Some (&only_main)), GnidPresence::Restricted );
  assert_eq! ( presence ("private", Some (&all)), GnidPresence::Present );
  assert_eq! ( presence ("private", None), GnidPresence::Present );
  assert_eq! ( presence ("nowhere", Some (&only_main)), GnidPresence::Absent );
  assert_eq! ( presence ("nowhere", None), GnidPresence::Absent ); }

// A record is visible when the restriction holds its relRepo, whatever
// its member: the buffer shows an unresolvable member as a reference
// to an unknown node, and a member whose home is restricted as a
// restricted headline.
#[test]
fn a_record_is_visible_when_the_restriction_holds_its_relRepo () {
  let only_main : SkgrepoRestriction = SkgrepoRestriction {
    name     : SkgrepoSetName ("main" . to_string ()),
    skgrepos : BTreeSet::from ([ SkgrepoName::from ("main") ]) };
  assert! ( SkgrepoRestriction::record_is_visible (
    &only_main, &SkgrepoName::from ("main") ));
  assert! ( ! SkgrepoRestriction::record_is_visible (
    &only_main, &SkgrepoName::from ("private") ));
  assert! ( SkgrepoRestriction::record_is_visible (
    None, &SkgrepoName::from ("private") ));
  assert! ( SkgrepoRestriction::node_is_restricted (
    &only_main, &SkgrepoName::from ("private") ));
  assert! ( ! SkgrepoRestriction::node_is_restricted (
    Some (&only_main), &SkgrepoName::from ("main") ));
  assert! ( ! SkgrepoRestriction::node_is_restricted (
    None, &SkgrepoName::from ("private") )); }

// Under 'all', a record in a skgrepo the config does not name is
// visible, and a node homed there is not restricted, on both sides:
// the render side's gated lists ('outbound_rel_partners_for_relation_gated')
// and the save's weave ('SkgrepoRestriction::record_is_visible') answer
// alike. The set 'all' here names no skgrepo at all.
#[test]
fn an_unnamed_relRepo_under_all_answers_alike_on_both_sides () {
  let mut recorder : Graphnode = node ("recorder", &[], &[], &[], &[]);
  recorder . contains = vec! [ RelPartner::at_relRepo (
    SkgrepoName::from ("unnamed"), ID::from ("member") ) ];
  let mut member : Graphnode = node ("member", &[], &[], &[], &[]);
  member . home_skgrepo = SkgrepoName::from ("unnamed");
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[ recorder, member ]);
  let all : SkgrepoRestriction = SkgrepoRestriction {
    name     : SkgrepoSetName ("all" . to_string ()),
    skgrepos : BTreeSet::new () };
  let shown_by_the_render : bool =
    graph . outbound_rel_partners_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, Some (&all) )
    . iter () . any ( |partner| partner . member == ID::from ("member") );
  let visible_to_the_save : bool =
    SkgrepoRestriction::record_is_visible (&all, &SkgrepoName::from ("unnamed"));
  assert! (shown_by_the_render);
  assert_eq! (shown_by_the_render, visible_to_the_save);
  assert_eq! ( graph . gnid_presence_gated (&ID::from ("member"), Some (&all)),
               GnidPresence::Present );
  assert! ( ! SkgrepoRestriction::node_is_restricted (
    &all, &SkgrepoName::from ("unnamed") )); }
