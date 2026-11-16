use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::query::find_related_nodes;
use skg::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition,
  GnidPresence,
  NodeRelation,
  RelationRole,
  UPDATE_RELEVANT_ORDINARY_RELATIONS,
};
use skg::types::misc::{ID, MSV, RelPartner, RelationshipMemberKey, SkgConfig, SkgrepoName, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::skgrepo_sets::SkgrepoRestriction;

use std::collections::HashMap;

/// The skgrepo restriction to 'main', over the privacy order 'main',
/// 'private'.
fn only_main () -> SkgrepoRestriction {
  SkgrepoRestriction::of_privacy_order_for_test (&["main", "private"], "main") }

/// No skgrepo restriction over the privacy order 'main', 'private'.
fn unrestricted_over_main_and_private () -> SkgrepoRestriction {
  SkgrepoRestriction::of_privacy_order_for_test (&["main", "private"], "private") }

/// No skgrepo restriction, for a graph built without a config.
fn unrestricted () -> SkgrepoRestriction {
  SkgrepoRestriction::unrestricted (
    &SkgConfig::dummyFromSkgrepos (HashMap::new ())) }

fn node (
  pid       : &str,
  extra_ids : &[&str],
  forks     : &[&str],
  hides     : &[&str],
) -> Graphnode {
  let mut node : Graphnode =
    empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . home_skgrepo = SkgrepoName::from ("main");
  node . extra_ids =
    extra_ids . iter () . map ( |gnid| ID::from (*gnid) ) . collect ();
  node . forks =
    if forks . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      forks . iter () . map ( |gnid| ID::from (*gnid) ) . collect ())) };
  node . hidesFromSubs =
    if hides . is_empty () { MSV::Unspecified }
    else { MSV::Specified ( rel_partners_at_relRepo (
      &node . home_skgrepo,
      hides . iter () . map ( |gnid| ID::from (*gnid) ) . collect ())) };
  node }

fn node_with_all_relations (
  pid       : &str,
  contains  : &[&str],
  forks     : &[&str],
  hides     : &[&str],
  links : &[&str],
) -> Graphnode {
  let mut result : Graphnode =
    node (pid, &[], forks, hides);
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
      node ("recorder", &[], &["forkee-alias"], &["hidden"]),
      node ("forkee", &["forkee-alias"], &[], &[]),
      node ("forker", &[], &["recorder"], &[]),
      node ("hidden", &[], &[], &[]),
      node ("hider", &[], &[], &["recorder"]),
    ]);

  assert_eq!(
    graph . other_member_pids (
      &ID::from ("recorder"),
      RelationRole::new (NodeRelation::Forks, BinaryRolePosition::First)),
    vec![ID::from ("forkee")] );
  assert_eq!(
    graph . other_member_pids (
      &ID::from ("recorder"),
      RelationRole::new (NodeRelation::Forks, BinaryRolePosition::Second)),
    vec![ID::from ("forker")] );
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
    vec![ID::from ("hider")] ); }

#[test]
fn stored_outbound_accessor_retains_unresolved_raw_members () {
  let mut recorder : Graphnode = node (
    "recorder", &[], &[], &[]);
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
    node ("known", &["known-extra"], &[], &[]),
  ]);
  assert_eq! (
    graph . outbound_rel_partners_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, &unrestricted () ),
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
      &ID::from ("recorder"), NodeRelation::Contains, &unrestricted () ),
    vec![ ID::from ("known-extra"), ID::from ("absent-raw") ] );
  assert_eq! (
    graph . outbound_pids_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, &unrestricted () ),
    vec![ID::from ("known")] );
  assert_eq! (
    graph . outbound_member_keys_for_relation_gated (
      &ID::from ("recorder"), NodeRelation::Contains, &unrestricted () ),
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
    node ("known", &["known-extra"], &[], &[]),
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
      "seed", &["contained"], &["forkee"], &["hidden"],
      &["link-dest", "dangling-link"]),
    node_with_all_relations ("contained", &[], &[], &[], &[]),
    node_with_all_relations ("container", &["seed"], &[], &[], &[]),
    node_with_all_relations ("forkee", &[], &[], &[], &[]),
    node_with_all_relations ("forker", &[], &["seed"], &[], &[]),
    node_with_all_relations ("hidden", &[], &[], &[], &[]),
    node_with_all_relations ("hider", &[], &[], &["seed"], &[]),
    node_with_all_relations ("link-dest", &[], &[], &[], &[]),
    node_with_all_relations ("link-source", &[], &[], &[], &["seed"]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood ([ID::from ("seed")]),
    gnid_set (&[
      "seed", "contained", "container", "forkee", "forker",
      "hidden", "hider", "link-dest", "link-source", "dangling-link",
    ]));
}

/// The neighborhood over the ordinary relations but links, as the
/// dirty-view check reads it: no link partner in either direction, but
/// every other ordinary partner and the mirrors.
#[test]
fn update_relevant_neighborhood_over_relations_without_links_omits_link_partners () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node_with_all_relations (
      "seed", &["contained"], &["forkee"], &[], &["link-dest"]),
    node_with_all_relations ("contained", &[], &[], &[], &[]),
    node_with_all_relations ("forkee", &[], &[], &[], &[]),
    node_with_all_relations ("link-dest", &[], &[], &[], &[]),
    node_with_all_relations ("link-source", &[], &[], &[], &["seed"]),
  ]);
  let ordinary_relations_without_links : Vec<NodeRelation> =
    UPDATE_RELEVANT_ORDINARY_RELATIONS . iter () . copied ()
    . filter (|relation| *relation != NodeRelation::LinksTo)
    . collect ();
  assert_eq! (
    graph . update_relevant_neighborhood_over (
      [ID::from ("seed")], &ordinary_relations_without_links),
    gnid_set (&["seed", "contained", "forkee"]));
  assert_eq! (
    graph . update_relevant_neighborhood ([ID::from ("seed")]),
    gnid_set (&[
      "seed", "contained", "forkee", "link-dest", "link-source"]));
}

/// Every mirror of the seed: its forkees and forkers, transitively, a
/// cycle included.
#[test]
fn update_relevant_neighborhood_holds_every_mirror_of_the_seed () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node_with_all_relations ("above-3", &[], &["above-2"], &[], &[]),
    node_with_all_relations ("above-2", &[], &["seed"], &[], &[]),
    node_with_all_relations ("above-branch", &[], &["seed"], &[], &[]),
    node_with_all_relations ("seed", &[], &["below-1"], &[], &[]),
    node_with_all_relations ("below-1", &[], &["below-2"], &[], &[]),
    node_with_all_relations ("below-2", &[], &["below-1"], &[], &[]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood ([ID::from ("seed")]),
    gnid_set (&[
      "seed", "above-2", "above-3", "above-branch", "below-1", "below-2",
    ]));
}

/// A path that leaves the mirrors by an ordinary relation, or enters
/// them only after one, adds nothing past its first hop: the mirrors
/// of a one-hop neighbor are not the seed's. A sibling fork, which
/// forks what the seed forks, is a mirror of the seed.
#[test]
fn update_relevant_neighborhood_mixes_no_paths () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node_with_all_relations ("above", &[], &["seed"], &[], &[]),
    node_with_all_relations ("side-of-above", &["above"], &[], &[], &[]),
    node_with_all_relations ("seed", &["ordinary"], &["below"], &[], &[]),
    node_with_all_relations ("ordinary", &[], &["mixed"], &[], &[]),
    node_with_all_relations ("below", &[], &[], &[], &[]),
    node_with_all_relations ("sibling", &[], &["below"], &[], &[]),
    node_with_all_relations ("two-hop", &["ordinary"], &[], &[], &[]),
    node_with_all_relations ("mixed", &[], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood ([ID::from ("seed")]),
    gnid_set (&["seed", "above", "ordinary", "below", "sibling"]));
}

/// A change to one fork of a note reaches a view showing another fork
/// of it, a sibling, in either graph: which of them stands in the
/// note's place can change.
#[test]
fn update_relevant_neighborhood_of_a_fork_holds_its_sibling () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node_with_all_relations ("original", &[], &[], &[], &[]),
    node_with_all_relations ("changed-fork", &[], &["original"], &[], &[]),
    node_with_all_relations ("shown-fork", &[], &["original"], &[], &[]),
  ]);
  assert! (
    graph . update_relevant_neighborhood ([ID::from ("changed-fork")])
      . contains (&ID::from ("shown-fork")));
}

#[test]
fn update_relevant_neighborhood_canonicalizes_aliases_and_retains_unknowns () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node ("known", &["alias"], &[], &[]),
    node_with_all_relations (
      "points-at-unknown", &["unknown"], &[], &[], &[]),
  ]);
  assert_eq! (
    graph . update_relevant_neighborhood (
      [ID::from ("alias"), ID::from ("unknown")]),
    gnid_set (&["known", "unknown", "points-at-unknown"]));
}

// A node the skgrepo restriction excludes is restricted, by its pid or
// an extra ID; without a skgrepo restriction, or under 'all', it is
// present; a gnid no node answers to is absent, whatever the
// skgrepo restriction.
#[test]
fn gnid_presence_gated_tells_hidden_from_absent () {
  let mut private : Graphnode = node ("private", &["private-extra"], &[], &[]);
  private . home_skgrepo = SkgrepoName::from ("private");
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node ("public", &[], &[], &[]), private ]);
  let only_main : SkgrepoRestriction = only_main ();
  let all : SkgrepoRestriction = unrestricted_over_main_and_private ();
  let presence = |gnid : &str, restriction : &SkgrepoRestriction|
    graph . gnid_presence_gated (&ID::from (gnid), restriction);
  assert_eq! ( presence ("public", &only_main), GnidPresence::Present );
  assert_eq! ( presence ("private", &only_main), GnidPresence::Restricted );
  assert_eq! ( presence ("private-extra", &only_main), GnidPresence::Restricted );
  assert_eq! ( presence ("private", &all), GnidPresence::Present );
  assert_eq! ( presence ("nowhere", &only_main), GnidPresence::Absent );
  assert_eq! ( presence ("nowhere", &all), GnidPresence::Absent ); }

// A record is visible when the skgrepo restriction holds its relRepo,
// whatever its member: the buffer shows an unresolvable member as a
// reference to an unknown node, and a member whose home is restricted
// as a restricted headline.
#[test]
fn a_record_is_visible_when_the_restriction_holds_its_relRepo () {
  let only_main : SkgrepoRestriction = only_main ();
  assert! ( SkgrepoRestriction::record_is_visible (
    &only_main, &SkgrepoName::from ("main") ));
  assert! ( ! SkgrepoRestriction::record_is_visible (
    &only_main, &SkgrepoName::from ("private") ));
  assert! ( SkgrepoRestriction::record_is_visible (
    &unrestricted (), &SkgrepoName::from ("private") ));
  assert! ( SkgrepoRestriction::node_is_restricted (
    &only_main, &SkgrepoName::from ("private") ));
  assert! ( ! SkgrepoRestriction::node_is_restricted (
    &only_main, &SkgrepoName::from ("main") ));
  assert! ( ! SkgrepoRestriction::node_is_restricted (
    &unrestricted (), &SkgrepoName::from ("private") )); }

// A record in a skgrepo the config does not name is visible under
// every skgrepo restriction, on both sides: the render side's gated
// lists ('outbound_rel_partners_for_relation_gated') and the save's
// weave ('SkgrepoRestriction::record_is_visible') answer alike.
#[test]
fn a_record_in_a_skgrepo_the_config_does_not_name_is_visible_under_every_restriction () {
  let mut recorder : Graphnode = node ("recorder", &[], &[], &[]);
  recorder . contains = vec! [ RelPartner::at_relRepo (
    SkgrepoName::from ("unnamed"), ID::from ("member") ) ];
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    recorder, node ("member", &[], &[], &[]) ]);
  for restriction in [ only_main (), unrestricted_over_main_and_private () ] {
    let shown_by_the_render : bool =
      graph . outbound_rel_partners_for_relation_gated (
        &ID::from ("recorder"), NodeRelation::Contains, &restriction )
      . iter () . any ( |partner| partner . member == ID::from ("member") );
    assert! ( shown_by_the_render,
              "under {}", restriction . most_private_shown );
    assert! ( SkgrepoRestriction::record_is_visible (
                &restriction, &SkgrepoName::from ("unnamed") ),
              "under {}", restriction . most_private_shown ); }}

// A graphnode homed in a skgrepo the config does not name is
// restricted under no skgrepo restriction: the gated graph shows it.
#[test]
fn a_graphnode_homed_in_a_skgrepo_the_config_does_not_name_is_never_restricted () {
  let mut member : Graphnode = node ("member", &[], &[], &[]);
  member . home_skgrepo = SkgrepoName::from ("unnamed");
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[ member ]);
  for restriction in [ only_main (), unrestricted_over_main_and_private () ] {
    assert! ( ! SkgrepoRestriction::node_is_restricted (
                &restriction, &SkgrepoName::from ("unnamed") ),
              "under {}", restriction . most_private_shown );
    assert_eq! ( graph . gnid_presence_gated (&ID::from ("member"), &restriction),
                 GnidPresence::Present,
                 "under {}", restriction . most_private_shown ); }}

/// A relation or role name no relation has is a caller's bug, and
/// 'find_related_nodes' says so rather than answering nothing.
#[test]
#[should_panic (expected = "unknown triple (subscribesTo, subscriber, subscribee)")]
fn find_related_nodes_panics_on_a_name_no_relation_has () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node ("recorder", &[], &[], &[]) ]);
  find_related_nodes (
    &graph, &[ID::from ("recorder")],
    "subscribesTo", "subscriber", "subscribee" ); }
